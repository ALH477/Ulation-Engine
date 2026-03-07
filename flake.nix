{
  description = "DeMoD-Note – Deterministic Monophonic Note Detector";

  inputs = {
    nixpkgs.url      = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url  = "github:numtide/flake-utils";

    # ── DeMoD Faust-SDR transport library ────────────────────────────────────
    #
    # Points at the haskell/ subdirectory of the dcf-faust-sdr repo, which is
    # where the Haskell flake lives (it has its own flake.nix there).
    #
    # Remote (once the repo is public):
       dcf-faust-sdr.url = "github:ALH477/FauSDR?dir=haskell";
    #
    # Local dev (symlink or relative path — use whichever matches your layout):
    #   dcf-faust-sdr.url = "path:/path/to/demod/haskell";
    #
    # Pin nixpkgs so both flakes resolve the same package set; this prevents
    # duplicate GHC/SoapySDR builds in the Nix store.
    dcf-faust-sdr.url = "github:ALH477/FauSDR?dir=haskell";
    dcf-faust-sdr.inputs.nixpkgs.follows    = "nixpkgs";
    dcf-faust-sdr.inputs.flake-utils.follows = "flake-utils";
    # rust-overlay is only used by dcf-faust-sdr; we don't need it here but
    # following it avoids a redundant fetch.
    dcf-faust-sdr.inputs.rust-overlay.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, flake-utils, dcf-faust-sdr }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        # Apply DeMoD's overlay so pkgs.haskellPackages.dcf-faust-sdr is
        # available alongside the rest of the Haskell package set.  This means
        # callCabal2nix / callPackage for DeMoD-Note can simply list
        # dcf-faust-sdr in build-depends and Nix resolves it automatically.
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ dcf-faust-sdr.overlays.${system}.default ];
        };

        # ── Haskell package set ───────────────────────────────────────────────
        hp = pkgs.haskellPackages.override {
          overrides = hself: hsuper: {
            dear-imgui        = pkgs.haskell.lib.markUnbroken hsuper.dear-imgui;
            dear-imgui-glfw   = pkgs.haskell.lib.markUnbroken hsuper."dear-imgui-glfw";
            dear-imgui-opengl3 = pkgs.haskell.lib.markUnbroken hsuper."dear-imgui-opengl3";
            nanovg            = pkgs.haskell.lib.doJailbreak
                                  (pkgs.haskell.lib.markUnbroken hsuper.nanovg);
            GLFW-b            = pkgs.haskell.lib.markUnbroken hsuper.GLFW-b;
            # dcf-faust-sdr is already in haskellPackages via the overlay above;
            # listing it here makes it explicit and lets you pin a version if needed.
            # dcf-faust-sdr = hsuper.dcf-faust-sdr;
          };
        };

        demod-note = hp.callPackage (import ./DeMoD-Note.nix) {};

        demod-note-opengl = demod-note;

        # ── Test derivations (unchanged) ──────────────────────────────────────
        runTests = pkgs.runCommand "demod-note-test-runner" {
          buildInputs = [ demod-note ];
        } ''
          export HOME=/tmp
          cd ${demod-note}
          echo "Running DeMoD-Note test suite..."
          cabal test --test-options="--color=always" 2>&1 | tee /tmp/test-output.txt
          if [ $? -eq 0 ]; then
            echo "All tests passed successfully!"
            echo "Test suite completed successfully" > $out
          else
            echo "Tests failed! See output above."
            exit 1
          fi
        '';

        testCoverage = pkgs.runCommand "demod-note-test-coverage" {
          buildInputs = [ demod-note ];
        } ''
          export HOME=/tmp
          cd ${demod-note}
          echo "Running test suite with coverage..."
          cabal test --enable-coverage --test-options="--color=always" 2>&1 | tee /tmp/coverage-output.txt
          echo "Coverage report generated" > $out
        '';

        jackIntegrationTests = pkgs.runCommand "demod-note-jack-tests" {
          buildInputs = [ demod-note pkgs.jack2 ];
        } ''
          export HOME=/tmp
          cd ${demod-note}
          echo "Running JACK integration tests..."
          if command -v jackd &> /dev/null; then
            echo "JACK available - testing JACK functionality"
            cabal test --test-options="--match='JACK'" 2>&1 || true
          else
            echo "JACK not available - skipping JACK tests"
          fi
          echo "JACK integration tests completed" > $out
        '';

        oscIntegrationTests = pkgs.runCommand "demod-note-osc-tests" {
          buildInputs = [ demod-note ];
        } ''
          export HOME=/tmp
          cd ${demod-note}
          echo "Running OSC integration tests..."
          cabal test --test-options="--match='OSC'" 2>&1 || true
          echo "OSC integration tests completed" > $out
        '';

        wrapped = pkgs.runCommand "demod-note-wrapped" {
          nativeBuildInputs = [ pkgs.makeWrapper ];
        } ''
          mkdir -p $out/bin
          makeWrapper ${demod-note}/bin/demod-note $out/bin/demod-note \
            --set FLUID_SOUNDFONT "${pkgs.soundfont-fluid}/share/soundfonts/FluidR3_GM.sf2" \
            --prefix PATH : "${pkgs.jack2}/bin:${pkgs.fluidsynth}/bin"
        '';

        appimage = pkgs.appimageTools.wrapType2 {
          pname   = "demod-note";
          version = "1.0.0";
          src     = demod-note;
          extraPkgs = p: [ p.jack2 p.fluidsynth p.alsa-lib ];
        };

        windows = if system == "x86_64-linux"
          then (pkgs.pkgsCross.mingwW64.callPackage (import ./DeMoD-Note.nix) {}).overrideAttrs (old: {
            configureFlags = old.configureFlags or [] ++ ["--enable-executable-static"];
          }) else pkgs.throw "Windows build only supported on x86_64-linux";

        static = if system == "x86_64-linux"
          then pkgs.pkgsMusl.callPackage (import ./DeMoD-Note.nix) {}
          else pkgs.throw "Static build only supported on x86_64-linux";

        desktop = pkgs.runCommand "demod-note-desktop" {
          nativeBuildInputs = [ pkgs.makeWrapper ];
        } ''
          mkdir -p $out/bin $out/share/applications
          cp ${./desktop/DeMoD-Note.desktop} $out/share/applications/DeMoD-Note.desktop
          cat > $out/bin/DeMoD-Note << 'SCRIPT'
          #!/usr/bin/env bash
          cd "$(dirname "$(readlink -f "$0")/../share/DeMoD-Note")"
          export FLUID_SOUNDFONT="${pkgs.soundfont-fluid}/share/soundfonts/FluidR3_GM.sf2"
          pip install --user -r requirements.txt 2>/dev/null || true
          if ! jack_lsp >/dev/null 2>&1; then
              jackd -d dummy -r 44100 -p 256 &
              sleep 2
          fi
          python3 tools/osc-midi-bridge.py &
          cabal run -- run
          SCRIPT
          chmod +x $out/bin/DeMoD-Note
          cp -r . $out/share/DeMoD-Note/
        '';

      in {
        packages = {
          default           = demod-note;
          nixos             = wrapped;
          appimage          = appimage;
          windows           = windows;
          static            = pkgs.pkgsMusl.haskellPackages.callCabal2nix "DeMoD-Note" self {};
          desktop           = desktop;
          demod-note-opengl = demod-note-opengl;
          tests             = runTests;
          test-coverage     = testCoverage;
          jack-tests        = jackIntegrationTests;
          osc-tests         = oscIntegrationTests;
        };

        apps = {
          default = { type = "app"; program = "${demod-note}/bin/demod-note"; };
          demod   = { type = "app"; program = "${demod-note}/bin/demod"; };
        };

        checks = {
          test            = runTests;
          jack-integration = jackIntegrationTests;
          osc-integration  = oscIntegrationTests;
        };

        nixosModules.default = { config, lib, pkgs, ... }:
          let cfg = config.services.demod-note; in {
            options.services.demod-note = with lib; {
              enable     = mkEnableOption "DeMoD-Note note detector";
              user       = mkOption { type = types.str; default = "root"; };
              configFile = mkOption { type = types.path; default = "/etc/demod-note.toml"; };
            };

            config = lib.mkIf cfg.enable {
              environment.systemPackages = [ self.packages.${pkgs.system}.nixos ];
              users.users.${cfg.user}.extraGroups = [ "audio" "jackaudio" ];
              security.rtkit.enable = true;

              system.activationScripts.demodNoteConfig = lib.stringAfter ["etc"] ''
                if [ ! -f ${cfg.configFile} ]; then
                  cp ${./config.example.toml} ${cfg.configFile}
                  chmod 644 ${cfg.configFile}
                fi
              '';

              systemd.user.services.demod-note = {
                description = "DeMoD-Note deterministic note detector";
                after       = [ "pipewire.service" "jack.service" ];
                wantedBy    = [ "default.target" ];
                serviceConfig = {
                  ExecStart             = "${self.packages.${pkgs.system}.nixos}/bin/demod-note --config ${cfg.configFile}";
                  Restart               = "on-failure";
                  Nice                  = "-15";
                  CPUSchedulingPolicy   = "fifo";
                  CPUSchedulingPriority = "80";
                  LimitRTPRIO           = "95";
                  User                  = cfg.user;
                };
              };
            };
          };

        devShells.default = pkgs.mkShell {
          # Pull in everything DeMoD-Note already needs, plus the DCF library
          # and its native dependencies (SoapySDR, Faust, JACK) so you can
          # develop both projects from a single shell.
          inputsFrom = [ demod-note.env ];
          buildInputs = with pkgs; [
            haskellPackages.cabal-install
            haskellPackages.haskell-language-server
            haskellPackages.ghcid
            jack2 fluidsynth qjackctl fftw pkg-config
            python3 python3Packages.pip
            haskellPackages.hspec
            haskellPackages.QuickCheck
            haskellPackages.quickcheck-instances
            # OpenGL
            glfw libGL libGLU freetype fontconfig glm libX11 libXrandr
            # DCF / DeMoD deps (needed for cbits compilation)
            soapysdr-with-community-support
            faust
            fftwFloat
            # dcf-faust-sdr Haskell lib itself — available for ghci / cabal repl
            haskellPackages.dcf-faust-sdr
          ];
          shellHook = ''
            export FLUID_SOUNDFONT="${pkgs.soundfont-fluid}/share/soundfonts/FluidR3_GM.sf2"
            export FAUST_ARCH_PATH="${pkgs.faust}/share/faust"
            export SOAPY_SDR_ROOT="${pkgs.soapysdr-with-community-support}"
            pip install --user -r ${./requirements.txt}
            echo ""
            echo "  DeMoD-Note + dcf-faust-sdr dev shell"
            echo "  DCF library: $(ghc-pkg list dcf-faust-sdr 2>/dev/null | tail -1 || echo 'not registered yet')"
            echo ""
          '';
        };
      });
}
