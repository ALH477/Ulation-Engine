-- | DeMoDNote.Intro
--
-- Shared intro-sequence assets used by both the TUI (DeMoD.hs / Brick) and
-- the OpenGL front-end (Opengl.hs).  Previously these ~80 lines were
-- copy-pasted verbatim in both modules; any change to the intro sequence now
-- only needs to be made here.

module DeMoDNote.Intro
    ( -- * Sierpiński triangle
      renderSierpinski
    , buildSierpinskiRow
      -- * ASCII banners
    , presentsLines
    , presentsText
    , presentsLength
    , presentsWidth
    , astartLines
      -- * Time-based depth
    , timeToDepthAndChar
    ) where

import Data.Time.Clock   (UTCTime, utctDayTime)
import Data.Time.LocalTime (timeToTimeOfDay, todHour, todMin, todSec)

-- ────────────────────────────────────────────────────────────────
-- Sierpiński triangle renderer
-- ────────────────────────────────────────────────────────────────

-- | Render a Sierpiński triangle of the given depth using fillChar.
-- Output is a list of strings, each padded to centre in an 80-column terminal.
renderSierpinski :: Int -> Char -> [String]
renderSierpinski depth fillChar =
  let size = 2 ^ depth
      pad  = max 0 ((80 - (2 * size - 1)) `div` 2)
      padS = replicate pad ' '
  in [ padS ++ buildSierpinskiRow size r fillChar | r <- [0 .. size - 1] ]

-- | Build a single row of the triangle.
buildSierpinskiRow :: Int -> Int -> Char -> String
buildSierpinskiRow size r fillChar =
  [ cell r c | c <- [0 .. 2 * size - 2] ]
  where
    cell row col =
      let k = col - (size - 1 - row)
      in if k >= 0 && k <= row && (row .&. k) == k then fillChar else ' '

-- ────────────────────────────────────────────────────────────────
-- "DeMoD-Note presents" banner
-- ────────────────────────────────────────────────────────────────

presentsLines :: [String]
presentsLines =
  [ "░███████              ░███     ░███            ░███████      ░██         ░██           ░██████  "
  , "░██   ░██             ░████   ░████            ░██   ░██     ░██         ░██          ░██   ░██ "
  , "░██    ░██  ░███████  ░██░██ ░██░██  ░███████  ░██    ░██    ░██         ░██         ░██        "
  , "░██    ░██ ░██    ░██ ░██ ░████ ░██ ░██    ░██ ░██    ░██    ░██         ░██         ░██        "
  , "░██    ░██ ░█████████ ░██  ░██  ░██ ░██    ░██ ░██    ░██    ░██         ░██         ░██        "
  , "░██   ░██  ░██        ░██       ░██ ░██    ░██ ░██   ░██     ░██         ░██          ░██   ░██ "
  , "░███████    ░███████  ░██       ░██  ░███████  ░███████      ░██████████ ░██████████   ░██████  "
  , "                                                                                                "
  , "                                                                                                "
  , "                                                                                                "
  , "              p r e s e n t s"
  ]

presentsText :: String
presentsText = unlines presentsLines

-- | Total character count of presentsText (used for typewriter animation).
presentsLength :: Int
presentsLength = length presentsText

-- | Width of the widest line in presentsLines.
presentsWidth :: Int
presentsWidth = if null presentsLines then 0 else maximum (map length presentsLines)

-- ────────────────────────────────────────────────────────────────
-- "Astart" banner
-- ────────────────────────────────────────────────────────────────

astartLines :: [String]
astartLines =
  [ "   ░███                  ░██                           ░██    "
  , "  ░██░██                 ░██                           ░██    "
  , " ░██  ░██   ░███████  ░████████  ░██████   ░██░████ ░████████ "
  , "░█████████ ░██           ░██          ░██  ░███        ░██    "
  , "░██    ░██  ░███████     ░██     ░███████  ░██         ░██    "
  , "░██    ░██        ░██    ░██    ░██   ░██  ░██         ░██    "
  , "░██    ░██  ░███████      ░████  ░█████░██ ░██          ░████ "
  ]

-- ────────────────────────────────────────────────────────────────
-- Time-based Sierpiński parameters
-- ────────────────────────────────────────────────────────────────

-- | Derive triangle depth, fill character, and a HH:MM:SS label from a UTCTime.
timeToDepthAndChar :: UTCTime -> (Int, Char, String)
timeToDepthAndChar utc =
  let tod   = timeToTimeOfDay (utctDayTime utc)
      h     = todHour tod
      m     = todMin  tod
      s     = floor (todSec tod) :: Int
      depth = 3 + ((h + m + s) `mod` 4)
      ch    = "█▓▒░" !! (s `mod` 4)
      label = pad2 h ++ ":" ++ pad2 m ++ ":" ++ pad2 s
  in (depth, ch, label)
  where
    pad2 n = (if n < 10 then "0" else "") ++ show n
