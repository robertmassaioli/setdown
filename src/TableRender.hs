module TableRender
   ( Align(..)
   , renderTable
   ) where

import Data.List (intercalate, transpose)

-- | How a column's cells are padded to the column width.
data Align = AlignLeft | AlignRight

-- | Render a header row and body rows as a Unicode box-drawing table: a bold
-- (heavy-lined) top border and header separator, single-lined body borders,
-- one alignment per column, and column widths sized to the widest cell
-- (including the header). This reproduces the exact visual style setdown has
-- always used for its results tables.
--
-- Column widths and the header row always account for the header text, even
-- when there are zero body rows, so headers never disappear on empty input.
renderTable :: [Align] -> [String] -> [[String]] -> [String]
renderTable aligns headers rows =
   [topBorder, headerRow, headerSeparator] ++ map bodyRow rows ++ [bottomBorder]
   where
      widths = zipWith columnWidth headers (columnsOf headers rows)
      columnWidth header column = maximum (length header : map length column)

      columnsOf hs [] = replicate (length hs) []
      columnsOf _  rs = transpose rs

      topBorder       = border '┏' '┳' '┓' '━'
      headerSeparator = border '┡' '╇' '┩' '━'
      bottomBorder    = border '└' '┴' '┘' '─'

      border left mid right line =
         [left] ++ intercalate [mid] (map (\w -> replicate (w + 2) line) widths) ++ [right]

      headerRow          = rowOf '┃' (zipWith padCenter widths headers)
      bodyRow cells       = rowOf '│' (zipWith3 padAlign widths aligns cells)

      rowOf sep cells =
         [sep] ++ intercalate [sep] (map (\c -> ' ' : c ++ " ") cells) ++ [sep]

      padCenter width s =
         let extra = width - length s
             left  = extra `div` 2
             right = extra - left
         in replicate left ' ' ++ s ++ replicate right ' '

      padAlign width AlignLeft  s = s ++ replicate (width - length s) ' '
      padAlign width AlignRight s = replicate (width - length s) ' ' ++ s
