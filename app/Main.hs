module Main where

import qualified System.Console.Edited as E
import qualified Language.Haskell.Lexer as L
import Data.Maybe (fromMaybe)

mapAdjacent :: (a -> a -> b) -> [a] -> [b]
mapAdjacent _ [] = []
mapAdjacent f xs@(_:xs') = zipWith f xs xs'

styles :: [(L.Token, E.EditedStyle)]
styles =
  [ (L.Varid, E.EditedStyle E.Default E.Unset False False False False)
  , (L.Conid, E.EditedStyle E.Default E.Unset False True False False)
  , (L.Varsym, E.EditedStyle E.Magenta E.Unset False False False False)
  , (L.Consym, E.EditedStyle E.Magenta E.Unset False True False False)
  , (L.Reservedid, E.EditedStyle E.Blue E.Unset False False False False)
  , (L.Reservedop, E.EditedStyle E.Blue E.Unset False False False False)
  , (L.Specialid, E.EditedStyle E.Blue E.Unset True False False False)
  , (L.IntLit, E.EditedStyle E.Red E.Unset False False False False)
  , (L.FloatLit, E.EditedStyle E.Red E.Unset False False False False)
  , (L.CharLit, E.EditedStyle E.Green E.Unset False False False False)
  , (L.StringLit, E.EditedStyle E.Green E.Unset False False False False)
  , (L.Qvarid, E.EditedStyle E.Default E.Unset False False False False)
  , (L.Qconid, E.EditedStyle E.Default E.Unset False True False False)
  , (L.Qvarsym, E.EditedStyle E.Magenta E.Unset False False False False)
  , (L.Qconsym, E.EditedStyle E.Magenta E.Unset False True False False)
  , (L.Special, E.EditedStyle E.Default E.Unset True True False False)
  , (L.Whitespace, E.EditedStyleReset)
  , (L.NestedComment, E.EditedStyle E.Blue E.Unset False True False False)
  , (L.Commentstart, E.EditedStyle E.Blue E.Unset False True False False)
  , (L.Comment, E.EditedStyle E.Blue E.Unset False True False False)
  , (L.ErrorToken, E.EditedStyle E.Red E.Unset False True True False)
  ]

highlight :: E.Edited -> String -> IO [E.EditedStyle]
highlight _ str = do
  let lexed = L.lexerPass0 str
  let colors = (fromMaybe E.EditedStyleReset . flip lookup styles . fst) <$> lexed
  let chunks = zipWith (\color count -> take count $ repeat color) colors $ (++ [length str - if null lexed then 0 else L.char (fst $ snd $ last lexed)]) $ mapAdjacent (\L.Pos{ L.char = char1 } L.Pos{ L.char = char2 } -> char2 - char1) $ fst . snd <$> lexed
  pure $ concat chunks

repl :: IO ()
repl = let
  loop el = do
    line <- E.getString el
    case line of
      Nothing -> pure ()
      Just [] -> pure ()
      Just line' -> do
        putStrLn line'
        loop el
  in do
    el <- E.edited "haskell highlighter"
    -- history
    E.setEditor el E.Emacs
    E.setPrompt' el "λ> "
    E.setUseStyle el True
    E.setStyleFunc el highlight
    
    E.addBind el "\\e[1~" "ed-move-to-beg"
    E.addBind el "\\e[4~" "ed-move-to-end"
    E.addBind el "\\e[7~" "ed-move-to-beg"
    E.addBind el "\\e[8~" "ed-move-to-end"
    E.addBind el "\\e[H" "ed-move-to-beg"
    E.addBind el "\\e[F" "ed-move-to-end"
    E.addBind el "\\e[3~" "ed-delete-next-char"
    E.addBind el "\\e[2~" "em-toggle-overwrite"
    E.addBind el "\\e[1;5C" "em-next-word"
    E.addBind el "\\e[1;5D" "ed-prev-word"
    E.addBind el "\\e[5C" "em-next-word"
    E.addBind el "\\e[5D" "ed-prev-word"
    E.addBind el "\\e\\e[C" "em-next-word"
    E.addBind el "\\e\\e[D" "ed-prev-word"
    
    loop el

main :: IO ()
main = repl