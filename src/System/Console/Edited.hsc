{-# Language EmptyDataDecls #-}

module System.Console.Edited
  ( Edited
  , edited
  , edited'
  , Editor(..)
  , setEditor
  , EditedPrompt
  , setPrompt
  , setRightPrompt
  , setPrompt'
  , setRightPrompt'
  , EditedFunctionResult(..)
  , EditedFunction
  , addFunction
  , addBind
  , setUseStyle
  , EditedColor(..)
  , EditedStyle(..)
  , setStyleFunc
  , getString
  , getOneChar
  , insertString
  ) where

import Control.Monad
import Data.Bits
import Foreign
import Foreign.C.String
import Foreign.C.Types
import GHC.IO.FD (fdFD)
import GHC.IO.Handle.FD
import System.IO (Handle)

#include "HsEdited.h"

data EditedInternal
type EditedPtr = Ptr EditedInternal

data EditedInternalStyle

newtype Edited = Edited (ForeignPtr EditedInternal)

withEdited :: Edited -> (EditedPtr -> IO a) -> IO a
withEdited (Edited fp) = withForeignPtr fp

type EditedPromptFunc = EditedPtr -> IO CWString
foreign import ccall "wrapper" mkEditedPromptFunc :: EditedPromptFunc -> IO (FunPtr EditedPromptFunc)

type EditedFunc = EditedPtr -> CInt -> IO CInt
foreign import ccall "wrapper" mkEditedFunc :: EditedFunc -> IO (FunPtr EditedFunc)

type EditedStyleFunc = EditedPtr -> CInt -> CWString -> Ptr EditedInternalStyle -> IO ()
foreign import ccall "wrapper" mkEditedStyleFunc :: EditedStyleFunc -> IO (FunPtr EditedStyleFunc)

foreign import ccall edited_init_hs :: CString -> CInt -> CInt -> CInt -> IO EditedPtr
foreign import ccall "&" edited_end :: FunPtr (EditedPtr -> IO ())
foreign import ccall edited_set_editor :: EditedPtr -> CString -> IO Bool
foreign import ccall edited_set_prompt :: EditedPtr -> FunPtr EditedPromptFunc -> IO Bool
foreign import ccall edited_set_rprompt :: EditedPtr -> FunPtr EditedPromptFunc -> IO Bool
foreign import ccall edited_set_addfn :: EditedPtr -> CString -> CString -> FunPtr EditedFunc -> IO Bool
foreign import ccall edited_set_bind :: EditedPtr -> CString -> CString -> IO Bool
foreign import ccall edited_set_use_style :: EditedPtr -> Bool -> IO Bool
foreign import ccall edited_set_style_func :: EditedPtr -> FunPtr EditedStyleFunc -> IO Bool
foreign import ccall edited_wgets :: EditedPtr -> Ptr CInt -> IO CWString
foreign import ccall edited_wgetc :: EditedPtr -> Ptr CWchar -> IO CInt
foreign import ccall edited_winsertstr :: EditedPtr -> CWString -> IO CInt

edited' :: String -> Handle -> Handle -> Handle -> IO Edited
edited' program fIn fOut fErr = do
  fdIn <- fdFD <$> handleToFd fIn
  fdOut <- fdFD <$> handleToFd fOut
  fdErr <- fdFD <$> handleToFd fErr
  el <- withCString program $ \prog -> edited_init_hs prog fdIn fdOut fdErr
  ptr <- newForeignPtr edited_end el
  return $ Edited ptr

edited :: String -> IO Edited
edited program = edited' program stdin stdout stderr

data Editor = Vi | Emacs

instance Show Editor where
  show Vi = "vi"
  show Emacs = "emacs"

setEditor :: Edited -> Editor -> IO ()
setEditor el' editor = withEdited el' $ \el -> withCString (show editor) $ \editorC -> do
  res <- edited_set_editor el editorC
  when res $ error "setEditor: C returned failure"

type EditedPrompt = Edited -> IO String

setPrompt :: Edited -> EditedPrompt -> IO ()
setPrompt el' f = withEdited el' $ \el -> do
  fC <- mkEditedPromptFunc $ \e -> do
    el1 <- Edited <$> newForeignPtr_ e
    res <- f el1
    -- leak!
    newCWString res
  res <- edited_set_prompt el fC
  when res $ error "setPrompt: C returned failure"

setRightPrompt :: Edited -> EditedPrompt -> IO ()
setRightPrompt el' f = withEdited el' $ \el -> do
  fC <- mkEditedPromptFunc $ \e -> do
    el1 <- Edited <$> newForeignPtr_ e
    res <- f el1
    -- leak!
    newCWString res
  res <- edited_set_rprompt el fC
  when res $ error "setRightPrompt: C returned failure"

setPrompt' :: Edited -> String -> IO ()
setPrompt' el' str = setPrompt el' (const $ pure str)

setRightPrompt' :: Edited -> String -> IO ()
setRightPrompt' el' str = setRightPrompt el' (const $ pure str)

data EditedFunctionResult
  = Normal
  | Newline
  | EOF
  | ArgHack
  | Refresh
  | Cursor
  | Error
  | Fatal
  | Redisplay
  | RefreshBeep
  deriving (Eq, Show)

instance Enum EditedFunctionResult where
  fromEnum Normal = (#const CC_NORM)
  fromEnum Newline = (#const CC_NEWLINE)
  fromEnum EOF = (#const CC_EOF)
  fromEnum ArgHack = (#const CC_ARGHACK)
  fromEnum Refresh = (#const CC_REFRESH)
  fromEnum Cursor = (#const CC_CURSOR)
  fromEnum Error = (#const CC_ERROR)
  fromEnum Fatal = (#const CC_FATAL)
  fromEnum Redisplay = (#const CC_REDISPLAY)
  fromEnum RefreshBeep = (#const CC_REFRESH_BEEP)
  toEnum (#const CC_NORM) = Normal
  toEnum (#const CC_NEWLINE) = Newline
  toEnum (#const CC_EOF) = EOF
  toEnum (#const CC_ARGHACK) = ArgHack
  toEnum (#const CC_REFRESH) = Refresh
  toEnum (#const CC_CURSOR) = Cursor
  toEnum (#const CC_ERROR) = Error
  toEnum (#const CC_FATAL) = Fatal
  toEnum (#const CC_REDISPLAY) = Redisplay
  toEnum (#const CC_REFRESH_BEEP) = RefreshBeep
  toEnum _ = error "EditedFunctionResult toEnum: bad value"

type EditedFunction = Edited -> Int -> IO EditedFunctionResult

addFunction :: Edited -> String -> String -> EditedFunction -> IO ()
addFunction el' name desc f = withEdited el' $ \el -> withCString name $ \nameC -> withCString desc $ \descC -> do
  fC <- mkEditedFunc $ \e i -> do
    el1 <- Edited <$> newForeignPtr_ e
    res <- f el1 $ fromEnum i
    pure $ toEnum $ fromEnum res
  res <- edited_set_addfn el nameC descC fC
  when res $ error "addFunction: C returned failure"

addBind :: Edited -> String -> String -> IO ()
addBind el' keybind fnName = withEdited el' $ \el -> withCString keybind $ \keybindC -> withCString fnName $ \fnNameC -> do
  res <- edited_set_bind el keybindC fnNameC
  when res $ error "addBind: C returned failure"

setUseStyle :: Edited -> Bool -> IO ()
setUseStyle el' use = withEdited el' $ \el -> do
  res <- edited_set_use_style el use
  when res $ error "setUseStyle: C returned failure"

data EditedColor
  = Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  | Default
  | Unset
  deriving (Eq, Show)

instance Enum EditedColor where
  fromEnum Black = (#const EDITED_COLOR_BLACK)
  fromEnum Red = (#const EDITED_COLOR_RED)
  fromEnum Green = (#const EDITED_COLOR_GREEN)
  fromEnum Yellow = (#const EDITED_COLOR_YELLOW)
  fromEnum Blue = (#const EDITED_COLOR_BLUE)
  fromEnum Magenta = (#const EDITED_COLOR_MAGENTA)
  fromEnum Cyan = (#const EDITED_COLOR_CYAN)
  fromEnum White = (#const EDITED_COLOR_WHITE)
  fromEnum Default = (#const EDITED_COLOR_DEFAULT)
  fromEnum Unset = (#const EDITED_COLOR_UNSET)
  toEnum (#const EDITED_COLOR_BLACK) = Black
  toEnum (#const EDITED_COLOR_RED) = Red
  toEnum (#const EDITED_COLOR_GREEN) = Green
  toEnum (#const EDITED_COLOR_YELLOW) = Yellow
  toEnum (#const EDITED_COLOR_BLUE) = Blue
  toEnum (#const EDITED_COLOR_MAGENTA) = Magenta
  toEnum (#const EDITED_COLOR_CYAN) = Cyan
  toEnum (#const EDITED_COLOR_WHITE) = White
  toEnum (#const EDITED_COLOR_DEFAULT) = Default
  toEnum (#const EDITED_COLOR_UNSET) = Unset
  toEnum _ = error "EditedColor toEnum: bad value"

data EditedStyle = EditedStyle
  { foreground :: EditedColor
  , background :: EditedColor
  , bold :: Bool
  , italic :: Bool
  , underline :: Bool
  , strikethrough :: Bool
  } | EditedStyleReset deriving (Eq, Show)

styleToBits :: (Bits r, Num r, Enum r) => EditedStyle -> r
styleToBits EditedStyleReset = 1 `shift` 8
styleToBits EditedStyle{ foreground = fg, background = bg, bold = b, italic = i, underline = u, strikethrough = s } =
  (toEnum $ fromEnum fg) `shift` 0 .|. (toEnum $ fromEnum bg) `shift` 4 .|. (toEnum $ fromEnum b) `shift` 9 .|. (toEnum $ fromEnum i) `shift` 10 .|. (toEnum $ fromEnum u) `shift` 11 .|. (toEnum $ fromEnum s) `shift` 12

setStyleFunc :: Edited -> (Edited -> String -> IO [EditedStyle]) -> IO ()
setStyleFunc el' f = withEdited el' $ \el -> do
  fC <- mkEditedStyleFunc $ \e len str styles -> do
    el1 <- Edited <$> newForeignPtr_ e
    str' <- peekCWStringLen (str, fromIntegral len)
    res <- f el1 str'
    when (length res /= fromIntegral len) $ error "setStyleFunc: length mismatch"
    forM_ (zip [0..] (map styleToBits res :: [Word32])) $ \(i, s) -> do
      -- print (styles, i, i * (#size edited_style_t))
      pokeByteOff styles (i * (#size edited_style_t)) s
  res <- edited_set_style_func el fC
  when res $ error "setStyleFunc: C returned failure"
  
getString :: Edited -> IO (Maybe String)
getString el' = withEdited el' $ \el -> with (0 :: CInt) $ \len -> do
  res <- edited_wgets el len
  if res == nullPtr then pure Nothing
  else do
    len' <- peek len
    (\case
      [] -> Nothing
      xs -> Just $ init xs) <$> peekCWStringLen (res, fromIntegral len')

getOneChar :: Edited -> IO (Maybe Char)
getOneChar el' = withEdited el' $ \el -> with (0 :: CWchar) $ \result -> do
  size <- edited_wgetc el result
  if size == 1 then Just . toEnum . fromEnum <$> peek result
  else pure Nothing

insertString :: Edited -> String -> IO ()
insertString el' str = withEdited el' $ \el -> withCWString str $ \strC -> do
  res <- edited_winsertstr el strC
  when (res /= 0) $ error "insertString: C returned failure"
