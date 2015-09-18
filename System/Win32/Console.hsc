#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Win32.Console
-- Copyright   :  (c) University of Glasgow 2006
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Esa Ilari Vuokko <ei@vuokko.info>
-- Stability   :  provisional
-- Portability :  portable
--
-- A collection of FFI declarations for interfacing with Win32 Console API
--
-----------------------------------------------------------------------------

module System.Win32.Console (
	-- * Console code pages
	getConsoleCP,
	setConsoleCP,
	getConsoleOutputCP,
	setConsoleOutputCP,
	-- * Ctrl events
	CtrlEvent, cTRL_C_EVENT, cTRL_BREAK_EVENT,
	generateConsoleCtrlEvent,
        -- * Screen buffer
        COORD(..), SMALL_RECT(..), CONSOLE_SCREEN_BUFFER_INFO(..),
        getConsoleScreenBufferInfo,
        setConsoleCursorPosition,
        setConsoleTextAttribute
  ) where

##include "windows_cconv.h"

#include <windows.h>
#include "ConsoleStubs.h"

import System.Win32.Types

import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))
import Foreign.C.Types (CShort(..))

foreign import WINDOWS_CCONV unsafe "windows.h GetConsoleCP"
	getConsoleCP :: IO UINT

foreign import WINDOWS_CCONV unsafe "windows.h SetConsoleCP"
	setConsoleCP :: UINT -> IO ()

foreign import WINDOWS_CCONV unsafe "windows.h GetConsoleOutputCP"
	getConsoleOutputCP :: IO UINT

foreign import WINDOWS_CCONV unsafe "windows.h SetConsoleOutputCP"
	setConsoleOutputCP :: UINT -> IO ()

type CtrlEvent = DWORD
#{enum CtrlEvent,
    , cTRL_C_EVENT      = 0
    , cTRL_BREAK_EVENT  = 1
    }

generateConsoleCtrlEvent :: CtrlEvent -> DWORD -> IO ()
generateConsoleCtrlEvent e p
    = failIfFalse_
        "generateConsoleCtrlEvent"
        $ c_GenerateConsoleCtrlEvent e p

foreign import WINDOWS_CCONV safe "windows.h GenerateConsoleCtrlEvent"
    c_GenerateConsoleCtrlEvent :: CtrlEvent -> DWORD -> IO BOOL

data COORD = COORD
    { coordX :: SHORT
    , coordY :: SHORT
    } deriving (Show)

instance Storable COORD where
    sizeOf = const (#size COORD)
    alignment = sizeOf
    poke buf c = do
        (#poke COORD, X) buf (coordX c)
        (#poke COORD, Y) buf (coordX c)
    peek buf = do
        x <- (#peek COORD, X) buf
        y <- (#peek COORD, Y) buf
        return $ COORD x y

data SMALL_RECT = SMALL_RECT
    { srLeft :: SHORT
    , srTop :: SHORT
    , srRight :: SHORT
    , srBottom :: SHORT
    } deriving (Show)

instance Storable SMALL_RECT where
    sizeOf = const (#size SMALL_RECT)
    alignment = sizeOf
    poke buf sr = do
        (#poke SMALL_RECT, Left) buf (srLeft sr)
        (#poke SMALL_RECT, Top) buf (srTop sr)
        (#poke SMALL_RECT, Right) buf (srRight sr)
        (#poke SMALL_RECT, Bottom) buf (srBottom sr)
    peek buf = do
        left <- (#peek SMALL_RECT, Left) buf
        top <- (#peek SMALL_RECT, Top) buf
        right <- (#peek SMALL_RECT, Right) buf
        bottom <- (#peek SMALL_RECT, Bottom) buf
        return $ SMALL_RECT left top right bottom

data CONSOLE_SCREEN_BUFFER_INFO = CONSOLE_SCREEN_BUFFER_INFO
    { csbiSize :: COORD
    , csbiCursorPosition :: COORD
    , csbiAttributes :: WORD
    , csbiWindow :: SMALL_RECT
    , csbiMaximumWindowSize :: COORD
    } deriving (Show)

instance Storable CONSOLE_SCREEN_BUFFER_INFO where
    sizeOf = const (#size CONSOLE_SCREEN_BUFFER_INFO)
    alignment = sizeOf
    poke buf csbi = do
        (#poke CONSOLE_SCREEN_BUFFER_INFO, dwSize) buf (csbiSize csbi)
        (#poke CONSOLE_SCREEN_BUFFER_INFO, dwCursorPosition) buf (csbiCursorPosition csbi)
        (#poke CONSOLE_SCREEN_BUFFER_INFO, wAttributes) buf (csbiAttributes csbi)
        (#poke CONSOLE_SCREEN_BUFFER_INFO, srWindow) buf (csbiWindow csbi)
        (#poke CONSOLE_SCREEN_BUFFER_INFO, dwMaximumWindowSize) buf (csbiMaximumWindowSize csbi)
    peek buf = do
        size <- (#peek CONSOLE_SCREEN_BUFFER_INFO, dwSize) buf
        cursorPosition <- (#peek CONSOLE_SCREEN_BUFFER_INFO, dwCursorPosition) buf
        attributes <- (#peek CONSOLE_SCREEN_BUFFER_INFO, wAttributes) buf
        window <- (#peek CONSOLE_SCREEN_BUFFER_INFO, srWindow) buf
        maximumWindowSize <- (#peek CONSOLE_SCREEN_BUFFER_INFO, dwMaximumWindowSize) buf
        return $ CONSOLE_SCREEN_BUFFER_INFO size cursorPosition attributes window maximumWindowSize

foreign import WINDOWS_CCONV safe "windows.h GetConsoleScreenBufferInfo"
    c_GetConsoleScreenBufferInfo :: HANDLE -> Ptr CONSOLE_SCREEN_BUFFER_INFO -> IO BOOL

getConsoleScreenBufferInfo :: HANDLE -> IO (Maybe CONSOLE_SCREEN_BUFFER_INFO)
getConsoleScreenBufferInfo consoleOut =
    alloca $ \ret -> do
        success <- c_GetConsoleScreenBufferInfo consoleOut ret
        if success then Just <$> peek ret else pure Nothing

foreign import WINDOWS_CCONV safe "windows.h SetConsoleTextAttribute"
    c_SetConsoleTextAttribute :: HANDLE -> WORD -> IO BOOL

setConsoleTextAttribute :: HANDLE -> WORD -> IO ()
setConsoleTextAttribute out attr
    = failIfFalse_ "SetConsoleTextAttribute" $ c_SetConsoleTextAttribute out attr

foreign import WINDOWS_CCONV safe "ConsoleStubs.h ConsoleStubs_SetConsoleCursorPosition"
    c_SetConsoleCursorPosition :: HANDLE -> SHORT -> SHORT -> IO BOOL

setConsoleCursorPosition :: HANDLE -> COORD -> IO ()
setConsoleCursorPosition out coord
    = failIfFalse_
        "SetConsoleCursorPosition"
        $ c_SetConsoleCursorPosition out (coordX coord) (coordY coord)

-- ToDo: lots more
