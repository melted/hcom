
module Main where

import Data.Maybe

import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types

import Graphics.Win32.GDI.Types

import System.Win32.Com
import System.Win32.Types hiding (HRESULT)



main :: IO ()
main = coRun $ do
        i <- coCreateInstance clsidUserNotification2 
                    Nothing CLSCTX_INPROC_SERVER iidUserNotification2
        setBalloonInfo i "Hello from hcom" "Hello" 1
        playSound i "Asterisk"
        setBalloonRetry i 5000 2000 3
        hr <- showBalloon i nullPtr 10000 nullPtr
        putStrLn "Done!"
        
-- The User notification interface

data UserNotification2 a
type IUserNotification2 a = IUnknown (UserNotification2 a)

iidUserNotification2 = mkIID  "{215913cc-57eb-4fab-AB5A-E5FA7BEA2A6C}"
clsidUserNotification2 = mkCLSID "{0010890e-8789-413c-adbc-48f5b511b3af}"

setBalloonInfo :: IUnknown (IUserNotification2 a) -> String -> String -> DWORD -> IO ()
setBalloonInfo iptr title text flags = do
    -- don't know what windows does with memory so leak it
    titleWStr <- newCWString title
    textWStr <- newCWString text
    invokeAndCheck (\meth ip -> prim_setBalloonInfo meth ip titleWStr textWStr flags) 3 iptr

setBalloonRetry :: IUnknown (IUserNotification2 a) -> DWORD -> DWORD -> UINT -> IO ()
setBalloonRetry iptr showTime interval retryCount = do
    invokeAndCheck (\meth ip -> prim_setBallonRetry meth ip showTime interval retryCount) 4 iptr

setIconInfo :: IUnknown (IUserNotification2 a) -> HICON -> String -> IO ()
setIconInfo iptr icon tooltip = do
    tooltipWStr <- newCWString tooltip
    invokeAndCheck (\meth ip -> prim_setIconInfo meth ip icon tooltipWStr) 5 iptr

showBalloon :: IUnknown (IUserNotification2 a) -> Ptr () -> DWORD -> Ptr () -> IO HRESULT
showBalloon iptr iqcont interval cb = do
    invokeIt (\meth ip -> prim_showBalloon meth ip iqcont interval cb) 6 iptr

playSound :: IUnknown (IUserNotification2 a) -> String -> IO ()
playSound iptr sound = withCWString sound $
                              (\s -> invokeAndCheck
                                         (\meth ip -> prim_playSound meth ip s) 7 iptr)

-- FFI declarations

foreign import stdcall "dynamic" prim_setBalloonInfo :: Ptr (Ptr() -> LPCWSTR -> LPCWSTR -> DWORD -> IO HRESULT) -> Ptr () -> LPCWSTR -> LPCWSTR -> DWORD -> IO HRESULT

foreign import stdcall "dynamic" prim_setBallonRetry :: Ptr (Ptr() -> DWORD -> DWORD -> UINT -> IO HRESULT) -> Ptr() -> DWORD -> DWORD -> UINT -> IO HRESULT

foreign import stdcall "dynamic" prim_setIconInfo :: Ptr (Ptr () -> HICON -> LPCWSTR -> IO HRESULT) -> Ptr () -> HICON -> LPCWSTR -> IO HRESULT

foreign import stdcall "dynamic" prim_showBalloon :: Ptr (Ptr () -> Ptr () -> DWORD -> Ptr() -> IO HRESULT) -> Ptr () -> Ptr () -> DWORD -> Ptr () -> IO HRESULT

foreign import stdcall "dynamic" prim_playSound :: Ptr (Ptr () -> LPCWSTR -> IO HRESULT) -> Ptr () -> LPCWSTR -> IO HRESULT