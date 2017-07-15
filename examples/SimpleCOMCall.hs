
module Main where

import Data.Maybe

import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types

import System.Win32.Com
import System.Win32.Types hiding (HRESULT)



main :: IO ()
main = coRun $ do
        i <- coCreateInstance clsidUserNotification2 
                    Nothing CLSCTX_INPROC_SERVER iidUserNotification2
        _ <- setBalloonInfo i "Hello from hcom" "Hello" 3
        hr <- showBalloon i nullPtr 10000 nullPtr
        if hr == s_OK then putStrLn "OK!" else putStrLn "BOO!"

        
-- The User notification interface

data UserNotification2 a = Un
type IUserNotification2 a = IUnknown (UserNotification2 a)

iidUserNotification2 = mkIID  "{215913cc-57eb-4fab-AB5A-E5FA7BEA2A6C}"
clsidUserNotification2 = mkCLSID "{0010890e-8789-413c-adbc-48f5b511b3af}"

setBalloonInfo :: IUnknown (IUserNotification2 a) -> String -> String -> DWORD -> IO HRESULT
setBalloonInfo iptr title text flags = do
    -- don't know what windows does with memory so leak it
    titleWStr <- newCWString title
    textWStr <- newCWString text
    invokeIt (\meth ip -> prim_setBalloonInfo meth ip titleWStr textWStr flags) 3 iptr


showBalloon :: IUnknown (IUserNotification2 a) -> Ptr () -> DWORD -> Ptr () -> IO HRESULT
showBalloon iptr iqcont interval cb = do
    invokeIt (\meth ip -> prim_showBalloon meth ip iqcont interval cb) 6 iptr

foreign import stdcall "dynamic" prim_setBalloonInfo :: Ptr (Ptr() -> Ptr CWchar -> Ptr CWchar -> DWORD -> IO HRESULT) -> Ptr () -> Ptr CWchar -> Ptr CWchar -> DWORD -> IO HRESULT

foreign import stdcall "dynamic" prim_showBalloon :: Ptr (Ptr () -> Ptr () -> DWORD -> Ptr() -> IO HRESULT) -> Ptr () -> Ptr () -> DWORD -> Ptr () -> IO HRESULT