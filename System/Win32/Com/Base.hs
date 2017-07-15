{-# OPTIONS_GHC -XCPP -fglasgow-exts #-}
-- Automatically generated by HaskellDirect (ihc.exe), snapshot 171208
-- Created: 23:37 Pacific Standard Time, Wednesday 17 December, 2008
-- Command line: -fno-qualified-names -fno-imports -fno-export-lists -fout-pointers-are-not-refs -c System/Win32/Com/Base.idl -o System/Win32/Com/Base.hs

module System.Win32.Com.Base where



import System.Win32.Com.HDirect.Pointer ( makeFO, finalNoFree )
import System.Win32.Com.HDirect.HDirect
import Foreign.ForeignPtr
import Foreign.Ptr
import Data.Dynamic
import Data.Int
import Data.Word       ( Word32 )
import System.IO.Unsafe ( unsafePerformIO )
import System.Win32.Com.HDirect.WideString ( WideString, marshallWideString, freeWideString,
            readWideString, writeWideString )
import System.IO        ( hPutStrLn, stderr )

import Control.Exception
import Data.Typeable
#if BASE <=3
import GHC.IOBase
#endif

newtype IUnknown_ a  = Unknown  (ForeignPtr ())
type IUnknown  a  = IUnknown_ a

type HRESULT = Int32

failed    :: HRESULT -> Bool
failed hr = hr < 0

ifaceToAddr :: IUnknown a -> Ptr b
ifaceToAddr (Unknown x)    = castPtr (foreignPtrToPtr x)

addrToIPointer :: Bool -> Ptr b -> IO (IUnknown a)
addrToIPointer finaliseMe x = do
  i <- makeFO x (castPtrToFunPtr $ if finaliseMe then addrOfReleaseIUnknown else finalNoFree)
  return (Unknown i)

marshallIUnknown :: IUnknown a -> IO (ForeignPtr b)
marshallIUnknown (Unknown x) = return (castForeignPtr x)

checkHR :: HRESULT -> IO ()
checkHR hr
      | failed hr   = coFailHR hr
      | otherwise   = return ()

coFailHR :: HRESULT -> IO a
coFailHR  hr         = do
    str <- stringFromHR hr
    coFailWithHR hr str

newtype ComError = ComError HRESULT
  deriving ( Show )

data ComException
 = ComException
     { comException     :: ComError
     , comExceptionMsg  :: String
     } deriving (Typeable,Show)

comExceptionHR :: ComException -> HRESULT
comExceptionHR ComException{comException=ComError hr} = hr

comErrorTc = mkTyCon3 "com2" "System.Win32.Com.Base" "ComError"

#if !defined(BASE) || BASE > 3 
data SomeCOMException = forall e . Exception e => SomeCOMException e 
    deriving Typeable 

instance Show SomeCOMException where 
    show (SomeCOMException e) = show e 

instance Exception SomeCOMException 

comToException :: Exception e => e -> SomeException 
comToException = toException . SomeCOMException 

comFromException :: Exception e => SomeException -> Maybe e 
comFromException x = do 
    SomeCOMException a <- fromException x 
    cast a 

throwComException :: ComException -> IO a
throwComException c = throwIO (toException c)

instance Exception ComException where
  toException   = comToException
  fromException = comFromException

#else
instance Typeable ComError where
#if __GLASGOW_HASKELL__ < 604
   typeOf _ = mkAppTy comErrorTc []
#else
   typeOf _ = mkTyConApp comErrorTc []
#endif

throwComException :: ComException -> IO a
throwComException c = throwDyn c
#endif

coFailWithHR :: HRESULT -> String -> IO a
coFailWithHR hr msg = throwComException ComException{comException=(ComError hr),comExceptionMsg=msg}

stringFromHR :: HRESULT -> IO String
stringFromHR hr = do
  pstr <- hresultString hr  -- static memory
  unmarshallString (castPtr pstr)

comInitialize :: IO ()
comInitialize =
  do
    o_comInitialize <- prim_System_Win32_Com_Base_comInitialize
    checkHR o_comInitialize

foreign import ccall "comInitialize" prim_System_Win32_Com_Base_comInitialize :: IO Int32
comUnInitialize :: IO ()
comUnInitialize =
  prim_System_Win32_Com_Base_comUnInitialize

foreign import ccall "comUnInitialize" prim_System_Win32_Com_Base_comUnInitialize :: IO ()
messageBox :: Ptr Char
           -> Ptr Char
           -> Word32
           -> IO ()
messageBox str title flg =
  prim_System_Win32_Com_Base_messageBox str
                                        title
                                        flg

foreign import ccall "messageBox" prim_System_Win32_Com_Base_messageBox :: Ptr Char -> Ptr Char -> Word32 -> IO ()
type PIID = Ptr ()
type PCLSID = Ptr ()
type PGUID = Ptr ()
primCLSIDFromProgID :: Ptr Char
                    -> PCLSID
                    -> IO ()
primCLSIDFromProgID str rptr =
  do
    o_primCLSIDFromProgID <- prim_System_Win32_Com_Base_primCLSIDFromProgID str rptr
    checkHR o_primCLSIDFromProgID

foreign import ccall "primCLSIDFromProgID" prim_System_Win32_Com_Base_primCLSIDFromProgID :: Ptr Char -> Ptr () -> IO Int32
primProgIDFromCLSID :: ForeignPtr ()
                    -> IO (Ptr ())
primProgIDFromCLSID pclsid =
  do
    pwide <- allocBytes (fromIntegral sizeofPtr)
    o_primProgIDFromCLSID <- withForeignPtr pclsid (\ pclsid -> prim_System_Win32_Com_Base_primProgIDFromCLSID pclsid pwide)
    checkHR o_primProgIDFromCLSID
    doThenFree free readPtr pwide

foreign import ccall "primProgIDFromCLSID" prim_System_Win32_Com_Base_primProgIDFromCLSID :: Ptr () -> Ptr (Ptr ()) -> IO Int32
primStringToGUID :: Ptr Wchar_t
                 -> Ptr ()
                 -> IO ()
primStringToGUID str pguid =
  do
    o_primStringToGUID <- prim_System_Win32_Com_Base_primStringToGUID str pguid
    checkHR o_primStringToGUID

foreign import ccall "primStringToGUID" prim_System_Win32_Com_Base_primStringToGUID :: Ptr Word16 -> Ptr () -> IO Int32
primGUIDToString :: ForeignPtr ()
                 -> IO (Ptr ())
primGUIDToString guid =
  do
    str <- allocBytes (fromIntegral sizeofPtr)
    o_primGUIDToString <- withForeignPtr guid (\ guid -> prim_System_Win32_Com_Base_primGUIDToString guid str)
    checkHR o_primGUIDToString
    doThenFree free readPtr str

foreign import ccall "primGUIDToString" prim_System_Win32_Com_Base_primGUIDToString :: Ptr () -> Ptr (Ptr ()) -> IO Int32
primCopyGUID :: ForeignPtr ()
             -> PGUID
             -> IO ()
primCopyGUID pguid1 pguid2 =
  do
    o_primCopyGUID <- withForeignPtr pguid1 (\ pguid1 -> prim_System_Win32_Com_Base_primCopyGUID pguid1 pguid2)
    checkHR o_primCopyGUID

foreign import ccall "primCopyGUID" prim_System_Win32_Com_Base_primCopyGUID :: Ptr () -> Ptr () -> IO Int32
primNewGUID :: ForeignPtr ()
            -> IO ()
primNewGUID pguid =
  do
    o_primNewGUID <- withForeignPtr pguid (\ pguid -> prim_System_Win32_Com_Base_primNewGUID pguid)
    checkHR o_primNewGUID

foreign import ccall "primNewGUID" prim_System_Win32_Com_Base_primNewGUID :: Ptr () -> IO Int32
bindObject :: Ptr Wchar_t
           -> ForeignPtr ()
           -> IO (Ptr (Ptr ()))
bindObject name iid =
  do
    ppv <- allocBytes (fromIntegral sizeofPtr)
    o_bindObject <- withForeignPtr iid (\ iid -> prim_System_Win32_Com_Base_bindObject name iid ppv)
    checkHR o_bindObject
    return (ppv)

foreign import ccall "bindObject" prim_System_Win32_Com_Base_bindObject :: Ptr Word16 -> Ptr () -> Ptr (Ptr ()) -> IO Int32
primComEqual :: IUnknown a0
             -> IUnknown a1
             -> IO Bool
primComEqual unk1 unk2 =
  do
    unk1 <- marshallIUnknown unk1
    unk2 <- marshallIUnknown unk2
    o_primComEqual <- withForeignPtr unk1 (\ unk1 -> withForeignPtr unk2 (\ unk2 -> prim_System_Win32_Com_Base_primComEqual unk1 unk2))
    unmarshallBool o_primComEqual

foreign import ccall "primComEqual" prim_System_Win32_Com_Base_primComEqual :: Ptr (IUnknown a) -> Ptr (IUnknown a) -> IO Int32
isEqualGUID :: ForeignPtr ()
            -> ForeignPtr ()
            -> Bool
isEqualGUID guid1 guid2 =
  unsafePerformIO (withForeignPtr guid1 (\ guid1 -> withForeignPtr guid2 (\ guid2 -> prim_System_Win32_Com_Base_isEqualGUID guid1 guid2)) >>= \ o_isEqualGUID ->
                   unmarshallBool o_isEqualGUID)

foreign import stdcall "IsEqualGUID" prim_System_Win32_Com_Base_isEqualGUID :: Ptr () -> Ptr () -> IO Int32
lOCALE_USER_DEFAULT :: Word32
lOCALE_USER_DEFAULT =
  unsafePerformIO (prim_System_Win32_Com_Base_lOCALE_USER_DEFAULT)

foreign import ccall "lOCALE_USER_DEFAULT" prim_System_Win32_Com_Base_lOCALE_USER_DEFAULT :: IO Word32
primCreateTypeLib :: Int32
                  -> WideString
                  -> IO (Ptr (Ptr ()))
primCreateTypeLib skind lpkind =
  do
    ppv <- allocBytes (fromIntegral sizeofPtr)
    lpkind <- marshallWideString lpkind
    o_primCreateTypeLib <- prim_System_Win32_Com_Base_primCreateTypeLib skind lpkind ppv
    freeWideString lpkind
    checkHR o_primCreateTypeLib
    return (ppv)

foreign import ccall "primCreateTypeLib" prim_System_Win32_Com_Base_primCreateTypeLib :: Int32 -> Ptr WideString -> Ptr (Ptr ()) -> IO Int32
getLastError :: IO Word32
getLastError = prim_System_Win32_Com_Base_getLastError

foreign import stdcall "GetLastError" prim_System_Win32_Com_Base_getLastError :: IO Word32
hresultString :: Int32
              -> IO (Ptr ())
hresultString i = prim_System_Win32_Com_Base_hresultString i

foreign import ccall "hresultString" prim_System_Win32_Com_Base_hresultString :: Int32 -> IO (Ptr ())
coCreateInstance :: ForeignPtr ()
                 -> ForeignPtr ()
                 -> Int32
                 -> ForeignPtr ()
                 -> Ptr ()
                 -> IO ()
coCreateInstance clsid inner ctxt riid ppv =
  do
    o_coCreateInstance <- withForeignPtr clsid (\ clsid -> withForeignPtr inner (\ inner -> withForeignPtr riid (\ riid -> prim_System_Win32_Com_Base_coCreateInstance clsid inner ctxt riid ppv)))
    checkHR o_coCreateInstance

foreign import stdcall "CoCreateInstance" prim_System_Win32_Com_Base_coCreateInstance :: Ptr () -> Ptr () -> Int32 -> Ptr () -> Ptr () -> IO Int32
type ULONG = Word32
type DWORD = Word32
data COAUTHIDENTITY = COAUTHIDENTITY {user :: String,
                                      domain :: String,
                                      password :: String,
                                      flags :: ULONG}
                        
writeCOAUTHIDENTITY :: Ptr COAUTHIDENTITY
                    -> COAUTHIDENTITY
                    -> IO ()
writeCOAUTHIDENTITY ptr (COAUTHIDENTITY user domain password flags) =
  let
   userLength = (fromIntegral (length user) :: Word32)
  in
  do
    user <- marshallString user
    let domainLength = (fromIntegral (length domain) :: Word32)
    domain <- marshallString domain
    let passwordLength = (fromIntegral (length password) :: Word32)
    password <- marshallString password
    let pf0 = ptr
        pf1 = addNCastPtr pf0 0
    writePtr pf1 user
    let pf2 = addNCastPtr pf1 4
    writeWord32 pf2 userLength
    let pf3 = addNCastPtr pf2 4
    writePtr pf3 domain
    let pf4 = addNCastPtr pf3 4
    writeWord32 pf4 domainLength
    let pf5 = addNCastPtr pf4 4
    writePtr pf5 password
    let pf6 = addNCastPtr pf5 4
    writeWord32 pf6 passwordLength
    let pf7 = addNCastPtr pf6 4
    writeWord32 pf7 flags

readCOAUTHIDENTITY :: Ptr COAUTHIDENTITY
                   -> IO COAUTHIDENTITY
readCOAUTHIDENTITY ptr =
  let
   pf0 = ptr
   pf1 = addNCastPtr pf0 0
  in
  do
    user <- readPtr pf1
    let pf2 = addNCastPtr pf1 4
    userLength <- readWord32 pf2
    let pf3 = addNCastPtr pf2 4
    domain <- readPtr pf3
    let pf4 = addNCastPtr pf3 4
    domainLength <- readWord32 pf4
    let pf5 = addNCastPtr pf4 4
    password <- readPtr pf5
    let pf6 = addNCastPtr pf5 4
    passwordLength <- readWord32 pf6
    let pf7 = addNCastPtr pf6 4
    flags <- readWord32 pf7
    userLength <- unmarshallWord32 userLength
    domainLength <- unmarshallWord32 domainLength
    passwordLength <- unmarshallWord32 passwordLength
    user <- unmarshallString user
    domain <- unmarshallString domain
    password <- unmarshallString password
    return (COAUTHIDENTITY user domain password flags)

sizeofCOAUTHIDENTITY :: Word32
sizeofCOAUTHIDENTITY = 28

data COAUTHINFO = COAUTHINFO {dwAuthnSvc :: DWORD,
                              dwAuthzSvc :: DWORD,
                              pwszServerPrincName :: WideString,
                              dwAuthnLevel :: DWORD,
                              dwImpersonationLevel :: DWORD,
                              pAuthIdentityData :: (Maybe COAUTHIDENTITY),
                              dwCapabilities :: DWORD}
                    
writeCOAUTHINFO :: Ptr COAUTHINFO
                -> COAUTHINFO
                -> IO ()
writeCOAUTHINFO ptr (COAUTHINFO dwAuthnSvc dwAuthzSvc pwszServerPrincName dwAuthnLevel dwImpersonationLevel pAuthIdentityData dwCapabilities) =
  let
   pf0 = ptr
   pf1 = addNCastPtr pf0 0
  in
  do
    writeWord32 pf1 dwAuthnSvc
    let pf2 = addNCastPtr pf1 4
    writeWord32 pf2 dwAuthzSvc
    let pf3 = addNCastPtr pf2 4
    writeWideString pf3 pwszServerPrincName
    let pf4 = addNCastPtr pf3 4
    writeWord32 pf4 dwAuthnLevel
    let pf5 = addNCastPtr pf4 4
    writeWord32 pf5 dwImpersonationLevel
    let pf6 = addNCastPtr pf5 4
    writeunique (allocBytes (fromIntegral sizeofCOAUTHIDENTITY)) writeCOAUTHIDENTITY pf6 pAuthIdentityData
    let pf7 = addNCastPtr pf6 4
    writeWord32 pf7 dwCapabilities

readCOAUTHINFO :: Ptr COAUTHINFO
               -> IO COAUTHINFO
readCOAUTHINFO ptr =
  let
   pf0 = ptr
   pf1 = addNCastPtr pf0 0
  in
  do
    dwAuthnSvc <- readWord32 pf1
    let pf2 = addNCastPtr pf1 4
    dwAuthzSvc <- readWord32 pf2
    let pf3 = addNCastPtr pf2 4
    pwszServerPrincName <- readWideString pf3
    let pf4 = addNCastPtr pf3 4
    dwAuthnLevel <- readWord32 pf4
    let pf5 = addNCastPtr pf4 4
    dwImpersonationLevel <- readWord32 pf5
    let pf6 = addNCastPtr pf5 4
    pAuthIdentityData <- readunique readCOAUTHIDENTITY pf6
    let pf7 = addNCastPtr pf6 4
    dwCapabilities <- readWord32 pf7
    return (COAUTHINFO dwAuthnSvc dwAuthzSvc pwszServerPrincName dwAuthnLevel dwImpersonationLevel pAuthIdentityData dwCapabilities)

sizeofCOAUTHINFO :: Word32
sizeofCOAUTHINFO = 28

data COSERVERINFO = COSERVERINFO {dwReserved1 :: DWORD,
                                  pwszName :: WideString,
                                  pAuthInfo :: (Maybe COAUTHINFO),
                                  dwReserved2 :: DWORD}
                      
writeCOSERVERINFO :: Ptr COSERVERINFO
                  -> COSERVERINFO
                  -> IO ()
writeCOSERVERINFO ptr (COSERVERINFO dwReserved1 pwszName pAuthInfo dwReserved2) =
  let
   pf0 = ptr
   pf1 = addNCastPtr pf0 0
  in
  do
    writeWord32 pf1 dwReserved1
    let pf2 = addNCastPtr pf1 4
    writeWideString pf2 pwszName
    let pf3 = addNCastPtr pf2 4
    writeunique (allocBytes (fromIntegral sizeofCOAUTHINFO)) writeCOAUTHINFO pf3 pAuthInfo
    let pf4 = addNCastPtr pf3 4
    writeWord32 pf4 dwReserved2

readCOSERVERINFO :: Ptr COSERVERINFO
                 -> IO COSERVERINFO
readCOSERVERINFO ptr =
  let
   pf0 = ptr
   pf1 = addNCastPtr pf0 0
  in
  do
    dwReserved1 <- readWord32 pf1
    let pf2 = addNCastPtr pf1 4
    pwszName <- readWideString pf2
    let pf3 = addNCastPtr pf2 4
    pAuthInfo <- readunique readCOAUTHINFO pf3
    let pf4 = addNCastPtr pf3 4
    dwReserved2 <- readWord32 pf4
    return (COSERVERINFO dwReserved1 pwszName pAuthInfo dwReserved2)

sizeofCOSERVERINFO :: Word32
sizeofCOSERVERINFO = 16

data MULTI_QI_PRIM = MULTI_QI {pIID :: PGUID,
                               pItf :: (Ptr ()),
                               hr :: HRESULT}
                       
writeMULTI_QI_PRIM :: Ptr MULTI_QI_PRIM
                   -> MULTI_QI_PRIM
                   -> IO ()
writeMULTI_QI_PRIM ptr (MULTI_QI pIID pItf hr) =
  let
   pf0 = ptr
   pf1 = addNCastPtr pf0 0
  in
  do
    writePtr pf1 pIID
    let pf2 = addNCastPtr pf1 4
    writePtr pf2 pItf
    let pf3 = addNCastPtr pf2 4
    writeInt32 pf3 hr

readMULTI_QI_PRIM :: Ptr MULTI_QI_PRIM
                  -> IO MULTI_QI_PRIM
readMULTI_QI_PRIM ptr =
  let
   pf0 = ptr
   pf1 = addNCastPtr pf0 0
  in
  do
    pIID <- readPtr pf1
    let pf2 = addNCastPtr pf1 4
    pItf <- readPtr pf2
    let pf3 = addNCastPtr pf2 4
    hr <- readInt32 pf3
    return (MULTI_QI pIID pItf hr)

sizeofMULTI_QI_PRIM :: Word32
sizeofMULTI_QI_PRIM = 12

coCreateInstanceEx :: ForeignPtr ()
                   -> ForeignPtr ()
                   -> DWORD
                   -> Maybe COSERVERINFO
                   -> [MULTI_QI_PRIM]
                   -> IO [MULTI_QI_PRIM]
coCreateInstanceEx clsid pUnkOuter dwClsCtx pServerInfo pResults =
  let
   cmq = (fromIntegral (length pResults) :: Word32)
  in
  do
    pResults <- marshalllist sizeofMULTI_QI_PRIM writeMULTI_QI_PRIM pResults
    pServerInfo <- marshallunique (allocBytes (fromIntegral sizeofCOSERVERINFO)) writeCOSERVERINFO pServerInfo
    o_coCreateInstanceEx <- withForeignPtr clsid (\ clsid -> withForeignPtr pUnkOuter (\ pUnkOuter -> prim_System_Win32_Com_Base_coCreateInstanceEx clsid pUnkOuter dwClsCtx pServerInfo cmq pResults))
    free pServerInfo
    checkHR o_coCreateInstanceEx
    cmq <- unmarshallWord32 cmq
    unmarshalllist sizeofMULTI_QI_PRIM 0 cmq readMULTI_QI_PRIM pResults

foreign import stdcall "CoCreateInstanceEx" prim_System_Win32_Com_Base_coCreateInstanceEx :: Ptr () -> Ptr () -> Word32 -> Ptr COSERVERINFO -> Word32 -> Ptr MULTI_QI_PRIM -> IO Int32
getActiveObject :: ForeignPtr ()
                -> Ptr ()
                -> Ptr ()
                -> IO ()
getActiveObject clsid inner ppv =
  do
    o_getActiveObject <- withForeignPtr clsid (\ clsid -> prim_System_Win32_Com_Base_getActiveObject clsid inner ppv)
    checkHR o_getActiveObject

foreign import stdcall "GetActiveObject" prim_System_Win32_Com_Base_getActiveObject :: Ptr () -> Ptr () -> Ptr () -> IO Int32
primQI :: Ptr ()
       -> Ptr ()
       -> ForeignPtr ()
       -> Ptr (Ptr ())
       -> IO ()
primQI methPtr iptr riid ppv =
  do
    o_primQI <- withForeignPtr riid (\ riid -> prim_System_Win32_Com_Base_primQI methPtr iptr riid ppv)
    checkHR o_primQI

foreign import ccall "primQI" prim_System_Win32_Com_Base_primQI :: Ptr () -> Ptr () -> Ptr () -> Ptr (Ptr ()) -> IO Int32
primAddRef :: Ptr ()
           -> Ptr ()
           -> IO Word32
primAddRef methPtr iptr =
  prim_System_Win32_Com_Base_primAddRef methPtr
                                        iptr

foreign import ccall "primAddRef" prim_System_Win32_Com_Base_primAddRef :: Ptr () -> Ptr () -> IO Word32
primRelease :: Ptr ()
            -> Ptr ()
            -> IO Word32
primRelease methPtr iptr =
  prim_System_Win32_Com_Base_primRelease methPtr
                                         iptr

foreign import ccall "primRelease" prim_System_Win32_Com_Base_primRelease :: Ptr () -> Ptr () -> IO Word32
primEnumNext :: Ptr ()
             -> Ptr ()
             -> Word32
             -> Ptr ()
             -> Ptr ()
             -> IO ()
primEnumNext methPtr iptr celt ptr po =
  do
    o_primEnumNext <- prim_System_Win32_Com_Base_primEnumNext methPtr iptr celt ptr po
    checkHR o_primEnumNext

foreign import ccall "primEnumNext" prim_System_Win32_Com_Base_primEnumNext :: Ptr () -> Ptr () -> Word32 -> Ptr () -> Ptr () -> IO Int32
primEnumSkip :: Ptr ()
             -> Ptr ()
             -> Word32
             -> IO ()
primEnumSkip methPtr iptr celt =
  do
    o_primEnumSkip <- prim_System_Win32_Com_Base_primEnumSkip methPtr iptr celt
    checkHR o_primEnumSkip

foreign import ccall "primEnumSkip" prim_System_Win32_Com_Base_primEnumSkip :: Ptr () -> Ptr () -> Word32 -> IO Int32
primEnumReset :: Ptr ()
              -> Ptr ()
              -> IO ()
primEnumReset methPtr iptr =
  do
    o_primEnumReset <- prim_System_Win32_Com_Base_primEnumReset methPtr iptr
    checkHR o_primEnumReset

foreign import ccall "primEnumReset" prim_System_Win32_Com_Base_primEnumReset :: Ptr () -> Ptr () -> IO Int32
primEnumClone :: Ptr ()
              -> Ptr ()
              -> Ptr ()
              -> IO ()
primEnumClone methPtr iptr ppv =
  do
    o_primEnumClone <- prim_System_Win32_Com_Base_primEnumClone methPtr iptr ppv
    checkHR o_primEnumClone

foreign import ccall "primEnumClone" prim_System_Win32_Com_Base_primEnumClone :: Ptr () -> Ptr () -> Ptr () -> IO Int32
primPersistLoad :: Ptr ()
                -> Ptr ()
                -> Ptr Wchar_t
                -> Word32
                -> IO ()
primPersistLoad methPtr iptr pszFileName dwMode =
  do
    o_primPersistLoad <- prim_System_Win32_Com_Base_primPersistLoad methPtr iptr pszFileName dwMode
    checkHR o_primPersistLoad

foreign import ccall "primPersistLoad" prim_System_Win32_Com_Base_primPersistLoad :: Ptr () -> Ptr () -> Ptr Word16 -> Word32 -> IO Int32
primNullIID :: IO (Ptr ())
primNullIID = prim_System_Win32_Com_Base_primNullIID

foreign import ccall "primNullIID" prim_System_Win32_Com_Base_primNullIID :: IO (Ptr ())
loadTypeLib :: Ptr Wchar_t
            -> Ptr ()
            -> IO ()
loadTypeLib pfname ptr =
  do
    o_loadTypeLib <- prim_System_Win32_Com_Base_loadTypeLib pfname ptr
    checkHR o_loadTypeLib

foreign import stdcall "LoadTypeLib" prim_System_Win32_Com_Base_loadTypeLib :: Ptr Word16 -> Ptr () -> IO Int32
loadTypeLibEx :: Ptr Wchar_t
              -> Int32
              -> Ptr ()
              -> IO ()
loadTypeLibEx pfname rkind ptr =
  do
    o_loadTypeLibEx <- prim_System_Win32_Com_Base_loadTypeLibEx pfname rkind ptr
    checkHR o_loadTypeLibEx

foreign import stdcall "LoadTypeLibEx" prim_System_Win32_Com_Base_loadTypeLibEx :: Ptr Word16 -> Int32 -> Ptr () -> IO Int32
loadRegTypeLib :: ForeignPtr ()
               -> Int32
               -> Int32
               -> Int32
               -> Ptr ()
               -> IO ()
loadRegTypeLib pguid maj min lcid ptr =
  do
    o_loadRegTypeLib <- withForeignPtr pguid (\ pguid -> prim_System_Win32_Com_Base_loadRegTypeLib pguid maj min lcid ptr)
    checkHR o_loadRegTypeLib

foreign import stdcall "LoadRegTypeLib" prim_System_Win32_Com_Base_loadRegTypeLib :: Ptr () -> Int32 -> Int32 -> Int32 -> Ptr () -> IO Int32
primQueryPathOfRegTypeLib :: ForeignPtr ()
                          -> Word16
                          -> Word16
                          -> IO (Ptr Wchar_t)
primQueryPathOfRegTypeLib pgd maj min =
  withForeignPtr pgd
                 (\ pgd -> prim_System_Win32_Com_Base_primQueryPathOfRegTypeLib pgd maj min)

foreign import ccall "primQueryPathOfRegTypeLib" prim_System_Win32_Com_Base_primQueryPathOfRegTypeLib :: Ptr () -> Word16 -> Word16 -> IO (Ptr Word16)
addrOfReleaseIUnknown :: Ptr ()
addrOfReleaseIUnknown =
  unsafePerformIO (prim_System_Win32_Com_Base_addrOfReleaseIUnknown)

foreign import ccall "addrOfReleaseIUnknown" prim_System_Win32_Com_Base_addrOfReleaseIUnknown :: IO (Ptr ())
bstrToStringLen :: Ptr String
                -> Int32
                -> Ptr Char
                -> IO ()
bstrToStringLen bstr len str =
  do
    o_bstrToStringLen <- prim_System_Win32_Com_Base_bstrToStringLen bstr len str
    checkHR o_bstrToStringLen

foreign import ccall "bstrToStringLen" prim_System_Win32_Com_Base_bstrToStringLen :: Ptr String -> Int32 -> Ptr Char -> IO Int32
bstrLen :: Ptr String
        -> Int32
bstrLen bstr =
  unsafePerformIO (prim_System_Win32_Com_Base_bstrLen bstr)

foreign import ccall "bstrLen" prim_System_Win32_Com_Base_bstrLen :: Ptr String -> IO Int32
stringToBSTR :: Ptr String
             -> IO (Ptr String)
stringToBSTR bstr =
  do
    res <- allocBytes (fromIntegral sizeofPtr)
    o_stringToBSTR <- prim_System_Win32_Com_Base_stringToBSTR bstr res
    checkHR o_stringToBSTR
    return (res)

foreign import ccall "stringToBSTR" prim_System_Win32_Com_Base_stringToBSTR :: Ptr String -> Ptr String -> IO Int32
getModuleFileName :: Ptr ()
                  -> IO String
getModuleFileName hModule =
  do
    o_getModuleFileName <- prim_System_Win32_Com_Base_getModuleFileName hModule
    doThenFree free_malloc unmarshallString o_getModuleFileName

foreign import ccall "getModuleFileName" prim_System_Win32_Com_Base_getModuleFileName :: Ptr () -> IO (Ptr String)
messagePump :: IO ()
messagePump =
  prim_System_Win32_Com_Base_messagePump

foreign import ccall "messagePump" prim_System_Win32_Com_Base_messagePump :: IO ()
postQuitMsg :: IO ()
postQuitMsg =
  prim_System_Win32_Com_Base_postQuitMsg

foreign import ccall "postQuitMsg" prim_System_Win32_Com_Base_postQuitMsg :: IO ()
primOutputDebugString :: String
                      -> IO ()
primOutputDebugString msg =
  do
    msg <- marshallString msg
    prim_System_Win32_Com_Base_primOutputDebugString msg
    freeString msg

foreign import stdcall "OutputDebugStringA" prim_System_Win32_Com_Base_primOutputDebugString :: Ptr String -> IO ()
primGetVersionInfo :: IO (Word32, Word32, Word32)
primGetVersionInfo =
  do
    maj <- allocBytes (fromIntegral sizeofWord32)
    min <- allocBytes (fromIntegral sizeofWord32)
    pid <- allocBytes (fromIntegral sizeofWord32)
    prim_System_Win32_Com_Base_primGetVersionInfo maj min pid
    maj <- doThenFree free readWord32 maj
    min <- doThenFree free readWord32 min
    pid <- doThenFree free readWord32 pid
    return (maj, min, pid)

foreign import ccall "primGetVersionInfo" prim_System_Win32_Com_Base_primGetVersionInfo :: Ptr Word32 -> Ptr Word32 -> Ptr Word32 -> IO ()
coRegisterClassObject :: ForeignPtr ()
                      -> ForeignPtr ()
                      -> Int32
                      -> Int32
                      -> IO Word32
coRegisterClassObject rclsid pUnk dwClsContext flags0 =
  do
    lpwRegister <- allocBytes (fromIntegral sizeofWord32)
    o_coRegisterClassObject <- withForeignPtr rclsid (\ rclsid -> withForeignPtr pUnk (\ pUnk -> prim_System_Win32_Com_Base_coRegisterClassObject rclsid pUnk dwClsContext flags0 lpwRegister))
    checkHR o_coRegisterClassObject
    doThenFree free readWord32 lpwRegister

foreign import stdcall "CoRegisterClassObject" prim_System_Win32_Com_Base_coRegisterClassObject :: Ptr () -> Ptr () -> Int32 -> Int32 -> Ptr Word32 -> IO Int32

