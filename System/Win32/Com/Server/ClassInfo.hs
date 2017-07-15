module System.Win32.Com.Server.ClassInfo
         (
	   mkIProvideClassInfo
	 , mkIProvideClassInfo2

	 , IProvideClassInfo(..)
	 , IProvideClassInfo_
	 , iidIProvideClassInfo

	 , IProvideClassInfo2(..)
	 , IProvideClassInfo2_
	 , iidIProvideClassInfo2

	 ) where

import System.Win32.Com
import Data.Word ( Word32 )
import System.Win32.Com.Exception
import System.Win32.Com.Automation.TypeLib
import System.Win32.Com.Server
import Foreign.Ptr
import System.Win32.Com.HDirect.HDirect ( Ptr, writePtr )
import System.Win32.Com.Base (lOCALE_USER_DEFAULT)
import System.IO.Unsafe

type ThisPtr = Ptr ()


data IProvideClassInfo_ a = IProvideClassInfo__ 
type IProvideClassInfo  a = IUnknown (IProvideClassInfo_ a)

iidIProvideClassInfo :: IID (IProvideClassInfo ())
iidIProvideClassInfo = mkIID "{B196B283-BAB4-101A-B69C-00AA00341D07}"

data IProvideClassInfo2_ a = IProvideClassInfo2__ 
type IProvideClassInfo2  a = IProvideClassInfo (IProvideClassInfo2_ a)

iidIProvideClassInfo2 :: IID (IProvideClassInfo2 ())
iidIProvideClassInfo2 = mkIID "{A6BC3AC0-DBAA-11CE-9DE3-00AA004BB851}"

mkIProvideClassInfo :: Either LIBID String
		    -> CLSID
		    -> IO (ComVTable (IProvideClassInfo ()) objState)
mkIProvideClassInfo libid clsid = do
  let mb_tlib = unsafePerformIO (hoistInTLB libid)
  addrOf_gci <- castFunPtrToPtr export_gci (getClassInfo mb_tlib clsid)
  createComVTable [addrOf_gci]

mkIProvideClassInfo2 :: Either LIBID String
		     -> CLSID
		     -> IID a
		     -> IO (ComVTable (IProvideClassInfo2 ()) objState)
mkIProvideClassInfo2 libid clsid iid = do
  let mb_tlib = unsafePerformIO (hoistInTLB libid)
  addrOf_gci <- export_gci (getClassInfo mb_tlib clsid)
  addrOf_gg  <- export_gg  (getGUID iid)
  createComVTable [castPtr addrOf_gci, castPtr addrOf_gg]


hoistInTLB :: Either LIBID String
	   -> IO (Maybe (ITypeLib ()))
hoistInTLB tlb_loc = 
 catch (
   case tlb_loc of
     Left libid -> do
        ip <- loadRegTypeLib libid 1 0 (fromIntegral lOCALE_USER_DEFAULT)
	return (Just ip)
     Right loc -> do
        ip <- loadTypeLibEx loc False{-don't register-}
	return (Just ip))
  (\ _ -> return Nothing)

getClassInfo :: Maybe (ITypeLib ()) -> CLSID -> ThisPtr -> Ptr (Ptr (ITypeInfo ())) -> IO HRESULT
getClassInfo mb_tlib clsid this ppTI 
  | ppTI == nullPtr = return e_POINTER
  | otherwise	    = do
     case mb_tlib of
       Nothing -> return e_FAIL
       Just tl -> do
	 catch (do
           ti <- tl # getTypeInfoOfGuid (clsidToGUID clsid)
	   writeIUnknown True ppTI ti
	   return s_OK)
	  (\ _ -> do
	      writePtr ppTI nullPtr
	      return e_FAIL)

foreign import stdcall "wrapper" 
   export_gci :: (Ptr () -> Ptr (Ptr (ITypeInfo ())) -> IO HRESULT) -> IO (Ptr (Ptr () -> Ptr (Ptr (ITypeInfo ())) -> IO HRESULT))

getGUID :: IID a
	-> ThisPtr
	-> Word32
	-> Ptr GUID
	-> IO HRESULT
getGUID iid this kind pGUID
  | pGUID == nullPtr  = return e_POINTER
  | kind  /= 1        = return e_INVALIDARG -- 1 == GUIDKIND_DEFAULT_SOUCE_DISP_IID
  | otherwise         = do
     writeGUID pGUID (iidToGUID iid)
     return s_OK

foreign import stdcall "wrapper"
   export_gg :: (Ptr () -> Word32 -> Ptr (GUID) -> IO HRESULT) -> IO (Ptr (Ptr () -> Word32 -> Ptr (GUID) -> IO HRESULT))
