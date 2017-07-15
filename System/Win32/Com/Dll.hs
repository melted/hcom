-----------------------------------------------------------------------------
-- |
-- Module      :  System.Win32.Com.Dll
-- Copyright   :  (c) Sigbjorn Finne <sof@dcs.gla.ac.uk> 1998-99
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  sof@forkIO.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Support for sealing up Haskell code as an in-proc COM server.
--
-- The main function is 'createIComDll' which packages up a list of 'ComponentInfo'
-- values specifying the Haskell implementation of COM objects. It returns
-- a method table which you can then wrap up as a DLL by calling its
-- COM-mandated entry points via.
-- 
-----------------------------------------------------------------------------
module System.Win32.Com.Dll
	(  
	   ComponentInfo(..)
	,  mkComponentInfo
	,  withComponentName
	,  withProgID
	,  withVerIndepProgID
	,  onRegister
	,  onFinalize
	,  hasTypeLib

	,  createIComDll   -- :: Ptr (){-HMODULE-} -> [ComponentInfo] -> IO (VTable iid_comDllState ComDllState)

	,  regAddEntry
	,  regRemoveEntry
	,  RegHive(..)
	
	,  stdRegComponent
	,  stdUnRegComponent
	
	,  ComponentFactory
	) where

import System.Win32.Com.ClassFactory
import System.Win32.Com
import System.Win32.Com.Exception
import System.Win32.Com.Server
import System.Win32.Com.Base ( getModuleFileName )
import System.Win32.Com.HDirect.HDirect ( Ptr, marshallString )

import Foreign  hiding ( Ptr )
import Data.Word ( Word32 )
import System.Win32.Com.HDirect.HDirect ( marshallMaybe )
import Data.IORef ( IORef, newIORef, readIORef, writeIORef )

import Control.Monad
import Data.List  ( find )

-- | The information an implementation of a Haskell COM component
-- needs to supply in order to hook into the machinery provided
-- by @System.Win32.Com.Dll@ for interfacing to how COM does
-- activation for in-proc components
--
-- To create one or more, you pass them into 'createIComDll'..
data ComponentInfo
  = ComponentInfo
      { newInstance       :: ComponentFactory -- ^ the object factory method.
      , componentFinalise :: IO ()    -- ^ IO action to call when the 
      , componentName     :: String   -- ^ the display name.
      , componentProgID   :: String   -- ^ the programmatic/VB ID string (versioned, if needed.)
      , componentVProgID  :: String   -- ^ and without version info.
      , componentTLB      :: Bool     -- ^ @True@ if backed by a TLB.
      , registerComponent :: ComponentInfo -> String -> Bool -> IO () -- ^ action to run when (un)registering this component.
      , componentCLSID    :: CLSID    -- ^ the @CLSID@; required, of course.
      }

-- | @((\ path final iid -> IO obj)::ComponentFactory@ is the component-specific
-- object factory method:
-- 
-- * @path@ is the path to the DLL implementing the component (useful for TLB lookup etc.)
--
-- * @final@ is finalization action for the object.
--
-- * @iid@ is the @IID@ to create object at.
--
-- * @obj@ is the newly created object at interface @iid@.
-- 
type ComponentFactory 
  = String
 -> IO ()
 -> IID (IUnknown ())
 -> IO (IUnknown ())

-- | @withProgID p ci@ returns a new ComponentInfo based on @ci@, 
-- but with 'ProgID' set to @p@.
withProgID :: String -> ComponentInfo -> ComponentInfo
withProgID p info = info{componentProgID=p}

-- | @onRegister act ci@ returns a new ComponentInfo based on @ci@, 
-- but with the (un)registration action /extended/ with @act@.
onRegister :: (ComponentInfo -> String -> Bool -> IO ()) -> ComponentInfo -> ComponentInfo
onRegister reg info = 
  info{registerComponent= \ a b c -> reg a b c >> (registerComponent info) a b c}

-- | @onFinalize act ci@ returns a new ComponentInfo based on @ci@, 
-- but with the finalization action /extended/ with @act@.
onFinalize :: IO () -> ComponentInfo -> ComponentInfo
onFinalize act info = info{componentFinalise= act >> (componentFinalise info)}

-- | @withVerProgID vp ci@ returns a new ComponentInfo based on @ci@, 
-- but with version-independent 'ProgID' set to @vp@.
withVerIndepProgID :: String -> ComponentInfo -> ComponentInfo
withVerIndepProgID p info = info{componentVProgID=p}

-- | @withFinalizer act ci@ returns a new ComponentInfo based on @ci@, 
-- but with its finalizer action set to @act@.
withFinaliser :: IO () -> ComponentInfo -> ComponentInfo
withFinaliser act info = info{componentFinalise=act}

-- | @withComponentName nm ci@ returns a new 'ComponentInfo' based on @ci@, 
-- but with its name set to @nm@.
withComponentName :: String -> ComponentInfo -> ComponentInfo
withComponentName n info = info{componentName=n}

-- | @hasTypeLib ci@ returns a new 'ComponentInfo' based on @ci@, but being type library backed.
hasTypeLib :: ComponentInfo -> ComponentInfo
hasTypeLib info = info{componentTLB=True}

-- | The @mkComponentInfo@ used to lessen the reliance on concrete representation
-- of 'ComponentInfo'.
mkComponentInfo :: CLSID
		-> (String -> Bool -> IO ())
		-> (String -> IO () -> IID (IUnknown ()) -> IO (IUnknown ()))
		-> ComponentInfo
mkComponentInfo cls reg n = ComponentInfo n (return ()) "" "" "" False (\ _ -> reg) cls

-- | The (internal) state maintained by each instance of a @ComDll@
-- wrapper:
data ComDllState
  = ComDllState {
      dllPath    :: String,
      components :: IORef [ComponentInfo],
      lockCount  :: IORef Int
     }

dllGetClassObject :: ComDllState -> Ptr CLSID -> Ptr (IID a) -> Ptr (Ptr (IUnknown a)) -> IO HRESULT
dllGetClassObject comDll rclsid riid ppvObject = do
  iid <- unmarshallIID False (castPtr riid)
  let g = iidToGUID iid
  if ( not (g == iidToGUID iidIClassFactory || g == iidToGUID iidIUnknown) ) then
     return e_NOINTERFACE
   else do
    clsid <- unmarshallCLSID False rclsid
    cs    <- readIORef (components comDll)
    case lookupCLSID clsid cs of
      Nothing -> return cLASS_E_CLASSNOTAVAILABLE
      Just i  -> do
         ip <- createClassFactory (newInstance i (dllPath comDll) (componentFinalise i))
	 writeIUnknown False ppvObject ip
	 return s_OK

lookupCLSID :: CLSID -> [ComponentInfo] -> Maybe ComponentInfo
lookupCLSID clsid cs = find (\ x -> clsidToGUID (componentCLSID x) == guid) cs
 where
  guid = clsidToGUID clsid

dllCanUnloadNow :: ComDllState -> IO HRESULT
dllCanUnloadNow state = do
   c <- readIORef (lockCount state)
   if c == 0 then 
     return s_OK
    else
     return s_FALSE

dllRegisterServer :: ComDllState -> IO HRESULT
dllRegisterServer = registerServer True

dllUnregisterServer :: ComDllState -> IO HRESULT
dllUnregisterServer = registerServer False

registerServer :: Bool -> ComDllState -> IO HRESULT
registerServer isReg st = do
  cs   <- readIORef (components st)
  let
   path = dllPath st
   regComponent info
     | not isReg = do
	 -- give the user-supplied un-reg action the opportunity
	 -- to delete some entries first.
       (registerComponent info) info path isReg
       stdUnRegComponent info True path
     | otherwise = do
       stdRegComponent info True path
       (registerComponent info) info path isReg


  mapM_ regComponent cs
  case s_OK of
    14 -> return s_OK
    x  -> return x

dllUnload :: ComDllState -> IO ()
dllUnload st = return ()

-- | @newComDllState h cis@ creates the internal representation of a Haskell
-- COM component
newComDllState :: Ptr (){-HANDLE-} -> [ComponentInfo] -> IO ComDllState
newComDllState hMod cs = do
  path   <- getModuleFileName hMod
  ref_cs <- newIORef cs
    -- The lock count is intended used by DllCanUnloadNow() to keep
    -- track of when the Com DLL can safely be unloaded. It is only
    -- safe to do so if no COM interface pointers handed out by the 
    -- component are currently alive.
    --
    -- The Com library doesn't yet try to keep track of this, so the
    -- lock count is always left at 1 (==> DllCanUnloadNow() always returns
    -- S_FALSE.)
  lc     <- newIORef 1
  return (ComDllState path ref_cs lc)

-- | @createIComDll hMod cis@ creates the method table for an inproc server
-- supporting the components specified by @cis@. The method table would
-- then be wrapped up by a Haskell DLL wrapper supporting the in-proc
-- DLL entry points.
createIComDll :: Ptr (){-HMODULE-} -> [ComponentInfo] -> IO (VTable iid_comDllState ComDllState)
createIComDll hMod components = do
   state       <- newComDllState hMod components
   meths       <- iComDllEntryPoints state
   createVTable meths

iComDllEntryPoints :: ComDllState -> IO [Ptr ()]
iComDllEntryPoints state = do
  addrOf_DllUnload	     <- export_DllUnload   (dllUnload state)
  addrOf_DllCanUnloadNow     <- export_nullaryMeth (dllCanUnloadNow state)
  addrOf_DllRegisterServer   <- export_nullaryMeth (dllRegisterServer state)
  addrOf_DllUnregisterServer <- export_nullaryMeth (dllUnregisterServer state)
  addrOf_DllGetClassObject   <- export_dllGetClassObject (dllGetClassObject state)
  return [ addrOf_DllUnload
         , addrOf_DllCanUnloadNow
	 , addrOf_DllRegisterServer
	 , addrOf_DllUnregisterServer
	 , addrOf_DllGetClassObject
	 ]

foreign import ccall "wrapper"
   export_DllUnload :: IO () -> IO (Ptr (IO ()))
foreign import ccall "wrapper"
   export_nullaryMeth :: IO HRESULT -> IO (Ptr (IO HRESULT))

foreign import ccall "wrapper"
   export_dllGetClassObject :: (Ptr CLSID -> Ptr (IID a) -> Ptr (Ptr (IUnknown a)) -> IO HRESULT)
                                 -> IO (Ptr (Ptr CLSID -> Ptr (IID a) -> Ptr (Ptr (IUnknown a)) -> IO HRESULT))

data RegHive
 = HKEY_CLASSES_ROOT
 | HKEY_CURRENT_USER
 | HKEY_LOCAL_MACHINE
 | HKEY_USERS
 | HKEY_CURRENT_CONFIG
   deriving ( Eq, Ord, Enum )

-- | @regAddEntry hive path val@ is a convenient local wrapper to the Win32
-- API function @RegAddEntry()@. 
regAddEntry :: RegHive
	    -> String
	    -> Maybe String
	    -> IO ()
regAddEntry hive path value = do
   m_path  <- marshallString path
   m_value <- marshallMaybe marshallString nullPtr value
   hr      <- primRegAddEntry (fromEnum hive) m_path m_value
   checkHR hr

-- | @regRemoveEntry hive path val doRemove@ is a convenient local wrapper to the Win32
-- API function @RegRemoveEntry()@. 
regRemoveEntry :: RegHive
	       -> String
	       -> String
	       -> Bool
	       -> IO ()
regRemoveEntry hive path value removeKey = do
   m_path  <- marshallString path
   m_value <- marshallString value
   let m_removeKey
        | removeKey = (1::Int)
        | otherwise = 0
   hr      <- primRegRemoveEntry (fromEnum hive) m_path m_value m_removeKey
   checkHR hr

foreign import ccall "primRegAddEntry" 
   primRegAddEntry :: Int -> Ptr String -> Ptr String -> IO HRESULT
foreign import ccall "primRegRemoveEntry" 
   primRegRemoveEntry :: Int -> Ptr String -> Ptr String -> Int -> IO HRESULT

stdRegComponent :: ComponentInfo -> Bool -> String -> IO ()
stdRegComponent info isInProc path = do
   let clsid_path = "CLSID\\" ++ clsid_str
       progid     = componentProgID info
       vprogid    = componentVProgID info
       clsid_str  = show (componentCLSID info)

        -- Add CLSID\{clsid}\friendly name
   regAddEntry HKEY_CLASSES_ROOT clsid_path (Just (componentName info))
        -- Add CLSID\{clsid}\ProgID (if any.)
   when (not (null progid)) (regAddEntry HKEY_CLASSES_ROOT (clsid_path++"\\ProgID") (Just progid))
        -- Add CLSID\{clsid}\VersionIndependentProgID (if any.)
   when (not (null vprogid)) (regAddEntry HKEY_CLASSES_ROOT (clsid_path++"\\VersionIndependentProgID") (Just vprogid))
        -- Add CLSID\{clsid}\{Inproc,Local}Server32\path
   regAddEntry HKEY_CLASSES_ROOT (clsid_path ++ (if isInProc then "\\InprocServer32" else "\\LocalServer32")) (Just path)

      -- register the type library; we don't care if it fails, or not.
   when (componentTLB info)
        (catch (loadTypeLibEx path True{-register-} >>= \ p -> p # release >> return ())
	       (\ _ -> return ()))

	-- Add the ProgID entries

   when (not (null progid))  (regAddEntry HKEY_CLASSES_ROOT (progid  ++ "\\CLSID") (Just clsid_str))
   when (not (null vprogid)) (regAddEntry HKEY_CLASSES_ROOT (vprogid ++ "\\CLSID") (Just clsid_str))
   when (not (null vprogid) && not (null progid))
                             (regAddEntry HKEY_CLASSES_ROOT (progid ++ "\\CurVer") (Just vprogid))
   return ()

--The removal of entries isn't quite right

stdUnRegComponent :: ComponentInfo -> Bool -> String -> IO ()
stdUnRegComponent info isInProc path = do
   let clsid_path = "CLSID\\" ++ clsid_str
       progid     = componentProgID info
       vprogid    = componentVProgID info
       clsid_str  = show (componentCLSID info)

        -- Remove CLSID\{clsid}\{Local,Inproc}Server32
   regRemoveEntry HKEY_CLASSES_ROOT clsid_path (if isInProc then "InprocServer32" else "LocalServer32") True
        -- Remove CLSID\{clsid}\VersionIndependentProgID (if any.)
   when (not (null vprogid)) (regRemoveEntry HKEY_CLASSES_ROOT clsid_path "VersionIndependentProgID" True)
        -- Remove CLSID\{clsid}\ProgID (if any.)
   when (not (null progid)) (regRemoveEntry HKEY_CLASSES_ROOT clsid_path "ProgID" True)
        -- Remove CLSID\{clsid}\friendly name
   regRemoveEntry HKEY_CLASSES_ROOT "CLSID" clsid_str True

	-- Remove the ProgID entries
   when (not (null progid))  (regRemoveEntry HKEY_CLASSES_ROOT (progid  ++ "\\CLSID") clsid_str False)
   when (not (null progid))  (regRemoveEntry HKEY_CLASSES_ROOT progid  "CLSID"  True)
   when (not (null progid) && not (null vprogid)) 
			     (regRemoveEntry HKEY_CLASSES_ROOT  progid "CurVer" True)
   when (not (null vprogid)) (regRemoveEntry HKEY_CLASSES_ROOT (vprogid ++ "\\CLSID") clsid_str False)
   when (not (null vprogid)) (regRemoveEntry HKEY_CLASSES_ROOT vprogid  "CLSID" True)
   return ()
