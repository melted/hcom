-----------------------------------------------------------------------------
-- |
-- Module      :  System.Win32.Com
-- Copyright   :  (c) Sigbjorn Finne, sof@dcs.gla.ac.uk 1999
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  sof@forkIO.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Support library for interacting with base Microsoft COM services and API.
-- 
-----------------------------------------------------------------------------
module System.Win32.Com
    (
      -- base COM interface, IUnknown:
      IUnknown_   -- abstract, instance of: Eq, Show.
    , IUnknown    
    , iidIUnknown  -- :: IID (IUnknown ())
    
    , interfaceNULL, isNullInterface, iidNULL

      -- its methods:
    , queryInterface -- :: IID (IUnknown b) -> IUnknown a -> IO (IUnknown b)
    , addRef -- :: IUnknown a -> IO Word32
    , release     -- :: IUnknown a -> IO Word32
    , withQueryInterface -- :: IID (IUnknown b) -> IUnknown a -> (IUnknown b -> IO c) -> IO c

      -- helpful operators:
    , ( # )    -- :: a -> (a -> IO b) -> IO b
    , ( ## )    -- :: IO a -> (a -> IO b) -> IO b


      -- setting up and shutting down.
    , coRun      -- :: IO a -> IO a
    , coPerformIO         -- :: IO a -> IO a
    , coUnsafePerformIO   -- :: IO a -> a
    , coInitialize    -- :: IO ()
    , coUnInitialize    -- :: IO ()
    
      -- GUID API:
    , GUID    -- abstract, instance of: Eq, Show
    , mkGUID    -- :: String -> GUID
    , newGUID           -- :: IO GUID
    , stringToGUID  -- :: String -> IO GUID
    , guidToString  -- :: GUID   -> String
    , nullGUID    -- :: GUID

      -- IID API:
    , IID    -- abstract, instance of: Eq, Show
    , mkIID    -- :: String -> IID a
    , stringToIID  -- :: String -> IO (IID a)
    , guidToIID    -- :: GUID   -> IID a
    , iidToGUID    -- :: IID a  -> GUID
    , castIID           -- :: IID a  -> IID b

      -- CLSID API:
    , CLSID     -- abstract, instance of: Eq, Show
    , mkCLSID     -- :: String -> CLSID
    , stringToCLSID   -- :: String -> IO CLSID
    , guidToCLSID   -- :: GUID   -> CLSID
    , clsidToGUID   -- :: CLSID  -> GUID
    , clsidToDisplayName -- :: CLSID  -> String

      -- LIBID
    , LIBID    -- (a guid)
    , mkLIBID           -- :: String -> LIBID

      -- HRESULT API:
    , HRESULT
    , s_FALSE    -- :: HRESULT
    , s_OK    -- :: HRESULT
    , succeeded          -- :: HRESULT -> Bool
    , failed          -- :: HRESULT -> Bool
    , checkHR    -- :: HRESULT -> IO ()
    , checkBool    -- :: Int32   -> IO ()
    , returnHR    -- :: IO ()   -> IO HRESULT
    , coFailHR    -- :: HRESULT -> IO a
    , coFailWithHR  -- :: HRESULT -> String  -> IO 
    , coAssert    -- :: Bool    -> String -> IO ()
    , coOnFail    -- :: IO a    -> String -> IO a
    , coFail    -- :: String  -> IO a
    , isCoError    -- :: IOError -> Bool
    , coGetErrorHR      -- :: IOError -> HRESULT
    , coGetErrorString  -- :: IOError -> String
    , hresultToString   -- :: HRESULT -> IO String
    
    , ComException(..)
    , catchComException
    , throwIOComException
    , throwComException

      -- component creation:
    , coCreateInstance -- :: CLSID -> Maybe (IUnknown b) -> CLSCTX
           -- -> IID (IUnknown a) -> IO (IUnknown a)
    , coCreateObject
    , coGetObject
    , coGetActiveObject
    , coGetFileObject
    , coCreateInstanceEx
    , COSERVERINFO(..)
    , COAUTHIDENTITY(..)
    , COAUTHINFO(..)

    , withObject   -- :: IUnknown a -> [IUnknown a -> IO b] -> IO [b]
    , withObject_  -- :: IUnknown a -> [IUnknown a -> IO b] -> IO ()
    , withMethod   -- :: (a -> IUnknown b -> IO c) -> [a] -> IUnknown b -> IO [c]
    , withMethod_  -- :: (a -> IUnknown b -> IO c) -> [a] -> IUnknown b -> IO ()


    , CLSCTX(..)

    , ProgID
    , progIDFromCLSID     -- :: CLSID  -> IO ProgID
    , clsidFromProgID     -- :: ProgID -> IO CLSID

    , printMessage
    , putMessage
    , messageBox
    , outputDebugString

    , OSVersionInfo(..)
    , isWindowsNT          -- :: OSVersionInfo -> Bool
    , isWindows95          -- :: OSVersionInfo -> Bool
    , isWindows98          -- :: OSVersionInfo -> Bool
    , versionInfo     -- :: OSVersionInfo

    , ifaceToAddr
    
      -- IEnum* methods.
    , enumNext
    , enumNextOne
    , enumClone
    , enumReset
    , enumSkip
    
    , BSTR
    , marshallBSTR
    , unmarshallBSTR
    , readBSTR
    , writeBSTR
    , freeBSTR
    , LPSTR
    
    , coFree
    , coAlloc
    
    , marshallIUnknown
    , unmarshallIUnknown
    , readIUnknown
    , writeIUnknown
    
    , unmarshallIUnknownFO
    , castIface
    
      -- Re-export WideStrings
    , WideString
    , marshallWideString
    , unmarshallWideString
    , writeWideString
    , readWideString
    , sizeofWideString
    , freeWideString
    
      -- marshallers
    , marshallGUID  -- :: GUID -> IO (ForeignPtr GUID)
    , unmarshallGUID  -- :: Bool -> Ptr GUID -> IO GUID
    , writeGUID
    , readGUID
    , copyGUID
    , sizeofGUID
    
      -- marshallers
    , marshallIID  -- :: GUID -> IO (ForeignPtr GUID)
    , unmarshallIID  -- :: Bool -> Ptr GUID -> IO GUID
    , writeIID
    , readIID
    , sizeofIID
    , copyIID
    
      -- marshallers
    , marshallCLSID  -- :: CLSID -> IO (ForeignPtr CLSID)
    , unmarshallCLSID  -- :: Bool -> Ptr CLSID -> IO GUID
    , writeCLSID
    , readCLSID
    , sizeofCLSID
    , copyCLSID
    
    , invokeAndCheck
    , invokeIt
    
    , loadTypeLib
    , loadTypeLibEx
    , loadRegTypeLib
    , queryPathOfRegTypeLib
    , createTypeLib
    
    , LCID
    
    , messagePump
    , postQuitMsg
    
    ) where


import System.Win32.Com.Exception
import System.Win32.Com.Base 
         hiding ( coCreateInstance, loadTypeLib, messageBox,
      loadTypeLibEx, loadRegTypeLib, coCreateInstanceEx
          )
import qualified System.Win32.Com.Base as Base
          ( coCreateInstance, loadTypeLib, messageBox,
         loadTypeLibEx, loadRegTypeLib, coCreateInstanceEx
          )
import System.Win32.Com.HDirect.HDirect
import System.Win32.Com.HDirect.Pointer hiding ( freeBSTR )
import qualified System.Win32.Com.HDirect.Pointer as P ( freeBSTR )
import System.Win32.Com.HDirect.WideString
import System.IO.Unsafe  ( unsafePerformIO )
import Control.Monad     ( when )
import Foreign.StablePtr ( deRefStablePtr )
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Marshal.Alloc ( allocaBytes )
import Data.Bits
import Control.Exception ( bracket ) 

infixl 1 #
infixl 0 ##

--Operators to provide OO-looking invocation of interface methods, e.g., 
--
--   ip # meth1 args

-- | The @#@ operator permits /OO-style/ method application with @do@ syntax:
--
-- @
--  obj # method arg1 arg2
-- @
--
-- is equivalent to @method arg1 arg2 obj@, so this assumes that the COM method 
-- wrappers takes the /this/ pointer as the last argument. Which the /HDirect/
-- generated wrappers do and the various base method provided by this COM+Automation library.
( # )  :: a -> (a -> IO b) -> IO b
obj # method          = method obj

-- | A variation on @(#)@ where the /this/ pointer is an action returning an object reference
-- rather than the reference itself. Sometimes useful when you create one-off objects
-- and call methods on them:
--
-- @
--    (createObject arg1) ## startUp arg2
-- @
--
-- instead of the wieldier,
--
-- @
--    obj <- createObject arg1
--    obj # startUp arg2          or createObject arg1 >>= (startUp arg2)
-- @
-- 
( ## ) :: IO a -> (a -> IO b) -> IO b
mObj ## method        = mObj >>= method


--IPersistFile - doesn't really belong here..

data PersistFile a    = PersistFile
type IPersistFile a   = IUnknown (PersistFile a)

iidIPersistFile       :: IID (IPersistFile ())
iidIPersistFile       = mkIID "{0000010B-0000-0000-C000-000000000046}"

-- | @coCreateInstance@ is the basic COM way of creating components. It takes
-- a 'CLSID', an interface to aggregate on, a process context and an IID to 
-- create an object:
--
-- @
--   coCreateInstance clsidAgentServer interfaceNULL LocalProcess iidIAgent
-- @
--
coCreateInstance :: CLSID 
     -> Maybe (IUnknown b)  
     -> CLSCTX 
     -> IID (IUnknown a) 
     -> IO (IUnknown a)
coCreateInstance clsid inner context iid = do
  ppvObject <- allocOutPtr
  clsid     <- marshallCLSID clsid
  inner     <- marshallInner inner
  let ctxt   = fromEnum context
  iid       <- marshallIID iid
  Base.coCreateInstance (castForeignPtr clsid) inner (fromIntegral ctxt)
           (castForeignPtr iid) ppvObject
  doThenFree free (readIUnknown False{-finalise only-}) ppvObject

coCreateInstanceEx :: CLSID 
       -> Maybe (IUnknown b)  
       -> CLSCTX 
       -> Maybe COSERVERINFO
       -> IID (IUnknown a) 
       -> IO (IUnknown a)
coCreateInstanceEx clsid pUnkOuter context mbServ iid = do
  clsid     <- marshallCLSID clsid
  pUnkOuter <- marshallInner pUnkOuter
  let ctxt   = fromEnum context
  iid       <- copyGUID (iidToGUID iid)
  let mqi = [ MULTI_QI iid nullPtr 0 ]
  r <- Base.coCreateInstanceEx (castForeignPtr clsid) pUnkOuter (fromIntegral ctxt) mbServ mqi
  case r of
    (MULTI_QI iid pItf hr:_) -> do
      coFree iid
      checkHR hr
      unmarshallIUnknown True{-finalise it-} pItf
    _ -> coFailHR e_FAIL

marshallInner :: Maybe (IUnknown a) -> IO (ForeignPtr b)
marshallInner Nothing  = return nullFO
marshallInner (Just v) = marshallIUnknown v

-- | @createObject@ creates an object from its progID: @createObject "Agent.Server"@.
-- @getObject@ creates an object from its progID and initializes it with a given file:
-- @getObject "spreadsheet.exl" "Excel.Application"@. If the filename is empty,
-- @getObject@ calls @getActiveObject@.
-- @getActiveObject@ tries to connect to an already running instance of the component:
-- @getActiveObject "Word.Application"@. 
-- @getFileObject@ opens a file or url and loads the associated or persistent object in it:
-- @getFileObject "spreadsheet.spd"@. 
-- @coCreateInstance@ is the basic com way of creating components. It takes
-- a CLSID, process context and IID to create an object: 
-- @coCreateInstance clsidAgentServer Nothing LocalProcess iidIAgent@.
--
-- NOTE: prepend @co@ to specify the initial IID, otherwise @iidIDispatch@ is
-- used (see 'System.Win32.Com.Automation' for more).
-- 
coCreateObject :: ProgID -> IID (IUnknown a) -> IO (IUnknown a)
coCreateObject progid iid = do
    clsid  <- clsidFromProgID progid
    coCreateInstance clsid Nothing AnyProcess iid

-- | Get Object from File and ProgID.
coGetFileObject :: String -> ProgID -> IID (IUnknown a) -> IO (IUnknown a)
coGetFileObject ""    progid iid = coGetActiveObject progid iid
coGetFileObject fname progid iid = do
    pf <- coCreateObject progid iidIPersistFile
    stackWideString fname $ \pfname -> do
      persistfileLoad pf pfname 0
      pf # queryInterface iid

-- | Look up and activate the given active/running object.
coGetActiveObject :: ProgID -> IID (IUnknown a) -> IO (IUnknown a)
coGetActiveObject progid iid = do
  clsid   <- clsidFromProgID progid
  iface   <- primGetActiveObject clsid
               `coOnFail` ("Could not connect to component '" ++ progid ++ "'")
  iface # queryInterface iid

primGetActiveObject :: CLSID -> IO (IUnknown a)
primGetActiveObject clsid = do
  clsid     <- marshallCLSID clsid
  ppvObject <- allocOutPtr
  hr        <- getActiveObject (castForeignPtr clsid) nullPtr ppvObject
  doThenFree free (readIUnknown False{-finalise only-}) ppvObject


-- | Bind to an object via its /moniker string/ or display name.
coGetObject :: String -> IID (IUnknown a) -> IO (IUnknown a)
coGetObject fname iid  = do
   stackWideString fname $ \pfname -> do 
      iid <- marshallIID iid
      ppv <- bindObject pfname (castForeignPtr iid)
      doThenFree free (readIUnknown False{-finalise only-}) ppv

--COM initialize/uninitialize:

-- | @coRun act@ is the toplevel action combinator to wrap up your
-- COM actions in. Takes care of calling 'coInitialize' (and un-initialize)
-- for you.
coRun :: IO a -> IO a
coRun io = do
  coInitialize
  v <-
    catchComException io
      (\ err -> do
        when (isCoError err) (putMessage $ coGetErrorString err)
        coUnInitialize
        throwIOComException err)
  coUnInitialize
  return v

-- | @coPerformIO act@ runs @act@ within an exception handler that
-- catches and displays any COM API errors in a message box. For debugging
-- purposes, mostly.
coPerformIO :: IO a -> IO a
coPerformIO io =
 catchComException io
       ( \ err -> do 
            putMessage (coGetErrorString err)
            throwIOComException err)

coUnsafePerformIO :: IO a -> a
coUnsafePerformIO = unsafePerformIO . coPerformIO

-- | @printMessage val@ /show/s @val@ in a message box.
printMessage :: Show a => a -> IO ()
printMessage x = putMessage (show x)

-- | @putMessage str@ displays @str@ in an informational message box containing an OK button.
putMessage :: String -> IO ()
putMessage msg = 
  stackString msg               $ \ _ m -> 
  stackString "Haskell message" $ \ _ t -> 
  Base.messageBox m t 0x40040
    {- To mere mortals, that's MB_OK | MB_ICONINFORMATION | MB_TOPMOST :-) -}

-- | @messageBox msg title flg@ displays a message box with the given title and content.
-- The @flg@ parameter is the bit pattern that makes up the @MB_*@ settings you want
-- to use (cf. underlying Win32 API documentation for @MessageBox@.)
messageBox :: String -> String -> Word32 -> IO ()
messageBox msg title flg = 
  stackString msg             $ \ _ m -> 
  stackString title           $ \ _ t -> 
  Base.messageBox m t flg

-- | @outputDebugString str@ adds an 
outputDebugString :: String -> IO ()
outputDebugString msg = primOutputDebugString ("haskell-com: " ++ msg ++ "\n")

{-
 Really belongs elsewhere...getting info of what kind of
 platform we're on. 
-}
data OSVersionInfo 
 = OSVersionInfo Word32 Word32 Word32

isWindowsNT :: OSVersionInfo -> Bool
isWindowsNT (OSVersionInfo _ _ 2{-VER_PLATFORM_WIN32_NT-}) = True
isWindowsNT _ = False

isWindows95 :: OSVersionInfo -> Bool
isWindows95 (OSVersionInfo _ 0 1{-VER_PLATFORM_WIN32_WINDOWS-}) = True
isWindows95 _ = False

isWindows98 :: OSVersionInfo -> Bool
isWindows98 (OSVersionInfo _ x 1{-VER_PLATFORM_WIN32_WINDOWS-}) = x /= 0
isWindows98 _ = False

versionInfo :: OSVersionInfo
versionInfo = unsafePerformIO $ do
  (j,n,d) <- primGetVersionInfo
  return (OSVersionInfo j n d)

-- | The @CLSCTX@ enumeration is used by @comCreateInstance@ to specify 
-- execution context in which we'd like to component to be created
-- (just use @AnyProcess@ if you're not too fussed..)
--
data CLSCTX 
 = CLSCTX_INPROC_SERVER 
 | CLSCTX_INPROC_HANDLER
 | CLSCTX_LOCAL_SERVER 
 | CLSCTX_INPROC_SERVER16
 | CLSCTX_REMOTE_SERVER
 | CLSCTX_INPROC_HANDLER16
 | CLSCTX_INPROC_SERVERX86
 | CLSCTX_INPROC_HANDLERX86
 | LocalProcess
 | InProcess
 | ServerProcess
 | AnyProcess
 deriving (Show)

instance Enum CLSCTX where
  fromEnum ctx =
    case ctx of
     CLSCTX_INPROC_SERVER -> 1
     CLSCTX_INPROC_HANDLER -> 2
     CLSCTX_LOCAL_SERVER -> 4
     CLSCTX_INPROC_SERVER16 -> 8
     CLSCTX_REMOTE_SERVER -> 16
     CLSCTX_INPROC_HANDLER16 -> 32
     CLSCTX_INPROC_SERVERX86 -> 64
     CLSCTX_INPROC_HANDLERX86 -> 128
     LocalProcess   -> localProcess
     InProcess      -> inProcess
     ServerProcess  -> serverProcess
     AnyProcess     -> anyProcess

  toEnum x =
   case x of
     1    -> CLSCTX_INPROC_SERVER
     2    -> CLSCTX_INPROC_HANDLER
     8    -> CLSCTX_INPROC_SERVER16
     16   -> CLSCTX_REMOTE_SERVER
     32   -> CLSCTX_INPROC_HANDLER16
     64   -> CLSCTX_INPROC_SERVERX86
     128  -> CLSCTX_INPROC_HANDLERX86
     0x04 -> LocalProcess
     0x0b -> InProcess
     0x0d -> ServerProcess
     4    -> CLSCTX_LOCAL_SERVER
     _    -> AnyProcess


localProcess    :: Int
localProcess    = 0x04
inProcess       :: Int
inProcess       = 0x0b
serverProcess   :: Int
serverProcess   = 0x0d
anyProcess      :: Int
anyProcess      = 0x0f


--VTable method invocation wrappers:

invokeAndCheck :: (Ptr any -> Ptr b -> IO HRESULT) -> Int -> IUnknown a -> IO ()
invokeAndCheck meth offset iptr = do
  hr <- primInvokeItFO meth offset (marshallIUnknown iptr)
  checkHR hr

-- | @invokeIt (\ methPtr ip -> action) offset obj@ sets up a vtbl-indexed
-- COM call, unmarshalling and indexing @obj@ before passing it along to the
-- method argument. The first argument is typically an FFI wrapped call to
-- a function pointer (@methPtr@ here).
--
-- 
invokeIt :: (Ptr any -> Ptr c -> IO a) -> Int -> IUnknown b -> IO a
invokeIt meth offset iptr = primInvokeItFO meth offset (marshallIUnknown iptr)

{-
Library provided stubs for IEnum* interfaces - the HaskellDirect compiler
knows how to generate code for these:
-}
enumNext :: Word32 -> (Ptr any -> IO a) -> Word32 -> IUnknown b -> IO [a]
enumNext szof read_elt celt iptr = do
    ptr     <- allocBytes (fromIntegral (celt * szof))
    po      <- allocBytes (fromIntegral sizeofWord32)
    invokeIt (\ methPtr ip -> primEnumNext methPtr ip celt ptr po) 3 iptr
    elts_read <- readWord32 (castPtr po)
--    v         <- peek ((castPtr ptr) :: Ptr (Ptr a))
    unmarshalllist szof 0 elts_read read_elt ptr

enumNextOne :: Word32 -> (Ptr any -> IO a) -> IUnknown b -> IO (Maybe a)
enumNextOne szof read_elt iptr = 
  allocaBytes (fromIntegral sizeofWord32) $ \ po -> do
    ptr       <- allocBytes (fromIntegral szof)
    invokeIt (\ methPtr ip -> primEnumNext methPtr ip 1 ptr po) 3 iptr
    elts_read <- readWord32 (castPtr po)
    if elts_read <= 0
     then return Nothing
     else do
       x <- read_elt (castPtr ptr)
       return (Just x)

enumSkip :: Word32 -> IUnknown a -> IO ()
enumSkip count iptr = 
   invokeIt (\ methPtr ip -> primEnumSkip methPtr ip count) 4 iptr

enumReset :: IUnknown a -> IO ()
enumReset iptr =
   invokeIt (\ methPtr ip -> primEnumReset methPtr ip) 5 iptr

enumClone :: IUnknown a -> IO (IUnknown b)
enumClone iptr = do
   ppv     <- allocOutPtr
   invokeIt (\ methPtr ip -> primEnumClone methPtr ip ppv) 6 iptr
   doThenFree free (readIUnknown False{-finalise only-}) ppv

{-
BSTRs were introduced by Automation, but their now used in non-Auto
contexts.
-}
data BSTR = BSTR

writeBSTR :: Ptr String -> String -> IO ()
writeBSTR ptr str =
  stackString str $ \_ pstr -> do
    o_stringToBSTR <- prim_System_Win32_Com_Base_stringToBSTR (castPtr pstr) ptr
    checkHR o_stringToBSTR

--readBSTR :: Ptr BSTR -> IO String
readBSTR :: Ptr (Ptr String) -> IO String
readBSTR ptr = do
  ptr' <- peek ptr
  unmarshallBSTR ptr'

unmarshallBSTR :: Ptr String -> IO String
unmarshallBSTR bstr 
  | bstr == nullPtr = return ""
  | len == 0  = return ""
  | otherwise = do
     stackStringLen (4 + fromIntegral len) "" $ \ pstr -> do
     bstrToStringLen (castPtr bstr) len (castPtr pstr)
     unmarshallString pstr
 where
   len  = bstrLen (castPtr bstr)

marshallBSTR :: String -> IO (Ptr String)
marshallBSTR s = 
  stackString s $ \ _ pstr -> do
  ptr <- stringToBSTR (castPtr pstr)
  x   <- peek (castPtr ptr)
  free ptr
  return x

freeBSTR x 
 | x == nullPtr = return ()
 | otherwise    = P.freeBSTR x

-- This type sometimes appear in IDL and tlbs, so
-- to avoid having to depend on wtypes for it, let's
-- simply define it here.
type LPSTR = String

-- | @coFree ptr@ releases storage that has been allocated via the COM task allocator;
-- explicitly via @coAlloc@ or indirectly via the APIs that handed the pointer to your
-- code.
coFree :: Ptr a -> IO ()
coFree p = freeMemory p

-- | @coAlloc sz@ allocates @sz@ bytes from the COM task allocator, returning a pointer.
-- The onus is on the caller to constrain the type of that pointer to capture what the
-- allocated memory points to.
coAlloc :: Word32 -> IO (Ptr a)
coAlloc sz = allocMemory sz

-- | @ProgID@s are represented in Haskell as mere strings
type ProgID = String

-- | @clsidFromProgID progid@ looks up a @ProgID@ and tries to translate it into
-- its registered @CLSID@. Raises an IO exception if the @ProgID@ isn't known.
clsidFromProgID :: ProgID -> IO CLSID
clsidFromProgID progid =
   stackString progid $ \ _ pprogid -> do
     pclsid  <- coAlloc sizeofCLSID
     coOnFail (primCLSIDFromProgID pprogid (castPtr pclsid))
             ("Component '" ++ progid ++ "' is unknown")
     unmarshallCLSID True pclsid

-- | @progIDFromCLSID cid@ is the dual @clsidFromProgID@, attempting to translate
-- in the other direction.
progIDFromCLSID :: CLSID -> IO ProgID
progIDFromCLSID clsid = do
   pclsid    <- marshallCLSID clsid
   pwide     <- primProgIDFromCLSID (castForeignPtr pclsid)
   (pstr,hr) <- wideToString (castPtr pwide)
   checkHR hr
   str       <- unmarshallString (castPtr pstr)
   coFree pstr
   coFree pwide
   return str


-- | Type libraries are identified by a GUID, the @LIBID@.
type LIBID = GUID

mkLIBID :: String -> LIBID
mkLIBID = mkGUID

type LCID = Word32

-- | Representing interface pointers via @IUnknown a@, where @a@ is
-- the particular @IUnknown@-extended interface, e.g., @IUnknown IDispatch_@.
-- If the interface pointer is /just/ @IUnknown@, use @IUnknown ()@.
--
-- Extend this to @IID@s and parameterize them over the interface they
-- represent.
iidIUnknown :: IID (IUnknown ())
iidIUnknown = mkIID "{00000000-0000-0000-C000-000000000046}"

-- | Equality of interface pointers is defined by the COM spec
-- as being equality of IUnknown (pointers to) implementations.
instance Eq (IUnknown_ a) where
   iface1 == iface2    = coEqual (castIface iface1) (castIface iface2)

-- | @castIface obj@ performs a type
castIface :: IUnknown a -> IUnknown b
castIface (Unknown o) = Unknown o

-- | @interfaceNULL@ is the @NULL@ interface pointer.
interfaceNULL :: IUnknown a
interfaceNULL = unsafePerformIO (unmarshallIUnknown False nullPtr)

-- | @isNullInterface iptr@ returns @True@ iff @iptr@ is the NULL pointer.
isNullInterface :: IUnknown a -> Bool
isNullInterface (Unknown ip)    = foreignPtrToPtr ip == nullPtr

-- | The null interface
iidNULL :: IID ()
iidNULL = mkIID "{00000000-0000-0000-0000-000000000000}"

instance Show (IUnknown_ a) where
  showsPrec _ iface   = 
    shows "<interface pointer = " . 
    shows (ifaceToAddr iface)     .
    shows ">"

-- | @queryInterface iid iunk@ queries @iunk@ if it supports the @iid@
-- interface, returning it. Notice that the type parameter to the @IID@
-- matches up with that of the resulting interface pointer, giving you
-- type safety - i.e., you can only use the interface pointer with methods
-- supported by that interface 
queryInterface :: IID (IUnknown b) -> IUnknown a -> IO (IUnknown b)
queryInterface riid iptr = do
    ppvObject <- allocOutPtr
    priid     <- marshallIID riid
    invokeIt (\ methPtr ip -> primQI methPtr ip (castForeignPtr priid) ppvObject) 0 iptr
    doThenFree free (readIUnknown False{-finalise only-}) ppvObject

-- | @addRef iptr@ increases the reference count of the interface pointer @iptr@.
-- Notice that interface pointers are managed and finalized when on the Haskell heap,
-- so manual reference counting is not required (and not encouraged as it could prevent
-- the underlying object from being properly released, should you forget to decrement
-- the ref count with @release@.)
addRef :: IUnknown a -> IO Word32
addRef iptr = invokeIt (\ methPtr ip -> primAddRef methPtr ip) 1 iptr

-- | @addRef iptr@ decrements the reference count of the interface pointer @iptr@.
release :: IUnknown a -> IO Word32
release iptr = invokeIt (\ methPtr ip -> primRelease methPtr ip) 2 iptr

withQueryInterface :: IID (IUnknown b)
             -> IUnknown a
             -> (IUnknown b -> IO c)
             -> IO c
withQueryInterface iid unk action 
 = bracket (queryInterface iid unk) release action

--HDirect generated stub needed by @coGetObject@:
persistfileLoad :: IPersistFile a -> Ptr Wchar_t -> Word32 -> IO ()
persistfileLoad iptr pszFileName dwMode =
    invokeIt (\ methPtr ip -> primPersistLoad methPtr ip pszFileName dwMode) 5 iptr

-- | @GUID@ is the Haskell representation for COM GUIDs. 
newtype GUID     = GUID (ForeignPtr ()) --(Pointer Guid)

data Guid   = Guid

mkGUID :: String -> GUID
mkGUID str = unsafePerformIO (stringToGUID str)

-- | @newGUID@ generates a new unique GUID.
newGUID :: IO GUID
newGUID = do
  pg  <- coAlloc sizeofGUID
  ng  <- makeFO pg (castPtrToFunPtr finalFreeMemory)
  primNewGUID ng
  return (GUID ng)

nullGUID :: GUID
nullGUID = unsafePerformIO $ do
  x <- primNullIID
  p <- makeFO x (castPtrToFunPtr finalNoFree) --primNoFree
  return (GUID p)

marshallGUID :: GUID -> IO (ForeignPtr GUID)
marshallGUID (GUID ptr) = return (castForeignPtr ptr)

-- | A version of the GUID marshaller which copies rather
-- than hands back a pointer to the (immutable) GUID.
copyGUID :: GUID -> IO (Ptr ())
copyGUID (GUID ptr) = do
    pg  <- coAlloc sizeofGUID
    primCopyGUID ptr pg
    return pg

-- | @unmarshallGUID finalize ptr@ unpacks a pointer to an incoming
-- GUID, wrapping it up as a Haskell 'GUID'. If @finalize@ is @True@,
-- the GUID is assumed allocated via the COM task allocator and will
-- be freed/finalized when the 'GUID' becomes garbage.
unmarshallGUID :: Bool -> Ptr GUID -> IO GUID
unmarshallGUID finaliseMe ptr = do
     -- ToDo: verify that HDirect *never ever* allocates and
     --       stores a GUID in malloc()-space, but consistently
     --       uses the COM task allocator. (Why? because the
     --       finalizer below will tell the COM task allocator
     --       to free the GUID once done with it)
   f  <- makeFO ptr (castPtrToFunPtr $ if finaliseMe then finalFreeMemory else finalNoFree)
   return (GUID f)

-- | @writeGUID ptr g@ packs the Haskell 'GUID' into the pointer; that is,
-- it writes a pointer to the GUID value to @ptr@ -- no copying of underlying
-- structure.
writeGUID :: Ptr GUID -> GUID -> IO ()
writeGUID ptr (GUID g) = poke (castPtr ptr) (foreignPtrToPtr g)

readGUID :: Bool -> Ptr GUID -> IO GUID
readGUID finaliseMe ptr = do
--  ptr  <- peek ptr
  unmarshallGUID finaliseMe ptr

sizeofGUID  :: Word32
sizeofGUID  = 16

-- | @stringToGUID "{00000000-0000-0000-C000-0000 0000 0046}"@ translates the
-- COM string representation for GUIDs into an actual 'GUID' value.
stringToGUID :: String -> IO GUID
stringToGUID str =
   stackWideString str $ \xstr -> do
   pg <- coAlloc sizeofGUID
   primStringToGUID xstr (castPtr pg)
   unmarshallGUID True pg

-- | @stringFromGUID g@ converts the 'GUID' @g@ into the COM string representation
-- @{aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee}@
stringFromGUID :: GUID -> IO String
stringFromGUID guid = do
   pguid     <- marshallGUID guid
   pwide     <- primGUIDToString (castForeignPtr pguid)
   (pstr,hr) <- wideToString (castPtr pwide)
   checkHR hr
   str       <- unmarshallString (castPtr pstr)
   coFree pstr
   coFree pwide
   return str

guidToString :: GUID -> String
guidToString ptr = unsafePerformIO (stringFromGUID ptr)

{-
-}
-- | Representation of @IID@s: Give the interface identifier
-- a type parameter, so that when we come to define the Haskell
-- type of @IUnknown.QueryInterface()@ we can rely on the type checker
-- to ensure that the @IID@ passed to @QueryInterface@ agrees with
-- the interface at which we're using the interface pointer that's
-- returned
newtype IID a  = IID GUID   deriving ( Eq )

newtype CLSID  = CLSID GUID deriving ( Eq )

mkIID   :: String -> IID a
mkIID str   = IID (mkGUID str)

mkCLSID :: String -> CLSID
mkCLSID str = CLSID (mkGUID str)

-- no need to provide marshallers for these, the IDL compiler
-- knows that they're both represented by GUIDs.

stringToIID :: String -> IID a
stringToIID str = mkIID str

stringToCLSID :: String -> CLSID
stringToCLSID str = mkCLSID str

iidToString :: IID a -> String
iidToString (IID i) = guidToString i

clsidToString :: CLSID -> String
clsidToString (CLSID clsid) = guidToString clsid

iidToGUID :: IID a -> GUID
iidToGUID (IID g) = g

castIID :: IID a -> IID b
castIID (IID i) = IID i

clsidToGUID :: CLSID -> GUID
clsidToGUID (CLSID g) = g

clsidToDisplayName :: CLSID  -> String
clsidToDisplayName (CLSID g) = "clsid:" ++ tail (init (show g))

guidToIID :: GUID -> IID a
guidToIID g = IID g

guidToCLSID :: GUID -> CLSID
guidToCLSID g = CLSID g

instance Show (IID a) where
  showsPrec _ (IID i)  = showString (guidToString i)

instance Show CLSID where
  showsPrec _ (CLSID c)  = showString (guidToString c)

instance Show GUID where
  showsPrec _ guid     = showString (guidToString guid)

instance Eq GUID where
  (GUID x) == (GUID y) = unsafePerformIO $ do
     return (isEqualGUID x y)

marshallIID :: IID a -> IO (ForeignPtr (IID a))
marshallIID (IID x) = marshallGUID x >>= return.castForeignPtr
unmarshallIID :: Bool -> Ptr (IID a) -> IO (IID a)
unmarshallIID finaliseMe x = do
  i <- unmarshallGUID finaliseMe (castPtr x)
  return (IID i)

copyIID (IID x) = copyGUID x

readIID :: Bool -> Ptr (Ptr (IID a)) -> IO (IID a)
readIID finaliseMe ptr = do
  a  <- peek ptr
  unmarshallIID finaliseMe (castPtr a)

writeIID :: Ptr (IID a) -> IID a -> IO ()
writeIID ptr (IID i) = writeGUID (castPtr ptr) i

--------

marshallCLSID (CLSID x) = marshallGUID x

unmarshallCLSID :: Bool -> Ptr CLSID -> IO CLSID
unmarshallCLSID finaliseMe x = do
  i <- unmarshallGUID finaliseMe (castPtr x)
  return (CLSID i)

copyCLSID (CLSID x) = copyGUID x

readCLSID :: Bool -> Ptr (Ptr CLSID) -> IO CLSID
readCLSID finaliseMe ptr = do
  a  <- peek ptr
  unmarshallCLSID finaliseMe (castPtr a)

writeCLSID :: Ptr CLSID -> CLSID -> IO ()
writeCLSID ptr (CLSID i) = writeGUID (castPtr ptr) i

sizeofCLSID = sizeofGUID

coInitialize :: IO ()
coInitialize = comInitialize

coUnInitialize :: IO ()
coUnInitialize = comUnInitialize

sizeofIID = sizeofGUID

coEqual :: IUnknown a -> IUnknown b -> Bool
coEqual ip1 ip2 = unsafePerformIO $ primComEqual (castIface ip1) (castIface ip2)

--Interface pointer marshallers:

-- marshallIUnknown is in Base.idl

unmarshallIUnknown :: Bool -> Ptr b -> IO (IUnknown a)
unmarshallIUnknown finaliseMe x = do
   ip <- addrToIPointer finaliseMe x
   case finaliseMe of
     True | x /= nullPtr -> ip # addRef >> return ip
     _    -> return ip

unmarshallIUnknownFO :: ForeignPtr b -> IO (IUnknown a)
unmarshallIUnknownFO i = return (Unknown (castForeignPtr i))
  -- ToDo: I believe it is correct never to do an AddRef()
  --       here, but double-check the spec.

{-
  addRefMe == True  => attach finaliser (which calls Release()), and
             call addRef on i-pointer before returning.
           == False => attach finaliser (which calls Release()) only.

  The former case is used when you receive an i-pointer from the outside
  world and want to copy a reference to it into the Haskell heap. This
  does not include i-pointers you receive via [out] pointers when calling
  a COM component method from Haskell, where it is the obligation of the
  server filling in the [out] pointer to call addRef() for you.
-}
readIUnknown :: Bool -> Ptr b -> IO (IUnknown a)
readIUnknown addRefMe x = do
  ptr <- peek (castPtr x)
  ip <- addrToIPointer True ptr
  case addRefMe of
     True | x /= nullPtr -> ip # addRef >> return ip
     _    -> return ip

writeIUnknown :: Bool -> Ptr (Ptr (IUnknown b)) -> IUnknown a -> IO ()
writeIUnknown addRefMe x v = do
   let a = ifaceToAddr v
   when (addRefMe && a /= nullPtr)
        (v # addRef >> return ())
   writePtr x a

{-
@withObject@ applies every method in a list to an 
object: @withObject genie [showUp, speak "hi", hide]@.
@withMethod@ applies every argument in a list to a
method: @genie # withMethod speak ["hello", "world"]@.
-}

withObject_ :: IUnknown a -> [IUnknown a -> IO b] -> IO ()
withObject_ obj       = sequence_ .  map ( obj # ) 

withMethod_ :: (a -> IUnknown b -> IO c) -> [a] -> IUnknown b -> IO ()
withMethod_ method args obj = sequence_ $ map (\x -> obj # method x) args

withObject :: IUnknown a -> [IUnknown a -> IO b] -> IO [b]
withObject obj        = sequence . map ( obj # )

withMethod :: (a -> IUnknown b -> IO c) -> [a] -> IUnknown b -> IO [c]
withMethod method args obj = sequence $ map (\x -> obj # method x) args

loadTypeLib :: String -> IO (IUnknown a)
loadTypeLib fname = do
    ptr <- allocOutPtr
    stackWideString fname $ \pfname -> do
      Base.loadTypeLib pfname ptr
      doThenFree free (readIUnknown False{-finalise only-}) ptr

loadRegTypeLib :: GUID -> Int -> Int -> Int -> IO (IUnknown a)
loadRegTypeLib guid maj min lcid = do
    ptr     <- allocOutPtr
    p_guid  <- marshallGUID guid
    Base.loadRegTypeLib (castForeignPtr p_guid)
             (fromIntegral maj) (fromIntegral min)
         (fromIntegral lcid) ptr
    doThenFree free (readIUnknown False{-finalise only-}) ptr

queryPathOfRegTypeLib :: GUID
          -> Word16
          -> Word16
          -> IO String
queryPathOfRegTypeLib gd maj min = do
    pgd   <- marshallGUID gd
    pbstr <- primQueryPathOfRegTypeLib (castForeignPtr pgd) maj min
    if nullPtr == pbstr then
        return ""
      else do
        str <- unmarshallBSTR (castPtr pbstr)
        freeBSTR pbstr
        return str

createTypeLib :: String -> IO (IUnknown a) --(ICreateTypeLib a)
createTypeLib nm = do
   wstr <- stringToWide nm
   pptr <- primCreateTypeLib 1{-SYS_WIN32-} wstr
   doThenFree free (readIUnknown False{-finalise only-}) pptr

loadTypeLibEx :: String -> Bool -> IO (IUnknown a)
loadTypeLibEx path reg_tlb = do
    let
      {-
        This Int is used to map onto the following enum

         typedef enum tagREGKIND { REGKIND_DEFAULT, REGKIND_REGISTER, REGKIND_NONE };
      -}
     rkind :: Int
     rkind
      | reg_tlb   = 1
      | otherwise = 2

    out_ptr <- allocOutPtr
    stackWideString path $ \pfname -> do
      Base.loadTypeLibEx pfname (fromIntegral rkind) out_ptr
      doThenFree free (readIUnknown False{-finalise only-}) out_ptr
