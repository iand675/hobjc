{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{- |
Module      : Language.ObjC.Runtime
Description : High-level bindings allowing developers to interface with Objective-C
License     : BSD3
Maintainer  : ian@iankduncan.com
Stability   : experimental
Portability : macOS only

Overview

The Objective-C runtime is a runtime library that provides support for the dynamic properties of the Objective-C language, and as such is linked to by all Objective-C apps. Objective-C runtime library support functions are implemented in the shared library found at /usr/lib/libobjc.A.dylib.

You typically don't need to use the Objective-C runtime library directly when programming in Objective-C. This API is useful primarily for developing bridge layers between Objective-C and other languages, or for low-level debugging.

The macOS implementation of the Objective-C runtime library is unique to the Mac. For other platforms, the GNU Compiler Collection provides a different implementation with a similar API. This document covers only the macOS implementation.

The low-level Objective-C runtime API is significantly updated in OS X version 10.5. Many functions and all existing data structures are replaced with new functions. The old functions and structures are deprecated in 32-bit and absent in 64-bit mode. The API constrains several values to 32-bit ints even in 64-bit mode—class count, protocol count, methods per class, ivars per class, arguments per method, sizeof(all arguments) per method, and class version number. In addition, the new Objective-C ABI (not described here) further constrains sizeof(anInstance) to 32 bits, and three other values to 24 bits—methods per class, ivars per class, and sizeof(a single ivar). Finally, the obsolete NXHashTable and NXMapTable are limited to 4 billion items.

String encoding

All char * in the runtime API should be considered to have UTF-8 encoding.
-}
module Language.ObjC.Runtime
    (
    -- * Constants
      yes
    , no
    , bool
    , nil
    , AssociativeObjectBehavior(..)
    -- * Data types
    , Class(..)
    , IVar(..)
    , Id(..)
    , ToId(..)
    , Method(..)
    , Selector(..)
    , Property(..)
    , Protocol(..)
    , Imp
    , Attribute(..)
    , IvarLayout(..)
    -- * Working with Classes
    , className
    , classSuperclass
    , classIsMetaClass
    , classInstanceSize
    , classInstanceVariable
    , classClassVariable
    , classAddIvar
    , classIvarList
    , classGetIvarLayout
    , classSetIvarLayout
    , classGetWeakIvarLayout
    , classSetWeakIvarLayout
    , classGetProperty
    , classPropertyList
    , classAddMethod
    , classGetInstanceMethod
    , classGetClassMethod
    , classMethodList
    , classReplaceMethod
    , classGetMethodImplementation
    , classGetMethodImplementationStret
    , respondsToSelector
    , classAddProtocol
    , classAddProperty
    , classReplaceProperty
    , classConformsToProtocol
    , classProtocolList
    , classGetVersion
    , classSetVersion
    -- * Adding Classs
    , allocateClassPair
    , disposeClassPair
    , registerClassPair
    -- * Instantiating Classes
    , createInstance
    , constructInstance
    , destructInstance
    -- * Working with Instances
    , copy
    , dispose
    , objectSetInstanceVariable
    , objectGetInstanceVariable
    , objectGetIndexedIvars
    , objectGetIVar
    , objectSetIVar
    , objectGetClassName
    , objectGetClass
    , objectSetClass
    -- * Obtaining Class Definitions
    -- , getClassList
    , classList
    , lookupClass
    , getClass
    , requireClass
    , getMetaClass
    -- * Working with Instance Variables
    , ivarName
    , ivarTypeEncoding
    , ivarOffset
    -- * Associative References
    , setAssociatedObject
    , getAssociatedObject
    , removeAssociatedObjects
    -- * Sending Messages
    , msgSend
    -- * Working with Methods
    -- methodInvoke
    -- methodInvokeStret
    , methodName
    , methodGetImplementation
    , methodTypeEncoding
    , methodReturnType
    -- , methodGetReturnType
    , methodNumberOfArguments
    , methodArgumentType
    , methodGetDescription
    , methodSetImplementation
    , methodExchangeImplementations
    -- * Working with Libraries
    , imageNames
    , classImageName
    , classNamesForImage
    -- * Working with Selectors
    , selectorName
    , selector
    -- * Working with Protocols
    , getProtocol
    , protocolList
    , allocateProtocol
    , registerProtocol
    , protocolAddMethodDescription
    , protocolAddProtocol
    , protocolAddProperty
    , protocolGetName
    , protocolMethodDescriptionList
    -- , protocolGetMethodDescription
    , protocolPropertyList
    , protocolProperty
    , protocolConformsToProtocol
    -- * Working with Properties
    , propertyName
    , propertyAttributes
    , propertyAttributeValue
    , propertyAttributeList
    -- * Using Objective-C Language Features
    , enumerationMutation
    , setEnumerationMutationHandler
    {-
    -- * Primitive API
    -- ** Working with Classes
    , class_getName
    , class_getSuperclass
    , class_isMetaClass
    , class_getInstanceSize
    , class_getInstanceVariable
    , class_getClassVariable
    , class_addIvar
    , class_copyIvarList
    , class_getIvarLayout
    , class_setIvarLayout
    , class_getWeakIvarLayout
    , class_setWeakIvarLayout
    , class_getProperty
    , class_copyPropertyList
    , class_addMethod
    , class_getInstanceMethod
    , class_getClassMethod
    , class_copyMethodList
    , class_replaceMethod
    , class_getMethodImplementation
    , class_getMethodImplementation_stret
    , class_respondsToSelector
    , class_addProtocol
    , class_addProperty
    , class_replaceProperty
    , class_conformsToProtocol
    , class_copyProtocolList
    , class_getVersion
    , class_setVersion
    -- ** Adding Classes
    , objc_allocateClassPair
    , objc_disposeClassPair
    , objc_registerClassPair
    -- ** Instantiating Classes
    , class_createInstance
    , objc_constructInstance
    , objc_destructInstance
    -- ** Working with Instances
    , object_copy
    , object_dispose
    , object_setInstanceVariable
    , object_getInstanceVariable
    , object_getIndexedIvars
    , object_getIvar
    , object_setIvar
    , object_getClassName
    , object_getClass
    , object_setClass
    -- ** Obtaining Class Definitions
    , objc_getClassList
    , objc_copyClassList
    , objc_lookUpClass
    , objc_getClass
    , objc_getRequiredClass
    , objc_getMetaClass
    -- ** Working with Instance Variables
    , ivar_getName
    , ivar_getTypeEncoding
    , ivar_getOffset
    -- ** Associative References
    , objc_setAssociatedObject
    , objc_getAssociatedObject
    , objc_removeAssociatedObjects
    -- ** Sending Messages
    , p_objc_msgSend
    , p_objc_msgSend_fpret
    , p_objc_msgSend_stret
    , p_objc_msgSendSuper
    , p_objc_msgSendSuper_stret
    -- ** Working with Methods
    , method_invoke
    , method_invoke_stret
    , method_getName
    , method_getImplementation
    , method_getTypeEncoding
    , method_copyReturnType
    , method_getReturnType
    , method_getNumberOfArguments
    , method_getArgumentType
    -- , method_getDescription
    , method_setImplementation
    , method_exchangeImplementations
    -- ** Working with Libraries
    , objc_copyImageNames
    , class_getImageName
    , objc_copyClassNamesForImage
    -- ** Working with Selectors
    , sel_getName
    , sel_registerName
    , sel_isEqual
    -- ** Working with Protocols
    , objc_getProtocol
    , objc_copyProtocolList
    , objc_allocateProtocol
    , objc_registerProtocol
    , protocol_addMethodDescription
    , protocol_addProtocol
    , protocol_addProperty
    , protocol_getName
    , protocol_isEqual
    , protocol_copyMethodDescriptionList
    , protocol_getMethodDescription
    , protocol_copyPropertyList
    , protocol_getProperty
    , protocol_conformsToProtocol
    -- ** Working with Properties
    , property_getName
    , property_getAttributes
    , property_copyAttributeValue
    , property_copyAttributeList
    -- ** Using Objective-C Language Features
    , objc_enumerationMutation
    , objc_setEnumerationMutationHandler
    , imp_implementationWithBlock
    , imp_getBlock
    , imp_removeBlock
    , objc_loadWeak
    , objc_storeWeak
    -- ** Associative References
    -}
    ) where

import Control.Monad
import Control.Monad.Trans
import Data.Char
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VS
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Storable as VS
import Control.Monad.Managed
import Data.Coerce
import Data.Maybe (fromMaybe)
import Data.StateVar
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.LibFFI
import Foreign.Marshal hiding (with, toBool)
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe

-- | Objective-C equivalents to @True@ and @False@
yes, no :: CUInt
yes = 1
no = 0

-- | Convert Haskell Boolean values to Objective-C equivalent
bool :: Bool -> CUInt
bool b = if b then yes else no

toBool :: CUInt -> Bool
toBool b = b == yes

-- | Subsumes Objective-C's @nil@ and @Nil@ constants
nil :: (Coercible a Id, ToId a) => a
nil = coerce nullPtr

nullCheck :: (Coercible a (Ptr b)) => a -> Maybe a
nullCheck x = if coerce x == nullPtr
  then Nothing
  else Just x

listCopier :: Storable a => (Ptr CUInt -> IO (Ptr a)) -> IO (VS.Vector a)
listCopier f = alloca $ \len -> do
  p <- f len
  fp <- newForeignPtr finalizerFree p
  l <- peek len
  return $ VS.unsafeFromForeignPtr0 fp (fromIntegral l)

-- | A class describes the behavior and properties common to any particular type of object. For a string object (in Objective-C, this is an instance of the class NSString), the class offers various ways to examine and convert the internal characters that it represents. Similarly, the class used to describe a number object (NSNumber) offers functionality around an internal numeric value, such as converting that value to a different numeric type.
--
-- In the same way that multiple buildings constructed from the same blueprint are identical in structure, every instance of a class shares the same properties and behavior as all other instances of that class. Every NSString instance behaves in the same way, regardless of the internal string of characters it holds.
--
-- Any particular object is designed to be used in specific ways. You might know that a string object represents some string of characters, but you don’t need to know the exact internal mechanisms used to store those characters. You don’t know anything about the internal behavior used by the object itself to work directly with its characters, but you do need to know how you are expected to interact with the object, perhaps to ask it for specific characters or request a new object in which all the original characters are converted to uppercase.
--
-- In Objective-C, the class interface specifies exactly how a given type of object is intended to be used by other objects. In other words, it defines the public interface between instances of the class and the outside world.
newtype Class = Class (Ptr CSize)
  deriving (Eq, Ord, Storable)

instance Show Class where
  show = className

newtype IVar = IVar (Ptr IVar)
  deriving (Show, Eq, Ord, Storable)

-- | A pointer to an instance of a class.
newtype Id = Id (Ptr Id)
  deriving (Show, Eq, Ord, Storable)

-- | An opaque type that represents a method in a class definition.
newtype Method = Method (Ptr Method)
  deriving (Eq, Ord, Storable)

-- | An opaque type that represents a method selector.
newtype Selector = Selector (Ptr Selector)
  deriving (Ord, Storable)

-- | An opaque type that represents an Objective-C declared property.
newtype Property = Property (Ptr Property)
  deriving (Storable)

newtype Protocol = Protocol (Ptr Protocol)
  deriving (Ord, Storable)

instance ToId Protocol where
  toId = coerce

type Imp p = FunPtr (Id -> Selector -> p)

data Attribute = Attribute
  { attributeName  :: String
  , attributeValue :: String
  } deriving (Show, Eq)

data RawAttribute = RawAttribute
  { rawAttributeName  :: !CString
  , rawAttributeValue :: !CString
  }

rawAttribute :: Attribute -> Managed RawAttribute
rawAttribute (Attribute k v) = RawAttribute
  <$> managed (withCString k)
  <*> managed (withCString v)

readAttribute :: RawAttribute -> IO Attribute
readAttribute (RawAttribute k v) = Attribute <$> peekCString k <*> peekCString v

instance Storable RawAttribute where
  sizeOf _ = 2 * sizeOf nullPtr
  alignment _ = 2 * alignment nullPtr
  {-
  peek p = do
    n <- peekCString (castPtr p)
    v <- peekCString $ plusPtr (castPtr p) (sizeOf nullPtr)
    return $ RawAttribute n v
  -}
  peek p = return $ RawAttribute (castPtr p) (plusPtr (castPtr p) (sizeOf nullPtr))
  -- Note that allocated strings need to be freed when @Ptr Attribute@ is out of use
  poke p (RawAttribute pn pv) = do
    poke (castPtr p) pn
    poke (plusPtr (castPtr p) (sizeOf nullPtr)) pv

data RawMethodDescription = RawMethodDescription
  { rawMethodDescriptionName  :: Selector
  , rawMethodDescriptionTypes :: CString
  } deriving (Show, Eq)

instance Storable RawMethodDescription where
  sizeOf _ = 2 * sizeOf nullPtr
  alignment _ = 2 * alignment nullPtr
  peek p = do
    n <- peek (castPtr p)
    t <- peek (castPtr p `plusPtr` sizeOf (Selector nullPtr))
    return $ RawMethodDescription n t
  poke p (RawMethodDescription n t) = do
    poke (castPtr p) n
    poke (castPtr p `plusPtr` sizeOf (Selector nullPtr)) t

data MethodDescription = MethodDescription
  { methodDescriptionName  :: Selector
  , methodDescriptionTypes :: String
  } deriving (Show, Eq)

peekMethodDescription :: RawMethodDescription -> IO MethodDescription
peekMethodDescription (RawMethodDescription s ts) = do
  tstr <- peekCString ts
  return $ MethodDescription s tstr

-- | Type class for representing any value that has an underlying pointer
-- that points to an Objective-C class. 
class ToId a where
  toId :: a -> Id

instance ToId Id where
  toId = id

instance ToId Class where
  toId (Class c) = Id (castPtr c)


-- * Working with classes
foreign import ccall "class_getName" class_getName :: Class -> CString

-- | Returns the name of a class.
className :: Class -> String
className = unsafePerformIO . peekCString . class_getName

foreign import ccall "class_getSuperclass" class_getSuperclass :: Class -> Class

-- | Returns the superclass of a class.
classSuperclass :: Class -> Maybe Class
classSuperclass c = let s = class_getSuperclass c in if s == nil
  then Nothing
  else Just s

foreign import ccall "class_isMetaClass" class_isMetaClass :: Class -> CUInt

-- | Returns a Boolean value that indicates whether a class object is a metaclass.
classIsMetaClass :: Class -> Bool
classIsMetaClass = toBool . class_isMetaClass

foreign import ccall "class_getInstanceSize" class_getInstanceSize :: Class -> CSize

-- | Returns the size of instances of a class.
classInstanceSize :: Class -> CSize
classInstanceSize = class_getInstanceSize

foreign import ccall "class_getInstanceVariable" class_getInstanceVariable :: Class -> CString -> IO IVar

-- | Returns the Ivar for a specified instance variable of a given class.
classInstanceVariable
  :: MonadIO m
  => Class -- ^ The class whose instance variable you wish to obtain.
  -> String -- ^ The name of the instance variable definition to obtain.
  -> m IVar
classInstanceVariable c str = liftIO $ withCString str (class_getInstanceVariable c)

foreign import ccall "class_getClassVariable" class_getClassVariable :: Class -> CString -> IO IVar

-- | Returns the Ivar for a specified class variable of a given class.
classClassVariable
  :: MonadIO m
  => Class -- ^ The class definition whose class variable you wish to obtain.
  -> String -- ^ The name of the class variable definition to obtain.
  -> m IVar -- ^ A pointer to an Ivar data structure containing information about the class variable specified by nam.
classClassVariable c str = liftIO $ withCString str (class_getClassVariable c)

foreign import ccall "class_addIvar" class_addIvar :: Class -> CString -> CSize -> Word8 -> CString -> IO CUInt

-- | Adds a new instance variable to a class.
--
-- This function may only be called after objc_allocateClassPair and before objc_registerClassPair. Adding an instance variable to an existing class is not supported.
--
-- The class must not be a metaclass. Adding an instance variable to a metaclass is not supported.
--
-- The instance variable's minimum alignment in bytes is 1<<align. The minimum alignment of an instance variable depends on the ivar's type and the machine architecture. For variables of any pointer type, pass log2(sizeof(pointer_type)).
classAddIvar
  :: MonadIO m
  => Class
  -> String -- ^ Instance variable name
  -> Int -- ^ Value size
  -> Word8 -- ^ Alignment
  -> String -- ^ Types
  -> m Bool -- ^ @True@ if the instance variable was added successfully, otherwise @False@ (for example, the class already contains an instance variable with that name).
classAddIvar c str size align enc = liftIO $ withCString str $ \strp -> withCString enc $ \encp ->
  fmap (== 1) $ class_addIvar c strp (fromIntegral size) align encp

foreign import ccall "class_copyIvarList" class_copyIvarList :: Class -> Ptr CUInt -> IO (Ptr IVar)

-- | Describes the instance variables declared by a class.
--
-- Any instance variables declared by superclasses are not included.
classIvarList
  :: MonadIO m
  => Class
  -> m (VS.Vector IVar)
classIvarList = liftIO . listCopier . class_copyIvarList

newtype IvarLayout = IvarLayout (Ptr Word8)
  deriving (Show, Eq)

foreign import ccall "class_getIvarLayout" class_getIvarLayout :: Class -> IO IvarLayout

-- | Returns a description of the Ivar layout for a given class.
classGetIvarLayout :: MonadIO m => Class -> m IvarLayout
classGetIvarLayout = liftIO . class_getIvarLayout

foreign import ccall "class_setIvarLayout" class_setIvarLayout :: Class -> IvarLayout -> IO ()

-- | Sets the Ivar layout for a given class.
classSetIvarLayout :: MonadIO m => Class -> IvarLayout -> m ()
classSetIvarLayout c = liftIO . class_setIvarLayout c

foreign import ccall "class_getWeakIvarLayout" class_getWeakIvarLayout :: Class -> IO IvarLayout

-- | Returns a description of the layout of weak Ivars for a given class.
classGetWeakIvarLayout :: MonadIO m => Class -> m IvarLayout
classGetWeakIvarLayout = liftIO . class_getWeakIvarLayout

foreign import ccall "class_setWeakIvarLayout" class_setWeakIvarLayout :: Class -> IvarLayout -> IO ()

-- | Sets the layout for weak Ivars for a given class.
classSetWeakIvarLayout :: MonadIO m => Class -> IvarLayout -> m ()
classSetWeakIvarLayout c = liftIO . class_setWeakIvarLayout c

foreign import ccall "class_getProperty" class_getProperty :: Class -> CString -> IO Property

-- | Returns a property with a given name of a given class.
classGetProperty :: MonadIO m => Class -> String -> m (Maybe Property)
classGetProperty c s = liftIO (nullCheck <$> (withCString s $ class_getProperty c))

foreign import ccall "class_copyPropertyList" class_copyPropertyList :: Class -> Ptr CUInt -> IO (Ptr Property)

-- | Describes the properties declared by a class.
--
-- Any properties declared by superclasses are not included.
classPropertyList
  :: MonadIO m
  => Class
  -> m (VS.Vector Property)
classPropertyList = liftIO . listCopier . class_copyPropertyList

foreign import ccall "class_addMethod" class_addMethod :: Class -> Selector -> Imp p -> CString -> IO CUInt

-- | Adds a new method to a class with a given name and implementation.
--
-- @classAddMethod@ will add an override of a superclass's implementation, but will not replace an existing implementation in this class. To change an existing implementation, use @methodSetImplementation@.
classAddMethod
  :: MonadIO m
  => Class -- ^ The class to which to add a method.
  -> Selector -- ^ A selector that specifies the name of the method being added.
  -> Imp p -- ^ A function which is the implementation of the new method. The function must take at least two arguments—self and _cmd.
  -> String -- ^ An array of characters that describe the types of the arguments to the method. For possible values, see Objective-C Runtime Programming Guide > Type Encodings. Since the function must take at least two arguments—self and _cmd, the second and third characters must be “@:” (the first character is the return type).

  -> m Bool -- ^ @True@ if the method was added successfully, otherwise @False@ (for example, the class already contains a method implementation with that name).
classAddMethod c s i n = liftIO (toBool <$> withCString n (class_addMethod c s i))

foreign import ccall "class_getInstanceMethod" class_getInstanceMethod :: Class -> Selector -> IO Method

-- | Returns a specified instance method for a given class.
--
-- The method that corresponds to the implementation of the selector specified for the given class, or @Nothing@ if the specified class or its superclasses do not contain an instance method with the specified selector.
--
-- Note that this function searches superclasses for implementations, whereas classMethodList does not.
classGetInstanceMethod :: MonadIO m => Class -> Selector -> m (Maybe Method)
classGetInstanceMethod c s = liftIO (nullCheck <$> class_getInstanceMethod c s)

foreign import ccall "class_getClassMethod" class_getClassMethod :: Class -> Selector -> IO Method

-- | Returns a pointer to the data structure describing a given class method for a given class.
--
-- Note that this function searches superclasses for implementations, whereas classMethodList does not.
classGetClassMethod :: MonadIO m => Class -> Selector -> m (Maybe Method)
classGetClassMethod c s = liftIO (nullCheck <$> class_getClassMethod c s)

foreign import ccall "class_copyMethodList" class_copyMethodList :: Class -> Ptr CUInt -> IO (Ptr Method)

-- | Describes the instance methods implemented by a class.
--
-- To get the class methods of a class, use @(objectGetClass classVal >>= classMethodList)@
classMethodList :: MonadIO m => Class -> m (VS.Vector Method)
classMethodList = liftIO . listCopier . class_copyMethodList

foreign import ccall "class_replaceMethod" class_replaceMethod :: Class -> Selector -> Imp a -> CString -> IO (Imp b)

-- | Replaces the implementation of a method for a given class.
classReplaceMethod
  :: MonadIO m
  => Class -- ^ The class you want to modify.
  -> Selector -- ^ A selector that identifies the method whose implementation you want to replace.
  -> Imp a -- ^ The new implementation for the method identified by name for the class identified by cls.
  -> String -- ^ An array of characters that describe the types of the arguments to the method. For possible values, see Objective-C Runtime Programming Guide > Type Encodings. Since the function must take at least two arguments—self and _cmd, the second and third characters must be “@:” (the first character is the return type).
  -> m (Imp b) -- ^ The previous implementation of the method identified by name for the class identified by cls.
classReplaceMethod c s imp str = liftIO $ withCString str $ \strp ->
  class_replaceMethod c s imp strp

foreign import ccall "class_getMethodImplementation" class_getMethodImplementation :: Class -> Selector -> IO (Imp a)

-- | Returns the function pointer that would be called if a particular message were sent to an instance of a class.
--
-- class_getMethodImplementation may be faster than method_getImplementation(class_getInstanceMethod(cls, name)).
--
-- The function pointer returned may be a function internal to the runtime instead of an actual method implementation. For example, if instances of the class do not respond to the selector, the function pointer returned will be part of the runtime's message forwarding machinery.
classGetMethodImplementation :: MonadIO m => Class -> Selector -> m (Imp a)
classGetMethodImplementation c = liftIO . class_getMethodImplementation c

foreign import ccall "class_getMethodImplementation_stret" class_getMethodImplementation_stret :: Class -> Selector -> IO (Imp a)

-- | Returns the function pointer that would be called if a particular message were sent to an instance of a class. Similar to @classGetMethodImplementation@, but intended for functions which return a C struct. This means that the FunPtr returned to Haskell is probably of questionable utility since there's no way for the Haskell FFI to handle returned structs. However, this may still have its uses in the event that a package like @inline-c@ is used in conjunction with this package
classGetMethodImplementationStret :: MonadIO m => Class -> Selector -> m (Imp a)
classGetMethodImplementationStret c = liftIO . class_getMethodImplementation_stret c

foreign import ccall "class_respondsToSelector" class_respondsToSelector :: Class -> Selector -> IO CUInt

-- | Returns a Boolean value that indicates whether instances of a class respond to a particular selector.
respondsToSelector
  :: MonadIO m => Class -> Selector -> m Bool
respondsToSelector c = liftIO . fmap toBool . class_respondsToSelector c

foreign import ccall "class_addProtocol" class_addProtocol :: Class -> Protocol -> IO CUInt

-- | Adds a protocol to a class.
classAddProtocol
  :: MonadIO m
  => Class
  -> Protocol
  -> m Bool -- ^ @True@ if the protocol was added successfully, otherwise @False@ (for example, the class already conforms to that protocol).
classAddProtocol c = liftIO . fmap toBool . class_addProtocol c

foreign import ccall "class_addProperty" class_addProperty :: Class -> CString -> Ptr RawAttribute -> CUInt -> IO CUInt

-- | Adds a property to a class.
classAddProperty
  :: MonadIO m
  => Class -- ^ The class to modify.
  -> String -- ^ The name of the property.
  -> [Attribute] -- ^ An array of property attributes.
  -> m Bool -- ^ @True@ if the property was added successfully; otherwise @False@ (for example, this function returns @False@ if the class already has that property).
classAddProperty c s as = liftIO $ fmap toBool $ with (mapM rawAttribute as) $ \ras ->
  withArrayLen ras $ \l rasp ->
  withCString s $ \sp ->
  class_addProperty c sp rasp (fromIntegral l)

foreign import ccall "class_replaceProperty" class_replaceProperty :: Class -> CString -> Ptr RawAttribute -> CUInt -> IO ()

-- | Replace a property of a class.
classReplaceProperty
  :: MonadIO m
  => Class -- ^ The class to modify.
  -> String -- ^ The name of the property.
  -> [Attribute] -- ^ An array of property attributes.
  -> m ()
classReplaceProperty c s as = liftIO $ with (mapM rawAttribute as) $ \ras ->
  withArrayLen ras $ \l rasp ->
  withCString s $ \sp ->
  class_replaceProperty c sp rasp (fromIntegral l)

foreign import ccall "class_conformsToProtocol" class_conformsToProtocol :: Class -> Protocol -> IO CUInt

-- | Returns a Boolean value that indicates whether a class conforms to a given protocol.
classConformsToProtocol :: MonadIO m => Class -> Protocol -> m Bool
classConformsToProtocol c p = liftIO (toBool <$> class_conformsToProtocol c p)

foreign import ccall "class_copyProtocolList" class_copyProtocolList :: Class -> Ptr CUInt -> IO (Ptr Protocol)

-- | Describes the protocols adopted by a class.
--
-- Any protocols adopted by superclasses or other protocols are not included.
classProtocolList :: MonadIO m => Class -> m (VS.Vector Protocol)
classProtocolList = liftIO . listCopier . class_copyProtocolList

foreign import ccall "class_getVersion" class_getVersion :: Class -> IO CInt

-- | Returns the version number of a class definition.
--
-- You can use the version number of the class definition to provide versioning of the interface that your class represents to other classes. This is especially useful for object serialization (that is, archiving of the object in a flattened form), where it is important to recognize changes to the layout of the instance variables in different class-definition versions.
--
-- Classes derived from the Foundation framework NSObject class can obtain the class-definition version number using the getVersion class method, which is implemented using the class_getVersion function.
classGetVersion
  :: MonadIO m
  => Class -- ^ A pointer to an Class data structure. Pass the class definition for which you wish to obtain the version.
  -> m Int -- ^ An integer indicating the version number of the class definition.
classGetVersion = liftIO . fmap fromIntegral . class_getVersion

foreign import ccall "class_setVersion" class_setVersion :: Class -> CInt -> IO ()

-- | Sets the version number of a class definition.
--
-- You can use the version number of the class definition to provide versioning of the interface that your class represents to other classes. This is especially useful for object serialization (that is, archiving of the object in a flattened form), where it is important to recognize changes to the layout of the instance variables in different class-definition versions.
--
-- Classes derived from the Foundation framework NSObject class can set the class-definition version number using the setVersion: class method, which is implemented using the class_setVersion function.
classSetVersion :: MonadIO m => Class -> Int -> m ()
classSetVersion c = liftIO . class_setVersion c . fromIntegral

-- classInstanceVariable :: Class -> CString -> IVar
-- classInstanceVariable = class_getInstanceVariable

-- * Adding classes

foreign import ccall "objc_allocateClassPair" objc_allocateClassPair :: Class -> CString -> CSize -> IO Class

-- | Creates a new class and metaclass.
allocateClassPair
  :: MonadIO m
  => Maybe Class -- ^ The class to use as the new class's superclass, or Nothing to create a new root class.
  -> String -- ^ The string to use as the new class's name.
  -> Int -- ^ The number of bytes to allocate for indexed ivars at the end of the class and metaclass objects. This should usually be 0.
  -> m (Maybe Class) -- ^ The new class, or Nothing if the class could not be created (for example, the desired name is already in use).
allocateClassPair c str extraBytes = liftIO $ fmap nullCheck $ withCString str $ \cstr ->
  objc_allocateClassPair (fromMaybe nil c) cstr (fromIntegral extraBytes)

foreign import ccall "objc_disposeClassPair" objc_disposeClassPair :: Class -> IO ()

-- | Destroys a class and its associated metaclass.
--
-- Do not call this function if instances of the class or any subclass exist.
disposeClassPair :: MonadIO m => Class -> m ()
disposeClassPair = liftIO . objc_disposeClassPair

foreign import ccall "objc_registerClassPair" objc_registerClassPair :: Class -> IO ()

-- | Registers a class that was allocated using allocateClassPair.
registerClassPair :: MonadIO m => Class -> m ()
registerClassPair = liftIO . objc_registerClassPair

-- * Instantiating classes

foreign import ccall "class_createInstance" class_createInstance :: Class -> CSize -> IO Id

-- | Creates an instance of a class, allocating memory for the class in the default malloc memory zone.
createInstance
  :: MonadIO m
  => Class -- ^ The class that you want to allocate an instance of.
  -> Int -- ^ An integer indicating the number of extra bytes to allocate. The additional bytes can be used to store additional instance variables beyond those defined in the class definition.
  -> m Id -- ^ An instance of the class cls.
createInstance c s = liftIO $ class_createInstance c (fromIntegral s)

foreign import ccall "objc_constructInstance" objc_constructInstance
  :: Class
  -> Ptr a -- ^ The location at which to allocate an instance of the cls class. bytes must point to at least class_getInstanceSize(cls) bytes of well-aligned, zero-filled memory.
  -> IO Id

-- | Creates an instance of a class at the specified location.
constructInstance
  :: MonadIO m
  => Class -- ^ The class that you want to allocate an instance of.
  -> Ptr a -- ^ The location at which to allocate an instance of the cls class. bytes myst point to at least class_getInstanceSize(cls) bytes of well-aligned, zero-filled memory.
  -> m (Maybe Id) -- ^ An instance of the class cls at bytes, if successful; otherwise @Nothing@ (for example, if the class or bytes are themselves null).
constructInstance c = liftIO . fmap nullCheck . objc_constructInstance c

foreign import ccall "objc_destructInstance" objc_destructInstance :: Id -> IO (Ptr a)

-- | Destroys an instance of a class without freeing memory and removes any of its associated references.
--
-- This method does nothing if obj is nil.
--
-- Important
--
-- The Objective-C garbage collector does not call this function. As a result, if you edit this function, you should also edit finalize. That said, Core Foundation and other clients do call this function under garbage collection.
destructInstance :: (MonadIO m, ToId id) => id -> m (Ptr a)
destructInstance = liftIO . objc_destructInstance . toId

-- * Working with instances

foreign import ccall "object_copy" object_copy :: Id -> CSize -> IO Id

-- | Returns a copy of a given object.
copy
  :: (MonadIO m, Coercible id Id, ToId id)
  => id -- ^ An Objective-C object.
  -> Int -- ^ The size of the object
  -> m id -- ^ A copy of the supplied object.
copy i s = liftIO $ coerce $ object_copy (toId i) (fromIntegral s)

foreign import ccall "object_dispose" object_dispose :: Id -> IO Id

-- | Frees the memory occupied by a given object.
dispose :: (MonadIO m, ToId id)
  => id -- ^ An Objective-C object.
  -> m ()
dispose = liftIO . Control.Monad.void . object_dispose . toId

foreign import ccall "object_setInstanceVariable" object_setInstanceVariable :: Id -> CString -> Ptr a -> IO IVar

-- | Changes the value of an instance variable of a class instance.
objectSetInstanceVariable
  :: (MonadIO m, ToId id)
  => id -- ^ A pointer to an instance of a class. Pass the object containing the instance variable whose value you wish to modify.
  -> String -- ^ Pass the name of the instance variable whose value you wish to modify.
  -> Ptr a -- ^ The new value for the instance variable.
  -> m IVar -- ^ A pointer to the Ivar data structure that defines the type and name of the instance variable specified by the name string.
objectSetInstanceVariable i str p = liftIO $ withCString str $ \cstr ->
  object_setInstanceVariable (toId i) cstr p

foreign import ccall "object_getInstanceVariable" object_getInstanceVariable :: Id -> CString -> Ptr (Ptr a) -> IO IVar

-- | Obtains the value of an instance variable of a class instance.
objectGetInstanceVariable
  :: (MonadIO m, ToId id)
  => id -- ^ A pointer to an instance of a class. Pass the object containing the instance variable whose value you wish to obtain.
  -> String -- ^ A C string. Pass the name of the instance variable whose value you wish to obtain.
  -> m (Ptr a, IVar) -- ^ Pointer to the value of the instance variable, and a pointer to the Ivar data structure that defines the type and name of the instance variable specified by name.
objectGetInstanceVariable i str = liftIO $ withCString str $ \cstr -> alloca $ \pp -> do
  i <- object_getInstanceVariable (toId i) cstr pp
  p <- peek pp
  return (p, i)

-- instanceVariable :: ToId id => id -> String -> StateVar ()

foreign import ccall "object_getIndexedIvars" object_getIndexedIvars :: Id -> IO (Ptr a)

-- | Returns a pointer to any extra bytes allocated with a instance given object.
objectGetIndexedIvars
  :: (MonadIO m, ToId id)
  => id -- ^ An Objective-C object.
  -> m (Ptr a) -- ^ A pointer to any extra bytes allocated for the given object. If the object was not allocated with any extra bytes, then dereferencing the returned pointer is undefined.
objectGetIndexedIvars = liftIO . object_getIndexedIvars . toId

foreign import ccall "object_getIvar" object_getIvar :: Id -> IVar -> IO Id

-- | Reads the value of an instance variable in an object.
objectGetIVar :: (MonadIO m, ToId id) => id -> IVar -> m (Maybe Id)
objectGetIVar i v = liftIO $ do
  r <- object_getIvar (toId i) v
  return $! if r == nil
    then Nothing
    else Just r

foreign import ccall "object_setIvar" object_setIvar :: Id -> IVar -> Id -> IO ()

-- | Sets the value of an instance variable in an object.
objectSetIVar :: (MonadIO m, ToId id, ToId a) => id -> IVar -> a -> m ()
objectSetIVar o iv x = liftIO $ object_setIvar (toId o) iv (toId x)

-- ivar :: (ToId id, ToId a) => id -> IVar -> StateVar a
-- ivar o v = makeStateVar (objectGetIVar o v) (objectSetIVar o v)

foreign import ccall "object_getClassName" object_getClassName :: Id -> IO CString

-- | Returns the class name of a given object.
objectGetClassName :: (MonadIO m, ToId id) => id -> m String
objectGetClassName i = liftIO (object_getClassName (toId i) >>= peekCString)

foreign import ccall "object_getClass" object_getClass :: Id -> IO Class

-- | Returns the class of an object.
--
-- The class object of which object is an instance, or @Nothing@ if object is nil.
objectGetClass :: (MonadIO m, ToId id) => id -> m (Maybe Class)
objectGetClass = liftIO . fmap nullCheck . object_getClass . toId

foreign import ccall "object_setClass" object_setClass :: Id -> Class -> IO Class

-- | Sets the class of an object.
objectSetClass
  :: (MonadIO m, ToId id)
  => id
  -> Class
  -> m (Maybe Class) -- ^ The previous value of object‘s class, or @Nothing@ if object is nil.
objectSetClass o = liftIO . fmap nullCheck . object_setClass (toId o)

-- * Obtaining class definitions

foreign import ccall "objc_getClassList" objc_getClassList :: Ptr Class -> Int -> IO Int

foreign import ccall "objc_copyClassList" objc_copyClassList :: Ptr CUInt -> IO (Ptr Class)

-- | Obtains the list of registered class definitions.
classList :: MonadIO m => m (VS.Vector Class)
classList = liftIO $ listCopier objc_copyClassList

foreign import ccall "objc_lookUpClass" objc_lookUpClass :: CString -> IO Class

-- | Returns the class definition of a specified class.
lookupClass
  :: MonadIO m
  => String -- ^ The name of the class to look up.
  -> m (Maybe Class) -- ^ The Class object for the named class, or @Nothing@ if the class is not registered with the Objective-C runtime.
lookupClass str = liftIO $ withCString str $ \cstr -> do
  c <- objc_lookUpClass cstr
  return $! if c == nil
    then Nothing
    else Just c

foreign import ccall "objc_getClass" objc_getClass :: CString -> IO Class

-- | Returns the class definition of a specified class.
--
-- getClass is different from lookupClass in that if the class is not registered, getClass calls the class handler callback and then checks a second time to see whether the class is registered. lookupClass does not call the class handler callback.
getClass
  :: MonadIO m
  => String -- ^ The name of the class to look up.
  -> m (Maybe Class) -- ^ The Class object for the named class, or @Nothing@ if the class is not registered with the Objective-C runtime.
getClass str = liftIO $ withCString str $ \cstr -> do
  c <- objc_getClass cstr
  return $! if c == nil
    then Nothing
    else Just c

foreign import ccall "objc_getRequiredClass" objc_getRequiredClass :: CString -> IO Class

-- | This function is the same as @getClass@, but kills the process if the class is not found.
requireClass :: MonadIO m => String -> m Class
requireClass str = liftIO $ withCString str objc_getRequiredClass

foreign import ccall "objc_getMetaClass" objc_getMetaClass :: CString -> IO Class

-- | Returns the metaclass definition of a specified class.
getMetaClass
  :: MonadIO m
  => String -- ^ The name of the class to look up.
  -> m (Maybe Class) -- ^ The Class object for the metaclass of the named class, or nil if the class is not registered with the Objective-C runtime.
getMetaClass str = liftIO $ withCString str $ \cstr -> do
  c <- objc_getMetaClass cstr
  return $! if c == nil
    then Nothing
    else Just c

-- * Working with instance variables

foreign import ccall "ivar_getName" ivar_getName :: IVar -> CString

-- | Returns the name of an instance variable.
ivarName :: IVar -> String
ivarName = unsafePerformIO . peekCString . ivar_getName

foreign import ccall "ivar_getTypeEncoding" ivar_getTypeEncoding :: IVar -> IO CString

-- | Returns the type string of an instance variable.
--
-- For possible values, see Objective-C Runtime Programming Guide > Type Encodings.
ivarTypeEncoding :: IVar -> String
ivarTypeEncoding i = unsafePerformIO $ do
  cstr <- ivar_getTypeEncoding i
  peekCString cstr

foreign import ccall "ivar_getOffset" ivar_getOffset :: IVar -> CPtrdiff

-- | Returns the offset of an instance variable.
--
-- For instance variables of type id or other object types, call object_getIvar and object_setIvar instead of using this offset to access the instance variable data directly.
ivarOffset :: IVar -> Int
ivarOffset = fromIntegral . ivar_getOffset

-- * Associative references

foreign import ccall "objc_setAssociatedObject" objc_setAssociatedObject :: Id -> Ptr a -> Id -> CInt -> IO ()

-- | Sets an associated value for a given object using a given key and association policy.
setAssociatedObject
  :: (MonadIO m, ToId id)
  => id -- ^ The source object for the association.
  -> Ptr a -- ^ The key for the association.
  -> Id -- ^ The value to associate with the key key for object. Pass nil to clear an existing association.
  -> AssociativeObjectBehavior -- ^ The policy for the association. For possible values, see Associative Object Behaviors.
  -> m ()
setAssociatedObject o k v pol = liftIO $ objc_setAssociatedObject (toId o) k v (fromIntegral $ fromEnum pol)

foreign import ccall "objc_getAssociatedObject" objc_getAssociatedObject :: Id -> Ptr a -> IO Id

-- | Returns the value associated with a given object for a given key.
getAssociatedObject
  :: (MonadIO m, ToId id)
  => id -- ^ The source object for the association.
  -> Ptr a -- ^ The key for the association.
  -> m Id -- ^ The value associated with the key key for object.
getAssociatedObject i = liftIO . objc_getAssociatedObject (toId i)

foreign import ccall "objc_removeAssociatedObjects" objc_removeAssociatedObjects :: Id -> IO ()

-- | Removes all associations for a given object.
--
-- The main purpose of this function is to make it easy to return an object to a "pristine state”. You should not use this function for general removal of associations from objects, since it also removes associations that other clients may have added to the object. Typically you should use objc_setAssociatedObject with a nil value to clear an association.
removeAssociatedObjects
  :: (MonadIO m, ToId id)
  => id -- ^ An object that maintains associated objects.
  -> m ()
removeAssociatedObjects = liftIO . objc_removeAssociatedObjects . toId

-- * Sending messages

-- TODO quasiquoter
-- send :: QuasiQuoter
-- send = undefined

foreign import ccall "&objc_msgSend" p_objc_msgSend :: FunPtr (Id -> Selector -> IO Id)

-- | Sends a message with a simple return value to an instance of a class.
msgSend :: (MonadIO m, ToId a) => a -> Selector -> [Arg] -> m Id
msgSend x (Selector s) as = liftIO ((Id . castPtr) <$> callFFI p_objc_msgSend (retPtr retVoid) (argPtr i : argPtr s : as))
  where (Id i) = toId x

foreign import ccall "&objc_msgSend_stret" p_objc_msgSend_stret :: FunPtr (Id -> Selector -> IO (Ptr ()))
foreign import ccall "&objc_msgSendSuper" p_objc_msgSendSuper :: FunPtr (Id -> Selector -> IO Id)
foreign import ccall "&objc_msgSendSuper_stret" p_objc_msgSendSuper_stret :: FunPtr (Id -> Selector -> IO (Ptr ()))

foreign import ccall "&method_invoke" method_invoke :: FunPtr (Id -> Method -> IO Id)
foreign import ccall "&method_invoke_stret" method_invoke_stret :: FunPtr (Id -> Method -> IO (Ptr ()))

foreign import ccall "method_getName" method_getName :: Method -> Selector

-- | Returns the name of a method.
--
-- To get the method name as a string, call selectorName(methodName(method)).
methodName :: Method -> Selector
methodName = method_getName

foreign import ccall "method_getImplementation" method_getImplementation :: Method -> IO (Imp a)

-- | Returns the implementation of a method.
methodGetImplementation :: MonadIO m => Method -> m (Imp a)
methodGetImplementation = liftIO . method_getImplementation

foreign import ccall "method_getTypeEncoding" method_getTypeEncoding :: Method -> IO CString

-- | Returns a string describing a method's parameter and return types.
methodTypeEncoding :: MonadIO m => Method -> m (Maybe String)
methodTypeEncoding m = liftIO $ do
  e <- method_getTypeEncoding m
  if e /= nullPtr
    then Just <$> peekCString e
    else pure Nothing

foreign import ccall "method_copyReturnType" method_copyReturnType :: Method -> IO CString

-- | Returns a string describing a method's return type.
methodReturnType :: MonadIO m => Method -> m String
methodReturnType m = liftIO $ do
  t <- method_copyReturnType m
  s <- peekCString t
  free t
  return s

foreign import ccall "method_copyArgumentType" method_copyArgumentType :: Method -> CUInt -> IO CString

-- | Returns a string describing a single parameter type of a method,
-- or @Nothing@ if method has no parameter at the given index.
methodArgumentType :: MonadIO m
  => Method
  -> Int -- ^ The index of the parameter to inspect.
  -> m (Maybe String)
methodArgumentType m i = liftIO $ do
  t <- method_copyArgumentType m (fromIntegral i)
  if t /= nullPtr
    then do
      s <- peekCString t
      free t
      return $ Just s
    else return Nothing

foreign import ccall "method_getReturnType" method_getReturnType :: Method -> CString -> CSize -> IO ()

foreign import ccall "method_getNumberOfArguments" method_getNumberOfArguments :: Method -> IO CUInt

-- | Returns the number of arguments accepted by a method.
methodNumberOfArguments :: Method -> Int
methodNumberOfArguments = fromIntegral . unsafePerformIO . method_getNumberOfArguments

foreign import ccall "method_getArgumentType" method_getArgumentType :: Method -> CUInt -> CString -> CSize -> IO ()

foreign import ccall "method_getDescription" method_getDescription :: Method -> IO (Ptr RawMethodDescription)

-- | Returns a method description structure for a specified method.
methodGetDescription :: MonadIO m => Method -> m MethodDescription
methodGetDescription m = liftIO $ do
  p <- method_getDescription m
  raw <- peek p
  str <- peekCString $ rawMethodDescriptionTypes raw
  return $ MethodDescription (rawMethodDescriptionName raw) str

foreign import ccall "method_setImplementation" method_setImplementation :: Method -> Imp a -> IO (Imp b)

-- | Sets the implementation of a method.
methodSetImplementation
  :: MonadIO m
  => Method
  -> Imp a
  -> m (Imp b) -- ^ The previous implementation of the method.
methodSetImplementation m = liftIO . method_setImplementation m

foreign import ccall "method_exchangeImplementations" method_exchangeImplementations :: Method -> Method -> IO ()

-- | Exchanges the implementations of two methods.
--
-- This is an atomic version of the following:
--
-- >  IMP imp1 = method_getImplementation(m1);
-- >  IMP imp2 = method_getImplementation(m2);
-- >  method_setImplementation(m1, imp2);
-- >  method_setImplementation(m2, imp1);
methodExchangeImplementations :: MonadIO m => Method -> Method -> m ()
methodExchangeImplementations m = liftIO . method_exchangeImplementations m

foreign import ccall "objc_copyImageNames" objc_copyImageNames :: Ptr CUInt -> IO (Ptr CString)

-- | Returns the names of all the loaded Objective-C frameworks and dynamic libraries.
imageNames
  :: MonadIO m
  => m [String] -- ^ An array of C strings representing the names of all the loaded Objective-C frameworks and dynamic libraries.
imageNames = liftIO $ alloca $ \len -> do
  strs <- objc_copyImageNames len
  l <- peek len
  arr <- peekArray (fromIntegral l) strs
  mapM peekCString arr

foreign import ccall "class_getImageName" class_getImageName :: Class -> IO CString

-- | Returns the name of the dynamic library a class originated from.
classImageName
  :: MonadIO m
  => Class -- ^ The class you are inquiring about.
  -> m (Maybe String) -- ^ A C string representing the name of the library containing the provided class.
classImageName c = liftIO $ do
  n <- class_getImageName c
  if n == nullPtr
    then pure Nothing
    else Just <$> peekCString n

foreign import ccall "objc_copyClassNamesForImage" objc_copyClassNamesForImage :: CString -> Ptr CUInt -> IO (Ptr CString)

-- | Returns the names of all the classes within a specified library or framework.
classNamesForImage
  :: MonadIO m
  => String -- ^ The library or framework you are inquiring about.
  -> m [String] -- ^ An array of C strings representing all of the class names within the specified library or framework.
classNamesForImage i = liftIO $ alloca $ \len -> withCString i $ \ip -> do
  nsp <- objc_copyClassNamesForImage ip len
  l <- peek len
  arr <- peekArray (fromIntegral l) nsp
  mapM peekCString arr

foreign import ccall "sel_getName" sel_getName :: Selector -> CString

-- | Returns the name of the method specified by a given selector.
selectorName
  :: Selector
  -> String
selectorName = unsafePerformIO . peekCString . sel_getName

foreign import ccall "sel_registerName" sel_registerName :: CString -> IO Selector

-- | Registers a method with the Objective-C runtime system, maps the method name to a selector, and returns the selector value.
selector
  :: String
  -> Selector
selector str = unsafeDupablePerformIO $ withCString str sel_registerName

foreign import ccall "sel_isEqual" sel_isEqual :: Selector -> Selector -> CUInt

instance Eq Selector where
  (==) s1 s2 = toBool (sel_isEqual s1 s2)

instance Show Selector where
  show = selectorName

foreign import ccall "objc_getProtocol" objc_getProtocol :: CString -> IO Protocol

-- | Returns a specified protocol.
--
-- This function acquires the Objective-C runtime lock.
getProtocol
  :: MonadIO m
  => String -- ^ The name of a protocol.
  -> m (Maybe Protocol) -- ^ The protocol named name, or @Nothing@ if no protocol named name could be found.
getProtocol str = liftIO $ withCString str $ \cstr -> do
  p <- objc_getProtocol cstr
  return $ if p == Protocol nullPtr
    then Nothing
    else Just p

foreign import ccall "objc_copyProtocolList" objc_copyProtocolList :: Ptr CUInt -> IO (Ptr Protocol)

-- | Returns an array of all the protocols known to the runtime.
protocolList :: MonadIO m => m (VS.Vector Protocol)
protocolList = liftIO $ listCopier objc_copyProtocolList

foreign import ccall "objc_allocateProtocol" objc_allocateProtocol :: CString -> IO Protocol

-- | Creates a new protocol instance.
--
-- You must register the returned protocol instance with the registerProtocol function before you can use it.
--
-- There is no dispose method associated with this function.
allocateProtocol
  :: MonadIO m
  => String -- ^ The name of the protocol you want to create.
  -> m (Maybe Protocol) -- ^ A new protocol instance or @Nothing@ if a protocol with the same name as name already exists.
allocateProtocol str = liftIO $ do
  r <- withCString str objc_allocateProtocol
  return $! if r == nil
    then Nothing
    else Just r

foreign import ccall "objc_registerProtocol" objc_registerProtocol :: Protocol -> IO ()

-- | Registers a newly created protocol with the Objective-C runtime.
--
-- When you create a new protocol using allocateProtocol, you then register it with the Objective-C runtime by calling this function. After a protocol is successfully registered, it is immutable and ready to use.
registerProtocol
  :: MonadIO m
  => Protocol -- ^ The protocol you want to register with the Objective-C runtime.
  -> m ()
registerProtocol = liftIO . objc_registerProtocol

foreign import ccall "protocol_addMethodDescription" protocol_addMethodDescription :: Protocol -> Selector -> CString -> CUInt -> CUInt -> IO ()

-- | Adds a method to a protocol.
--
-- To add a method to a protocol using this function, the protocol must be under construction. That is, you must add any methods to proto before you register it with the Objective-C runtime (via the objc_registerProtocol function).
protocolAddMethodDescription
  :: MonadIO m
  => Protocol -- ^ The protocol you want to add a method to.
  -> Selector -- ^ The name of the method you want to add.
  -> String -- ^ A string representing the signature of the method you want to add.
  -> Bool -- ^ A Boolean indicating whether the method is a required method of throto protocol. If @True@, the method is a required method; if @False@, the method is an optional method.
  -> Bool -- ^ A Boolean indicating whether the method is an instance method. If @True@, the method is an instance method; if @False@, the method is a class method.
  -> m ()
protocolAddMethodDescription p s str req inst = liftIO $ withCString str $ \cstr ->
  protocol_addMethodDescription p s cstr (if req then yes else no) (if inst then yes else no)

foreign import ccall "protocol_addProtocol" protocol_addProtocol :: Protocol -> Protocol -> IO ()

-- | Adds a registered protocol to another protocol that is under construction.
--
-- The protocol you want to add to must be under construction— allocated but not yet registered with the Objective-C runtime. The protocol you want to add must be registered already.
protocolAddProtocol
  :: MonadIO m
  => Protocol -- ^ The protocol you want to add the registered protocol to.
  -> Protocol -- ^ The registered protocol you want to add
  -> m ()
protocolAddProtocol p = liftIO . protocol_addProtocol p

foreign import ccall "protocol_addProperty" protocol_addProperty :: Protocol -> CString -> Ptr RawAttribute -> CUInt -> CUInt -> CUInt -> IO ()

-- | Adds a property to a protocol that is under construction.
protocolAddProperty
  :: MonadIO m
  => Protocol
  -> String -- ^ The name of the property you want to add.
  -> [Attribute] -- ^ An array of property attributes.
  -> Bool -- ^ A Boolean indicating whether the property’s accessor methods are required methods of the protocol. If @True@, the property’s accessor methods are required methods; if @False@, the property’s accessor methods are optional methods.
  -> m ()
protocolAddProperty p n as req = liftIO $
  withCString n $ \np ->
  with (mapM rawAttribute as) $ \ras ->
  withArrayLen ras $ \asl asp ->
  protocol_addProperty p np asp (fromIntegral asl) (if req then yes else no) yes
foreign import ccall "protocol_getName" protocol_getName :: Protocol -> IO CString

-- | Returns a the name of a protocol.
protocolGetName :: Protocol -> String
protocolGetName p = unsafePerformIO $ do
  s <- protocol_getName p
  peekCString s

instance Show Protocol where
  show = protocolGetName

foreign import ccall "protocol_isEqual" protocol_isEqual :: Protocol -> Protocol -> CUInt

instance Eq Protocol where
  (==) a b = toBool $ protocol_isEqual a b

foreign import ccall "protocol_copyMethodDescriptionList" protocol_copyMethodDescriptionList :: Protocol -> CUInt -> CUInt -> Ptr CUInt -> IO (Ptr RawMethodDescription)

-- | Returns an array of method descriptions of methods meeting a given specification for a given protocol.
protocolMethodDescriptionList
  :: MonadIO m
  => Protocol
  -> Bool -- ^ A Boolean value that indicates whether returned methods should be required methods (pass @True@ to specify required methods).
  -> Bool -- ^ A Boolean value that indicates whether returned methods should be instance methods (pass @True@ to specify instance methods).
  -> m (V.Vector MethodDescription)
protocolMethodDescriptionList p req inst = liftIO $ do
  l <- listCopier (protocol_copyMethodDescriptionList p (if req then yes else no) (if inst then yes else no))
  mapM peekMethodDescription $ G.convert l

-- | TODO this returns a struct directly, needs a wrapper. Don't use this
foreign import ccall "protocol_getMethodDescription" protocol_getMethodDescription :: Protocol -> Selector -> CUInt -> CUInt -> IO (Ptr RawMethodDescription)

foreign import ccall "protocol_copyPropertyList" protocol_copyPropertyList :: Protocol -> Ptr CUInt -> IO (Ptr Property)

-- | Returns an array of the properties declared by a protocol.
protocolPropertyList :: MonadIO m => Protocol -> m (VS.Vector Property)
protocolPropertyList = liftIO . listCopier . protocol_copyPropertyList

foreign import ccall "protocol_getProperty" protocol_getProperty :: Protocol-> CString -> CUInt -> CUInt -> IO Property

-- | Returns the specified property of a given protocol.
protocolProperty
  :: MonadIO m
  => Protocol
  -> String -- ^ The name of a property.
  -> Bool -- ^ A Boolean value that indicates whether name is a required property.
  -> Bool -- ^ A Boolean value that indicates whether name is an instance property.
  -> m (Maybe Property) -- ^ The property specified by name, isRequiredProperty, and isInstanceProperty for proto, or @Nothing@ if none of proto’s properties meets the specification.
protocolProperty p s req inst = liftIO $ withCString s $ \cstr -> do
  pr@(Property prp) <- protocol_getProperty p cstr (if req then yes else no) (if inst then yes else no)
  return $! if prp == nullPtr
    then Nothing
    else Just pr

foreign import ccall "protocol_conformsToProtocol" protocol_conformsToProtocol :: Protocol -> Protocol -> IO CUInt

-- | Returns a Boolean value that indicates whether one protocol conforms to another protocol.
protocolConformsToProtocol :: MonadIO m => Protocol -> Protocol -> m Bool
protocolConformsToProtocol p1 p2 = liftIO $ fmap toBool $ protocol_conformsToProtocol p1 p2

foreign import ccall "property_getName" property_getName :: Property -> CString

-- | Returns the name of a property.
propertyName :: Property -> String
propertyName = unsafePerformIO . peekCString . property_getName

instance Show Property where
  show = propertyName

foreign import ccall "property_getAttributes" property_getAttributes :: Property -> CString

-- | Returns the attribute string of a property.
--
-- The format of the attribute string is described in Declared Properties in Objective-C Runtime Programming Guide.
propertyAttributes :: Property -> String
propertyAttributes = unsafePerformIO . peekCString . property_getAttributes

foreign import ccall "property_copyAttributeValue" property_copyAttributeValue :: Property -> CString -> IO CString

-- | Returns the value of a property attribute given the attribute name.
propertyAttributeValue
  :: MonadIO m
  => Property -- ^ The property whose value you are interested in.
  -> String -- ^ A C string representing the name of the attribute.
  -> m (Maybe String) -- ^ The value string of the attributeName attribute, if one exists in property; otherwise, @Nothing@
propertyAttributeValue p str = liftIO $ withCString str $ \cstr -> do
  r <- property_copyAttributeValue p cstr
  if r /= nullPtr
    then do
      val <- peekCString r
      free r
      return $ Just val
    else pure Nothing

foreign import ccall "property_copyAttributeList" property_copyAttributeList :: Property -> Ptr CUInt -> IO (Ptr RawAttribute)

-- | Returns an array of property attributes for a given property.
propertyAttributeList
  :: MonadIO m
  => Property -- ^ The property whose attributes you want to list.
  -> m (V.Vector Attribute)
propertyAttributeList p = liftIO $ do
  l <- listCopier (property_copyAttributeList p)
  mapM readAttribute $ G.convert l

foreign import ccall "objc_enumerationMutation" objc_enumerationMutation :: Id -> IO ()

-- | The Objective-C compiler inserts this function when it detects that an object is mutated during a foreach iteration. The function is called when a mutation occurs, and the enumeration mutation handler is enacted if it is set up (via the objc_setEnumerationMutationHandler function). If the handler is not set up, a fatal error occurs.
enumerationMutation
  :: MonadIO m
  => Id -- ^ The object being mutated.
  -> m ()
enumerationMutation = liftIO . objc_enumerationMutation

foreign import ccall "objc_setEnumerationMutationHandler" objc_setEnumerationMutationHandler :: FunPtr (Id -> IO ()) -> IO ()

foreign import ccall "wrapper" enumerationMutationCallback :: (Id -> IO ()) -> IO (FunPtr (Id -> IO ()))

-- | Sets the current mutation handler.
setEnumerationMutationHandler
  :: MonadIO m
  => (Id -> IO ()) -- ^ A function pointer to the new mutation handler.
  -> m (IO ()) -- ^ Returns a cleanup function that frees the exported callback
setEnumerationMutationHandler f = liftIO $ do
  cb <- enumerationMutationCallback f
  objc_setEnumerationMutationHandler cb
  return (freeHaskellFunPtr cb)

foreign import ccall "imp_implementationWithBlock" imp_implementationWithBlock :: Id -> IO (Imp a)
foreign import ccall "imp_getBlock" imp_getBlock :: Imp a -> IO Id
foreign import ccall "imp_removeBlock" imp_removeBlock :: Imp a -> IO CUInt
foreign import ccall "objc_loadWeak" objc_loadWeak :: Ptr Id -> IO Id
foreign import ccall "objc_storeWeak" objc_storeWeak :: Ptr Id -> Id -> IO Id

-- | Policies related to associative references.
data AssociativeObjectBehavior
  = Assign -- ^ Specifies a weak reference to the associated object.
  | RetainNonatomic -- ^ Specifies a strong reference to the associated object, and that the association is not made atomically.
  | CopyNonatomic -- ^ Specifies that the associated object is copied, and that the association is not made atomically.
  | Retain -- ^ Specifies a strong reference to the associated object, and that the association is made atomically.
  | Copy -- ^ Specifies that the associated object is copied, and that the association is made atomically.

instance Enum AssociativeObjectBehavior where
  toEnum x = case x of
    0 -> Assign
    1 -> RetainNonatomic
    3 -> CopyNonatomic
    01401 -> Retain
    01403 -> Copy

  fromEnum x = case x of
    Assign -> 0
    RetainNonatomic -> 1
    CopyNonatomic -> 3
    Retain -> 01401
    Copy -> 01403

-- TESTBED

makeNSObj :: IO Id
makeNSObj = do
  (Just c) <- lookupClass "NSObject"
  r <- msgSend c (selector "new") []
  cn <- objectGetClassName r
  print cn
  return r

printClassHierarchy :: IO ()
printClassHierarchy = do
  cs <- classList
  mcs <- VS.thaw cs
  VS.sortBy (\a b -> compare (className a) (className b)) mcs
  css <- VS.unsafeFreeze mcs
  G.forM_ (G.filter ((/= '_') . head . className) css) $ \c -> do
    print $ classChain c
    putStrLn "Properties"
    print =<< classPropertyList c
    objectClass <- objectGetClass c
    forM_ objectClass $ \oc -> do
      putStrLn "Class properties"
      print =<< classPropertyList oc
      putStrLn "Class methods"
      print =<< (V.mapM methodTypeEncoding) =<< (G.convert <$> classMethodList oc)
    putStrLn "Instance methods"
    print =<< fmap (V.filter ((/= '_') . head . selectorName) . V.map methodName) (G.convert <$> classMethodList c)
    putStrLn ""
  where
    classChain c = c : case classSuperclass c of
      Nothing -> []
      Just s -> classChain s

foreign import ccall "wrapper" printyExport :: (Id -> Selector -> CString -> IO ()) -> IO (FunPtr (Id -> Selector -> CString -> IO ()))

mkPrinter :: IO Class
mkPrinter = do
  expp <- printyExport $ \_ sel cstr -> do
    print sel
    peekCString cstr >>= putStrLn
  parent <- lookupClass "NSObject"
  (Just cls) <- allocateClassPair parent "Printy" 0
  print =<< classAddMethod cls (selector "print:") expp "v@:"
  print =<< classAddProperty cls "woot" []
  registerClassPair cls
  return cls

tryPrinting = do
  (Just c) <- lookupClass "Printy"
  r <- msgSend c (selector "new") []
  cn <- objectGetClassName  r
  print cn
  withCString "yo" $ \cstr -> msgSend r (selector "print:") [argPtr cstr]
  return ()

data TypeEncoding
  = Char
  | Int
  | Short
  | Long -- ^ Treated as a 32-bit quantity on 64-bit programs
  | LongLong
  | UnsignedChar
  | UnsignedInt
  | UnsignedShort
  | UnsignedLong
  | UnsignedLongLong
  | Float
  | Double
  | Bool
  | Void
  | CharString
  | Object
  | RTClass
  | RTSelector
  | Array Int TypeEncoding
  | Structure String [TypeEncoding]
  | Union String [TypeEncoding]
  | Bits Int
  | Pointer TypeEncoding
  | Unknown
  | ConstModifier TypeEncoding
  | InModifier TypeEncoding
  | InOutModifier TypeEncoding
  | OutModifier TypeEncoding
  | ByCopyModifier TypeEncoding
  | ByRefModifier TypeEncoding
  | OneWayModifier TypeEncoding
  deriving (Show)

readTypeEncoding :: String -> Either String (TypeEncoding, String)
readTypeEncoding [] = Left []
readTypeEncoding (c:cs) = case c of
  'c' -> pure (Char, dropWhile isDigit cs)
  'i' -> pure (Int, dropWhile isDigit cs)
  's' -> pure (Short, dropWhile isDigit cs)
  'l' -> pure (Long, dropWhile isDigit cs)
  'q' -> pure (LongLong, dropWhile isDigit cs)
  'C' -> pure (UnsignedChar, dropWhile isDigit cs)
  'I' -> pure (UnsignedInt, dropWhile isDigit cs)
  'S' -> pure (UnsignedShort, dropWhile isDigit cs)
  'L' -> pure (UnsignedLong, dropWhile isDigit cs)
  'Q' -> pure (UnsignedLongLong, dropWhile isDigit cs)
  'f' -> pure (Float, dropWhile isDigit cs)
  'd' -> pure (Double, dropWhile isDigit cs)
  'B' -> pure (Bool, dropWhile isDigit cs)
  'v' -> pure (Void, dropWhile isDigit cs)
  '*' -> pure (CharString, dropWhile isDigit cs)
  '@' -> pure (Object, dropWhile isDigit cs)
  '#' -> pure (RTClass, dropWhile isDigit cs)
  ':' -> pure (RTSelector, dropWhile isDigit cs)
  {-
  '[' -> case span isDigit cs of
    (n, rest) -> case readTypeEncoding rest of
      Nothing -> error "Barf"
      Just (enc, rest') -> pure (Array (read n) enc, tail rest')
  '{' -> case span (\c -> c /= '=' && c /= '}') cs of
    (n, rest) -> case rest of
      [] -> pure (Structure n [], )
      ('=':rest') -> case span (/= '}') rest' of
        (inStruct, outStruct) -> pure (Structure n (readEncodingArray inStruct), tail outStruct)

  '(' -> case span (/= '=') cs of
    (n, rest) -> case span (/= ')') (tail rest) of
      (inUnion, outUnion) -> pure (Union n (readEncodingArray inUnion), tail outUnion)
  -}
  'b' -> case span isDigit cs of
    (n, rest) -> pure (Bits (read n), rest)
  '^' -> do
    (val, rest) <- readTypeEncoding cs
    pure (Pointer val, rest)
  '?' -> pure (Unknown, dropWhile isDigit cs)
  'r' -> do
    (val, rest) <- readTypeEncoding cs
    pure (ConstModifier val, rest)
  'n' -> do
    (val, rest) <- readTypeEncoding cs
    pure (InModifier val, rest)
  'N' -> do
    (val, rest) <- readTypeEncoding cs
    pure (InOutModifier val, rest)
  'o' -> do
    (val, rest) <- readTypeEncoding cs
    pure (OutModifier val, rest)
  'O' -> do
    (val, rest) <- readTypeEncoding cs
    pure (ByCopyModifier val, rest)
  'R' -> do
    (val, rest) <- readTypeEncoding cs
    pure (ByRefModifier val, rest)
  'V' -> do
    (val, rest) <- readTypeEncoding cs
    pure (OneWayModifier val, rest)
  _ -> Left (c : cs)

readEncodingArray :: String -> [TypeEncoding]
readEncodingArray [] = []
readEncodingArray str = case readTypeEncoding str of
  Left str -> []
  Right (enc, rest) -> enc : readEncodingArray rest
