module Type.Reflection where


import Prelude

import Prim.TypeError (class Warn, Beside, Quote, Text)

import Data.Generic.Rep (class Generic, Argument, Constructor, NoArguments, NoConstructors, Product, Sum)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)

import Unsafe.Coerce (unsafeCoerce)


quote :: String -> String
quote s = "\"" <> s <> "\""

group :: String -> String
group s = "(" <> s <> ")"


-- Type representations --------------------------------------------------------


data TypeRep r
  = Argument (forall p. (forall a. TypeRep a -> p) -> p)
  | NoArguments
  | Constructor (forall p. (forall a. String -> TypeRep a -> p) -> p)
  | NoConstructors
  | Product (forall p. (forall a b. TypeRep a -> TypeRep b -> p) -> p)
  | Sum (forall p. (forall a b. TypeRep a -> TypeRep b -> p) -> p)
  | Recurse
  | Boolean
  | Int
  | Number
  | Char
  | String
  | Array (forall p. (forall a. TypeRep a -> p) -> p)
  | Function (forall p. (forall a b. TypeRep a -> TypeRep b -> p) -> p)


instance showTypeRep :: Show (TypeRep r) where
  show (Argument unpack) = unpack \inner ->
    "(Argument " <> show inner <> ")"
    -- group $ show inner
  show NoArguments =
    "NoArguments"
    -- "_"
  show (Constructor unpack) = unpack \name prod ->
    "(Constructor \"" <> name <> "\" " <> show prod <> ")"
    -- name <> " " <> show prod
  show NoConstructors =
    "NoConstructors"
    -- "_"
  show (Product unpack) = unpack \left right ->
    "(Product " <> show left <> " " <> show right <> ")"
    -- show left <> " " <> show right
  show (Sum unpack) = unpack \left right ->
    "(Sum " <> show left <> " " <> show right <> ")"
    -- show left <> " | " <> show right
  show Recurse =
    "Recurse"
    -- "recurse"
  show Boolean = "Boolean"
  show Int = "Int"
  show Number = "Number"
  show Char = "Char"
  show String = "String"
  show (Array unpack) = unpack \inner ->
    "(Array " <> show inner <> ")"
    -- "Array " <> group (show inner)
  show (Function unpack) = unpack \from to ->
    "(" <> show from <> " -> " <> show to <> ")"
    -- show from <> " -> " <> show to


-- Typeable --------------------------------------------------------------------


class GenericTypeable r where
  genTypeRep :: TypeRep r

class BasicTypeable b where
  basicTypeRep :: TypeRep b

class Typeable a where
  typeRep :: TypeRep a


instance
  genericTypeableArgument :: BasicTypeable a =>
    GenericTypeable (Argument a)
  where
    genTypeRep = Argument \pack -> pack (basicTypeRep :: TypeRep a)
else instance
  genericTypeableNoArguments ::
    GenericTypeable NoArguments
  where
    genTypeRep = NoArguments
else instance
  genericTypeableConstructor :: (IsSymbol name, GenericTypeable a) =>
    GenericTypeable (Constructor name a)
  where
    genTypeRep = Constructor \pack ->
      pack (reflectSymbol (SProxy :: SProxy name)) (genTypeRep :: TypeRep a)
else instance
  genericTypeableNoConstructors ::
    GenericTypeable NoConstructors
  where
    genTypeRep = NoConstructors
else instance
  genericTypeableSum :: (GenericTypeable a, GenericTypeable b) =>
    GenericTypeable (Sum a b)
  where
    genTypeRep = Sum \pack ->
      pack (genTypeRep :: TypeRep a) (genTypeRep :: TypeRep b)
else instance
  genericTypeableProduct :: (GenericTypeable a, GenericTypeable b) =>
    GenericTypeable (Product a b)
  where
    genTypeRep = Product \pack ->
      pack (genTypeRep :: TypeRep a) (genTypeRep :: TypeRep b)

instance
  basicTypeableBoolean :: BasicTypeable Boolean
  where
    basicTypeRep = Boolean
else instance
  basicTypeableInt :: BasicTypeable Int
  where
    basicTypeRep = Int
else instance
  basicTypeableNumber :: BasicTypeable Number
  where
    basicTypeRep = Number
else instance
  basicTypeableChar :: BasicTypeable Char
  where
    basicTypeRep = Char
else instance
  basicTypeableString :: BasicTypeable String
  where
    basicTypeRep = String
else instance
  basicTypeableArray :: Typeable a => BasicTypeable (Array a)
  where
    basicTypeRep = Array \pack -> pack (typeRep :: TypeRep a)
else instance
  basicTypeableFunction :: (Typeable a, Typeable b) => BasicTypeable (Function a b)
  where
    basicTypeRep = Function \pack -> pack (typeRep :: TypeRep a) (typeRep :: TypeRep b)
else instance
  basicTypeableFail :: Warn (Beside (Text "Non basic types like `") (Beside (Quote a) (Text "` are represented uniformly!"))) => BasicTypeable a
  where
    basicTypeRep = Recurse

instance
  typeableBoolean :: Typeable Boolean
  where
    typeRep = Boolean
else instance
  typeableInt :: Typeable Int
  where
    typeRep = Int
else instance
  typeableNumber :: Typeable Number
  where
    typeRep = Number
else instance
  typeableChar :: Typeable Char
  where
    typeRep = Char
else instance
  typeableString :: Typeable String
  where
    typeRep = String
else instance
  typeableArray :: Typeable a => Typeable (Array a)
  where
    typeRep = Array \pack -> pack (typeRep :: TypeRep a)
else instance
  typeableFunction :: (Typeable a, Typeable b) => Typeable (Function a b)
  where
    typeRep = Function \pack -> pack (typeRep :: TypeRep a) (typeRep :: TypeRep b)
else instance
  typeableGeneric :: (Generic a r, GenericTypeable r) => Typeable a
  where
    typeRep = unsafeCoerce (genTypeRep :: TypeRep r)

-- else instance typeableFail ::
--   Fail (Beside (Text "Can not generate a type representation for ") (Quote a)) => Typeable a where
--     typeRep = unsafeCoerce unit


typeOf :: forall a. Typeable a => a -> TypeRep a
typeOf _ = typeRep
