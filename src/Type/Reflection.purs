module Type.Reflection
  ( TypeRep, same
  , class Typeable, typeRep, typeOf
  , cast
  ) where


import Prelude

import Data.Generic.Rep (class Generic, Argument, Constructor, NoArguments, NoConstructors, Product, Sum)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)

import Unsafe.Coerce (unsafeCoerce)



-- Type representations --------------------------------------------------------


data TypeRep r
  = Argument (forall p. (forall a. TypeRep a -> p) -> p)
  | NoArguments
  | Constructor (forall p. (forall a. String -> TypeRep a -> p) -> p)
  | NoConstructors
  | Product (forall p. (forall a b. TypeRep a -> TypeRep b -> p) -> p)
  | Sum (forall p. (forall a b. TypeRep a -> TypeRep b -> p) -> p)
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


instance eqTypeRep :: Eq (TypeRep a) where
  eq (Argument unl) (Argument unr) = unl \l -> unr \r -> same l r
  eq (NoArguments) (NoArguments) = true
  eq (Constructor unl) (Constructor unr) = unl \n l -> unr \m r -> n == m && same l r
  eq (NoConstructors) (NoConstructors) = true
  eq (Product unl) (Product unr) = unl \la lb -> unr \ra rb -> same la lb && same ra rb
  eq (Sum unl) (Sum unr) = unl \la lb -> unr \ra rb -> same la lb && same ra rb
  eq (Boolean) (Boolean) = true
  eq (Int) (Int) = true
  eq (Number) (Number) = true
  eq (Char) (Char) = true
  eq (String) (String) = true
  eq (Array unl) (Array unr) = unl \l -> unr \r -> same l r
  eq (Function unl) (Function unr) = unl \la lb -> unr \ra rb -> same la ra && same ra rb
  eq _ _ = false


same :: forall a b. TypeRep a -> TypeRep b -> Boolean
same a b = a == unsafeCoerce b



-- Typeable --------------------------------------------------------------------


class Typeable a where
  typeRep :: TypeRep a


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
  typeableArgument :: Typeable a =>
    Typeable (Argument a)
  where
    typeRep = Argument \pack -> pack (typeRep :: TypeRep a)
else instance
  typeableNoArguments ::
    Typeable NoArguments
  where
    typeRep = NoArguments
else instance
  typeableConstructor :: (IsSymbol name, Typeable a) =>
    Typeable (Constructor name a)
  where
    typeRep = Constructor \pack ->
      pack (reflectSymbol (SProxy :: SProxy name)) (typeRep :: TypeRep a)
else instance
  typeableNoConstructors ::
    Typeable NoConstructors
  where
    typeRep = NoConstructors
else instance
  typeableSum :: (Typeable a, Typeable b) =>
    Typeable (Sum a b)
  where
    typeRep = Sum \pack ->
      pack (typeRep :: TypeRep a) (typeRep :: TypeRep b)
else instance
  typeableProduct :: (Typeable a, Typeable b) =>
    Typeable (Product a b)
  where
    typeRep = Product \pack ->
      pack (typeRep :: TypeRep a) (typeRep :: TypeRep b)

else instance
  typeableGeneric :: (Generic a r, Typeable r) => Typeable a
  where
    typeRep = unsafeCoerce (typeRep :: TypeRep r)


typeOf :: forall a. Typeable a => a -> TypeRep a
typeOf _ = typeRep



-- Casts -----------------------------------------------------------------------


cast :: forall a b. Typeable a => Typeable b => a -> Maybe b
cast x
  | same (typeRep :: TypeRep a) (typeRep :: TypeRep b) = Just $ unsafeCoerce x
  | otherwise = Nothing



-- Helpers ---------------------------------------------------------------------


quote :: String -> String
quote s = "\"" <> s <> "\""

group :: String -> String
group s = "(" <> s <> ")"
