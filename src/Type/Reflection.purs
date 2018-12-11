module Type.Reflection where


import Prelude

import Data.Generic.Rep (class Generic, Argument, Constructor, NoArguments, NoConstructors, Product, Sum)
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
  | Int
  | Number
  | String
  | Array (forall p. (forall a. TypeRep a -> p) -> p)


instance showTypeRep :: Show (TypeRep r) where
  show (Argument unpack) = unpack \inner ->
    "(Argument " <> show inner <> ")"
  show NoArguments = "NoArguments"
  show (Constructor unpack) = unpack \name inner ->
    "(Constructor \"" <> name <> "\" " <> show inner <> ")"
  show NoConstructors = "NoConstructors"
  show (Product unpack) = unpack \left right ->
    "(Product " <> show left <> " " <> show right <> ")"
  show (Sum unpack) = unpack \left right ->
    "(Sum " <> show left <> " " <> show right <> ")"
  show Int = "Int"
  show Number = "Number"
  show String = "String"
  show (Array unpack) = unpack \inner ->
    "(Array " <> show inner <> ")"


class GenericTypeable r where
  typeRep' :: TypeRep r


instance genericTypeableArgument ::
  Typeable a => GenericTypeable (Argument a)
  where
    typeRep' = Argument \pack -> pack (typeRep :: TypeRep a)


instance genericTypeableNoArguments ::
  GenericTypeable NoArguments where
    typeRep' = NoArguments


instance genericTypeableConstructor ::
  (IsSymbol name, GenericTypeable a) => GenericTypeable (Constructor name a)
  where
    typeRep' = Constructor \pack -> pack ctor rep
      where
        ctor = reflectSymbol (SProxy :: SProxy name)
        rep = typeRep' :: TypeRep a


instance genericTypeableNoConstructors ::
  GenericTypeable NoConstructors
  where
    typeRep' = NoConstructors


instance genericTypeableSum ::
  (GenericTypeable a, GenericTypeable b) => GenericTypeable (Sum a b)
  where
    typeRep' = Sum \pack ->
      pack (typeRep' :: TypeRep a) (typeRep' :: TypeRep b)


instance genericTypeableProduct ::
  (GenericTypeable a, GenericTypeable b) => GenericTypeable (Product a b)
  where
    typeRep' = Product \pack ->
      pack (typeRep' :: TypeRep a) (typeRep' :: TypeRep b)



-- Typeable --------------------------------------------------------------------


-- type SomeTypeRep = Pack (forall r. TypeRep r)


class Typeable a where
  typeRep :: TypeRep a

instance typeableInt ::
  Typeable Int where
    typeRep = Int
else instance typeableNumber ::
  Typeable Number where
    typeRep = Number
else instance typeableString ::
  Typeable String where
    typeRep = String
else instance typeableArray ::
  Typeable a => Typeable (Array a) where
    typeRep = Array \pack -> pack (typeRep :: TypeRep a)
else instance typeableGeneric ::
  (Generic a r, GenericTypeable r) => Typeable a where
    typeRep = unsafeCoerce (typeRep' :: TypeRep r)


typeOf :: forall a. Typeable a => a -> TypeRep a
typeOf _ = typeRep


-- typeRep :: forall a r.
--   Generic a r => GenericTypeable r =>
--   a -> TypeRep a
-- typeRep _ = unsafeCoerce (typeRep' :: TypeRep r)
