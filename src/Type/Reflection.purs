module Type.Reflection where


import Data.Generic.Rep (class Generic, Argument, Constructor, NoArguments, NoConstructors, Product, Sum)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)

import Unsafe.Coerce



-- Type representations --------------------------------------------------------


data TypeRep r
  = Argument (forall p. (forall a. TypeRep a -> p) -> p)
  | NoArguments
  | Constructor (forall p. (forall a. String -> TypeRep a -> p) -> p)
  | NoConstructors
  | Product (forall p. (forall a b. TypeRep a -> TypeRep b -> p) -> p)
  | Sum (forall p. (forall a b. TypeRep a -> TypeRep b -> p) -> p)


class GenericTypeable r where
  typeRep' :: TypeRep r


instance genericTypeableArgument ::
  GenericTypeable a => GenericTypeable (Argument a)
  where
    typeRep' = Argument \pack -> pack (typeRep' :: TypeRep a)


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


-- class Typeable a where
--   typeRep :: a -> TypeRep a

typeRep :: forall a r.
  Generic a r => GenericTypeable r =>
  a -> TypeRep a
typeRep _ = unsafeCoerce (typeRep' :: TypeRep r)

-- instance typeableAll :: (Generic a r, GenericTypeable r) => Typeable a where
--   typeRep _ = ?h
