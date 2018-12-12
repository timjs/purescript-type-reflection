module Type.Reflection
  ( Fingerprint
  , class Typeable, fingerprint, typeOf
  , Same(Refl), same, cast
  , module Type.Equality
  , module Type.Proxy
  ) where


import Prelude

import Data.Generic.Rep (class Generic, Argument, Constructor, NoArguments, NoConstructors, Product, Sum)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)

import Type.Equality (class TypeEquals, from, to)
import Type.Proxy (Proxy(..))

import Unsafe.Coerce (unsafeCoerce)



-- Type representations --------------------------------------------------------


-- | Non-type indexed representation of types.
data Fingerprint
  = Argument Fingerprint
  | NoArguments
  | Constructor String Fingerprint
  | NoConstructors
  | Product Fingerprint Fingerprint
  | Sum Fingerprint Fingerprint
  | Boolean
  | Int
  | Number
  | Char
  | String
  | Array Fingerprint
  | Function Fingerprint Fingerprint


instance showFingerprint :: Show Fingerprint where
  show (Argument inner)        = "(Argument " <> show inner <> ")"
  show NoArguments             = "NoArguments"
  show (Constructor name prod) = "(Constructor \"" <> name <> "\" " <> show prod <> ")"
  show NoConstructors          = "NoConstructors"
  show (Product a b)           = "(Product " <> show a <> " " <> show b <> ")"
  show (Sum a b)               = "(Sum " <> show a <> " " <> show b <> ")"
  show Boolean                 = "Boolean"
  show Int                     = "Int"
  show Number                  = "Number"
  show Char                    = "Char"
  show String                  = "String"
  show (Array inner)           = "(Array " <> show inner <> ")"
  show (Function a b)          = "(" <> show a <> " -> " <> show b <> ")"


instance eqFingerprint :: Eq Fingerprint where
  eq (Argument l)      (Argument r)      = l  == r
  eq (NoArguments)     (NoArguments)     = true
  eq (Constructor n l) (Constructor m r) = n  == m && l   == r
  eq (NoConstructors)  (NoConstructors)  = true
  eq (Product la lb)   (Product ra rb)   = la == lb && ra == rb
  eq (Sum la lb)       (Sum ra rb)       = la == lb && ra == rb
  eq (Boolean)         (Boolean)         = true
  eq (Int)             (Int)             = true
  eq (Number)          (Number)          = true
  eq (Char)            (Char)            = true
  eq (String)          (String)          = true
  eq (Array l)         (Array r)         = l  == r
  eq (Function la lb)  (Function ra rb)  = la == ra && ra == rb
  eq _ _                                 = false



-- Typeable --------------------------------------------------------------------


class Typeable a where
  fingerprint :: Proxy a -> Fingerprint


instance
  typeableBoolean :: Typeable Boolean
  where
    fingerprint _ = Boolean
else instance
  typeableInt :: Typeable Int
  where
    fingerprint _ = Int
else instance
  typeableNumber :: Typeable Number
  where
    fingerprint _ = Number
else instance
  typeableChar :: Typeable Char
  where
    fingerprint _ = Char
else instance
  typeableString :: Typeable String
  where
    fingerprint _ = String
else instance
  typeableArray :: Typeable a => Typeable (Array a)
  where
    fingerprint _ = Array (fingerprint (Proxy :: Proxy a))
else instance
  typeableFunction :: (Typeable a, Typeable b) => Typeable (Function a b)
  where
    fingerprint _ = Function (fingerprint (Proxy :: Proxy a)) (fingerprint (Proxy :: Proxy b))

else instance
  typeableArgument :: Typeable a =>
    Typeable (Argument a)
  where
    fingerprint _ = Argument (fingerprint (Proxy :: Proxy a))
else instance
  typeableNoArguments ::
    Typeable NoArguments
  where
    fingerprint _ = NoArguments
else instance
  typeableConstructor :: (IsSymbol name, Typeable a) =>
    Typeable (Constructor name a)
  where
    fingerprint _ = Constructor (reflectSymbol (SProxy :: SProxy name)) (fingerprint (Proxy :: Proxy a))
else instance
  typeableNoConstructors ::
    Typeable NoConstructors
  where
    fingerprint _ = NoConstructors
else instance
  typeableSum :: (Typeable a, Typeable b) =>
    Typeable (Sum a b)
  where
    fingerprint _ = Sum (fingerprint (Proxy :: Proxy a)) (fingerprint (Proxy :: Proxy b))
else instance
  typeableProduct :: (Typeable a, Typeable b) =>
    Typeable (Product a b)
  where
    fingerprint _ = Product (fingerprint (Proxy :: Proxy a)) (fingerprint (Proxy :: Proxy b))

else instance
  typeableGeneric :: (Generic a r, Typeable r) => Typeable a
  where
    fingerprint _ = unsafeCoerce (fingerprint (Proxy :: Proxy r))


typeOf :: forall a. Typeable a => a -> Fingerprint
typeOf _ = fingerprint (Proxy :: Proxy a)



-- Casts -----------------------------------------------------------------------


-- | `Refl` encodes a proof that type `a` equals type `b`,
-- | making use of the `TypeEquals a b` class.
-- | A type safe cast, for example, could be written like:
-- |
-- |     cast' :: forall a b. Typeable a => Typeable b => a -> Maybe b
-- |     cast' x
-- |       | Just (Refl proof) <- same (Proxy :: Proxy a) (Proxy :: Proxy b) =
-- |           proof \_ -> Just $ to x
-- |       | otherwise = Nothing
-- |
data Same a b
  = Refl (forall p. (TypeEquals a b => Unit -> p) -> p)


-- | Calculates if two proxies `a` and `b` represent the same type.
-- | If they do, we give a proof in form of `Just Refl`,
-- | otherwise we return `Nothing`.
same :: forall a b. Typeable a => Typeable b => Proxy a -> Proxy b -> Maybe (Same a b)
same a b
  | fingerprint a == fingerprint b = Just $ unsafeCoerce Refl
  | otherwise = Nothing


-- | Similar to `same`, but taking concrete values instead of proxies.
sameOf :: forall a b. Typeable a => Typeable b => a -> b -> Maybe (Same a b)
sameOf _ _ = same (Proxy :: Proxy a) (Proxy :: Proxy b)


-- | Type safe cast from any typeable `a` to typeble `b`.
cast :: forall a b. Typeable a => Typeable b => a -> Maybe b
cast x
  | typeOf x == fingerprint (Proxy :: Proxy b) = Just $ unsafeCoerce x
  | otherwise = Nothing
