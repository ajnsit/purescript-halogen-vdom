module Halogen.VDom.HostConfig
  ( createTextNode
  , setTextContent
  , createElement
  , insertChildIx
  , removeChild
  , parentNode
  , setAttribute
  , removeAttribute
  , hasAttribute
  , addEventListener
  , removeEventListener
  , class HostConfig
  ) where

import Prelude

import Data.Nullable (Nullable)
import Effect.Uncurried as EFn
import Halogen.VDom.Types (ElemName, Namespace)

class HostConfig node where
  createTextNode ∷ EFn.EffectFn2 String node node
  setTextContent ∷ EFn.EffectFn2 String node Unit
  createElement ∷ EFn.EffectFn3 (Nullable Namespace) ElemName node node
  insertChildIx ∷ EFn.EffectFn3 Int node node Unit
  removeChild ∷ EFn.EffectFn2 node node Unit
  parentNode ∷ EFn.EffectFn1 node node
  setAttribute ∷ EFn.EffectFn4 (Nullable Namespace) String String node Unit
  removeAttribute ∷ EFn.EffectFn3 (Nullable Namespace) String node Unit
  hasAttribute ∷ EFn.EffectFn3 (Nullable Namespace) String node Boolean
  addEventListener ∷ EFn.EffectFn3 String (forall a. EFn.EffectFn1 a Unit) node Unit
  removeEventListener ∷ EFn.EffectFn3 String (forall a. EFn.EffectFn1 a Unit) node Unit
