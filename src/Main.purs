module Main where

import Prelude

import AppComponent.PlainInOut (component)
import Control.Monad.Eff (Eff)
import DOM.Node.ParentNode (QuerySelector(..))
import Data.Foldable (traverse_)

import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import HeadComponent.SiimpleCSS (styleComponent)

main :: Eff (HA.HalogenEffects ()) Unit
main = HA.runHalogenAff do
--  body <- HA.awaitBody
  HA.awaitLoad
  
  traverse_ (runUI styleComponent unit) =<< HA.selectElement ( QuerySelector "head")
  traverse_ (runUI component unit) =<< HA.selectElement ( QuerySelector "body")



