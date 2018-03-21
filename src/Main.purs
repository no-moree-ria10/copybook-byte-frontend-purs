module Main where

import Prelude

import AppComponent.AjaxInOut (component)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff)
import DOM.Node.ParentNode (QuerySelector(..))
import Data.Foldable (traverse_)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import HeadComponent.SiimpleCSS (styleComponent)

import Network.HTTP.Affjax

type AppEffects eff = 
  HA.HalogenEffects (ajax :: AJAX | eff)

main :: forall eff. Eff (AppEffects () ) Unit
main = HA.runHalogenAff do
--  body <- HA.awaitBody
  HA.awaitLoad
  traverse_ (runUI styleComponent unit) =<< HA.selectElement ( QuerySelector "head")
  traverse_ (runUI component unit)  =<< HA.selectElement ( QuerySelector "body")

