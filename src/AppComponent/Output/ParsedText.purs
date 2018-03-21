module AppComponent.Output.ParsedText (
      component
    , Query(..)
)where 

import Data.String
import Prelude
import Control.Monad.Aff (Aff) 
import Network.HTTP.Affjax as AX

import AppComponent.Output.PlainText as OTxt
import Component (component)
import DOM.Event.Event as DE
import DOM.HTML.Indexed.ButtonType (renderButtonType)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.Aff.Driver.Eval (eval)
import Halogen.HTML (a)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Query (Action)
import Halogen.Query.InputF (InputF(..))
import Halogen.Query.InputF as Otxt
import Halogen.VDom (VDom(..))

data Query a = ParseText  String a

type State = {
    parsed :: String 
  , loading :: Boolean
}

type ChildQuery = OTxt.Query

type ChildSlot = Unit

component :: forall eff m. Applicative m =>
             H.Component HH.HTML Query State Void (Aff (ajax :: AX.AJAX | eff))
component = 
    H.parentComponent 
        {
          initialState: const  
        , render 
        , eval 
        , receiver : const Nothing
        }
    where 

    render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
    render state = HH.div_
        [ HH.div_ [ HH.slot'  CP.cp1  unit ITxt.component unit (HE.input WriteString) ]
        , HH.div_ [HH.slot'  CP.cp2 unit OTxt.component state absurd ]
        ]

    eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void m
    eval = case _ of 
        WriteString st next -> do
          H.put st
          pure next