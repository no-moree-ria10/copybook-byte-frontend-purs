module AppComponent.AjaxInOut where 

import Data.String
import Prelude

import AppComponent.Input.PlainText as ITxt
import AppComponent.Output.ParsedText as PTxt
import AppComponent.Output.PlainText as OTxt
import Component (component)
import Control.Monad.Aff (Aff)
import DOM.Event.Event as DE
import DOM.HTML.Indexed.ButtonType (renderButtonType)
import Data.Either (Either(..))
import Data.Either.Nested (Either3)
import Data.Functor.Coproduct.Nested (Coproduct3)
import Data.Maybe (Maybe(..))
import Halogen (HalogenF(..))
import Halogen as H
import Halogen.Aff.Driver.Eval (eval)
import Halogen.Component.ChildPath as CP
import Halogen.HTML (a)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Query (Action)
import Halogen.Query.InputF (InputF(..))
import Halogen.VDom (VDom(..))
import Network.HTTP.Affjax as AX

data Query a = WriteString ITxt.Message a 
type State = String 

type ChildQuery = Coproduct3 ITxt.Query OTxt.Query PTxt.Query

type ChildSlot = Either3 Unit Unit Unit

component :: forall eff. 
             H.Component HH.HTML Query Unit Void (Aff (ajax :: AX.AJAX | eff))
component = 
    H.parentComponent 
        {
          initialState: const "" 
        , render 
        , eval 
        , receiver : const Nothing
        }
    where 

    render :: 
        State -> H.ParentHTML Query ChildQuery ChildSlot (Aff (ajax :: AX.AJAX | eff))
    render state = HH.div_
        [ HH.slot'  CP.cp1  unit ITxt.component unit (HE.input WriteString) 
        , HH.slot'  CP.cp2 unit OTxt.component state absurd 
        , HH.slot'  CP.cp3 unit PTxt.component state absurd        
        ]

    eval :: 
        Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void (Aff (ajax :: AX.AJAX | eff))
    eval = case _ of 
        WriteString st next -> do
          H.put st
          pure next 


