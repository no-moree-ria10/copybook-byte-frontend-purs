module AppComponent.PlainInOut where 

import Data.String
import Prelude

import AppComponent.Input.PlainText as ITxt
import AppComponent.Output.PlainText as OTxt
import Component (component)
import DOM.Event.Event as DE
import DOM.HTML.Indexed.ButtonType (renderButtonType)
import Data.Either (Either(..))
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
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

data Query a = WriteString ITxt.Message a 
type State = String 

type ChildQuery = Coproduct2 ITxt.Query OTxt.Query

type ChildSlot = Either2 Unit Unit 

component :: forall m. Applicative m =>
             H.Component HH.HTML Query Unit Void m
component = 
    H.parentComponent 
        {
          initialState: const "" 
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
