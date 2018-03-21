module AppComponent.Input.PlainText(
    component
  , Query(..)
  , Message
)where 



import Data.String
import Prelude

import Component (component)
import DOM.Event.Event as DE
import DOM.HTML.Indexed.ButtonType (renderButtonType)
import Data.Maybe (Maybe(..))
import Halogen (ClassName(..))
import Halogen as H
import Halogen.Aff.Driver.Eval (eval)
import Halogen.HTML (a)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (class_, classes)
import Halogen.Query (Action)
import Halogen.Query.InputF (InputF(..))
import Halogen.VDom (VDom(..))

data Query a = Reset a
             | SendString String a 
type State = Unit 

type Message = String

component :: forall m. H.Component HH.HTML Query Unit Message m
component = 
    H.component
        { initialState: const unit 
        , render
        , eval
        , receiver: const ( Just ( Reset unit) )
        }
    where 

    render :: State -> H.ComponentHTML Query
    render state = 
        HH.div_  [
            HH.textarea [
                HE.onValueInput (HE.input sendQuery)
              , classes [ClassName "siimple-textarea", ClassName "siimple-textarea-fluid"] 
--   <textarea class="siimple-textarea siimple-textarea--fluid">
            ] 

        ]
    
    
    sendQuery  ::  forall a. String -> (a -> Query a)
    sendQuery st = SendString st 
            

    eval :: Query ~> H.ComponentDSL State Query Message m
    eval = case _ of 
        Reset next -> 
            pure next 
        SendString st next -> do
            H.raise st 
            pure next
        

