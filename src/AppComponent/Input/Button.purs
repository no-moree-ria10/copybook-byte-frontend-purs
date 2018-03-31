module AppComponent.Input.Button where 


import Data.String
import Prelude

import Component (component)
import DOM.Event.Event as DE
import DOM.Event.Types(MouseEvent)
import DOM.HTML.Indexed.ButtonType (renderButtonType)
import Data.Maybe (Maybe(..))
import Halogen (ClassName(..), Component)
import Halogen as H
import Halogen.Aff.Driver.Eval (eval)
import Halogen.HTML (a, div_)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (class_, classes)
import Halogen.Query (Action)
import Halogen.Query.InputF (InputF(..))
import Halogen.VDom (VDom(..))


-- | buttonに対する薄いラッパ
-- | styleを付けるため

data Query a = Clicked a

type State = Unit

type Message = Unit 

component :: forall m. H.Component HH.HTML Query Unit Message m
component = 
    H.component
        { initialState: const unit 
        , render
        , eval
        , receiver: const Nothing
        }
    where 

    render :: State -> H.ComponentHTML Query
    render state = 
        HH.div_ [
            HH.button [
                  HE.onClick (HE.input $ \e -> Clicked )
                , classes [ ClassName "siimple-btn" 
                          , ClassName "siimple-btn--blue" ]
            ] [] 

        ]

    eval :: Query ~> H.ComponentDSL State Query Message m
    eval = case _ of 
        Clicked next -> do
            H.raise unit 
            pure next

                