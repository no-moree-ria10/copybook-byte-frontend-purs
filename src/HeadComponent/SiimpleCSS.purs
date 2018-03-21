module HeadComponent.SiimpleCSS(
      styleComponent
    , Query(..)
)where 


import Data.String
import Prelude

import Component (component)
import DOM.Event.Event as DE
import DOM.HTML.HTMLObjectElement (type_)
import DOM.HTML.Indexed.ButtonType (renderButtonType)
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType(..))
import Data.Newtype (unwrap)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.Aff.Driver.Eval (eval)
import Halogen.HTML (a)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (class_, classes)
import Halogen.HTML.Properties as HP
import Halogen.Query (Action)
import Halogen.Query.InputF (InputF(..))
import Halogen.VDom (VDom(..))

data Query a = VoidQ a

type State = Unit 

type Message = Void

styleComponent :: forall m. H.Component HH.HTML Query State Message m
styleComponent = 
    H.component
        { initialState: const unit 
        , render : const $ HH.link [ HP.type_ $ MediaType "text/css"
                                   , HP.rel "stylesheet"
                                   , HP.href "siimple.min.css"
                                   ]
        , eval 
        , receiver: const Nothing
        }

    where 


--    render :: State -> H.ComponentHTML Query
--    render state = 
     
    eval :: Query ~> H.ComponentDSL State Query Message m
    eval = case _ of 
        VoidQ next -> pure next
        



