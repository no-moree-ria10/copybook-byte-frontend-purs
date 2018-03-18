module AppComponent.Output.PlainText (
      component
    , Query(..)
)where 

import Data.String
import Prelude

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
import Halogen.VDom (VDom(..))

data Query a = ChangeText  String a

type State = String 

component :: forall m. H.Component HH.HTML Query String Void m
component = 
    H.component 
        { initialState: id
        , render 
        , eval
        , receiver         
        }

    where 
    render :: State -> H.ComponentHTML Query 
    render stateString = 
        HH.pre_ [
            HH.text stateString 
        ]

    eval :: Query ~> H.ComponentDSL State Query Void m
    eval = case _ of 
        ChangeText st next -> do
            oldSt <- H.get
            when (oldSt /= st) $ H.put st
            pure next 

    receiver ::  String -> Maybe( Query Unit )
    receiver string = Just $ ChangeText string unit


