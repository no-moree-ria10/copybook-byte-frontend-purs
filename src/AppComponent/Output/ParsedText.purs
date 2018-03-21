module AppComponent.Output.ParsedText (
      component
    , Query(..)
)where 

import Data.String
import Prelude

import AppComponent.Output.PlainText as OTxt
import Component (component)
import Control.Monad.Aff (Aff)
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
import Network.HTTP.Affjax as AX

data Query a = ParseText String a
            
type State = {
    parsed :: String 
  , loading :: Boolean
}

type ChildQuery = OTxt.Query

type ChildSlot = Unit

component :: forall eff .
             H.Component HH.HTML Query String Void (Aff (ajax :: AX.AJAX | eff))
component = 
    H.parentComponent 
        {
          initialState: const {parsed: "", loading: false}
        , render 
        , eval 
        , receiver 
        }
    where 

    render :: State -> H.ParentHTML Query ChildQuery ChildSlot (Aff (ajax :: AX.AJAX | eff))
    render state = if state.loading then 
            HH.div_ [HH.text "loading.."]    
        else 
            HH.pre_ [ HH.slot unit OTxt.component state.parsed absurd ] 

    eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void 
                    (Aff (ajax :: AX.AJAX | eff))
    eval = case _ of 
        ParseText st next -> do
            H.modify ( _{loading = true})
            response <- H.liftAff $ AX.get ("http://app:8081/copybook-byte?contents=" <> st)
            H.modify (_ { loading = false, parsed = response.response })
            pure $ next

    receiver ::  String -> Maybe( Query Unit )
    receiver string = Just $ ParseText string unit


