module AppComponent.AjaxInOutWithButton where 

import Data.String
import Prelude

import AppComponent.Input.Button as B
import AppComponent.Input.PlainText as ITxt
import AppComponent.Output.ParsedText as PTxt

import AppComponent.Output.PlainText as OTxt
import Component (component)
import Control.Monad.Aff (Aff)
import DOM.Event.Event as DE
import DOM.HTML.Indexed.ButtonType (renderButtonType)
import Data.Either (Either(..))
import Data.Either.Nested (Either4)
import Data.Functor.Coproduct.Nested (Coproduct4)
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

-- | stateにparseしたいテキストと
-- | inputから流れてきたtextを持つことで、
-- | buttonが押された時のみparseリクエストを投げる


data Query a = ChangeText ITxt.Message a 
             | Parse a     
type State = {
      nowText ::String
    , parseText :: String
}

type ChildQuery = Coproduct4 ITxt.Query OTxt.Query PTxt.Query B.Query

type ChildSlot = Either4 Unit Unit Unit Unit

component :: forall eff. 
             H.Component HH.HTML Query Unit Void (Aff (ajax :: AX.AJAX | eff))
component = 
    H.parentComponent 
        {
          initialState: const { nowText: "" , parseText: ""}
        , render 
        , eval 
        , receiver : const Nothing
        }
    where 

    render :: 
        State -> H.ParentHTML Query ChildQuery ChildSlot (Aff (ajax :: AX.AJAX | eff))
    render state = HH.div_
        [ HH.slot'  CP.cp1  unit ITxt.component unit (HE.input ChangeText) 
        , HH.slot'  CP.cp2 unit OTxt.component state.nowText   absurd 
        , HH.slot'  CP.cp3 unit PTxt.component state.parseText absurd 
        , HH.slot'  CP.cp4 unit B.component unit (HE.input $ const Parse)
        ]

    eval :: 
        Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void (Aff (ajax :: AX.AJAX | eff))
    eval = case _ of 
        ChangeText st next -> do
          state <- H.get
          when (state.nowText /= st) $ H.put $ state { nowText = st} 
          pure next
        Parse next -> do
          stt <- H.get
          let now = stt.nowText
              parse = stt.parseText
          when (now /= parse) $ H.put $ stt { parseText = now }
          pure next


