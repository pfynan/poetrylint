{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where
-- import Reflex.Dom
import Reflex.Dom.Core (mainWidgetWithCss)
import Reflex.Dom hiding (mainWidgetWithCss,run)

import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.Element as DOM
import qualified GHCJS.DOM.Node as DOM
import qualified GHCJS.DOM.NodeList as DOM
import qualified GHCJS.DOM.Text as DOM
import qualified GHCJS.DOM.Document as DOM
import qualified GHCJS.DOM.Window as DOM.Window
import qualified GHCJS.DOM.Selection as DOM
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Traversable
import Data.Maybe
import Control.Applicative
import Data.Monoid
import Data.Void
import Control.Monad
import Common
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import qualified Language.Javascript.JSaddle as JS
import Language.Javascript.JSaddle.Warp
import Debug.Trace

-- Future: use contenteditable
    -- (mdiv, _) <- elAttr' "div" ("contenteditable" =: "true" <> "style" =: "width: 540px; height: 200px; border: 1px #000000 solid; padding 12px; overflow: scroll;") blank
    -- let rawdiv = Reflex._element_raw mdiv

mainCss :: ByteString
mainCss = ByteString.unlines
    [ "@import url('https://fonts.googleapis.com/css?family=Cormorant|Questrial');"
    , "header {"
    , "    font-size: 40px;"
    , "    font-family: Questrial, sans-serif;"
    , "    text-align: center;"
    , "    padding-bottom: 20px;"
    , "}"
    , ".wrap {"
    , "    display: flex;"
    , "    flex-flow: row wrap;"
    , "    margin-left: auto;"
    , "    margin-right: auto;"
    , "    max-width: 800px;"
    , "    min-height: 200px;"
    , "    font-family: Cormorant, serif;"
    , "}"
    , ".poetryinput {"
    , "    font-family: inherit;"
    , "    font-size: inherit;"
    , "    padding: 10px;"
    , "    border: 1px #c0c0e4 solid;"
    , "    resize: none;"
    , "    min-width: 200px;"
    , "    flex: 1;"
    , "    overflow: hidden;"
    , "}"
    , ".poetrydisplay {"
    , "    border: 1px #e0e0ff solid;"
    , "    height: 100%;"
    , "    padding: 10px;"
    , "}"
    , ".help {"
    , "    padding-left: 20px;"
    , "}"
    , ".incorrect_stress {"
    , "    background-color: #ffaaaa;"
    , "    border-radius: 5px;"
    , "}"
    , ".unknown_word {"
    , "    background-color: #aaffaa;"
    , "    border-radius: 5px;"
    , "}"
    , ".extra_missing {"
    , "    background-color: #aaaaff;"
    , "    border-radius: 5px;"
    , "}"
    ]

main :: IO ()
main = run 3003 $ do
    JS.eval =<< liftIO (readFile "frontend/src/style.js")
    mainWidgetWithCss mainCss $ do
        win <- DOM.currentWindowUnchecked
        el "header" $ do
            text "Poetry Checker"
        elClass "div" "wrap" $ do
            (text_area, _) <- elAttr' "div" ("class" =: "poetryinput" <> "contenteditable" =: "true") $ text "enter text here"
            let raw_input_el = _element_raw text_area
                input_event = domEvent Input text_area
            -- performEvent_ (DOM.setInnerHTML raw_input_el ("foo" :: Text) <$ input_event)
            t_input_event <- debounce 0.5 input_event
            -- performEvent_ (DOM.Window.alert win ("Done" :: DOM.JSString) <$ t_input_event)
            e_new_text <- performEvent (doPoetryCheck (DOM.toNode raw_input_el) <$ t_input_event)
            d_new_text <- holdDyn "PLACE" e_new_text
            elClass "div" "help" $ do
                -- el "header" $ text "Legend"
                -- el "div" $ do
                elClass "span" "incorrect_stress" $ text "Incorrect stress"
                el "br" $ blank
                elClass "span" "unknown_word" $ text "Unknown word"
                el "br" $ blank
                elClass "span" "extra_missing" $ text "Extra/missing word"
                el "br" $ blank
                (text_area2, _) <- el' "pre" $ dynText d_new_text

                return ()



data SourceNode = SourceNode {
    _sourceNodeNode :: DOM.Node,
    _sourceNodePos :: SourcePos
    }

scansionToSourcePos :: ScansionError -> Maybe SourcePos
scansionToSourcePos (IncorrectStress sp _ _) = Just sp
scansionToSourcePos (UnknownWord sp) = Just sp
scansionToSourcePos (ExtraWord sp) = Just sp
scansionToSourcePos (MissingWord) = Nothing

getTextFromInput :: (DOM.IsNode n1) => n1 -> JS.JSM Text
getTextFromInput n1 = do
    val <- JS.toJSVal n1
    st <- JS.jsg1 ("getTextFromInput" :: JS.JSString) val
    t <- JS.fromJSValUnchecked st
    return t

styleContentNodes :: (DOM.IsNode n1) => n1 -> Text -> [ScansionError] -> JS.JSM ()
styleContentNodes n1 t sps = do
    jn1 <- JS.toJSVal n1
    jt <- JS.toJSVal t
    jsses <- JS.toJSVal <$> traverse scansionErrorToJS sps
    JS.jsg3 ("styleContentNodes" :: JS.JSString) jn1 jt jsses
    return ()

scansionErrorToJS sp = do
    j <- JS.obj
    let Just (SourcePos start len) = scansionToSourcePos sp
    (j JS.<# ("start" :: JS.JSString)) start
    (j JS.<# ("length" :: JS.JSString)) len
    (j JS.<# ("style" :: JS.JSString)) (case sp of
                                          IncorrectStress _ _ _ -> "incorrect_stress" :: JS.JSString
                                          UnknownWord _ -> "unknown_word"
                                          ExtraWord _ -> "extra_missing"
                                          MissingWord -> "missing_word")

    return j

doPoetryCheck :: (DOM.MonadDOM m, DOM.IsNode n1) => n1 -> m Text
doPoetryCheck node = do
  traceM "Enter check"
  -- (poetry_text, source_data) <- getTextFromInput (DOM.toNode node)
  poetry_text <- JS.liftJSM $ getTextFromInput (DOM.toNode node)
  traceM "got source"
  let syls_error = checkTextIambicPentameter poetry_text
  -- let text_lines = poetry_text
  --     syls_error = checkTextIambicPentameter poetry_text

  -- nodenames <- traverse ( DOM.getNodeValue . _sourceNodeNode) source_data
  -- nodetypes <- traverse ( DOM.getNodeType . _sourceNodeNode) source_data
  JS.liftJSM $ styleContentNodes (DOM.toNode node) poetry_text (filter (/= MissingWord) syls_error)
  traceM "styled"
  return $ debugRender poetry_text (Data.Maybe.mapMaybe scansionToSourcePos syls_error)
  -- return $ (Text.pack . show $ poetry_text) <> "<br>" <> (Text.pack . show $ syls_error) <> "<br>" <> (Text.pack . show . map _sourceNodePos $ source_data)

slice :: Int -> Int -> Text -> Text
slice a b text = Text.take b (Text.drop a text)

debugRender :: Text -> [SourcePos] -> Text
debugRender t sps = Text.unlines [t, "", go 0 sps]
    where go i ((SourcePos s l):sps) = slice i (s - i) t <> "<" <> slice s l t <> ">" <> go (s + l) sps
          go i [] = Text.drop i t
