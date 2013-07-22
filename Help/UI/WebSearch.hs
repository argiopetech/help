{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax, LambdaCase #-}
module Help.UI.WebSearch where

import Help.Imports hiding (find)
import qualified Prelude as P
import Help.Settings

import Yesod hiding (count)
import Yesod.Form.Jquery
import Network.Wai.Handler.Warp (run)

import Data.Text (init, tail)
import Data.Text.Read (decimal)
import Control.Lens.Getter ((^.))
import Database.MongoDB as M

-- |Start a web application to query the database and return log entries
webSearch ∷ Settings → IO ()
webSearch s = run 3000 =<< (toWaiApp $ Minimal s)

data Minimal = Minimal { settings ∷ Settings }

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage Minimal FormMessage where
    renderMessage _ _ = defaultFormMessage

-- mkYesodData "Minimal"  [parseRoutes|
--     / QueryR GET
-- |]

-- mkYesodDispatch "Minimal" resourcesApp
instance YesodJquery Minimal

mkYesod "Minimal" [parseRoutes|
    / QueryR GET
|]

instance Yesod Minimal where
    defaultLayout widget = do
        master <- getYesod
        pc <- widgetToPageContent $ do
                  addScriptEither $ urlJqueryJs master
                  addStylesheetRemote "http://netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/css/bootstrap-combined.min.css"
                  addScriptRemote "http://netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/js/bootstrap.min.js"
                  widget
        giveUrlRenderer [hamlet|
            $doctype 5
            <html lang="en">
                <head>
                    <meta charset="utf-8">
                    $# 良からぬ狼
                    <title>#{pageTitle pc}
                    <meta name="description" content="HELP -- Haskell Enterprise Logging Platform">
                    <meta name="author" content="Elliot Robinson">
                    <meta name="viewport" content="width=device-width, initial-scale=1.0">
                    ^{pageHead pc}
                <body>
                  <div .container style="width=80%">
                    ^{pageBody pc}
            |]

--getBootstrapR ∷ Handler Html
--getBootstrapR = defaultLayout $ do
--    toWidget $(luciusFile "Help/UI/WebSearch/bootstrap.lucius")
--    toWidget $(juliusFile "Help/UI/WebSearch/bootstrap.julius")

data QueryType = WithSearch { _t ∷ Text, skipRecs ∷ Word32 }
               | WithoutSearch { skipRecs ∷ Word32 }

getQueryR ∷ Handler Html
getQueryR = do
    v1 ← lookupGetParam "query"
    v2 ← (maybe (Right (0 ∷ Word32,"")) decimal) <$> (lookupGetParam "skip")
    let input = case (v1, v2) of
                  (Just q, s) → WithSearch q $ either (const 0) (fromIntegral . fst) s
                  (_, s)      → WithoutSearch $ either (const 0) (fromIntegral . fst) s

    s ← settings <$> getYesod
    pipe ← liftIO $ runIOE $ connect (host $ s^.mongoHost)
    eResults ← liftIO $ access pipe slaveOk (s^.database) $ do
                curs ← case input of
                         (WithSearch query skipRec) → find $ (select ["$or" :=
                                                    (M.Array [ Doc ["message" =: Regex query ""]
                                                             , Doc ["status"  =: Regex query ""]])] "default") {M.sort = ["$natural" := Int32 (-1)], limit = 10, skip = skipRec}
                         (WithoutSearch skipRec)    → find $ (select [] "default") {skip = skipRec, limit = 10, M.sort = ["$natural" := Int32 (-1)]}
                results <- rest curs
                closeCursor curs
                return $ map (exclude ["_id"]) results

    liftIO $ close pipe
    results ← case eResults of
                (Right r) → return r
                (Left  f) → invalidArgs [show f]

    defaultLayout $ do
                setTitle "Query"
                [whamlet|
<header .clearfix>
  <h3 .pull-right .muted>
    Haskell Enterprise Logging Platform
  <h3 .pull-left>HELP
<hr>

<div #form>
  <form method=get action=@{QueryR}#form .form-search>
    <input type=hidden name=skip value=0>
    <input type=search name=query value=#{fromMaybe "" v1} autofocus . input-medium .search-query>
    <button type=submit .btn>
      Search

<div #results>
  <table .table .table-striped>
    <tbody>
      $forall document <- results
        <tr>
          $forall field <- document
            <td>
$#              <span rel="tooltip" data-toggle="tooltip" title=#{retLabel field}>
              <small .muted>
                #{retLabel field}
              <br>
              #{retValue field}
<div #nav>
  $if skipRecs input >= 10
    <div .pull-left>
      <form method=get action=@{QueryR}#form >
        <input type=hidden name=skip value=#{P.show $ skipRecs input - 10}>
        <input type=hidden name=query value=#{fromMaybe "" v1} autofocus>
        <input type=submit .textButton value="Prev 10">
  <div .pull-right>
    <form method=get action=@{QueryR}#form >
      <input type=hidden name=skip value=#{P.show $ skipRecs input + 10}>
      <input type=hidden name=query value=#{fromMaybe "" v1} autofocus>
      <input type=submit .textButton value="Next 10">
|]
                toWidget $ [julius|
$(document).ready(function () {
    $("[rel=tooltip]").tooltip();
});
|]
                toWidget $ [cassius|
.textButton
   border: none;
   background-color: transparent;
   padding: 0;
   text-decoration: underline; /* if desired */
   color: #00c;  /* or whatever other color you want */
|]

retValue ∷ M.Field → Text
retValue = tail . init . show . value

retLabel ∷ M.Field → Text
retLabel = label
