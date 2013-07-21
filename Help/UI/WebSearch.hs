{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax, LambdaCase #-}
module Help.UI.WebSearch where

import Help.Imports hiding (find)
import Help.Settings

import Yesod hiding (count)
import Network.Wai.Handler.Warp (run)
--import Help.UI.WebSearch.Internal

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

mkYesod "Minimal" [parseRoutes|
    /query QueryR GET
    /cols ColsR GET
    /colsrels ColsRelsR GET
|]

instance Yesod Minimal where
    defaultLayout widget = do
        pc <- widgetToPageContent widget
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
                    ^{pageBody pc}
            |]

getQueryR ∷ Handler Html
getQueryR = do
    ((result, formWidget), formEnctype) <- runFormGet carForm
    let query = case result of
            FormSuccess res -> Just res
            _ -> Nothing

    s ← settings <$> getYesod
    pipe ← liftIO $ runIOE $ connect (host $ s^.mongoHost)
    eResults ← liftIO $ access pipe slaveOk (s^.database) $ do
                curs ← case query of
                         (Just (SearchQuery q)) → find $ (select ["$or" :=
                                                    (M.Array [ Doc ["message" =: Regex q ""]
                                                             , Doc ["status"  =: Regex q ""]])] "default") {M.sort = ["$natural" := Int32 (-1)], limit = 10}
                         Nothing  → find $ (select [] "default") {limit = 10, M.sort = ["$natural" := Int32 (-1)]}
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
<form method=get action=@{QueryR}#form enctype=#{formEnctype}>
  ^{formWidget}
    <input type="submit" value="Send it!">
<ul>
  $forall document <- results
    <li>
      <ul>
        $forall field <- document
          <li><b>#{retLabel field}</b>
        $forall field <- document
          <li>#{retValue field}
|]

getColsR ∷ Handler Html
getColsR = defaultLayout $ do
    s ← settings <$> getYesod
    pipe ← liftIO $ runIOE $ connect (host $ s^.mongoHost)
    eResults ← liftIO $ access pipe slaveOk (s^.database) $ do
                               allCollections >>= (return . filter (/="system.indexes"))
    results ← case eResults of
                (Right r) → return r
                (Left  f) → invalidArgs [show f]
    setTitle "Query"
    [whamlet|
<ul>
  $forall col <- results
    <li>
      #{col}
|]
    liftIO $ close pipe

getColsRelsR ∷ Handler Html
getColsRelsR = defaultLayout $ do
    s ← settings <$> getYesod
    pipe ← liftIO $ runIOE $ connect (host $ s^.mongoHost)
    eResults ← liftIO $ access pipe slaveOk (s^.database) $ do
                               cols ← allCollections >>= (return . filter (/="system.indexes"))
                               curs ← mapM (\col → find $ (select [] col) {limit = 10, M.sort = ["$natural" := Int32 (-1)]}) cols
                               results <- mapM rest curs
                               _ ← mapM closeCursor curs
                               return $ map (map $ exclude ["_id"]) results
    results ← case eResults of
                (Right r) → return r
                (Left  f) → invalidArgs [show f]
    setTitle "Query"
    [whamlet|
<ul>
  $forall collection <- results
    <ul>
      $forall document <- collection
        <li>
          <ul>
            $forall field <- document
              <li><b>#{retLabel field}</b>
            $forall field <- document
              <li>#{retValue field}
|]
    liftIO $ close pipe


retValue ∷ M.Field → Text
retValue = show . value

retLabel ∷ M.Field → Text
retLabel = show . label

data SearchQuery = SearchQuery Text
  deriving Show

carAForm ∷ AForm Handler SearchQuery
carAForm = SearchQuery <$> (areq textField "Search" Nothing)

carForm ∷ Html → MForm Handler (FormResult SearchQuery, Widget)
carForm = renderDivs carAForm
