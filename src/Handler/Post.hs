{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}


module Handler.Post where

import Import
import Yesod.Form.Bootstrap3
import Yesod.Text.Markdown

blogPostForm :: AForm Handler BlogPost
blogPostForm = BlogPost
                <$> areq textField (bfs ("Title" :: Text)) Nothing
                <*> areq markdownField (bfs ("Article" :: Text)) Nothing

getPostR :: Handler Html
getPostR = do
    muser <- maybeAuthId
    (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm blogPostForm
    defaultLayout $ do
        $(widgetFile "posts/new")

postPostR :: Handler Html
postPostR = do
    ((res, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm blogPostForm
    case res of
        FormSuccess blogPost -> do
                blogPostId <- runDB $ insert blogPost
                redirect $ PostDetailsR blogPostId
        _ -> defaultLayout $(widgetFile "posts/new")

