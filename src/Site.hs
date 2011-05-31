{-# LANGUAGE OverloadedStrings #-}

{-|

This is where all the routes and handlers are defined for your site. The
'site' function combines everything together and is exported by this module.

-}

module Site
  ( site
  ) where

import           Control.Applicative
import           Data.Maybe
import qualified Data.Text.Encoding as T
import           Snap.Extension.Heist
import           Snap.Extension.Timer
import           Snap.Util.FileServe
import           Snap.Types
import           Text.Templating.Heist
import qualified Data.ByteString as B
import qualified Codec.Binary.UTF8.String as BS
import           Application
import           LearnMath


------------------------------------------------------------------------------
-- | Renders the front page of the sample site.
--
-- The 'ifTop' is required to limit this to the top of a route.
-- Otherwise, the way the route table is currently set up, this action
-- would be given every request.
index :: Application ()
index = ifTop $ heistLocal (bindSplices indexSplices) $ render "index"
  where
    indexSplices =
        [ ("start-time",   startTimeSplice)
        , ("current-time", currentTimeSplice)
        ]


------------------------------------------------------------------------------
-- | Renders the solution.
quiz :: Application ()
quiz = do render "quiz"

solve :: Application ()
solve = do eqn <- decodedParam "eqn"
           heistLocal (bindString "solution" $ T.decodeUtf8 $ withByteString renderEqSolution eqn) $ render "solve"
        where
          decodedParam p = fromMaybe "" <$> getParam p
          
solveJSON :: Application ()
solveJSON = do eqn <- decodedParam "eqn"
               heistLocal (bindString "json" $ T.decodeUtf8 $ withByteString renderEqSolutionJSON eqn) $ render "json"
            where
              decodedParam p = fromMaybe "" <$> getParam p

verifyAnswerJSON :: Application ()
verifyAnswerJSON = do {solution <- decodedParam "solution"
                      ;question <- decodedParam "question"
                      ;heistLocal (bindString "json" $ T.decodeUtf8 $ toByteString . renderAnswerJSON (toString solution) $ toString question) $ render "json"}
                    where
                     decodedParam p = fromMaybe "" <$> getParam p

getQuestionJSON :: Application ()
getQuestionJSON = do heistLocal (bindString "json" $ T.decodeUtf8 $ toByteString $ "{\"question\":{\"lhs\":\"2^x\",\"rhs\":\"4\"}}") $ render "json"

withByteString fn str = B.pack . BS.encode $ fn . BS.decode . B.unpack $ str
toString = BS.decode . B.unpack
toByteString = B.pack . BS.encode

------------------------------------------------------------------------------
-- | The main entry point handler.
site :: Application ()
site = route [ ("/",                index)
             , ("/quiz",            quiz)
             , ("/solve",           solve)
             , ("/solveJSON",       solveJSON)
             , ("/getQuestionJSON", getQuestionJSON)
       , ("/verifyAnswerJSON",verifyAnswerJSON)
             ]
       <|> serveDirectory "resources/static"
