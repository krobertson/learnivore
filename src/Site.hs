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
solve :: Application ()
solve = do eqn <- decodedParam "eqn"
           heistLocal (bindString "solution" $ T.decodeUtf8 $ withByteString renderEqSolution eqn) $ render "solve"
        where
          decodedParam p = fromMaybe "" <$> getParam p
          
solveJSON :: Application ()
solveJSON = do eqn <- decodedParam "eqn"
               heistLocal (bindString "solution" $ T.decodeUtf8 $ withByteString renderEqSolutionJSON eqn) $ render "solveJSON"
            where
              decodedParam p = fromMaybe "" <$> getParam p


withByteString fn str = B.pack . BS.encode $ fn . BS.decode . B.unpack $ str


------------------------------------------------------------------------------
-- | The main entry point handler.
site :: Application ()
site = route [ ("/",            index)
             , ("/solve",       solve)
             , ("/solveJSON",   solveJSON)
             ]
       <|> serveDirectory "resources/static"
