-- Copyright (c)2011, Mark Wright.  All rights reserved.

-- | A snap extension interface to blaze-html and digestive-functors-snap

module Snap.Extension.BlazeDF
  ( 
    -- * Blaze Html and digestive functors inside snap monad
    MonadBlazeDF(..)
  
    -- * Blaze HTML digestive functors state
  , BlazeDFState(..)
  , HasBlazeDFState(..)
    
  , blazeDFInitializer ) where

import           Control.Monad.Trans
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Unsafe.Coerce
import           Snap.Types
import           Snap.Extension

import qualified Text.Blaze.Renderer.Utf8 as R
import           Text.Blaze.Internal (Html)

import qualified Text.Digestive.Forms.Snap as DFS

-- | The blaze-html digestive-functors-snap class interface.
class MonadSnap m => MonadBlazeDF m where
  -- | Provide the blaze-html renderHtml function.
  renderHtml :: Html -> m BL.ByteString

  -- | Provide the digestive-functors-snap eitherSnapForm function.
  eitherSnapForm :: DFS.SnapForm m e v a -> String -> m (Either v a)

-- | The blaze-html digestive-functors-snap state.
data BlazeDFState = BlazeDFState
  {
  }
    
-- | Class interface functions to access the blaze-html digestive-functors-snap state.
class HasBlazeDFState s where
  getBlazeDFState :: s -> BlazeDFState
  setBlazeDFState :: BlazeDFState -> s -> s
  
  modifyBlazeDFState :: (BlazeDFState -> BlazeDFState) -> s -> s
  modifyBlazeDFState f s = setBlazeDFState (f $ getBlazeDFState s) s

-- | Intialize the blaze-html digestive-functors-snap state.
blazeDFInitializer :: Initializer BlazeDFState
blazeDFInitializer = do
  blazeDFState <- liftIO $ do
    return $ BlazeDFState
  mkInitializer blazeDFState
  
-- | The blaze-html digestive-functors-snap state intializer instance.
instance InitializerState BlazeDFState where
  extensionId = const (B.pack (map unsafeCoerce "BlazeDF/BlazeDF"))
  mkCleanup _ = return ()
  mkReload = const $ return ()

-- | The blaze-html digestive-functors-snap snap extension instance.
instance HasBlazeDFState s => MonadBlazeDF (SnapExtend s) where
  renderHtml h = do 
    return $ R.renderHtml h

  eitherSnapForm = DFS.eitherSnapForm
