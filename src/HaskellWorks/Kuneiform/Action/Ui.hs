{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Kuneiform.Action.Ui where

import Brick.Types             (Widget)
import Brick.Util              (fg, on)
import Brick.Widgets.Core      (hLimit, str, vBox, vLimit, withAttr, (<+>))
import Control.Monad           (void)
import Data.Maybe              (fromMaybe)
import Data.Monoid
import HaskellWorks.Kuneiform.Option.Cmd.Ui
import Lens.Micro              ((^.))

import qualified Brick.AttrMap        as A
import qualified Brick.Main           as M
import qualified Brick.Types          as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.List   as L
import qualified Data.Vector          as Vec
import qualified Graphics.Vty         as V

drawUI :: Show a => L.List Name a -> [Widget Name]
drawUI l = [ui]
  where label = str "Item " <+> cur <+> str " of " <+> total
        cur = case l ^. L.listSelectedL of
                Nothing -> str "-"
                Just i  -> str (show (i + 1))
        total = str $ show $ Vec.length $ l ^. L.listElementsL
        box :: Widget Name
        box = B.borderWithLabel label $
              hLimit 25 $
              vLimit 15 $
              L.renderList listDrawElement True l
        ui :: Widget Name
        ui = C.vCenter $ vBox [ C.hCenter box
                              , str " "
                              , C.hCenter $ str "Press +/- to add/remove list elements."
                              , C.hCenter $ str "Press Esc to exit."
                              ]

appEvent :: L.List Name Char -> T.BrickEvent Name e -> T.EventM Name (T.Next (L.List Name Char))
appEvent l (T.VtyEvent e) =
  case e of
    V.EvKey (V.KChar '+') [] ->
      let el  = nextElement (L.listElements l)
          pos = Vec.length $ l ^. L.listElementsL
      in M.continue $ L.listInsert pos el l
    V.EvKey (V.KChar '-') [] ->
      case l ^. L.listSelectedL of
        Nothing -> M.continue l
        Just i  -> M.continue $ L.listRemove i l
    V.EvKey V.KEsc [] -> M.halt l
    ev -> M.continue =<< L.handleListEvent ev l
  where
    nextElement :: Vec.Vector Char -> Char
    nextElement v = fromMaybe '?' $ Vec.find (`Vec.notElem` v) (Vec.fromList ['a' .. 'z'])
appEvent l _ = M.continue l

listDrawElement :: Show a => Bool -> a -> Widget Name
listDrawElement sel a =
  let selStr s = if sel
                 then withAttr customAttr (str $ "<" <> s <> ">")
                 else str s
  in C.hCenter (str "Item " <+> selStr (show a))

initialState :: L.List Name Char
initialState = L.list View (Vec.fromList ['a','b','c']) 1

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
  [ (L.listAttr         , V.white `on` V.blue)
  , (L.listSelectedAttr , V.blue `on` V.white)
  , (customAttr         , fg V.cyan)
  ]

data Name = View deriving (Eq, Ord, Show)

theApp :: M.App (L.List Name Char) e Name
theApp = M.App
  { M.appDraw         = drawUI
  , M.appChooseCursor = M.showFirstCursor
  , M.appHandleEvent  = appEvent
  , M.appStartEvent   = return
  , M.appAttrMap      = const theMap
  }

actionUi :: CmdUi -> IO ()
actionUi _ = void $ M.defaultMain theApp initialState
