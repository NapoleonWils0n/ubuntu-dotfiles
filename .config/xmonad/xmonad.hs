------------------------------------------------------------------------
-- import
------------------------------------------------------------------------

import XMonad hiding ( (|||) ) -- jump to layout
import XMonad.Layout.LayoutCombinators (JumpToLayout(..), (|||)) -- jump to layout
import XMonad.Config.Desktop
import System.Exit
import qualified XMonad.StackSet as W

-- data
import Data.Char (isSpace)
import Data.List
import Data.Monoid
import Data.Maybe (isJust)
import Data.Ratio ((%)) -- for video
import qualified Data.Map as M

-- util
import XMonad.Util.Run (safeSpawn, unsafeSpawn, runInTerm, spawnPipe)
import XMonad.Util.SpawnOnce
import XMonad.Util.EZConfig (additionalKeysP, additionalMouseBindings)  
import XMonad.Util.NamedScratchpad
import XMonad.Util.NamedWindows
import XMonad.Util.WorkspaceCompare

-- hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks (avoidStruts, docksStartupHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.EwmhDesktops -- to show workspaces in application switchers
import XMonad.Hooks.ManageHelpers (isFullscreen, isDialog,  doFullFloat, doCenterFloat, doRectFloat) 
import XMonad.Hooks.Place (placeHook, withGaps, smart)
import XMonad.Hooks.UrgencyHook

-- actions
import XMonad.Actions.CopyWindow -- for dwm window style tagging
import XMonad.Actions.UpdatePointer -- update mouse postion

-- layout 
import XMonad.Layout.Renamed (renamed, Rename(Replace))
-- import XMonad.Layout.NoBorders
import qualified XMonad.Layout.NoBorders as BO
import XMonad.Layout.Spacing
import XMonad.Layout.GridVariants
import XMonad.Layout.ResizableTile
import XMonad.Layout.BinarySpacePartition

------------------------------------------------------------------------
-- variables
------------------------------------------------------------------------

myModMask = mod4Mask -- Sets modkey to super/windows key
myTerminal = "urxvtc" -- Sets default terminal
myBorderWidth = 2 -- Sets border width for windows
myNormalBorderColor = "#839496"
myFocusedBorderColor = "#268BD2"
myWorkspaces = ["1","2","3","4","5","6","7","8","9"]

------------------------------------------------------------------------
-- desktop notifications -- dunst package required
------------------------------------------------------------------------

data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        name     <- getName w
        Just idx <- fmap (W.findTag w) $ gets windowset

        safeSpawn "notify-send" [show name, "workspace " ++ idx]

------------------------------------------------------------------------
-- Startup hook
------------------------------------------------------------------------

myStartupHook = do
      spawnOnce "urxvtd &" -- start urxvt terminal daemon
      spawnOnce "emacs --daemon" -- emacs
      spawnOnce "sxhkd &" -- sxhkd keyboard
      spawnOnce "dunst &" -- dunst
      spawnOnce "nm-applet &" -- network applet
      spawnOnce "tint2 -c /home/djwilcox/.config/tint2/tint2rc"
      spawnOnce "mpd --no-daemon /home/djwilcox/.config/mpd/mpd.conf"

------------------------------------------------------------------------
-- layout
------------------------------------------------------------------------

myLayout = BO.lessBorders BO.Never $ avoidStruts (full ||| tiled ||| grid ||| bsp)
  where
     -- full
     full = renamed [Replace "Full"] 
           $ BO.noBorders (Full)

     -- tiled
     tiled = renamed [Replace "Tall"] 
           $ spacingRaw True (Border 10 0 10 0) True (Border 0 10 0 10) True 
           $ ResizableTall 1 (3/100) (1/2) []

     -- grid
     grid = renamed [Replace "Grid"] 
          $ spacingRaw True (Border 10 0 10 0) True (Border 0 10 0 10) True 
          $ Grid (16/10)

     -- bsp
     bsp = renamed [Replace "BSP"] 
         $ emptyBSP

     -- The default number of windows in the master pane
     nmaster = 1
     
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

------------------------------------------------------------------------
-- Window rules:
------------------------------------------------------------------------
 
myManageHook = composeAll
    [ className =? "mpv"            --> doRectFloat (W.RationalRect (1 % 4) (1 % 4) (1 % 2) (1 % 2))
    , className =? "Gimp"           --> doFloat
    , className =? "Firefox" <&&> resource =? "Toolkit" --> doFloat -- firefox pip
    , className =? "qjackctl"       --> doFloat
    , className =? "Google Earth Pro" --> doFloat
    , title =? "emacs-capture"      --> doRectFloat (W.RationalRect (1 % 4) (1 % 4) (1 % 2) (1 % 2)) -- emacs org capture
    , title =? "emacs-scratch"      --> doRectFloat (W.RationalRect (1 % 4) (1 % 4) (1 % 2) (1 % 2)) -- emacs org capture
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore 
    , isFullscreen --> doFullFloat
    ] <+> namedScratchpadManageHook myScratchpads
    
------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
------------------------------------------------------------------------

myKeys =
    [("M-" ++ m ++ k, windows $ f i)
        | (i, k) <- zip (myWorkspaces) (map show [1 :: Int ..])
        , (f, m) <- [(W.view, ""), (W.shift, "S-"), (copy, "S-C-")]]
    ++
    [("S-C-a", windows copyToAll)   -- copy window to all workspaces
     , ("S-C-z", killAllOtherCopies)  -- kill copies of window on other workspaces
     , ("M-a", sendMessage MirrorExpand)
     , ("M-z", sendMessage MirrorShrink)
     , ("M-s", sendMessage ToggleStruts)
     , ("M-f", sendMessage $ JumpToLayout "Full")
     , ("M-t", sendMessage $ JumpToLayout "Tall")
     , ("M-g", sendMessage $ JumpToLayout "Grid")
     , ("M-b", sendMessage $ JumpToLayout "BSP")
    -- , ("M-p", spawn "rofi -show combi -modi combi") -- rofi
     , ("S-M-t", withFocused $ windows . W.sink) -- flatten floating window to tiled
     , ("M-C-<Space>", namedScratchpadAction myScratchpads "terminal")
     , ("M-C-<Return>", namedScratchpadAction myScratchpads "emacs-scratch")
    ]

------------------------------------------------------------------------
-- scratchpads
------------------------------------------------------------------------

myScratchpads = [ NS "terminal" spawnTerm findTerm manageTerm
              , NS "emacs-scratch" spawnEmacsScratch findEmacsScratch manageEmacsScratch
                ] 
    where
    role = stringProperty "WM_WINDOW_ROLE"
    spawnTerm = myTerminal ++  " -name scratchpad"
    findTerm = resource =? "scratchpad"
    manageTerm = nonFloating
    findEmacsScratch = title =? "emacs-scratch"
    spawnEmacsScratch = "emacsclient -a='' -nc --frame-parameters='(quote (font . \"Inconsolata:size=24:weight=regular:antialias=true:hinting=true:hintstyle=hintfull\")(name . \"emacs-scratch\"))'"
    manageEmacsScratch = nonFloating
    
------------------------------------------------------------------------
-- main
------------------------------------------------------------------------
main = do
    xmonad $ withUrgencyHook LibNotifyUrgencyHook $ ewmh desktopConfig
        { manageHook = ( isFullscreen --> doFullFloat ) <+> manageDocks <+> myManageHook <+> manageHook desktopConfig
        , startupHook        = myStartupHook
        , layoutHook         = myLayout
        , handleEventHook    = handleEventHook desktopConfig
        , workspaces         = myWorkspaces
        , borderWidth        = myBorderWidth
        , terminal           = myTerminal
        , modMask            = myModMask
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , logHook            = updatePointer (0.25, 0.25) (0.25, 0.25)
        }
        `additionalKeysP` myKeys
