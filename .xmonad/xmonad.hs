-------------------------------------------------------------------------------
--                  __  ____  __                       _                     --
--                  \ \/ /  \/  | ___  _ __   __ _  __| |                    --
--                   \  /| |\/| |/ _ \| '_ \ / _` |/ _` |                    --
--                   /  \| |  | | (_) | | | | (_| | (_| |                    --
--                  /_/\_\_|  |_|\___/|_| |_|\__,_|\__,_|                    --
--                                                                           --
-------------------------------------------------------------------------------

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

-- system
import System.IO (hPutStrLn) -- for xmobar

-- util
import XMonad.Util.Run (safeSpawn, unsafeSpawn, runInTerm, spawnPipe)
import XMonad.Util.SpawnOnce
import XMonad.Util.EZConfig (additionalKeysP, additionalMouseBindings)  
import XMonad.Util.NamedScratchpad

-- hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks (avoidStruts, docksStartupHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.EwmhDesktops -- for rofi
import XMonad.Hooks.ManageHelpers (isFullscreen, isDialog,  doFullFloat, doCenterFloat, doRectFloat) 
import XMonad.Hooks.Place (placeHook, withGaps, smart)

-- actions
import XMonad.Actions.CopyWindow -- for dwm window style tagging

-- layout 
import XMonad.Layout.Renamed (renamed, Rename(Replace))
import XMonad.Layout.LayoutHints
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.GridVariants
import XMonad.Layout.ResizableTile
import XMonad.Layout.BinarySpacePartition

------------------------------------------------------------------------
-- variables
------------------------------------------------------------------------

myModMask :: KeyMask
myModMask = mod4Mask -- Sets modkey to super/windows key

myTerminal :: [Char]
myTerminal = "urxvtc" -- Sets default terminal

myBorderWidth :: Dimension
myBorderWidth = 2 -- Sets border width for windows

myNormalBorderColor :: [Char]
myNormalBorderColor = "#839496"

myFocusedBorderColor :: [Char]
myFocusedBorderColor = "#268BD2"

myppCurrent :: [Char]
myppCurrent = "#cb4b16"

myppVisible :: [Char]
myppVisible = "#cb4b16"

myppHidden :: [Char]
myppHidden = "#268bd2"

myppHiddenNoWindows :: [Char]
myppHiddenNoWindows = "#93A1A1"

myppTitle :: [Char]
myppTitle = "#FDF6E3"

myppUrgent :: [Char]
myppUrgent = "#DC322F"

myWorkspaces :: [String]
myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

------------------------------------------------------------------------
-- Startup hook
------------------------------------------------------------------------

myStartupHook = do
      spawnOnce "urxvtd &" -- start urxvt terminal daemon
      spawnOnce "feh --no-fehbg --bg-center --image-bg '#353535' '/home/djwilcox/.wallpaper/freebsd.png'"
      spawnOnce "xsetroot -cursor_name left_ptr" -- set cursor

------------------------------------------------------------------------
-- Event hook
------------------------------------------------------------------------

myEventHook = hintsEventHook

------------------------------------------------------------------------
-- layout
------------------------------------------------------------------------
-- using toggleStruts with monocle
myLayout = avoidStruts (full ||| tiled ||| grid ||| bsp)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled = renamed [Replace "Tall"] $ layoutHintsWithPlacement (1.0, 0.0) (spacingRaw True (Border 10 0 10 0) True (Border 0 10 0 10) True $ ResizableTall 1 (3/100) (1/2) [])

     -- grid
     grid = renamed [Replace "Grid"] $ spacingRaw True (Border 10 0 10 0) True (Border 0 10 0 10) True $ Grid (16/10)

     -- full
     full = renamed [Replace "Full"] $ smartBorders (Full)

     -- bsp
     bsp = renamed [Replace "BSP"] $ emptyBSP

     -- The default number of windows in the master pane
     nmaster = 1
     
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

------------------------------------------------------------------------
-- Window rules:
------------------------------------------------------------------------

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--

myManageHook :: Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
    [ className =? "mpv"            --> doRectFloat (W.RationalRect (1 % 4) (1 % 4) (1 % 2) (1 % 2))
    , className =? "Gimp"           --> doFloat
    , className =? "Firefox" <&&> resource =? "Toolkit" --> doFloat -- firefox pip
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore 
    ] <+> namedScratchpadManageHook scratchpads
    
------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
------------------------------------------------------------------------

myKeys :: [([Char], X ())]
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
     , ("M-p", spawn "dmenu_run -p 'Yes Master ?' -fn 'xft:Inconsolata:size=9:lcdfilter=lcddefault:hintstyle=hintnone:rgba=rgb:antialias=true:autohint=false:style=bold' -nb '#292929' -nf '#eee8d5' -sb '#fdf6e3' -sf '#292929'") -- dmenu
     , ("S-M-t", withFocused $ windows . W.sink) -- flatten flaoting window to tiled
     , ("M-C-<Return>", namedScratchpadAction scratchpads "terminal")
     , ("M-C-<Space>", namedScratchpadAction scratchpads "emacs-scratch")
    ]

------------------------------------------------------------------------
-- scratchpads
------------------------------------------------------------------------

scratchpads :: [NamedScratchpad]
scratchpads = [ NS "terminal" spawnTerm findTerm manageTerm
              , NS "emacs-scratch" spawnEmacsScratch findEmacsScratch manageEmacsScratch
                ]
    where
    spawnTerm = myTerminal ++  " -name scratchpad"
    findTerm = resource =? "scratchpad"
    manageTerm = nonFloating
    findEmacsScratch = title =? "emacs-scratch"
    spawnEmacsScratch = "emacsclient -a='' -nc --frame-parameters='(quote (name . \"emacs-scratch\"))'"
    manageEmacsScratch = nonFloating
    
------------------------------------------------------------------------
-- main
------------------------------------------------------------------------

main :: IO ()
main = do
    xmproc <- spawnPipe "/usr/local/bin/xmobar -x 0 /home/djwilcox/.config/xmobar/xmobarrc"
    xmonad $ ewmh desktopConfig
        { manageHook = manageDocks <+> myManageHook <+> manageHook desktopConfig
        , startupHook        = myStartupHook
        , layoutHook         = myLayout
        , handleEventHook    = myEventHook <+>  handleEventHook desktopConfig
        , workspaces         = myWorkspaces
        , borderWidth        = myBorderWidth
        , terminal           = myTerminal
        , modMask            = myModMask
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = \x -> hPutStrLn xmproc x
                        , ppCurrent = xmobarColor myppCurrent "" . wrap "[" "]" -- Current workspace in xmobar
                        , ppVisible = xmobarColor myppVisible ""                -- Visible but not current workspace
                        , ppHidden = xmobarColor myppHidden "" . wrap "+" ""   -- Hidden workspaces in xmobar
                        , ppHiddenNoWindows = xmobarColor  myppHiddenNoWindows ""        -- Hidden workspaces (no windows)
                        , ppTitle = xmobarColor  myppTitle "" . shorten 80     -- Title of active window in xmobar
                        , ppSep =  "<fc=#586E75> | </fc>"                     -- Separators in xmobar
                        , ppUrgent = xmobarColor  myppUrgent "" . wrap "!" "!"  -- Urgent workspace
                        , ppExtras  = [windowCount]                           -- # of windows current workspace
                        , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
                        }
          } `additionalKeysP`         myKeys
