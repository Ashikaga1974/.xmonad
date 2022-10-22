--
--   ++++++++
--   ++
--   ++++++++
--         ++
--   ++++++++
--         ++
--         ++
--         ++++++++
--

import XMonad
import Data.Monoid
import Data.Maybe (fromJust)
import System.Exit

import XMonad.Actions.WorkspaceNames
import XMonad.Actions.SpawnOn
import XMonad.Actions.GridSelect
import XMonad.Actions.WindowBringer

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.EwmhDesktops

import XMonad.Layout.Fullscreen
import XMonad.Layout.Magnifier
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.Spacing
import XMonad.Layout.Grid
import XMonad.Layout.Named
import XMonad.Layout.Renamed

import XMonad.Layout.ShowWName

import XMonad.Util.SpawnOnce
import XMonad.Util.Run
import XMonad.Util.Cursor

import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import Control.Monad (liftM2)

import Graphics.X11.ExtraTypes.XF86

-- Importiert Library aus /lib/Colors
import Colors.Col

myLayoutPrinter :: String -> String
myLayoutPrinter "M" = "<icon=/home/saschalerschen/.xmonad/xpm/mirrorspacingtall.xbm/>"
myLayoutPrinter "T" = "<icon=/home/saschalerschen/.xmonad/xpm/spacingtall.xbm/>"
myLayoutPrinter "F" = "<icon=/home/saschalerschen/.xmonad/xpm/full.xbm/>"
myLayoutPrinter "#" = "<icon=/home/saschalerschen/.xmonad/xpm/grid.xbm/>"
myLayoutPrinter x = x

myTerminal :: String
myTerminal                          = "alacritty"

myFont :: String
myFont                              = "xft:Source Code Pro:bold:size=10:antialias=true:hinting=true"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse                 = True

windowCount :: X (Maybe String)
windowCount                         = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myClickJustFocuses :: Bool
myClickJustFocuses                  = False
myBorderWidth                       = 2
myModMask                           = mod1Mask
altMask                             = mod4Mask

myWorkspaces :: [String]
myWorkspaces                        = ["1:TERM", "2:WEB", "3:CODE", "4:MISC", "5:GFX"]

myWorkspaceIndices                  = M.fromList $ zip myWorkspaces [1..]
clickable ws                        = "<action=xdotool key alt+" ++ show i ++ ">" ++ ws ++ "</action>"
                                            where
                                                i = fromJust $ M.lookup ws myWorkspaceIndices

myGSconfig colorizer                = (buildDefaultGSConfig myColorizer) 

                                                                            { 
                                                                                gs_cellheight = 40,
                                                                                gs_cellwidth = 400, 
                                                                                gs_font = myFont,
                                                                                gs_cellpadding = 4
                                                                            }

myColorizer                         = colorRangeFromClassName
                                        (0x00, 0x00, 0x00)          -- Grün         -- lowest inactive bg
                                        (0x00, 0x00, 0x00)          -- Grün         -- highest inactive bg
                                        (0x52, 0x92, 0xC7)          -- blue         -- active bg
                                        (0x70, 0xFF, 0x70)          -- Schwarz      -- inactive fg
                                        (0x70, 0xFF, 0x70)          -- rot          -- active fg

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
------------------------------------------------------------------------
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    
    [ 
        ((modm .|. shiftMask      ,               xK_Return),             spawn $ XMonad.terminal conf),                                   -- launch a terminal
        ((modm                    ,               xK_p     ),             spawn "rofi -show drun -show-icons -font 'Source Code Pro 12'"), -- launch rofi
        ((modm                    ,               xK_o     ),             spawn "rofi -show window -show-icons -font 'Source Code Pro 10'"), -- launch rofi        
        ((modm                    ,               xK_c     ),             kill),                                                           -- close focused window
        ((modm                    ,               xK_space ),             sendMessage NextLayout),                                         -- Rotate through the available layout algorithms
        ((modm .|. shiftMask      ,               xK_space ),             setLayout $ XMonad.layoutHook conf),                             -- Reset the layouts on the current workspace to default
        ((modm                    ,               xK_n     ),             refresh),                                                        -- Resize viewed windows to the correct size
        ((modm                    ,               xK_Tab   ),             windows W.focusDown),                                            -- Move focus to the next window
        ((modm                    ,               xK_k     ),             windows W.focusUp  ),                                            -- Move focus to the previous window
        ((modm                    ,               xK_m     ),             windows W.focusMaster  ),                                        -- Move focus to the master window
        ((modm                    ,               xK_Return),             windows W.swapMaster),                                           -- Swap the focused window and the master window
        ((modm .|. shiftMask      ,               xK_j     ),             windows W.swapDown  ),                                           -- Swap the focused window with the next window
        ((modm .|. shiftMask      ,               xK_k     ),             windows W.swapUp    ),                                           -- Swap the focused window with the previous window
        ((modm                    ,               xK_h     ),             sendMessage Shrink),                                             -- Shrink the master area
        ((modm                    ,               xK_l     ),             sendMessage Expand),                                             -- Expand the master area
        ((modm                    ,               xK_t     ),             withFocused $ windows . W.sink),                                 -- Push window back into tiling
        ((modm                    ,               xK_comma ),             sendMessage (IncMasterN 1)),                                     -- Increment the number of windows in the master area
        ((modm                    ,               xK_period),             sendMessage (IncMasterN (-1))),                                  -- Deincrement the number of windows in the master area
        ((modm .|. shiftMask      ,               xK_q     ),             io (exitWith ExitSuccess)),                                      -- Quit xmonad
        ((modm                    ,               xK_q     ),             spawn "xmonad --recompile; xmonad --restart"),                   -- Restart xmonad
        ((modm .|. shiftMask      ,               xK_h     ),             spawn ("echo \"" ++ help ++ "\" | xmessage -file -")),           -- Run xmessage with a summary of the default keybindings (useful for beginners)

        -- Sound
        ((0                       ,               0x1008FF11   ),         spawn "amixer -q set Master 5%-"),                               -- Controls the amixer (volume) 0 is used for directly accessing the keys
        ((0                       ,               0x1008FF13   ),         spawn "amixer -q set Master 5%+"),
        ((0                       ,               xF86XK_AudioMute ),     spawn "amixer set Master toggle"),
        ((altMask                 ,               xK_Tab),                goToSelected $ myGSconfig myColorizer),                          -- Open Windows
        -- Applications
        --((modm                    ,               xK_d      ),            spawn "emacs"),                                                  -- Doom Emacs
        --((modm                    ,               xK_b      ),            spawn "firefox"),                                                -- Firefox
        ((modm                    ,               xK_b      ),            spawn "brave-browser"),                                                  -- Opera
        ((modm                    ,               xK_s      ),            spawn "maim --quality 4 ~/Bilder/screenshot_$(date +'%Y-%m-%d--%H%M%S').png")                                                   -- Firefox
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
------------------------------------------------------------------------
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    [
        ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster)),                                   -- mod-button1, Set the window to floating mode and move by dragging
        ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster)),                                   -- mod-button3, Set the window to floating mode and resize by dragging
        ((modm, button4), (\w -> kill)),                                                             -- Applikation schliessen
        ((modm, button5), (\w -> spawn "rofi -show drun -show-icons -font 'Source Code Pro 10'"))    -- geöffnete Programme anzeigen                                            
    ]

------------------------------------------------------------------------
-- Layouts:
------------------------------------------------------------------------
myLayout = avoidStruts (renamed [Replace "T"] (tiled) ||| renamed [Replace "M"] (Mirror tiled) ||| renamed [Replace "F"] (Full) ||| renamed [Replace "#"] (Grid))
    where
        tiled       = spacing 4 $ Tall nmaster delta ratio                  -- default tiling algorithm partitions the screen into two panes
        nmaster     = 1                                                     -- The default number of windows in the master pane
        ratio       = 1/2                                                   -- Default proportion of screen occupied by master pane
        delta       = 3/100                                                 -- Percent of screen to increment by when resizing panes

------------------------------------------------------------------------
-- Window rules:
------------------------------------------------------------------------
-- > xprop | grep WM_CLASS
--
myManageHook = composeAll
    [ 
        className =? "MPlayer"              --> doFloat
        , className =? "Steam"              --> doFloat    
        , className =? "firefox"            --> viewShift   "2:WEB"   
        , className =? "Opera"              --> viewShift   "2:WEB"                  
        , className =? "brave-browser"      --> viewShift   "2:WEB"    
        , className =? "Brave-browser"      --> viewShift   "2:WEB"                  
        , className =? "Code"               --> viewShift   "3:CODE"              
        , className =? "alacritty"          --> viewShift   "1:TERM"     
        , className =? "Alacritty"          --> viewShift   "1:TERM"   
        , className =? "thunderbird"        --> doShift     "4:MISC"      
        , className =? "Mail"               --> doShift     "4:MISC"              
        , className =? "gnome-calculator"   --> doFloat 
        , className =? "Gimp-2.10"          --> viewShift   "5:GFX"       
        , resource =? "desktop_window"      --> doIgnore
        , resource =? "kdesktop"            --> doIgnore
        , isDialog                          --> doCenterFloat
        , manageDocks
    ] where viewShift = doF . liftM2 (.) W.greedyView W.shift

------------------------------------------------------------------------
-- Startup hook
------------------------------------------------------------------------
myStartupHook = do

    setDefaultCursor xC_left_ptr                                          -- Default Mauszeiger (import xmonad.util.mouse)

    spawnOnce "xrandr --auto --output DisplayPort-0 --right-of DVI-D-0 &" -- Monitorausgabe
    spawnOnce "nitrogen --restore &"                                      -- Wallpaper wiederherstellen (install)

    spawnOnce "picom &"                                                   -- Transzparenz (install)
    spawnOnce "thunderbird &"                                             -- install
    spawnOnce "numlockx &"                                                -- install
    spawnOnce "blueman-applet &"                                          -- install
    --spawn "/usr/bin/emacs --daemon"
    spawn ("sleep 2 && trayer --edge top --align right --widthtype request --width 10 --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor 1 --transparent true --alpha 0 --tint black --height 19")   

    spawnOnce "flameshot &"                                               -- install        

------------------------------------------------------------------------
-- Start xmonad
------------------------------------------------------------------------
main = do
    xmproc0 <- spawnPipe "xmobar -x 1 ~/.xmonad/xmobar/xmobarrc"
    xmproc1 <- spawnPipe "xmobar -x 2 ~/.xmonad/xmobar/xmobarrc1"    

    xmonad $ docks $ ewmh $ defaults xmproc0 xmproc1

------------------------------------------------------------------------
-- Einstellungen
------------------------------------------------------------------------
defaults xmproc0 xmproc1 = def  
                            {
                                terminal              = myTerminal,
                                focusFollowsMouse     = myFocusFollowsMouse,
                                borderWidth           = myBorderWidth,
                                modMask               = myModMask,
                                workspaces            = myWorkspaces,
                                normalBorderColor     = Colors.Col.myNormalBorderColor,
                                focusedBorderColor    = Colors.Col.myFocusedBorderColor,

                                keys                  = myKeys,
                                mouseBindings         = myMouseBindings,

                                layoutHook            = myLayout,    
                                startupHook           = myStartupHook,
                                manageHook            = myManageHook    ,
                                logHook               = dynamicLogWithPP $ xmobarPP
                                                            {
                                                                ppOutput            = \x -> hPutStrLn xmproc0 x 
                                                                                        >> hPutStrLn xmproc1 x,
                                                                ppTitle             = xmobarColor xmobarTitleColor "" . shorten 100,
                                                                ppCurrent           = xmobarColor xmobarTitleColor "" .wrap "[" "]",
                                                                ppHidden            = xmobarColor xmobarCurrentWorkspaceColor "" .wrap "*" "" . clickable,
                                                                ppHiddenNoWindows   = xmobarColor xmobarHiddenNoWindows "" . clickable,
                                                                ppVisible           = xmobarColor xmobarVisible "" . clickable,
                                                                ppExtras            = [windowCount],
                                                                ppSep               = " | ",
                                                                ppLayout            = myLayoutPrinter
                                                            }
                            }    

------------------------------------------------------------------------
-- Hilfetext
------------------------------------------------------------------------
help :: String
help = unlines ["The default modifier key is 'alt'. Default keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Shift-Enter  Launch xterminal",
    "mod-p            Launch dmenu",
    "mod-Shift-p      Launch gmrun",
    "mod-Shift-c      Close/kill the focused window",
    "mod-Space        Rotate through the available layout algorithms",
    "mod-Shift-Space  Reset the layouts on the current workSpace to default",
    "mod-n            Resize/refresh viewed windows to the correct size",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab        Move focus to the next window",
    "mod-Shift-Tab  Move focus to the previous window",
    "mod-j          Move focus to the next window",
    "mod-k          Move focus to the previous window",
    "mod-m          Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Return   Swap the focused window and the master window",
    "mod-Shift-j  Swap the focused window with the next window",
    "mod-Shift-k  Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "mod-h  Shrink the master area",
    "mod-l  Expand the master area",
    "",
    "-- floating layer support",
    "mod-t  Push window back into tiling; unfloat and re-tile it",
    "",
    "-- increase or decrease number of windows in the master area",
    "mod-comma  (mod-,)   Increment the number of windows in the master area",
    "mod-period (mod-.)   Deincrement the number of windows in the master area",
    "",
    "-- quit, or restart",
    "mod-Shift-q  Quit xmonad",
    "mod-q        Restart xmonad",
    "mod-[1..9]   Switch to workSpace N",
    "",
    "-- Workspaces & screens",
    "mod-Shift-[1..9]   Move client to workspace N",
    "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging"]