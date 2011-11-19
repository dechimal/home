--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--

import XMonad
import Data.Monoid
import qualified System.IO.UTF8 as U
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map as M
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import qualified XMonad.Layout.WindowNavigation as N
import qualified XMonad.Layout.ResizableTile as R
import qualified XMonad.Layout.SubLayouts as S
import qualified XMonad.Layout.BoringWindows as B
import qualified XMonad.Hooks.FadeInactive as F

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces = ["1","2","3","4","5","6","7","8","9"]


------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--

myKeys conf@(XConfig {XMonad.modMask = modm}) =
    let ms = modm .|. shiftMask
        ma = modm .|. mod1Mask
        msa = ms .|. mod1Mask
    in M.fromList $

    -- launch a terminal
    [ ((ms,xK_Return), spawn $ XMonad.terminal conf)

    -- launch dmenu
    , ((modm, xK_p), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")

    -- launch gmrun
    -- , ((ms,xK_p), spawn "gmrun")

    -- close focused window
    , ((ms,xK_c), kill)

     -- Rotate through the available layout algorithms
    , ((modm, xK_space), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((ms,xK_space), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm, xK_n), refresh)

    -- Move focus to the next window
    , ((modm, xK_Tab), B.focusDown)

    -- Move focus to the previous window
    , ((ms, xK_Tab),   B.focusUp)

    -- Move fucus to the right/left/up/down window
    , ((modm, xK_k), sendMessage $ N.Go N.R)
    , ((modm, xK_j), sendMessage $ N.Go N.L)
    , ((modm, xK_i), sendMessage $ N.Go N.U)
    , ((modm, xK_m), sendMessage $ N.Go N.D)
    , ((ms,   xK_k), sendMessage $ N.Swap N.R)
    , ((ms,   xK_j), sendMessage $ N.Swap N.L)
    , ((ms,   xK_i), sendMessage $ N.Swap N.U)
    , ((ms,   xK_m), sendMessage $ N.Swap N.D)

    -- Pull adjacent window to focused group
    , ((ma, xK_j), sendMessage $ S.pullGroup L)
    , ((ma, xK_k), sendMessage $ S.pullGroup R)
    , ((ma, xK_i), sendMessage $ S.pullGroup U)
    , ((ma, xK_m), sendMessage $ S.pullGroup D)

    -- 
    , ((modm, xK_o), withFocused (sendMessage . S.MergeAll))
    , ((ms,   xK_o), withFocused (sendMessage . S.UnMerge))

    , ((mod1Mask .|. shiftMask,  xK_Tab), S.onGroup W.focusUp')
    , ((mod1Mask, xK_Tab), S.onGroup W.focusDown')
    , ((ma, xK_space), S.toSubl NextLayout)

    -- Movefocus to the master window
    -- , ((modm, xK_m), windows W.focusMaster)

    -- Swapthe focused window and the master window
    , ((modm, xK_Return), windows W.swapMaster)

    -- Swapthe focused window with the next window
    -- , ((ms,xK_j), windows W.swapDown)

    -- Swapthe focused window with the previous window
    -- , ((ms,xK_k), windows W.swapUp)

    -- Shrink the master area
    , ((modm, xK_h), sendMessage Shrink)

    -- Expand the master area
    , ((modm, xK_l), sendMessage Expand)

    -- Shrink the slave area alternate direction
    , ((ms,   xK_h), sendMessage R.MirrorShrink)

    -- Expand the slave area alternate direction 
    , ((ms,   xK_l), sendMessage R.MirrorExpand)

    -- Pushwindow back into tiling
    , ((modm, xK_t), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm, xK_comma), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm, xK_period), sendMessage (IncMasterN (-1)))
    
    -- Screen shot
    , ((0, xK_Print), spawn "scrot")

    -- Firefox
    , ((mod4Mask, xK_f), spawn "~/repos/firefox/i/bin/firefox -P -no-remote")

    -- Open latest C++ specification draft with evince
    , ((ms, xK_n), spawn "evince ~/junk/doc/newer.pdf")

    -- Copy primary selection to clipboard
    , ((modm, xK_c), spawn "xsel -o -p | xsel -i -b")

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b), sendMessage ToggleStruts)

    -- Quitxmonad
    , ((ms,xK_q), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm, xK_q), spawn "xmonad --recompile && xmonad --restart")
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
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
           >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
           >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = nav $ B.boringWindows $ S.subLayout []
                                               (Full ||| tiled ||| mirrorTiled)
                                               (tiled ||| mirrorTiled)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled = R.ResizableTall nmaster delta ratio []
     mirrorTiled = Mirror $ R.ResizableTall nmaster mirrorDelta mirrorRatio []

     -- WindowNavigation
     nav layout = N.configurableNavigation (N.navigateColor navColor) layout

     -- Thedefault number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio = 7/10
     mirrorRatio = 2/3

     -- Percent of screen to increment by when resizing panes
     delta = 3/100
     mirrorDelta = 1/20

     -- Color of focus navigation
     navColor = "#0099ff"


------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To matchon the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    -- , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook= mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook = F.fadeInactiveLogHook 0.8

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = return ()

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
--  xmproc <- spawnPipe "dzen2"
--  xmonad $ defaults xmproc
  xmonad defaults

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
-- defaults hook = defaultConfig {
defaults = defaultConfig {
      -- simple stuff
        terminal           = "urxvt",
        focusFollowsMouse  = False,
        borderWidth        = 0,
        modMask            = mod4Mask,
        numlockMask        = mod2Mask,
        workspaces         = myWorkspaces,
        normalBorderColor  = "#dddddd",
        focusedBorderColor = "#ff0000",

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = avoidStruts myLayout,
        manageHook         = manageDocks <+> myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook
    }

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--

-- The mask for the numlock key. Numlock status is "masked" from the
-- current modifier status, so the keybindings will work with numlock on or
-- off. You may need to change this on some systems.
--
-- You can find the numlock modifier by running "xmodmap" and looking for a
-- modifier with Num_Lock bound to it:
--
-- > $ xmodmap | grep Num
-- > mod2        Num_Lock (0x4d)
--
-- Set numlockMask = 0 if you don't have a numlock key, or want to treat
-- numlock status separately.
--
