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
import Data.Maybe
import Data.List(isPrefixOf)
import System.Exit
import Control.Monad(sequence)
import Control.Applicative

import qualified XMonad.StackSet as W
import Data.Map(fromList)
import XMonad.Hooks.ManageDocks
import qualified XMonad.Layout.WindowNavigation as N
import qualified XMonad.Layout.ResizableTile as R
import qualified XMonad.Layout.SubLayouts as S
import qualified XMonad.Layout.BoringWindows as B
import qualified XMonad.Layout.Drawer as D
import qualified XMonad.Layout.PerWorkspace as PW
import qualified XMonad.Layout.Magnifier as M
import qualified XMonad.Layout.Grid as G
import qualified XMonad.Layout.Master as A
import qualified XMonad.Layout.Tabbed as Tab

import qualified XMonad.Util.WindowProperties as P

import qualified XMonad.Hooks.FadeInactive as F
import qualified XMonad.Hooks.InsertPosition as I

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces = ["main","sub","gimp","im","video","6","7","8","9"]


------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--

myKeys conf@(XConfig {XMonad.modMask = modm, XMonad.terminal = term}) =
    let ms = modm .|. shiftMask
        ma = modm .|. mod1Mask
        msa = ms .|. mod1Mask
    in fromList $

    -- launch a terminal
    [ ((ms,xK_Return), spawn term)

    -- launch a bits work terminal
    , ((msa, xK_Return), spawn $ term ++ " -e ssh-agent zsh -c 'ssh-add ~/.ssh/id_rsa; exec zsh'")

    -- launch dmenu
    , ((modm, xK_p), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")

    -- close focused window
    , ((ms,xK_c), kill)

     -- Rotate through the available layout algorithms
    , ((modm, xK_space), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((ms,xK_space), setLayout $ XMonad.layoutHook conf)

    -- Move focus to the next window
    , ((modm, xK_Tab), B.focusDown)

    -- Move focus to the previous window
    , ((ms, xK_Tab),   B.focusUp)

    -- Move fucus to the right/left/up/down window
    , ((modm, xK_l), sendMessage $ N.Go N.R)
    , ((modm, xK_j), sendMessage $ N.Go N.L)
    , ((modm, xK_i), sendMessage $ N.Go N.U)
    , ((modm, xK_k), sendMessage $ N.Go N.D)
    , ((ms,   xK_l), sendMessage $ N.Swap N.R)
    , ((ms,   xK_j), sendMessage $ N.Swap N.L)
    , ((ms,   xK_i), sendMessage $ N.Swap N.U)
    , ((ms,   xK_k), sendMessage $ N.Swap N.D)

    -- Pull adjacent window to focused group
    , ((ma, xK_l), sendMessage $ S.pullGroup R)
    , ((ma, xK_j), sendMessage $ S.pullGroup L)
    , ((ma, xK_i), sendMessage $ S.pullGroup U)
    , ((ma, xK_k), sendMessage $ S.pullGroup D)

    , ((ms,   xK_o), withFocused (sendMessage . S.UnMerge))

    -- Focus next/prev window on sublayout
    , ((mod1Mask .|. shiftMask,  xK_Tab), S.onGroup W.focusUp')
    , ((mod1Mask, xK_Tab), S.onGroup W.focusDown')
    , ((ma, xK_space), S.toSubl NextLayout)

    -- Swap the focused window and the master window
    , ((modm, xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((ms,xK_period), windows W.swapDown)

    -- Swap the focused window with the previous window
    , ((ms,xK_comma), windows W.swapUp)

    -- Shrink the master area
    , ((modm, xK_n), sendMessage Shrink)

    -- Expand the master area
    , ((modm, xK_m), sendMessage Expand)

    -- Shrink the slave area alternate direction
    , ((ms,   xK_n), sendMessage R.MirrorExpand)

    -- Expand the slave area alternate direction 
    , ((ms,   xK_m), sendMessage R.MirrorShrink)

    -- Push window back into tiling
    , ((modm, xK_t), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm, xK_comma), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm, xK_period), sendMessage (IncMasterN (-1)))

    -- Toggle Magnifier Mode
    , ((modm, xK_x), sendMessage M.Toggle)

    -- Increase/Decrease Magnifying Rate
    , ((modm, xK_z), sendMessage M.MagnifyMore)
    , ((ms,   xK_z), sendMessage M.MagnifyLess)
    
    -- Screen shot
    , ((modm, xK_Print), spawn "gnome-screenshot -f ~/ss-`date +%Y-%m-%d-%T`.png")

    -- Firefox
    , ((mod4Mask, xK_f), spawn "firefox -P -no-remote")

    -- Emacs
    , ((modm, xK_e), spawn "emacs")

    -- Open latest C++ specification draft with evince
    , ((modm, xK_semicolon), spawn "evince ~/doc/c++/newer.pdf")

    -- Copy primary selection to clipboard
    , ((modm, xK_c), spawn "xsel -o -p | xsel -i -b")

    -- Copy clipboard to primary selection
    , ((ma, xK_c), spawn "xsel -o -b | xsel -i -p")

    -- Show Clock
    , ((modm, xK_d), spawn "date '+%Y/%m/%d(%a) %T ' | dzen2 -p 3 -ta r -e button1=exit")

    -- Restart xmonad
    , ((modm, xK_q), spawn "xmonad --recompile && xmonad --restart")
      
    -- Control sound volume
    , ((modm, xK_F11), spawn "amixer -q sset Master 2%-")
    , ((modm, xK_F12), spawn "amixer -q sset Master 2%+")
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
    -- mod--alt-{2,1,3}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-alt-shift-{2,1,3}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_2, xK_1, xK_3] [0..]
        , (f, m) <- [(W.view, mod1Mask), (W.shift, shiftMask .|. mod1Mask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = fromList $

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
myLayout = nav
           $ B.boringWindows
           $ msgWS "im"
           $ gimpWS "gimp" 
           -- $ M.magnifiercz (1.1/1)
           $ S.subLayout []
                 (Full ||| tiled ||| mirrorTiled)
                 (tiled ||| mirrorTiled)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled = R.ResizableTall nmaster delta ratio []
     mirrorTiled = Mirror $ R.ResizableTall nmaster mirrorDelta mirrorRatio []

     -- Layout modifier for GIMP
     gimpWS wsname = PW.onWorkspace wsname Full
     gimpLayout = leftTiled $ rightTiled Full
     leftTiled = D.onLeft $ dock $ P.Role "gimp-toolbox"
     rightTiled = D.onRight $ dock $ P.Role "gimp-dock"
     dock prop = D.simpleDrawer closedRatio openRatio prop
     closedRatio = 0.01
     openRatio = 0.2

     -- Layout modifier for Skype
     msgWS wsname = PW.onWorkspace wsname msgLayout
     msgLayout = A.mastered delta msgRatio
                 (M.magnifiercz msgZoomDelta G.Grid ||| Tab.tabbed Tab.shrinkText theme)
     msgRatio = 1/6
     msgZoomDelta = 1.2/1
     theme = Tab.defaultTheme

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
               [ className =? "MPlayer"       --> doFloat
               , appName  =? "zenity"         --> doFloat
               , windowType <==> atom "_NET_WM_WINDOW_TYPE_DIALOG" --> doFloat
               , moveToWS [ className =? "Skype"
                          , className =? "Iptux"] "im"
               , moveToWS [ className =? "Smplayer"
                          , windowRole =? "vlc-video"] "video"
               -- , moveToWS [ className =? "Gimp"] "gimp"
               , windowType <==> atom "_NET_WM_WINDOW_TYPE_TOOLBAR" --> doIgnore
               , firefoxHooks
               ]
    where moveToWS queries wsName = or <$> sequence queries --> (I.insertPosition I.End I.Older <+> doShift wsName)
          firefoxHooks = composeAll $ map (className =? "Firefox" -->) $
                         [ windowRole =? "Preferences" --> doFloat
                         , windowRole =? "About"       --> doFloat
                         , windowRole =? "Manager"     --> doFloat
                         , isPrefixOf <$> pure "StylishEdit" <*> windowRole --> doFloat
                         ]

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

myLogHook = composeAll
            [ fadeHook
            -- , setCurrentWallPaper
            ]
    where fadeHook = F.fadeOutLogHook $
                     F.fadeIf (F.isUnfocused <&&> (fmap (not . or) $ sequence fadeExcept))
                          0.8
          fadeExcept = [ className =? "Smplayer"
                       , className =? "Gimp"
                       , className =? "Inkscape"
                       , windowRole =? "vlc-video"
                       , windowRole =? "vlc-main"
                       ]

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
  xmonad $ myConfig

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
myConfig = defaultConfig {
      -- simple stuff
        terminal           = "urxvt",
        focusFollowsMouse  = False,
        borderWidth        = 0,
        modMask            = mod4Mask,
        workspaces         = myWorkspaces,
        normalBorderColor  = "#dddddd",
        focusedBorderColor = "#ff0000",

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
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

atom :: String -> Query Atom
atom = liftX . getAtom

windowRole :: Query String
windowRole = stringProperty "WM_WINDOW_ROLE"

windowType :: Query Atom
windowType = do
    w <- ask
    liftX $ withDisplay $ \d -> do
               a <- getAtom "_NET_WM_WINDOW_TYPE"
               p <- io $ getWindowProperty32 d a w
               return $ fromIntegral $ fromMaybe 0 $ p >>= safeHead
    where
      safeHead (x:_) = Just x
      safeHead _ = Nothing

(<==>), (</=>) :: (Applicative f, Eq a) => f a -> f a -> f Bool
(<==>) = liftA2 (==)
(</=>) = liftA2 (/=)
infix 4 <==>, </=>

-- setCurrentWallPaper :: X ()
-- setCurrentWallPaper = do
--   xstate <- get
--   let ws = windowset xstate
--       tag = W.tag $ W.workspace $ W.current ws
--       wptable = [ ("main", "~/doc/picture/wp-handmade/th2/wp-th2-fullhd.png")
--                 , ("gimp", "~/doc/picture/wp-handmade/aznyan/aznyan.png")
--                 , ("im", "~/doc/picture/wp-handmade/aznyan/aznyan4.png")
--                 , ("sub", "~/doc/picture/wp-handmade/dechimal/dechimal.png")
--                 , ("video", "~/doc/picture/wp-handmade/th2/wp-th2-fullhd-yellow.png")
--                 ]
--       wpname = fromMaybe (snd $ head $ wptable) $ lookup tag wptable
--   -- spawn $ "hsetroot -center " ++ wpname -- hsetroot work poor when change to no window workspace.
--   return ()
