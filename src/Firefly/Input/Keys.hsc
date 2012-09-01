--------------------------------------------------------------------------------
-- | Designed for qualified import...
{-# LANGUAGE CPP #-}
module Firefly.Input.Keys
    ( unknown
    , first
    , backspace
    , tab
    , clear
    , return
    , pause
    , escape
    , space
    , exclaim
    , quotedbl
    , hash
    , dollar
    , ampersand
    , quote
    , leftparen
    , rightparen
    , asterisk
    , plus
    , comma
    , minus
    , period
    , slash
    , k0
    , k1
    , k2
    , k3
    , k4
    , k5
    , k6
    , k7
    , k8
    , k9
    , colon
    , semicolon
    , less
    , equals
    , greater
    , question
    , at
    , leftbracket
    , backslash
    , rightbracket
    , caret
    , underscore
    , backquote
    , a
    , b
    , c
    , d
    , e
    , f
    , g
    , h
    , i
    , j
    , k
    , l
    , m
    , n
    , o
    , p
    , q
    , r
    , s
    , t
    , u
    , v
    , w
    , x
    , y
    , z
    , delete
    , world0
    , world1
    , world2
    , world3
    , world4
    , world5
    , world6
    , world7
    , world8
    , world9
    , world10
    , world11
    , world12
    , world13
    , world14
    , world15
    , world16
    , world17
    , world18
    , world19
    , world20
    , world21
    , world22
    , world23
    , world24
    , world25
    , world26
    , world27
    , world28
    , world29
    , world30
    , world31
    , world32
    , world33
    , world34
    , world35
    , world36
    , world37
    , world38
    , world39
    , world40
    , world41
    , world42
    , world43
    , world44
    , world45
    , world46
    , world47
    , world48
    , world49
    , world50
    , world51
    , world52
    , world53
    , world54
    , world55
    , world56
    , world57
    , world58
    , world59
    , world60
    , world61
    , world62
    , world63
    , world64
    , world65
    , world66
    , world67
    , world68
    , world69
    , world70
    , world71
    , world72
    , world73
    , world74
    , world75
    , world76
    , world77
    , world78
    , world79
    , world80
    , world81
    , world82
    , world83
    , world84
    , world85
    , world86
    , world87
    , world88
    , world89
    , world90
    , world91
    , world92
    , world93
    , world94
    , world95
    , kp0
    , kp1
    , kp2
    , kp3
    , kp4
    , kp5
    , kp6
    , kp7
    , kp8
    , kp9
    , kpperiod
    , kpdivide
    , kpmultiply
    , kpminus
    , kpplus
    , kpenter
    , kpequals
    , up
    , down
    , right
    , left
    , insert
    , home
    , end
    , pageup
    , pagedown
    , f1
    , f2
    , f3
    , f4
    , f5
    , f6
    , f7
    , f8
    , f9
    , f10
    , f11
    , f12
    , f13
    , f14
    , f15
    , numlock
    , capslock
    , scrollock
    , rshift
    , lshift
    , rctrl
    , lctrl
    , ralt
    , lalt
    , rmeta
    , lmeta
    , lsuper
    , rsuper
    , mode
    , compose
    , help
    , print
    , sysreq
    , break
    , menu
    , power
    , euro
    , undo
    ) where


--------------------------------------------------------------------------------
import           Prelude                hiding (break, print, return)


--------------------------------------------------------------------------------
import           Firefly.Input.Internal


--------------------------------------------------------------------------------
#include <SDL/SDL.h>


--------------------------------------------------------------------------------

#{enum Key, Key, unknown = SDLK_UNKNOWN}
{-# INLINE unknown #-}
#{enum Key, Key, first = SDLK_FIRST}
{-# INLINE first #-}
#{enum Key, Key, backspace = SDLK_BACKSPACE}
{-# INLINE backspace #-}
#{enum Key, Key, tab = SDLK_TAB}
{-# INLINE tab #-}
#{enum Key, Key, clear = SDLK_CLEAR}
{-# INLINE clear #-}
#{enum Key, Key, return = SDLK_RETURN}
{-# INLINE return #-}
#{enum Key, Key, pause = SDLK_PAUSE}
{-# INLINE pause #-}
#{enum Key, Key, escape = SDLK_ESCAPE}
{-# INLINE escape #-}
#{enum Key, Key, space = SDLK_SPACE}
{-# INLINE space #-}
#{enum Key, Key, exclaim = SDLK_EXCLAIM}
{-# INLINE exclaim #-}
#{enum Key, Key, quotedbl = SDLK_QUOTEDBL}
{-# INLINE quotedbl #-}
#{enum Key, Key, hash = SDLK_HASH}
{-# INLINE hash #-}
#{enum Key, Key, dollar = SDLK_DOLLAR}
{-# INLINE dollar #-}
#{enum Key, Key, ampersand = SDLK_AMPERSAND}
{-# INLINE ampersand #-}
#{enum Key, Key, quote = SDLK_QUOTE}
{-# INLINE quote #-}
#{enum Key, Key, leftparen = SDLK_LEFTPAREN}
{-# INLINE leftparen #-}
#{enum Key, Key, rightparen = SDLK_RIGHTPAREN}
{-# INLINE rightparen #-}
#{enum Key, Key, asterisk = SDLK_ASTERISK}
{-# INLINE asterisk #-}
#{enum Key, Key, plus = SDLK_PLUS}
{-# INLINE plus #-}
#{enum Key, Key, comma = SDLK_COMMA}
{-# INLINE comma #-}
#{enum Key, Key, minus = SDLK_MINUS}
{-# INLINE minus #-}
#{enum Key, Key, period = SDLK_PERIOD}
{-# INLINE period #-}
#{enum Key, Key, slash = SDLK_SLASH}
{-# INLINE slash #-}
#{enum Key, Key, k0 = SDLK_0}
{-# INLINE k0 #-}
#{enum Key, Key, k1 = SDLK_1}
{-# INLINE k1 #-}
#{enum Key, Key, k2 = SDLK_2}
{-# INLINE k2 #-}
#{enum Key, Key, k3 = SDLK_3}
{-# INLINE k3 #-}
#{enum Key, Key, k4 = SDLK_4}
{-# INLINE k4 #-}
#{enum Key, Key, k5 = SDLK_5}
{-# INLINE k5 #-}
#{enum Key, Key, k6 = SDLK_6}
{-# INLINE k6 #-}
#{enum Key, Key, k7 = SDLK_7}
{-# INLINE k7 #-}
#{enum Key, Key, k8 = SDLK_8}
{-# INLINE k8 #-}
#{enum Key, Key, k9 = SDLK_9}
{-# INLINE k9 #-}
#{enum Key, Key, colon = SDLK_COLON}
{-# INLINE colon #-}
#{enum Key, Key, semicolon = SDLK_SEMICOLON}
{-# INLINE semicolon #-}
#{enum Key, Key, less = SDLK_LESS}
{-# INLINE less #-}
#{enum Key, Key, equals = SDLK_EQUALS}
{-# INLINE equals #-}
#{enum Key, Key, greater = SDLK_GREATER}
{-# INLINE greater #-}
#{enum Key, Key, question = SDLK_QUESTION}
{-# INLINE question #-}
#{enum Key, Key, at = SDLK_AT}
{-# INLINE at #-}
#{enum Key, Key, leftbracket = SDLK_LEFTBRACKET}
{-# INLINE leftbracket #-}
#{enum Key, Key, backslash = SDLK_BACKSLASH}
{-# INLINE backslash #-}
#{enum Key, Key, rightbracket = SDLK_RIGHTBRACKET}
{-# INLINE rightbracket #-}
#{enum Key, Key, caret = SDLK_CARET}
{-# INLINE caret #-}
#{enum Key, Key, underscore = SDLK_UNDERSCORE}
{-# INLINE underscore #-}
#{enum Key, Key, backquote = SDLK_BACKQUOTE}
{-# INLINE backquote #-}
#{enum Key, Key, a = SDLK_a}
{-# INLINE a #-}
#{enum Key, Key, b = SDLK_b}
{-# INLINE b #-}
#{enum Key, Key, c = SDLK_c}
{-# INLINE c #-}
#{enum Key, Key, d = SDLK_d}
{-# INLINE d #-}
#{enum Key, Key, e = SDLK_e}
{-# INLINE e #-}
#{enum Key, Key, f = SDLK_f}
{-# INLINE f #-}
#{enum Key, Key, g = SDLK_g}
{-# INLINE g #-}
#{enum Key, Key, h = SDLK_h}
{-# INLINE h #-}
#{enum Key, Key, i = SDLK_i}
{-# INLINE i #-}
#{enum Key, Key, j = SDLK_j}
{-# INLINE j #-}
#{enum Key, Key, k = SDLK_k}
{-# INLINE k #-}
#{enum Key, Key, l = SDLK_l}
{-# INLINE l #-}
#{enum Key, Key, m = SDLK_m}
{-# INLINE m #-}
#{enum Key, Key, n = SDLK_n}
{-# INLINE n #-}
#{enum Key, Key, o = SDLK_o}
{-# INLINE o #-}
#{enum Key, Key, p = SDLK_p}
{-# INLINE p #-}
#{enum Key, Key, q = SDLK_q}
{-# INLINE q #-}
#{enum Key, Key, r = SDLK_r}
{-# INLINE r #-}
#{enum Key, Key, s = SDLK_s}
{-# INLINE s #-}
#{enum Key, Key, t = SDLK_t}
{-# INLINE t #-}
#{enum Key, Key, u = SDLK_u}
{-# INLINE u #-}
#{enum Key, Key, v = SDLK_v}
{-# INLINE v #-}
#{enum Key, Key, w = SDLK_w}
{-# INLINE w #-}
#{enum Key, Key, x = SDLK_x}
{-# INLINE x #-}
#{enum Key, Key, y = SDLK_y}
{-# INLINE y #-}
#{enum Key, Key, z = SDLK_z}
{-# INLINE z #-}
#{enum Key, Key, delete = SDLK_DELETE}
{-# INLINE delete #-}
#{enum Key, Key, world0 = SDLK_WORLD_0}
{-# INLINE world0 #-}
#{enum Key, Key, world1 = SDLK_WORLD_1}
{-# INLINE world1 #-}
#{enum Key, Key, world2 = SDLK_WORLD_2}
{-# INLINE world2 #-}
#{enum Key, Key, world3 = SDLK_WORLD_3}
{-# INLINE world3 #-}
#{enum Key, Key, world4 = SDLK_WORLD_4}
{-# INLINE world4 #-}
#{enum Key, Key, world5 = SDLK_WORLD_5}
{-# INLINE world5 #-}
#{enum Key, Key, world6 = SDLK_WORLD_6}
{-# INLINE world6 #-}
#{enum Key, Key, world7 = SDLK_WORLD_7}
{-# INLINE world7 #-}
#{enum Key, Key, world8 = SDLK_WORLD_8}
{-# INLINE world8 #-}
#{enum Key, Key, world9 = SDLK_WORLD_9}
{-# INLINE world9 #-}
#{enum Key, Key, world10 = SDLK_WORLD_10}
{-# INLINE world10 #-}
#{enum Key, Key, world11 = SDLK_WORLD_11}
{-# INLINE world11 #-}
#{enum Key, Key, world12 = SDLK_WORLD_12}
{-# INLINE world12 #-}
#{enum Key, Key, world13 = SDLK_WORLD_13}
{-# INLINE world13 #-}
#{enum Key, Key, world14 = SDLK_WORLD_14}
{-# INLINE world14 #-}
#{enum Key, Key, world15 = SDLK_WORLD_15}
{-# INLINE world15 #-}
#{enum Key, Key, world16 = SDLK_WORLD_16}
{-# INLINE world16 #-}
#{enum Key, Key, world17 = SDLK_WORLD_17}
{-# INLINE world17 #-}
#{enum Key, Key, world18 = SDLK_WORLD_18}
{-# INLINE world18 #-}
#{enum Key, Key, world19 = SDLK_WORLD_19}
{-# INLINE world19 #-}
#{enum Key, Key, world20 = SDLK_WORLD_20}
{-# INLINE world20 #-}
#{enum Key, Key, world21 = SDLK_WORLD_21}
{-# INLINE world21 #-}
#{enum Key, Key, world22 = SDLK_WORLD_22}
{-# INLINE world22 #-}
#{enum Key, Key, world23 = SDLK_WORLD_23}
{-# INLINE world23 #-}
#{enum Key, Key, world24 = SDLK_WORLD_24}
{-# INLINE world24 #-}
#{enum Key, Key, world25 = SDLK_WORLD_25}
{-# INLINE world25 #-}
#{enum Key, Key, world26 = SDLK_WORLD_26}
{-# INLINE world26 #-}
#{enum Key, Key, world27 = SDLK_WORLD_27}
{-# INLINE world27 #-}
#{enum Key, Key, world28 = SDLK_WORLD_28}
{-# INLINE world28 #-}
#{enum Key, Key, world29 = SDLK_WORLD_29}
{-# INLINE world29 #-}
#{enum Key, Key, world30 = SDLK_WORLD_30}
{-# INLINE world30 #-}
#{enum Key, Key, world31 = SDLK_WORLD_31}
{-# INLINE world31 #-}
#{enum Key, Key, world32 = SDLK_WORLD_32}
{-# INLINE world32 #-}
#{enum Key, Key, world33 = SDLK_WORLD_33}
{-# INLINE world33 #-}
#{enum Key, Key, world34 = SDLK_WORLD_34}
{-# INLINE world34 #-}
#{enum Key, Key, world35 = SDLK_WORLD_35}
{-# INLINE world35 #-}
#{enum Key, Key, world36 = SDLK_WORLD_36}
{-# INLINE world36 #-}
#{enum Key, Key, world37 = SDLK_WORLD_37}
{-# INLINE world37 #-}
#{enum Key, Key, world38 = SDLK_WORLD_38}
{-# INLINE world38 #-}
#{enum Key, Key, world39 = SDLK_WORLD_39}
{-# INLINE world39 #-}
#{enum Key, Key, world40 = SDLK_WORLD_40}
{-# INLINE world40 #-}
#{enum Key, Key, world41 = SDLK_WORLD_41}
{-# INLINE world41 #-}
#{enum Key, Key, world42 = SDLK_WORLD_42}
{-# INLINE world42 #-}
#{enum Key, Key, world43 = SDLK_WORLD_43}
{-# INLINE world43 #-}
#{enum Key, Key, world44 = SDLK_WORLD_44}
{-# INLINE world44 #-}
#{enum Key, Key, world45 = SDLK_WORLD_45}
{-# INLINE world45 #-}
#{enum Key, Key, world46 = SDLK_WORLD_46}
{-# INLINE world46 #-}
#{enum Key, Key, world47 = SDLK_WORLD_47}
{-# INLINE world47 #-}
#{enum Key, Key, world48 = SDLK_WORLD_48}
{-# INLINE world48 #-}
#{enum Key, Key, world49 = SDLK_WORLD_49}
{-# INLINE world49 #-}
#{enum Key, Key, world50 = SDLK_WORLD_50}
{-# INLINE world50 #-}
#{enum Key, Key, world51 = SDLK_WORLD_51}
{-# INLINE world51 #-}
#{enum Key, Key, world52 = SDLK_WORLD_52}
{-# INLINE world52 #-}
#{enum Key, Key, world53 = SDLK_WORLD_53}
{-# INLINE world53 #-}
#{enum Key, Key, world54 = SDLK_WORLD_54}
{-# INLINE world54 #-}
#{enum Key, Key, world55 = SDLK_WORLD_55}
{-# INLINE world55 #-}
#{enum Key, Key, world56 = SDLK_WORLD_56}
{-# INLINE world56 #-}
#{enum Key, Key, world57 = SDLK_WORLD_57}
{-# INLINE world57 #-}
#{enum Key, Key, world58 = SDLK_WORLD_58}
{-# INLINE world58 #-}
#{enum Key, Key, world59 = SDLK_WORLD_59}
{-# INLINE world59 #-}
#{enum Key, Key, world60 = SDLK_WORLD_60}
{-# INLINE world60 #-}
#{enum Key, Key, world61 = SDLK_WORLD_61}
{-# INLINE world61 #-}
#{enum Key, Key, world62 = SDLK_WORLD_62}
{-# INLINE world62 #-}
#{enum Key, Key, world63 = SDLK_WORLD_63}
{-# INLINE world63 #-}
#{enum Key, Key, world64 = SDLK_WORLD_64}
{-# INLINE world64 #-}
#{enum Key, Key, world65 = SDLK_WORLD_65}
{-# INLINE world65 #-}
#{enum Key, Key, world66 = SDLK_WORLD_66}
{-# INLINE world66 #-}
#{enum Key, Key, world67 = SDLK_WORLD_67}
{-# INLINE world67 #-}
#{enum Key, Key, world68 = SDLK_WORLD_68}
{-# INLINE world68 #-}
#{enum Key, Key, world69 = SDLK_WORLD_69}
{-# INLINE world69 #-}
#{enum Key, Key, world70 = SDLK_WORLD_70}
{-# INLINE world70 #-}
#{enum Key, Key, world71 = SDLK_WORLD_71}
{-# INLINE world71 #-}
#{enum Key, Key, world72 = SDLK_WORLD_72}
{-# INLINE world72 #-}
#{enum Key, Key, world73 = SDLK_WORLD_73}
{-# INLINE world73 #-}
#{enum Key, Key, world74 = SDLK_WORLD_74}
{-# INLINE world74 #-}
#{enum Key, Key, world75 = SDLK_WORLD_75}
{-# INLINE world75 #-}
#{enum Key, Key, world76 = SDLK_WORLD_76}
{-# INLINE world76 #-}
#{enum Key, Key, world77 = SDLK_WORLD_77}
{-# INLINE world77 #-}
#{enum Key, Key, world78 = SDLK_WORLD_78}
{-# INLINE world78 #-}
#{enum Key, Key, world79 = SDLK_WORLD_79}
{-# INLINE world79 #-}
#{enum Key, Key, world80 = SDLK_WORLD_80}
{-# INLINE world80 #-}
#{enum Key, Key, world81 = SDLK_WORLD_81}
{-# INLINE world81 #-}
#{enum Key, Key, world82 = SDLK_WORLD_82}
{-# INLINE world82 #-}
#{enum Key, Key, world83 = SDLK_WORLD_83}
{-# INLINE world83 #-}
#{enum Key, Key, world84 = SDLK_WORLD_84}
{-# INLINE world84 #-}
#{enum Key, Key, world85 = SDLK_WORLD_85}
{-# INLINE world85 #-}
#{enum Key, Key, world86 = SDLK_WORLD_86}
{-# INLINE world86 #-}
#{enum Key, Key, world87 = SDLK_WORLD_87}
{-# INLINE world87 #-}
#{enum Key, Key, world88 = SDLK_WORLD_88}
{-# INLINE world88 #-}
#{enum Key, Key, world89 = SDLK_WORLD_89}
{-# INLINE world89 #-}
#{enum Key, Key, world90 = SDLK_WORLD_90}
{-# INLINE world90 #-}
#{enum Key, Key, world91 = SDLK_WORLD_91}
{-# INLINE world91 #-}
#{enum Key, Key, world92 = SDLK_WORLD_92}
{-# INLINE world92 #-}
#{enum Key, Key, world93 = SDLK_WORLD_93}
{-# INLINE world93 #-}
#{enum Key, Key, world94 = SDLK_WORLD_94}
{-# INLINE world94 #-}
#{enum Key, Key, world95 = SDLK_WORLD_95}
{-# INLINE world95 #-}
#{enum Key, Key, kp0 = SDLK_KP0}
{-# INLINE kp0 #-}
#{enum Key, Key, kp1 = SDLK_KP1}
{-# INLINE kp1 #-}
#{enum Key, Key, kp2 = SDLK_KP2}
{-# INLINE kp2 #-}
#{enum Key, Key, kp3 = SDLK_KP3}
{-# INLINE kp3 #-}
#{enum Key, Key, kp4 = SDLK_KP4}
{-# INLINE kp4 #-}
#{enum Key, Key, kp5 = SDLK_KP5}
{-# INLINE kp5 #-}
#{enum Key, Key, kp6 = SDLK_KP6}
{-# INLINE kp6 #-}
#{enum Key, Key, kp7 = SDLK_KP7}
{-# INLINE kp7 #-}
#{enum Key, Key, kp8 = SDLK_KP8}
{-# INLINE kp8 #-}
#{enum Key, Key, kp9 = SDLK_KP9}
{-# INLINE kp9 #-}
#{enum Key, Key, kpperiod = SDLK_KP_PERIOD}
{-# INLINE kpperiod #-}
#{enum Key, Key, kpdivide = SDLK_KP_DIVIDE}
{-# INLINE kpdivide #-}
#{enum Key, Key, kpmultiply = SDLK_KP_MULTIPLY}
{-# INLINE kpmultiply #-}
#{enum Key, Key, kpminus = SDLK_KP_MINUS}
{-# INLINE kpminus #-}
#{enum Key, Key, kpplus = SDLK_KP_PLUS}
{-# INLINE kpplus #-}
#{enum Key, Key, kpenter = SDLK_KP_ENTER}
{-# INLINE kpenter #-}
#{enum Key, Key, kpequals = SDLK_KP_EQUALS}
{-# INLINE kpequals #-}
#{enum Key, Key, up = SDLK_UP}
{-# INLINE up #-}
#{enum Key, Key, down = SDLK_DOWN}
{-# INLINE down #-}
#{enum Key, Key, right = SDLK_RIGHT}
{-# INLINE right #-}
#{enum Key, Key, left = SDLK_LEFT}
{-# INLINE left #-}
#{enum Key, Key, insert = SDLK_INSERT}
{-# INLINE insert #-}
#{enum Key, Key, home = SDLK_HOME}
{-# INLINE home #-}
#{enum Key, Key, end = SDLK_END}
{-# INLINE end #-}
#{enum Key, Key, pageup = SDLK_PAGEUP}
{-# INLINE pageup #-}
#{enum Key, Key, pagedown = SDLK_PAGEDOWN}
{-# INLINE pagedown #-}
#{enum Key, Key, f1 = SDLK_F1}
{-# INLINE f1 #-}
#{enum Key, Key, f2 = SDLK_F2}
{-# INLINE f2 #-}
#{enum Key, Key, f3 = SDLK_F3}
{-# INLINE f3 #-}
#{enum Key, Key, f4 = SDLK_F4}
{-# INLINE f4 #-}
#{enum Key, Key, f5 = SDLK_F5}
{-# INLINE f5 #-}
#{enum Key, Key, f6 = SDLK_F6}
{-# INLINE f6 #-}
#{enum Key, Key, f7 = SDLK_F7}
{-# INLINE f7 #-}
#{enum Key, Key, f8 = SDLK_F8}
{-# INLINE f8 #-}
#{enum Key, Key, f9 = SDLK_F9}
{-# INLINE f9 #-}
#{enum Key, Key, f10 = SDLK_F10}
{-# INLINE f10 #-}
#{enum Key, Key, f11 = SDLK_F11}
{-# INLINE f11 #-}
#{enum Key, Key, f12 = SDLK_F12}
{-# INLINE f12 #-}
#{enum Key, Key, f13 = SDLK_F13}
{-# INLINE f13 #-}
#{enum Key, Key, f14 = SDLK_F14}
{-# INLINE f14 #-}
#{enum Key, Key, f15 = SDLK_F15}
{-# INLINE f15 #-}
#{enum Key, Key, numlock = SDLK_NUMLOCK}
{-# INLINE numlock #-}
#{enum Key, Key, capslock = SDLK_CAPSLOCK}
{-# INLINE capslock #-}
#{enum Key, Key, scrollock = SDLK_SCROLLOCK}
{-# INLINE scrollock #-}
#{enum Key, Key, rshift = SDLK_RSHIFT}
{-# INLINE rshift #-}
#{enum Key, Key, lshift = SDLK_LSHIFT}
{-# INLINE lshift #-}
#{enum Key, Key, rctrl = SDLK_RCTRL}
{-# INLINE rctrl #-}
#{enum Key, Key, lctrl = SDLK_LCTRL}
{-# INLINE lctrl #-}
#{enum Key, Key, ralt = SDLK_RALT}
{-# INLINE ralt #-}
#{enum Key, Key, lalt = SDLK_LALT}
{-# INLINE lalt #-}
#{enum Key, Key, rmeta = SDLK_RMETA}
{-# INLINE rmeta #-}
#{enum Key, Key, lmeta = SDLK_LMETA}
{-# INLINE lmeta #-}
#{enum Key, Key, lsuper = SDLK_LSUPER}
{-# INLINE lsuper #-}
#{enum Key, Key, rsuper = SDLK_RSUPER}
{-# INLINE rsuper #-}
#{enum Key, Key, mode = SDLK_MODE}
{-# INLINE mode #-}
#{enum Key, Key, compose = SDLK_COMPOSE}
{-# INLINE compose #-}
#{enum Key, Key, help = SDLK_HELP}
{-# INLINE help #-}
#{enum Key, Key, print = SDLK_PRINT}
{-# INLINE print #-}
#{enum Key, Key, sysreq = SDLK_SYSREQ}
{-# INLINE sysreq #-}
#{enum Key, Key, break = SDLK_BREAK}
{-# INLINE break #-}
#{enum Key, Key, menu = SDLK_MENU}
{-# INLINE menu #-}
#{enum Key, Key, power = SDLK_POWER}
{-# INLINE power #-}
#{enum Key, Key, euro = SDLK_EURO}
{-# INLINE euro #-}
#{enum Key, Key, undo = SDLK_UNDO}
{-# INLINE undo #-}
