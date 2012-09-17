--------------------------------------------------------------------------------
-- | Designed for qualified import...
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
unknown :: Key
unknown = Key 0
{-# INLINE unknown #-}

first :: Key
first = Key 0
{-# INLINE first #-}

backspace :: Key
backspace = Key 8
{-# INLINE backspace #-}

tab :: Key
tab = Key 9
{-# INLINE tab #-}

clear :: Key
clear = Key 12
{-# INLINE clear #-}

return :: Key
return = Key 13
{-# INLINE return #-}

pause :: Key
pause = Key 19
{-# INLINE pause #-}

escape :: Key
escape = Key 27
{-# INLINE escape #-}

space :: Key
space = Key 32
{-# INLINE space #-}

exclaim :: Key
exclaim = Key 33
{-# INLINE exclaim #-}

quotedbl :: Key
quotedbl = Key 34
{-# INLINE quotedbl #-}

hash :: Key
hash = Key 35
{-# INLINE hash #-}

dollar :: Key
dollar = Key 36
{-# INLINE dollar #-}

ampersand :: Key
ampersand = Key 38
{-# INLINE ampersand #-}

quote :: Key
quote = Key 39
{-# INLINE quote #-}

leftparen :: Key
leftparen = Key 40
{-# INLINE leftparen #-}

rightparen :: Key
rightparen = Key 41
{-# INLINE rightparen #-}

asterisk :: Key
asterisk = Key 42
{-# INLINE asterisk #-}

plus :: Key
plus = Key 43
{-# INLINE plus #-}

comma :: Key
comma = Key 44
{-# INLINE comma #-}

minus :: Key
minus = Key 45
{-# INLINE minus #-}

period :: Key
period = Key 46
{-# INLINE period #-}

slash :: Key
slash = Key 47
{-# INLINE slash #-}

k0 :: Key
k0 = Key 48
{-# INLINE k0 #-}

k1 :: Key
k1 = Key 49
{-# INLINE k1 #-}

k2 :: Key
k2 = Key 50
{-# INLINE k2 #-}

k3 :: Key
k3 = Key 51
{-# INLINE k3 #-}

k4 :: Key
k4 = Key 52
{-# INLINE k4 #-}

k5 :: Key
k5 = Key 53
{-# INLINE k5 #-}

k6 :: Key
k6 = Key 54
{-# INLINE k6 #-}

k7 :: Key
k7 = Key 55
{-# INLINE k7 #-}

k8 :: Key
k8 = Key 56
{-# INLINE k8 #-}

k9 :: Key
k9 = Key 57
{-# INLINE k9 #-}

colon :: Key
colon = Key 58
{-# INLINE colon #-}

semicolon :: Key
semicolon = Key 59
{-# INLINE semicolon #-}

less :: Key
less = Key 60
{-# INLINE less #-}

equals :: Key
equals = Key 61
{-# INLINE equals #-}

greater :: Key
greater = Key 62
{-# INLINE greater #-}

question :: Key
question = Key 63
{-# INLINE question #-}

at :: Key
at = Key 64
{-# INLINE at #-}

leftbracket :: Key
leftbracket = Key 91
{-# INLINE leftbracket #-}

backslash :: Key
backslash = Key 92
{-# INLINE backslash #-}

rightbracket :: Key
rightbracket = Key 93
{-# INLINE rightbracket #-}

caret :: Key
caret = Key 94
{-# INLINE caret #-}

underscore :: Key
underscore = Key 95
{-# INLINE underscore #-}

backquote :: Key
backquote = Key 96
{-# INLINE backquote #-}

a :: Key
a = Key 97
{-# INLINE a #-}

b :: Key
b = Key 98
{-# INLINE b #-}

c :: Key
c = Key 99
{-# INLINE c #-}

d :: Key
d = Key 100
{-# INLINE d #-}

e :: Key
e = Key 101
{-# INLINE e #-}

f :: Key
f = Key 102
{-# INLINE f #-}

g :: Key
g = Key 103
{-# INLINE g #-}

h :: Key
h = Key 104
{-# INLINE h #-}

i :: Key
i = Key 105
{-# INLINE i #-}

j :: Key
j = Key 106
{-# INLINE j #-}

k :: Key
k = Key 107
{-# INLINE k #-}

l :: Key
l = Key 108
{-# INLINE l #-}

m :: Key
m = Key 109
{-# INLINE m #-}

n :: Key
n = Key 110
{-# INLINE n #-}

o :: Key
o = Key 111
{-# INLINE o #-}

p :: Key
p = Key 112
{-# INLINE p #-}

q :: Key
q = Key 113
{-# INLINE q #-}

r :: Key
r = Key 114
{-# INLINE r #-}

s :: Key
s = Key 115
{-# INLINE s #-}

t :: Key
t = Key 116
{-# INLINE t #-}

u :: Key
u = Key 117
{-# INLINE u #-}

v :: Key
v = Key 118
{-# INLINE v #-}

w :: Key
w = Key 119
{-# INLINE w #-}

x :: Key
x = Key 120
{-# INLINE x #-}

y :: Key
y = Key 121
{-# INLINE y #-}

z :: Key
z = Key 122
{-# INLINE z #-}

delete :: Key
delete = Key 127
{-# INLINE delete #-}

world0 :: Key
world0 = Key 160
{-# INLINE world0 #-}

world1 :: Key
world1 = Key 161
{-# INLINE world1 #-}

world2 :: Key
world2 = Key 162
{-# INLINE world2 #-}

world3 :: Key
world3 = Key 163
{-# INLINE world3 #-}

world4 :: Key
world4 = Key 164
{-# INLINE world4 #-}

world5 :: Key
world5 = Key 165
{-# INLINE world5 #-}

world6 :: Key
world6 = Key 166
{-# INLINE world6 #-}

world7 :: Key
world7 = Key 167
{-# INLINE world7 #-}

world8 :: Key
world8 = Key 168
{-# INLINE world8 #-}

world9 :: Key
world9 = Key 169
{-# INLINE world9 #-}

world10 :: Key
world10 = Key 170
{-# INLINE world10 #-}

world11 :: Key
world11 = Key 171
{-# INLINE world11 #-}

world12 :: Key
world12 = Key 172
{-# INLINE world12 #-}

world13 :: Key
world13 = Key 173
{-# INLINE world13 #-}

world14 :: Key
world14 = Key 174
{-# INLINE world14 #-}

world15 :: Key
world15 = Key 175
{-# INLINE world15 #-}

world16 :: Key
world16 = Key 176
{-# INLINE world16 #-}

world17 :: Key
world17 = Key 177
{-# INLINE world17 #-}

world18 :: Key
world18 = Key 178
{-# INLINE world18 #-}

world19 :: Key
world19 = Key 179
{-# INLINE world19 #-}

world20 :: Key
world20 = Key 180
{-# INLINE world20 #-}

world21 :: Key
world21 = Key 181
{-# INLINE world21 #-}

world22 :: Key
world22 = Key 182
{-# INLINE world22 #-}

world23 :: Key
world23 = Key 183
{-# INLINE world23 #-}

world24 :: Key
world24 = Key 184
{-# INLINE world24 #-}

world25 :: Key
world25 = Key 185
{-# INLINE world25 #-}

world26 :: Key
world26 = Key 186
{-# INLINE world26 #-}

world27 :: Key
world27 = Key 187
{-# INLINE world27 #-}

world28 :: Key
world28 = Key 188
{-# INLINE world28 #-}

world29 :: Key
world29 = Key 189
{-# INLINE world29 #-}

world30 :: Key
world30 = Key 190
{-# INLINE world30 #-}

world31 :: Key
world31 = Key 191
{-# INLINE world31 #-}

world32 :: Key
world32 = Key 192
{-# INLINE world32 #-}

world33 :: Key
world33 = Key 193
{-# INLINE world33 #-}

world34 :: Key
world34 = Key 194
{-# INLINE world34 #-}

world35 :: Key
world35 = Key 195
{-# INLINE world35 #-}

world36 :: Key
world36 = Key 196
{-# INLINE world36 #-}

world37 :: Key
world37 = Key 197
{-# INLINE world37 #-}

world38 :: Key
world38 = Key 198
{-# INLINE world38 #-}

world39 :: Key
world39 = Key 199
{-# INLINE world39 #-}

world40 :: Key
world40 = Key 200
{-# INLINE world40 #-}

world41 :: Key
world41 = Key 201
{-# INLINE world41 #-}

world42 :: Key
world42 = Key 202
{-# INLINE world42 #-}

world43 :: Key
world43 = Key 203
{-# INLINE world43 #-}

world44 :: Key
world44 = Key 204
{-# INLINE world44 #-}

world45 :: Key
world45 = Key 205
{-# INLINE world45 #-}

world46 :: Key
world46 = Key 206
{-# INLINE world46 #-}

world47 :: Key
world47 = Key 207
{-# INLINE world47 #-}

world48 :: Key
world48 = Key 208
{-# INLINE world48 #-}

world49 :: Key
world49 = Key 209
{-# INLINE world49 #-}

world50 :: Key
world50 = Key 210
{-# INLINE world50 #-}

world51 :: Key
world51 = Key 211
{-# INLINE world51 #-}

world52 :: Key
world52 = Key 212
{-# INLINE world52 #-}

world53 :: Key
world53 = Key 213
{-# INLINE world53 #-}

world54 :: Key
world54 = Key 214
{-# INLINE world54 #-}

world55 :: Key
world55 = Key 215
{-# INLINE world55 #-}

world56 :: Key
world56 = Key 216
{-# INLINE world56 #-}

world57 :: Key
world57 = Key 217
{-# INLINE world57 #-}

world58 :: Key
world58 = Key 218
{-# INLINE world58 #-}

world59 :: Key
world59 = Key 219
{-# INLINE world59 #-}

world60 :: Key
world60 = Key 220
{-# INLINE world60 #-}

world61 :: Key
world61 = Key 221
{-# INLINE world61 #-}

world62 :: Key
world62 = Key 222
{-# INLINE world62 #-}

world63 :: Key
world63 = Key 223
{-# INLINE world63 #-}

world64 :: Key
world64 = Key 224
{-# INLINE world64 #-}

world65 :: Key
world65 = Key 225
{-# INLINE world65 #-}

world66 :: Key
world66 = Key 226
{-# INLINE world66 #-}

world67 :: Key
world67 = Key 227
{-# INLINE world67 #-}

world68 :: Key
world68 = Key 228
{-# INLINE world68 #-}

world69 :: Key
world69 = Key 229
{-# INLINE world69 #-}

world70 :: Key
world70 = Key 230
{-# INLINE world70 #-}

world71 :: Key
world71 = Key 231
{-# INLINE world71 #-}

world72 :: Key
world72 = Key 232
{-# INLINE world72 #-}

world73 :: Key
world73 = Key 233
{-# INLINE world73 #-}

world74 :: Key
world74 = Key 234
{-# INLINE world74 #-}

world75 :: Key
world75 = Key 235
{-# INLINE world75 #-}

world76 :: Key
world76 = Key 236
{-# INLINE world76 #-}

world77 :: Key
world77 = Key 237
{-# INLINE world77 #-}

world78 :: Key
world78 = Key 238
{-# INLINE world78 #-}

world79 :: Key
world79 = Key 239
{-# INLINE world79 #-}

world80 :: Key
world80 = Key 240
{-# INLINE world80 #-}

world81 :: Key
world81 = Key 241
{-# INLINE world81 #-}

world82 :: Key
world82 = Key 242
{-# INLINE world82 #-}

world83 :: Key
world83 = Key 243
{-# INLINE world83 #-}

world84 :: Key
world84 = Key 244
{-# INLINE world84 #-}

world85 :: Key
world85 = Key 245
{-# INLINE world85 #-}

world86 :: Key
world86 = Key 246
{-# INLINE world86 #-}

world87 :: Key
world87 = Key 247
{-# INLINE world87 #-}

world88 :: Key
world88 = Key 248
{-# INLINE world88 #-}

world89 :: Key
world89 = Key 249
{-# INLINE world89 #-}

world90 :: Key
world90 = Key 250
{-# INLINE world90 #-}

world91 :: Key
world91 = Key 251
{-# INLINE world91 #-}

world92 :: Key
world92 = Key 252
{-# INLINE world92 #-}

world93 :: Key
world93 = Key 253
{-# INLINE world93 #-}

world94 :: Key
world94 = Key 254
{-# INLINE world94 #-}

world95 :: Key
world95 = Key 255
{-# INLINE world95 #-}

kp0 :: Key
kp0 = Key 256
{-# INLINE kp0 #-}

kp1 :: Key
kp1 = Key 257
{-# INLINE kp1 #-}

kp2 :: Key
kp2 = Key 258
{-# INLINE kp2 #-}

kp3 :: Key
kp3 = Key 259
{-# INLINE kp3 #-}

kp4 :: Key
kp4 = Key 260
{-# INLINE kp4 #-}

kp5 :: Key
kp5 = Key 261
{-# INLINE kp5 #-}

kp6 :: Key
kp6 = Key 262
{-# INLINE kp6 #-}

kp7 :: Key
kp7 = Key 263
{-# INLINE kp7 #-}

kp8 :: Key
kp8 = Key 264
{-# INLINE kp8 #-}

kp9 :: Key
kp9 = Key 265
{-# INLINE kp9 #-}

kpperiod :: Key
kpperiod = Key 266
{-# INLINE kpperiod #-}

kpdivide :: Key
kpdivide = Key 267
{-# INLINE kpdivide #-}

kpmultiply :: Key
kpmultiply = Key 268
{-# INLINE kpmultiply #-}

kpminus :: Key
kpminus = Key 269
{-# INLINE kpminus #-}

kpplus :: Key
kpplus = Key 270
{-# INLINE kpplus #-}

kpenter :: Key
kpenter = Key 271
{-# INLINE kpenter #-}

kpequals :: Key
kpequals = Key 272
{-# INLINE kpequals #-}

up :: Key
up = Key 273
{-# INLINE up #-}

down :: Key
down = Key 274
{-# INLINE down #-}

right :: Key
right = Key 275
{-# INLINE right #-}

left :: Key
left = Key 276
{-# INLINE left #-}

insert :: Key
insert = Key 277
{-# INLINE insert #-}

home :: Key
home = Key 278
{-# INLINE home #-}

end :: Key
end = Key 279
{-# INLINE end #-}

pageup :: Key
pageup = Key 280
{-# INLINE pageup #-}

pagedown :: Key
pagedown = Key 281
{-# INLINE pagedown #-}

f1 :: Key
f1 = Key 282
{-# INLINE f1 #-}

f2 :: Key
f2 = Key 283
{-# INLINE f2 #-}

f3 :: Key
f3 = Key 284
{-# INLINE f3 #-}

f4 :: Key
f4 = Key 285
{-# INLINE f4 #-}

f5 :: Key
f5 = Key 286
{-# INLINE f5 #-}

f6 :: Key
f6 = Key 287
{-# INLINE f6 #-}

f7 :: Key
f7 = Key 288
{-# INLINE f7 #-}

f8 :: Key
f8 = Key 289
{-# INLINE f8 #-}

f9 :: Key
f9 = Key 290
{-# INLINE f9 #-}

f10 :: Key
f10 = Key 291
{-# INLINE f10 #-}

f11 :: Key
f11 = Key 292
{-# INLINE f11 #-}

f12 :: Key
f12 = Key 293
{-# INLINE f12 #-}

f13 :: Key
f13 = Key 294
{-# INLINE f13 #-}

f14 :: Key
f14 = Key 295
{-# INLINE f14 #-}

f15 :: Key
f15 = Key 296
{-# INLINE f15 #-}

numlock :: Key
numlock = Key 300
{-# INLINE numlock #-}

capslock :: Key
capslock = Key 301
{-# INLINE capslock #-}

scrollock :: Key
scrollock = Key 302
{-# INLINE scrollock #-}

rshift :: Key
rshift = Key 303
{-# INLINE rshift #-}

lshift :: Key
lshift = Key 304
{-# INLINE lshift #-}

rctrl :: Key
rctrl = Key 305
{-# INLINE rctrl #-}

lctrl :: Key
lctrl = Key 306
{-# INLINE lctrl #-}

ralt :: Key
ralt = Key 307
{-# INLINE ralt #-}

lalt :: Key
lalt = Key 308
{-# INLINE lalt #-}

rmeta :: Key
rmeta = Key 309
{-# INLINE rmeta #-}

lmeta :: Key
lmeta = Key 310
{-# INLINE lmeta #-}

lsuper :: Key
lsuper = Key 311
{-# INLINE lsuper #-}

rsuper :: Key
rsuper = Key 312
{-# INLINE rsuper #-}

mode :: Key
mode = Key 313
{-# INLINE mode #-}

compose :: Key
compose = Key 314
{-# INLINE compose #-}

help :: Key
help = Key 315
{-# INLINE help #-}

print :: Key
print = Key 316
{-# INLINE print #-}

sysreq :: Key
sysreq = Key 317
{-# INLINE sysreq #-}

break :: Key
break = Key 318
{-# INLINE break #-}

menu :: Key
menu = Key 319
{-# INLINE menu #-}

power :: Key
power = Key 320
{-# INLINE power #-}

euro :: Key
euro = Key 321
{-# INLINE euro #-}

undo :: Key
undo = Key 322
{-# INLINE undo #-}
