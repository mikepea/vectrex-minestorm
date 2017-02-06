;  'EXECUTIVE' EQUATES
;  ===================
;
;
REG0     equ     $C800            ;  SOUND GENERATOR (PSG) MIRROR IMAGE
REG2     equ     $C802            ;  .
;
TRIGGR   equ     $C80F            ;  COLLECTIVE SWITCH SETTINGS
KEY0     equ     $C812            ;  CONTROLLER #1 - SWITCH #0 (LEFT)
KEY1     equ     $C813            ;  .
KEY2     equ     $C814            ;  .
KEY3     equ     $C815            ;  .             - SWITCH #3 (RIGHT)
;
POT0     equ     $C81B            ;  JOYSTICK #1 - 'X' AXIS
POT1     equ     $C81C            ;  .           - 'Y' AXIS
;
EPOT0    equ     $C81F            ;  ENABLE POT READ
EPOT2    equ     $C821            ;  .
;
LIST     equ     $C823            ;  NUMBER OF VECTORS - 1
ZSKIP    equ     $C824            ;  SKIP INTEGRATOR ZEROING AND ACTIVE GROUND
FRAME    equ     $C826            ;  FRAME COUNTER
;
SIZRAS   equ     $C82A            ;  RASTER MESSAGE SIZE
LEG      equ     $C83B            ;  EXECUTIVE WORKING STORAGE
TSTAT    equ     $C856            ;  TUNE STATUS
;
PLAYRS   equ     $C879            ;  NUMBER OF PLAYERS IN GAME
OPTION   equ     $C87A            ;  GAME OPTION NUMBER
;
HISCOR   equ     $CBEB            ;  HIGH-SCORE
;
;
T1LOLC   equ     $D004            ;  TIMER #1 (LSB)
;
;
PWRUP    equ     $F000            ;  POWER-UP HANDLER
COLD0    equ     $F01C            ;  VECTREX RESTART ENTRY FOR MINE-STORM
INTALL   equ     $F18B            ;  COMPLETE INTIALIZATION
FRWAIT   equ     $F192            ;  WAIT FOR FRAME BOUNDARY
;
DPIO     equ     $F1AA            ;  SET DIRECT REGISTER
DPRAM    equ     $F1AF            ;  .
;
DBNCE    equ     $F1B4            ;  READ CONTROLLER BUTTONS
INPUT    equ     $F1BA            ;  .
JOYBIT   equ     $F1F8            ;  READ JOYSTICKS
;
WRREG    equ     $F256            ;  WRITE TO PSG
INTPSG   equ     $F272            ;  INITIALIZE SOUND GENERATOR
PSGLST   equ     $F27D            ;  SEND SOUND STRING TO PSG
REQOUT   equ     $F289            ;  SEND 'REQX' TO PSG AND MIRROR
;
INT3Q    equ     $F2A5            ;  SET INTENSITY
INTMAX   equ     $F2A9            ;  .
INTENS   equ     $F2AB            ;  .
;
DOTAB    equ     $F2C3            ;  DRAW ONE DOT FROM THE CONTENTS OF 'A' & 'B'
DIFDOT   equ     $F2D5            ;  DRAW DOTS ACCORDING TO 'DIFFY' FORMAT
;
DEFLOK   equ     $F2E6            ;  OVER-COME SCAN COLLAPSE CIRCUITRY
POSWID   equ     $F2F2            ;  POSITION RELATIVE VECTOR
POSITD   equ     $F2FC            ;  .
POSITN   equ     $F312            ;  .
ZERGND   equ     $F354            ;  ZERO INTEGRATORS AND SET ACTIVE GROUND
RSTSIZ   equ     $F373            ;  DISPLAY RASTER MESSAGE
;
TPACK    equ     $F40E            ;  DRAW FROM 'PACKET' STYLE LIST
PACKET   equ     $F410            ;  .
RASTER   equ     $F495            ;  DISPLAY RASTER STRING
;
RANDOM   equ     $F517            ;  CALCULATE RANDOM NUMBER
BCLR     equ     $F53F            ;  CLEAR MEMORY BLOCK
CMPASS   equ     $F593            ;  DETERMINE ANGLE FROM DELTA 'Y:X'
LNROT    equ     $F601            ;  SINGLE LINE ROTATE
PROT     equ     $F61F            ;  'PACKET' STYLE ROTATE
;
REPLAY   equ     $F687            ;  SET 'REQX' FOR GIVEN TUNE
SPLAY    equ     $F68D            ;  .
;
SELOPT   equ     $F7A9            ;  FETCH GAME OPTIONS
;
SCLR     equ     $F84F            ;  CLEAR INDICATED SCORE
SCRADD   equ     $F87C            ;  ADD CONTENTS OF 'D' TO INDICATED SCORE
HISCR    equ     $F8D8            ;  CALCULATE HIGH SCORE AND SAVE FOR LOGO
BXTEST   equ     $F8FF            ;  SYMMETRIC COLLISION TEST
;
VIBENL   equ     $FEB6
;
;
;
;
;  ***************************************
;  ***************************************
;  ***                                 ***
;  ***          E Q U a T E S          ***
;  ***                                 ***
;  ***************************************
;  ***************************************
;
;
RATE     equ     50               ;  FRAME RATE (HERTZ)
;
;
THYPER   equ     $20              ;  HYPER-SPACE TIME
;
BULLETS  equ     4                ;  NUMBER OF BULLETS
MINES    equ     28               ;  NUMBER OF MINES
;
;
PSCOR1   equ     $7FA0            ;  POSITION OF PLAYER #1 SCORE
PSCOR2   equ     $7F10            ;  POSITION OF PLAYER #2 SCORE
;
;
P_SHPSZ  equ     $0C              ;  INITIAL STAR-SWEEPER SIZE
;
P_LYRSZ  equ     $08              ;  INITIAL MINE-LAYER SIZE
P_LYRSP  equ     $10              ;  MINE-LAYER SPEED OFFSET
P_LYRBX  equ     $0616            ;  MINE-LAYER COLLISION BOX
;
;
MIN_SIZ1 equ     $10              ;  MINE #1 (LARGE) SIZE
MIN_SIZ2 equ     $07              ;  MINE #2
MIN_SIZ3 equ     $02              ;  MINE #3 (SMALLEST)
;
MIN_SPD1 equ     $10              ;  MINE #1 (LARGE) SPEED
MIN_SPD2 equ     $18              ;  MINE #2
MIN_SPD3 equ     $20              ;  MINE #3 (SMALLEST)
;
MIN_BOX1 equ     $0D0D            ;  MINE #1 (LARGE) COLLISION BOX
MIN_BOX2 equ     $0808            ;  MINE #2
MIN_BOX3 equ     $0404            ;  MINE #3 (SMALLEST)
;
M_DUMB   equ     $00              ;  'DUMB' MINE FLAG
M_MAG    equ     $01              ;  'MAGNETIC' MINE FLAG
M_FIRE   equ     $02              ;  'FIREBALL' MINE FLAG
M_MFIRE  equ     $03              ;  'MAGNETIC FIREBALL' MINE FLAG
FIRBALL  equ     $04              ;  'RELEASED' FIREBALL
;
;
S_ABRT   equ     KEY3             ;  TITLE PAGE ABORT
S_THRST  equ     KEY2             ;  THRUST SWITCH
S_FIRE   equ     KEY3             ;  FIRE SWITCH
S_HYPER  equ     KEY1             ;  HYPERSPACE SWITCH
