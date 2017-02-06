
;  *******************************************************
;  *******************************************************
;  ***                                                 ***
;  ***          W O R K I N G   S T O R a G E          ***
;  ***                                                 ***
;  *******************************************************
;  *******************************************************

         org     $C880
         bss

SBTN     db      0                ;  CONTROLLER DEBOUNCE FLAGS
SJOY     dw      0                ;  JOYSTICK 'BANG' FLAGS
;
;
ETMP1    db      0                ;  TEMPORARY WORKING STORAGE (FIRST LEVEL)
ETMP2    db      0                ;  .
ETMP3    db      0                ;  .
ETMP4    db      0                ;  .
ETMP5    db      0                ;  .
ETMP6    db      0                ;  .
ETMP7    db      0                ;  .
ETMP8    db      0                ;  .
ETMP9    db      0                ;  .
ETMP10   db      0                ;  .
;
         dw      0                ;  .    WORKING STORAGE SLOP
;
TEMP1    db      0                ;  TEMPORARY WORKING STORAGE (SECOND LEVEL)
TEMP2    db      0                ;  .
TEMP3    db      0                ;  .
TEMP4    db      0                ;  .
TEMP5    db      0                ;  .
TEMP6    db      0                ;  .
TEMP7    db      0                ;  .
TEMP8    db      0                ;  .
TEMP9    db      0                ;  .
TEMP10   db      0                ;  .
;
         dw      0                ;  .    WORKING STORAGE SLOP
;
;
ACTPLY   db      0                ;  ACTIVE PLAYER FLAG ($00 / $02)
;
;
TMR1     db      0                ;  TIMER #1 - DOWN COUNTER
         dw      0                ;  .        - TIME-OUT ROUTINE POINTER
;
TMR2     db      0                ;  TIMER #2 - DOWN COUNTER
         dw      0                ;  .        - TIME-OUT ROUTINE POINTER
;
TMR3     db      0                ;  TIMER #3 - DOWN COUNTER
         dw      0                ;  .        - TIME-OUT ROUTINE POINTER
;
TMR4     db      0                ;  TIMER #4 - DOWN COUNTER
         dw      0                ;  .        - TIME-OUT ROUTINE POINTER
;
;
SCOR1    ds      7                ;  PLAYER #1 SCORE
SCOR2    ds      7                ;  PLAYER #2 SCORE
;
;
BLTSND   db      0                ;  BULLET SOUND FLAG
;
;
STAR1    db      0                ;  TEMPORARY WORKING STORAGE (THIRD LEVEL)
STAR2    db      0                ;  .
STAR3    db      0                ;  .
STAR4    db      0                ;  .
STAR5    db      0                ;  .
STAR6    db      0                ;  .
;
ABORT    db      0                ;  GAME ABORT FLAG
LOCK     db      0                ;  LOCK-UP ON GAME SEQUENCE
;
RSMINES  db      0                ;  MINE-LAYER RE-SEED MINE COUNT
RESEED   db      0                ;  MINE-LAYER RESEED FLAG
FRCTIME  db      0                ;  FORCED GROWTH COUNTER
;
MINTABLE dw      0                ;  MINE FIELD POINTER FOR LAYER SEQUENCE
TBLPTR1  dw      0                ;  MINE FIELD POINTER FOR PLAYER #1
TBLPTR2  dw      0                ;  MINE FIELD POINTER FOR PLAYER #2
;
WSHIPY   dw      0                ;  WORKING 'Y' POSITION
WSHIPX   dw      0                ;  WORKING 'X' POSITION
DSHIPY1  dw      0                ;  'Y' DISPLACEMENT FOR MOMENTUM AXIS #1
DSHIPX1  dw      0                ;  'X' DISPLACEMENT FOR MOMENTUM AXIS #1
DSHIPY2  dw      0                ;  'Y' DISPLACEMENT FOR MOMENTUM AXIS #2
DSHIPX2  dw      0                ;  'X' DISPLACEMENT FOR MOMENTUM AXIS #2
SHIPROT  db      0                ;  CURRENT SHIP ROTATION
SHIPSPD1 db      0                ;  SPEED ALONG MOMENTUM AXIS #1
SHIPDIR1 db      0                ;  DIRECTION OF MOMENTUM AXIS #1
SHIPSPD2 db      0                ;  SPEED ALONG MOMENTUM AXIS #2
SHIPDIR2 db      0                ;  DIRECTION OF MOMENTUM AXIS #2
SHIPCNT  db      0                ;  CURRENT SHIP COUNT FOR ACTIVE PLAYER
SHIPCNT0 db      0                ;  CURRENT SHIP COUNT FOR PLAYER #1
SHIPCNT1 db      0                ;  CURRENT SHIP COUNT FOR PLAYER #2
;
LAYRYX   dw      0                ;  CURRENT MINE-LAYER POSITION (Y:X)
WLAYRY   dw      0                ;  .    WORKING 'Y' POSITION
WLAYRX   dw      0                ;  .    WORKING 'X' POSITION
DLAYRY   dw      0                ;  .    'Y' DISPLACEMENT
DLAYRX   dw      0                ;  .    'X' DISPLACEMENT
LAYRDIR  db      0                ;  CURRENT MINE-LAYER DIRECTION
LAYRSPD  db      0                ;  CURRENT MINE-LAYER SPEED
LAYRPTR  dw      0                ;  MINE-LAYER RE-SEEDING SEQUENCE POINTER
;
CBULLET  db      0                ;  ACTIVE BULLET COUNTER
CMINES   db      0                ;  ACTIVE MINE COUNTER
CEXPLS   db      0                ;  ACTIVE EXPLOSION COUNTER
;
MINMAX   db      0                ;  MAXIMUM MINES COUNT
;
HYPFLAG  db      0                ;  HYPER-SPACE FLAG
HYPCNT   db      0                ;  HYPER-SPACE SEQUENCE COUNTER
;
TIMEOUT  dw      0                ;  LONG TIME-OUT DELAY
;
THRSND   db      0                ;  THRUSTER SOUND FLAG
EXPLSND  db      0                ;  OBJECT EXPLOSION SOUND FLAG
POPSND   db      0                ;  MINE 'POP' SOUND FLAG
FIRSND   db      0                ;  'RELEASED' FIRE-BALL SOUND FLAG
HYPSND   db      0                ;  HYPER-SPACE SOUND FLAG
;
SEXPCNT  db      0                ;  SHIP EXPLOSION COUNTERS
SEXP1    db      0                ;  .
;
MNLVL1   ds      7                ;  ACTIVE MINE-FIELD LEVEL FOR PLAYER #1
MNLVL2   ds      7                ;  ACTIVE MINE-FIELD LEVEL FOR PLAYER #2;
;
;        BULLET TABLE
;        ------------
;
DBLTY    dw      0                ;  'Y' DISPLACEMENT FOR ALL NEW BULLETS
DBLTX    dw      0                ;  'X' DISPLACMENET FOR ALL NEW BULLETS
;
;
BLT_FLG  equ     0                ;  BULLET FLAG
BLT_YD   equ     BLT_FLG + 1      ;  WORKING 'Y' DISPLACEMENT
BLT_XD   equ     BLT_YD + 2       ;  WORKING 'X' DISPLACEMENT
BLT_WY   equ     BLT_XD + 2       ;  WORKING 'Y' POSITION
BLT_WX   equ     BLT_WY + 2       ;  WORKING 'X' POSITION
BLT_DC   equ     BLT_WX + 2       ;  BULLET DOWN-COUNTER
;
BLT_LEN  equ     BLT_DC + 1       ;  BULLET TABLE LENGTH
;
BLT_TBL  ds      BLT_LEN * BULLETS
;
;      
;        MINE TABLE
;        ----------
;
MIN_FLG  equ     0                ;  MINE FLAG
MIN_PAK  equ     MIN_FLG + 1      ;  PACKET TYPE (NUMBER)
MIN_SIZ  equ     MIN_PAK + 1      ;  SIZE (ZOOM VALUE)
MIN_BSZ  equ     MIN_SIZ + 1      ;  BASE MINE SIZE (0 - 3)
MIN_YW   equ     MIN_BSZ + 1      ;  WORKING 'Y' POSITION
MIN_XW   equ     MIN_YW + 2       ;  WORKING 'X' POSITION
MIN_YD   equ     MIN_XW + 2       ;  WORKING 'Y' DISPLACEMENT
MIN_XD   equ     MIN_YD + 2       ;  WORKING 'X' DISPLACEMENT
MIN_BOX  equ     MIN_XD + 2       ;  COLLISION BOX PARAMETERS
MIN_SCR  equ     MIN_BOX + 2      ;  SCORE VALUE
MIN_T1   equ     MIN_SCR + 2      ;  MINE TEMPORARY WORKING STORAGE
MIN_T2   equ     MIN_T1 + 1       ;  .
;
MIN_LEN  equ     MIN_T2 + 1       ;  MINE TABLE LENGTH
;
MIN_TBL  ds      MIN_LEN * MINES  
;
;
;        EXPLOSION TABLE
;        ---------------
;
EXP_FLG  equ     0                ;  EXPLOSION FLAG
EXP_SIZ  equ     EXP_FLG + 1      ;  SIZE (ZOOM VALUE)
EXP_YX   equ     EXP_SIZ + 1      ;  ABSOLUTE Y:X POSITIONS
EXP_CNT  equ     EXP_YX + 2       ;  FRAME COUNTER
;
EXP_LEN  equ     EXP_CNT + 1      ;  EXPLOSION TABLE LENGTH
EXPLSN   equ     14               ;  NUMBER OF EXPLOSION ENTRIES
;
EXP_TBL  ds      EXP_LEN * EXPLSN
;
;
;        STAR FIELD TABLES
;        -----------------
;
FSTAR    ds      16
ZSTAR    ds      8
;
;
;
;==========================================================================JJH
;PACKET1 ds      30               ;  CODE DELETED - REV. C CHANGES   ======JJH
;PACKET2 ds      30               ;  .                               ======JJH
;==========================================================================JJH
;
;==========================================================================JJH
PACKET1  ds      27               ;  CODE ADDED - REV. C CHANGES     ======JJH
PACKET2  ds      15               ;  .    PACKET WORKING AREA        ======JJH
;==========================================================================JJH
;
;==========================================================================JJH
MINTBL1  db      0                ;  MINE TABLE FOR PLAYER #1        ======JJH
         db      0                ;  .    CODE ADDED -               ======JJH
         db      0                ;  .        REV. C CHANGES         ======JJH
         db      0                ;  .                               ======JJH
;                                                                    ======JJH
;                                                                    ======JJH
MINTBL2  db      0                ;  MINE TABLE FOR PLAYER #2        ======JJH
         db      0                ;  .                               ======JJH
         db      0                ;  .                               ======JJH
         db      0                ;  .                               ======JJH
;==========================================================================JJH
;
ENDRAM   db      0                ;  END-OF-RAM FLAG

