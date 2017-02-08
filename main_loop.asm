;
;
;  POWER-UP INITIALIZATION
;  =======================
;
         direct  $D0
;        =====   ===
;

ENTRY    ldx     #ETMP1           ;  CLEAR MEMORY
CLRALL   clr     ,x+               ;  .
         cmpx    #ENDRAM          ;  .
         bne     CLRALL           ;  .
;
         jsr     I_STARS          ;  INITIALIZE STAR FIELDS
         inc     ZSKIP            ;  SET POST-PACKET ZEROING FLAG
;
         lda     #$BB             ;  SET-UP CONTROLLER FLAGS
         sta     SBTN             ;  .
         ldx     #$0101           ;  .
         stx     SJOY             ;  .

;
;  INITIALIZE MINE-SWEEPER
;  =======================
;
;
NEWGAME  ldx     #ETMP1           ;  CLEAR MEMORY
CLRMEM   clr     ,x+               ;  .
         cmpx    #FSTAR - 1       ;  .
         bne     CLRMEM           ;  .
         bra     GAME1            ;  .
;
GAME1    jsr     DPRAM            ;  SET "DP" REGISTER FOR RAM
         direct  $C8              ;  .
;
         ldd     #$0200           ;  SELECT OPTIONS
         jsr     SELOPT           ;  .
         dec     <PLAYRS          ;  .
;
         clr     <TSTAT           ;  CLEAR TUNE FLAG
         clr     <ACTPLY          ;  CLEAR ACTIVE PLAYER FLAG
;
         ldx     #SCOR1           ;  CLEAR PLAYERS SCORE
         jsr     SCLR             ;  .
         ldx     #SCOR2           ;  .
         jsr     SCLR             ;  .
;
         ldx     #MNLVL1          ;  CLEAR ACTIVE MINE-FIELD COUNTER
         jsr     SCLR             ;  .    FOR PLAYER #1
         ldd     #$0001           ;  .    .
         jsr     SCRADD           ;  .    .
;
         ldx     #MNLVL2          ;  .    FOR PLAYER #2
         jsr     SCLR             ;  .    .
         ldd     #$0001           ;  .    .
         jsr     SCRADD           ;  .    .
         ldx     #MINTBL1         ;  CODE ADDED - REV. C CHANGES     ======JJH
         stx     <TBLPTR1         ;  .    SET-UP NEW MINE TABLES     ======JJH
         ldx     #MINTBL2         ;  .    .                          ======JJH
         stx     <TBLPTR2         ;  .    .                          ======JJH
         ldb     #$08             ;  .    CLEAR NEW MINE TABLES      ======JJH
         ldx     #MINTBL1         ;  .    .                          ======JJH
         jsr     BCLR             ;  .    .                          ======JJH
         lda     #5               ;  SET SHIP COUNT
         sta     <SHIPCNT         ;  .
         sta     <SHIPCNT0        ;  .
         sta     <SHIPCNT1        ;  .
;
         bra     LVLN1            ;  LEVEL #1 ENTRY POINT IS DIFFERENT

;
;  GAME LEVEL SEQUENCER
;  ====================
;
;
LEVELN   jsr     FALL             ;  FALL-THRU TO NEXT GAME LEVEL
;
         ldy     #TBLPTR1         ;  BUMP GAME DATA POINTER FOR ACTIVE PLAYER
         lda     <ACTPLY          ;  .
         ldx     a,y             ;  .
         jsr     REVC_0           ;  CODE ADDED - REV. C CHANGES     ======JJH
         ldx     #PMNLVL          ;  BUMP ACTIVE MINE-FIELD COUNTER
         lda     <ACTPLY          ;  .    WHICH PLAYER IS ACTIVE ?
         ldx     a,x             ;  .    .
;
         lda     5,x             ;  .    BONUS SHIP ?
         anda    #$03             ;  .    .
         bne     LVLN01           ;  .    .
         inc     SHIPCNT          ;  .    .    BUMP SHIP COUNT FOR ACTIVE PLAYER
;
LVLN01   ldd     #$0001           ;  .    BUMP GAME LEVEL
         jsr     SCRADD           ;  .    .
;
LVLN1    jsr     SWPINT           ;  ENTRY FOR LEVEL RE-START
;
         ldx     #TBLPTR1         ;  .    SET-UP FOR NEXT GAME LEVEL
         lda     <ACTPLY          ;  .    .    WHICH PLAYER IS ACTIVE
         ldx     a,x             ;  .    .    .
         jsr     MINLAY           ;  INITIALIZE FOR GAME LEVEL
         bra     LVLN3            ;  .
;
LVLN2    ldd     <TIMEOUT         ;  LOCK-UP ON GAME SEQUENCE
         subd    #$0001           ;  .    TIME-OUT ON SEQUENCE ?
         std     <TIMEOUT         ;  .    .
         beq     LVLN21           ;  .    .
;
         pshs    dp               ;  .    DISPLAY BOTH PLAYERS SCORE
         jsr     DPIO             ;  .    .
         jsr     SCRBTH           ;  .    .
         ldu     #M_END           ;  .    DISPLAY 'GAME OVER' MESSAGE
         jsr     MESS             ;  .    .
         puls    dp               ;  .    .
;
         ldx     #SCOR1           ;  ESCAPE FROM GAME LEVEL LOCK-UP
         ldu     #HISCOR          ;  .    IS PLAYER #1 SCORE HIGHEST ?
         jsr     HISCR            ;  .    .
;
         ldx     #SCOR2           ;  .    IS PLAYER #2 SCORE HIGHEST ?
         ldu     #HISCOR          ;  .    .
         jsr     HISCR            ;  .    .
         lda     <TRIGGR          ;  CODE ADDED - REV. B CHANGES     ======JJH
         beq     LVLN3            ;  .                               ======JJH
;
LVLN21   ldd     <TIMEOUT         ;  LOCK TIME-OUT ?
         lbne    NEWGAME          ;  .    START GAME OVER
         jmp     REVB_0           ;  CODE ADDED - REV. B CHANGES     ======JJH
         nop                      ;  .    FILLER                     ======JJH
         nop                      ;  .    .                          ======JJH
         nop                      ;  .    .                          ======JJH
;
LVLN3    pshs    dp               ;  SAVE "DP" REGISTER
         jsr     WAIT             ;  WAIT FOR FRAME BOUNDARY
         jsr     GMINE            ;  HANDLE MINE GAME LOGIC
         jsr     GSHIP            ;  HANDLE SWEEPER GAME LOGIC
         jsr     GBULLET          ;  HANDLE BULLET GAME LOGIC
         jsr     MSHIP            ;  HANDLE MINE-LAYER GAME LOGIC
;
         puls    dp               ;  SET "DP" REGISTER TO RAM
         direct  $C8              ;  .
;
         jsr     CBULMIN          ;  HANDLE BULLET/MINE COLLISIONS
         jsr     CMINSHIP         ;  HANDLE SHIP/MINE COLLISIONS
         jsr     CSHPLYR          ;  HANDLE SHIP/LAYER COLLISIONS
         jsr     TAIL             ;  HANDLE TAIL-END LOGIC
         bcs     LVLN3            ;  .    MORE GAME LOGIC ?
;
         lda     <ABORT           ;  RESTART CURRENT GAME POSITION ?
         lbeq    LEVELN           ;  .    WAS GAME ABORTED ?
         lda     <LOCK            ;  .    .    LOCK-UP ON GAME SEQUENCE ?
         lbne    LVLN2            ;  .    .    .
         jmp     LVLN1            ;  .    .    RESTART GAME SEQUENCE


