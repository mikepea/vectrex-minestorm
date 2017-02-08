;==========================================================================
; Init ROM
;==========================================================================

         code
         org     $0000

         db      "g GCE 2017", $80       ; 'g' is copyright sign
         dw      LAYTUNE
;
         dw      $F850
         dw      $40E8
         db      'MINE',$80
;
         dw      $F850
         dw      $10DE
         db      'STORM',$80
         dw      $F850
         dw      $E0DE
         db      'DEVEL',$80
         db      0

