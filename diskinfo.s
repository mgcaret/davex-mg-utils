; %help
; diskinfo -- Display ProDOS disk/volume info.
;
; syntax:   diskinfo
; %hend

.p02
.include  "davex-mg.inc"

tptr      = xczpage           ; generic pointer
tmp       = tptr+1
atflag    = tmp+2               ; but 7 = off if AT present
spflag    = atflag+1            ; zero if smartport
slot      = spflag+1            ; slot address of device being processed
unit      = slot+2              ; unit number being analyzed
devno     = unit+1              ; device number being analyzed
drvr      = devno+1             ; drvr = driver address

sessbuf   = filebuff3
sessbufsz = $200

DEVCNT    = $bf31               ; # of devices-1 (there must always be 1+)
DEVLST    = $bf32
DEVADR    = $bf10

          DX_start dx_mg_auto_origin ; load address
          DX_info $01,$12,dx_cc_any,$00
          DX_ptab
          DX_end_ptab
          DX_desc "Display disk/volunme info."
          DX_main main
          
.proc     main
          lda   #$00
          sta   slot            ; init low byte
          sta   devno           ; init dev number
          
          jsr   atcheck
loop:     ldx   devno
          cpx   DEVCNT
          bcc   :+              ; not done yet
          beq   :+              ; nor if equal to
          rts                   ; exit if greater than
:         jsr   doid            ; identify/get info for it
          jsr   printinfo       ; print the device info
          inc   devno           ; next device
          jmp   loop
.endproc

; See TN.PDOS.20 and 21
; expects device number (relative to DEVADR) in x
.proc     doid
          lda   DEVLST,x
          and   #$f0
          sta   unit
          sta   spflag          ; flag no smartport (spflag = nonzero)
          lsr
          lsr
          lsr
          lda   DEVADR,x
          sta   drvr
          lda   DEVADR+1,x
          sta   drvr+1
          jsr   isslot
          sta   slot+1          ; zeroed if not slot device
          beq   :+              ; and done
          ldy   #$07
          lda   (slot),y
          sta   spflag          ; otherwise set smartport flag as needed
:         rts
.endproc

.proc     printinfo
          jsr   prheader
          jsr   my_crout        ; header does not print it
          jsr   prslot
          jsr   my_crout        ; extra space between devices
.endproc


.proc     prheader
          lda   unit
          jsr   xprint_sd       ; print .sd
          jsr   prcolon
          jsr   maybeat
          bcc   :+
          jmp   printat
:         jsr   ismirr
          bcc   :+
          jsr   xmess
          asc_hi "mirrored"
          .byte $00
:         jsr   isslot
          bcc   :+
          ldy   #$ff
          lda   (slot),y
          beq   diskii
          clc
          adc   #$01
          beq   diskii          ; 13-sector, should not happen
          lda   spflag
          bne   :+
          jsr   xmess
          asc_hi "SmartPort "
          .byte $00
:         jsr   xmess
          asc_hi "device "
          .byte $00
          rts
diskii:   jsr   xmess
          asc_hi "Disk II"
          .byte $00
          rts
.endproc

.proc     prslot
          jsr   tab2
          lda   slot+1
          beq   :+
          jsr   xmess
          asc_hi "Installed in slot "
          lda   slot+1
          clc
          sbc   #$10
          jsr   cout
          jsr   my_crout
:         rts
.endproc


; check to see if current device's driver is in a slot
; return $Cn if so, $00 if otherwise, and Z flag set if so
.proc     isslot
          lda   drvr+1
          and   #$f0
          cmp   #$c0
          bne   :+
          lda   drvr+1
          rts
:         lda   #$00
          rts
.endproc

; check to see if current device is a mirrored device
; return carry set if it is, clear otherwise
; if carry set, A contains actual slot number of device
.proc     ismirr
          lda   slot+1          ; high byte of slot pointer
          clc                   ; anticipate no slot
          beq   :+              ; and bail if not
          lda   drvr+1          ; get driver address high byte
          and   #$0f            ; mask off slot number
          sta   tmp
          lda   unit            ; %DSSSXXXX unit number
          lsr                   ; %0DSSSXXX
          lsr                   ; %00DSSSXX
          lsr                   ; %000DSSSX
          lsr                   ; %0000DSSS
          and   #$07            ; %00000SSS forget drive
          cmp   slot+1          ; compare unit to slot
          clc                   ; anticipate not
          beq   :+              ; if equal, it isn't
          sec                   ; flag it
:         rts
.endproc

.proc     printat
          jsr   prafp
          jsr   xmess
          asc_hi "session "
          .byte $00
          ldy   #$00
          lda   (tptr),y
          tay
          lda   #$00
          jsr   xprdec_2
          jsr   xmess
          asc_hi ", ID "
          .byte $00
          ldy   #$1f
          lda   (tptr),y
          tax
          dey
          lda   (tptr),y
          tay
          txa
          jsr   xprdec_2
          jsr   my_crout
          jsr   tab2
          jsr   prafp
          ldy   #$01            ; slot+drive and user flag
          lda   (tptr),y
          ror                   ; bit 0 -> carry
          bcc   :+
          jsr   xmess
          .byte "user "
          .byte $00
:         jsr   prvol
          jsr   prcolon
          lda   tptr            ; get tptr to AFP volume name
          clc                   ; into AY
          adc   #$02
          tay
          lda   tptr+1
          adc   #$00
          jmp   xprint_path     ; print pascal str in AY and leave
.endproc

.proc     prafp
          jsr   xmess
          asc_hi "AFP "
          .byte $00
          rts
.endproc

.proc     prvol
          jsr   xmess
          asc_hi "volume "
          .byte $00
          rts
.endproc

.proc     prcolon
          jsr   xmess
          asc_hi ": "
          .byte $00
          rts
.endproc

.proc     maybeat
          bit   atflag
          bmi   :+
notat:    clc
          rts
.pushcpu
.pc02                           ; AT requires one anyway
:         lda   nument
          beq   notat
          sta   tmp
loop:     lda   #<sessbuf
          sta   tptr
          lda   #>sessbuf
          sta   tptr+1
          ldy   #$01
          lda   (tptr),y
          and   #$f0
          cmp   unit
          beq   :+
          dec   tmp
          beq   notat
          lda   #32
          clc
          adc   tptr
          sta   tptr
          bcc   loop
          inc   tptr+1
          bra   loop
:         sec
          rts
.popcpu
.endproc


.proc     tab2
          jsr   xmess
          asc_hi "  "
          .byte $00
          rts
.endproc

.proc     my_crout
          jsr   xcheck_wait
          jmp   crout
.endproc

.proc     atcheck
          php
          cli                   ; allow interrupts for AT calls
          ATcall inforeq
          ror                   ; move carry into high bit
          sta   atflag          ; and use as flag
          rol                   ; put carry back
          bcs   :+              ; and exit if none
          ATcall filsess        ; get sessions
          bcc   :+
          lda   #$00
          sta   nument          ; ensure nument is 0 if error
:         plp
          rts
; param list for GetInfo
inforeq:  .byte 0,2             ; sync GetInfo
          .word $0000           ; result code
          .dword $00000000      ; completion address
thisnet:  .word $0000           ; this network #
abridge:  .byte $00             ; local bridge
hwid:     .byte $00             ; hardware ID, IIgs only
romver:   .word $00             ; ROM version, IIgs only
nodenum:  .byte $00             ; node number
; param list for FIListSessions
; note format returned by FIListSessions
; is nument 32-byte entries in the format:
; $00 session reference number
; $01 slot/drive
; $02-$1D volume name
; $13-$1F volume ID
; the max number of these under P8 is 14, so the max data returned
; is 14*32 = 448 bytes'
filsess:  .byte 0,$2f           ; sync FIListSessions
          .word $0000           ; result code
buflen:   .word sessbufsz       ; buffer length
bufp:     .dword sessbuf        ; buffer pointer
nument:   .byte $00             ; entries returned
.endproc
nument    = atcheck::nument

          DX_end
