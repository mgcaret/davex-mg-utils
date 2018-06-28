; %help
; mig.insp -- MIG Inspector by M.G., davex version
;
; Displays the MIG RAM from the Apple IIc Plus this is used by the 3.5 drive
; firmware and the accelerator.
;
; When run, 4 pages of the MIG RAM are displayed at a time.  If a dynamic
; buffer is allocated, its address is placed on the screen in the lower-right
; corner.
;
; Keys:
; Arrows - change pages in view
; 0 - 9 - jump to page n*7, 0 = page 0, 9 = page 63
; ESC - quit
; ~ - Jump to page 0 and copy all 2K of the MIG to the buffer, if it's big
; enough.  Beeps if not.
; %hend

; Technical:

; The MIG is briefly mentioned in the Apple IIc
; Techincal Reference 2nd Edition.  It gives the
; pinouts of the chip and general function description
; but not the level of detail we are used to for Apple
; II technical manuals.

; I wanted to see what was behind the smoke and mirrors.

; The MIG RAM is a 2K SRAM that is accessed through
; a small window from $CE00-$CFFF (and $DE00-$DFFF)
; when the alternate firmware bank is active.

; 32 bytes of MIG RAM are accessed in 64 pages, with
; $CEA0 resetting to page 0, and $CE20 incrementing it

; All but page 2 is used by the 3.5 code.  Page 2 is
; used by the accelerator code.

; Other locations in the MIG window are likely used to
; control the 3.5 drive control signals that the MIG
; outputs.

.pc02
.code
.include "davex-mg.inc"

; build options
SKIPROMID = 0                 ; skip ROM identification
                              ; lets you use on a non-
                              ; IIc Plus, but obviously
                              ; isn't useful beyond some
                              ; testing purposes

; Our authorized zero page storage
BufFlag   = xczpage           ; if $FF, have big buffer
BufLoc    = BufFlag+1         
BufPtr    = BufLoc+2
MigPage   = BufPtr+2

; entry points
COut      = $fded
COut1     = $fdf0
TabV      = $fb5b
PrByte    = $fdda
Home      = $fc58
VTab      = $fc22
;KeyIn     = $fd1b            ; do not use for davex
PrntAX    = $f941

; locs
CH        = $24
CV        = $25
AltBuffer = filebuff3         ; if we can't get big buffer
ROMBank   = $c028
MigBase   = $ce00
MigRAM    = MigBase
MigPage0  = MigBase+$A0
MigPageI  = MigBase+$20

          DX_start dx_mg_auto_origin ; load address
.if SKIPROMID
          DX_info $01,$12,dx_cc_any|dx_cc_40col,$00
.else
          DX_info $01,$12,dx_cc_iic|dx_cc_40col,$00
.endif
          DX_ptab
          DX_end_ptab
          DX_desc "Display MIG RAM."
          DX_main

          cld
.if SKIPROMID
          bra   init
.else
          lda   $fbbf           ; davex says it's a //c already
          cmp   #$05            ; but is it a IIc Plus?
          beq   init            ; all good!
          lda   #$01
          jsr   xredirect
          jsr   xmess
          asc_hi "Requires a IIc Plus!"
          .byte $00
.endif
exiterr:  lda   #$ff
          jsr   xredirect
          jmp   xerr
init:     stz   MigPage
          stz   BufFlag
          lda   #<AltBuffer
          sta   BufLoc
          lda   #>AltBuffer
          sta   BufLoc+1
          ldx   #mli_read
          jsr   xmmgr
          cmp   #$08            ; 8 pages available?
          bcc   :+              ; nope
          lda   #$08
          ldx   #mli_open
          jsr   xmmgr
          bcs   :+              ; shouldn't happen
          stz   BufLoc
          sta   BufLoc+1
          dec   BufFlag         ; flag it
:         ; moving on...        
          ; davex does the below for us
          ;lda   #$91
          ;jsr   COut1           ; go to 40 cols if 80 col firmware active
          jsr   Home
          lda   #21
          jsr   TabV
          jsr   xmess
          asc_hi "Arrows=Page, 0-9=Jump, ESC=Quit"
          .byte $8d,$8d
          asc_hi "MIG Inspector by M.G. 08/30/2017  "
          .byte $00
          bit   BufFlag
          bpl   dispmig         ; Don't print addr if using AltBuffer
          lda   #'@'|$80
          jsr   COut1
          ldx   BufLoc          ; display buffer location
          lda   BufLoc+1
          jsr   PrntAX
dispmig:  jsr   get4mig         ; 4 mig pages to buffer
          jsr   d4page
uinput:   lda   #' '+$80
          jsr   xrdkey          ; davex read key
          cmp   #$8b            ; up arrow
          beq   goup
          cmp   #$8a            ; down arrow
          beq   godn
          cmp   #$88            ; left arrow
          bne   :+
goup:     dec   MigPage
          bra   dispmig
:         cmp   #$95            ; right arrow
          bne   :+
godn:     inc   MigPage
          bra   dispmig
:         cmp   #'~'+$80        ; tilde - git all MIG RAM to buffer, maybe
          bne   :+
          jsr   getallmig
          bra   dispmig
:         cmp   #$9b            ; escape
          bne   jump
          lda   #18             ; make sure DaveX "hit a key"
          jsr   TabV            ; is on a blank line
          lda   #$8d            ; and displays properly
          jmp   COut            ; CR and exit
jump:     sbc   #$b0            ; check for digit for page jump
          bmi   uinput          ; nope
          cmp   #10             ; 10 or bigger?
          bcs   uinput          ; also nope
          sta   MigPage         ; compute digit * 7
          asl                   ; * 2
          asl                   ; * 4
          asl                   ; * 8
          sec
          sbc   MigPage         ; * 7
          sta   MigPage
          bra   dispmig
; display 4 MIG pages on screen
.proc     d4page
          jsr   rsetbptr
          lda   #$00
          jsr   TabV
          ldx   #$00
:         stz   CH
          txa
          clc
          adc   MigPage
          and   #$3f
          jsr   PrByte
          lda   #':'+$80
          jsr   COut1
          jsr   d4line
          inc   CV
          jsr   VTab
          inx
          cpx   #$04
          bne   :-
          rts
.endproc
; display 4 lines at BufPtr, inc bufptr as we go
; assume CV is where we want it to be
.proc     d4line
          phx
          ldx   #$03
:         jsr   dline
          inc   CV
          jsr   VTab
          lda   #$08
          jsr   addbptr
          dex
          bpl   :-
          plx
          rts
.endproc
; display 1 line at BufPtr
.proc     dline
          lda   #4
          sta   CH
          ldy   #$00            ; start hex display
:         lda   (BufPtr),y
          jsr   PrByte
          lda   #' '+$80
          jsr   COut1
          iny
          cpy   #$08            ; done?
          bne   :-              ; nope, next hex
          ldy   #$00            ; start ASCII display
:         lda   (BufPtr),y
          ora   #$80
          cmp   #' '+$80        ; space
          bcs   :+              ; if not ctrl char
          lda   #'.'+$80        ; if so, use dot
:         jsr   COut1
          iny
          cpy   #$08            ; done?
          bne   :--             ; nope, next ASCII
          rts
.endproc
; reset buffer BufPtr
.proc     rsetbptr
          pha
          lda   BufLoc
          sta   BufPtr
          lda   BufLoc+1
          sta   BufPtr+1
          pla
          rts
.endproc
; add A to buffer BufPtr
.proc     addbptr
          clc
          adc   BufPtr
          bcc   done
          inc   BufPtr+1
done:     sta   BufPtr
          rts
.endproc
; copy all mig pages (2048 bytes) to (bufptr)          
.proc     getallmig
          bit   BufFlag
          bmi   :+
          jsr   xbell
          rts
:         stz   MigPage
          ldx   #$3f
          bra   getxmig
.endproc
; copy 4 mig pages (128 bytes) to (bufptr)          
.proc     get4mig
          ldx #$03
          ; fall through
.endproc
; copy x mig pages (x*32 bytes) to (bufptr)          
.proc     getxmig
          lda   MigPage
          and   #$3f            ; enforce range
          sta   MigPage
          sta   ROMBank         ; mig only visible when alt ROM switched in
          jsr   setmigpg
          jsr   rsetbptr
:         jsr   copymig
          lda   #$20
          jsr   addbptr         ; next buffer segment
          bit   MigPageI        ; next MIG page
          dex
          bpl   :-
          sta   ROMBank
          rts
.endproc
; copy one mig page (32 bytes) to (bufptr)
.proc     copymig
          phy
          ldy   #$1f
:         lda   MigRAM,y
          sta   (BufPtr),y
          dey
          bpl   :-
          ply
          rts
.endproc
; set MIG page
.proc     setmigpg
          phx
          bit   MigPage0
          ldx   MigPage
          beq   done
:         bit   MigPageI
          dex
          bne   :-
done:     plx
          rts
.endproc
          DX_end
