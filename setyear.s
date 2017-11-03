; %help
; setyear - Patch the ProDOS year table in-memory
;
; usage: setyear <00-99> [-v] [-n]
;
; Patches the ProDOS year table to include the given year plus at least
; 4 more years beyond.
;
; Options:  -v  Show generated year table
;           -n  Don't actually patch ProDOS.
; %hend

.p02
.include  "davex-mg.inc"

tmp       = xczpage
year      = tmp+1
count     = year+1

prbyte	  = $fdda
;cout      = $fded

          DX_start dx_mg_auto_origin ; load address
          DX_info $01,$12,dx_cc_any,$00
          DX_ptab
          DX_parm $00,t_int1    ; year (two-digit)
          DX_parm 'v',t_nil     ; verbose
          DX_parm 'n',t_nil     ; no-patch
          DX_end_ptab
          DX_desc "Set ProDOS year table."
          DX_main
          lda   #$00            ; parm
          jsr   xgetparm_n      ; davex ensures this is here
          cpy   #100
          bcs   badparms
          jsr   mkyt
          lda   #'v'|$80        ; "verbose"
          jsr   xgetparm_ch
          bcs   :+              ; not given, don't display
          jsr   dispyt
:         lda   #'n'|$80        ; "no patch"
          jsr   xgetparm_ch
          bcs   :+              ; not given, do it
          rts
          ; see if we can patch the driver
:         lda		$c08b
          lda		$c08b
          ldx		#$03
:					lda		$d7b4,x					; tdays table, at september
          cmp		tdaysv,x				; that what ProDOS has?
          bne		setyterr				; not the thunderclock driver
          dex
          bpl		:-
          ; go ahead and patch  
          ldx		#6
:					lda   yeartab,x
          sta		$d7b8,x					; address of year table in P8
          dex
          bpl		:-
          bit		$c082						; ROM back before exit
          rts
setyterr:	bit		$c082						; ROM back before death
          lda		#$01
          jsr 	xredirect
          jsr		xmess
          asc_hi "Can't patch clock driver!"
          .byte $00
          jmp		exiterr
badparms: lda   #$01
          jsr   xredirect
          jsr   xmess
          asc_hi "Bad year!"
          .byte $00
exiterr:  lda   #$ff
          jsr   xredirect
          jmp   xerr
;
.proc     mkyt
          tya
          and   #%00000011      ; divisible by 4?
          bne   :+              ; nope
          dey                   ; avoid starting table on leap year
          bpl   :+              ; did't wrap around
          ldy   #99             ; 1999
:         sty   year
          lda   #$06            ; do for 6 years
          sta   count
yloop:    lda   year
          cmp   #40             ; 40 or greater?
          bcs   :+              ; Do not adjust for 1940-1999
          adc   #100            ; add 100 years for 2000-2040   
:         tay                   ; year
          lda   #1              ; day
          tax                   ; month
          jsr   weekday
          tax
          ldy   dayidx,x        ; get index into ProDOS year table
          lda   year
          cmp   #100            ; need to account for incrementing past 100
          bcc   :+
          sbc   #100
:         sta   yeartab,y       ; put year in the appropriate slot
          inc   year
          dec   count
          bne   yloop
          ; now fix leap year gap
          ldy   yeartab         ; first year
          ldx   #$06            ; index of last year
lloop:    lda   yeartab,x
          cmp   #$ff            ; not filled in?
          bne   :+
          tya                   ; yes, use previous value
:         sta   yeartab,x       ; write back
          tay                   ; and save as new 'previous' value
          dex
          bpl   lloop
          rts
.endproc
.proc     dispyt
          lda   #$00
          sta   year
          lda   #$07
          sta   count
prloop:   ldx   year
          ldy   yeartab,x
          sty   tmp
          lda   #$20
          cpy   #40
          bcc   :+
          lda   #$19   
:         jsr   prbyte
          lda   tmp
          cmp   #$10
          bcs   :+
          lda   #'0'|$80
          jsr   cout
:         ldy   tmp
          lda   #$00
          jsr   xprdec_2
          lda   #$8d
          jsr   cout
          inc   year
          dec   count
          bne   prloop
          rts
.endproc
; table to index into year-starts in P8 tclock driver
dayidx:   .byte 1,0,6,5,4,3,2   ; MonSunSatFriThuWedTue
; Table we will copy into ProDOS
yeartab:  .res  7,$ff
; table to validate the P8 tclock driver, 4 bytes at $d7b4
tdaysv:		.byte 242,20,51,81
; adapted from http://6502.org/source/misc/dow.htm
; inputs: y = year (0-255 = 1900-2155)
;         x = month
;         a = day
; output: a = weekday (0 = Sunday)
.proc     weekday
          cpx #3                ; year starts in march to bypass
          bcs march             ; leap year problem
          dey                   ; if jan or feb, decrement year
march:    eor #$7f              ; invert a so carry works right
          cpy #200              ; carry will be 1 if 22nd century
          adc mtab-1,x          ; a is now day+month offset
          sta tmp
          tya                   ; get the year
          jsr mod7              ; do a modulo to prevent overflow
          sbc tmp               ; combine with day+month
          sta tmp
          tya                   ; get the year again
          lsr                   ; divide it by 4
          lsr
          clc                   ; add it to y+m+d and fall through
          adc tmp
mod7:     adc #7                ; returns (a+3) modulo 7
          bcc mod7              ; for a in 0..255
          rts
mtab:     .byte 1,5,6,3,1,5,3,0,4,2,6,4   	; month offsets
.endproc
          DX_end
