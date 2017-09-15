; %help 
; nbp.parse <name> - parse and output parts of NBP <name>
;
; options:   -s Substitute defaults for unspecified portions.
; %hend

.pc02
.include  "davex-mg.inc"

ch        = $24                 ; cursor horizontal pos

sptr      = xczpage
sptr2     = sptr+2
stemp     = sptr2+2
mode      = stemp+1

namebuf   = filebuff2

          DX_start dx_mg_auto_origin
          DX_info $01,$12,dx_cc_iie_or_iigs,$00
          DX_ptab
          DX_parm $00,t_string  ; NBP name to parse
          DX_parm 's',t_nil     ; if given, sub defaults
          DX_end_ptab
          DX_desc "Parse AppleTalk NBP names."
          DX_main
          lda   #$00
          sta   mode
          lda   #'s'+$80
          jsr   xgetparm_ch
          bcs   :+
          lda   #$80
          sta   mode
:         jsr   setsptr
          lda   #$00
          jsr   xgetparm_n
          ldx   mode
          jsr   get_nbp
          php
          phx
          ply
          lda   #$00
          jsr   xprdec_2
          jsr   xmess
          .byte ' '+$80,$00
          plp
          bcc   printv
          lda   #<nonestr
          sta   sptr
          lda   #>nonestr
          sta   sptr+1
printv:   jsr   prpas
          jsr   xmess
          .byte $8d,$00
next:     inc   mode
          lda   mode
          and   #$7f
          cmp   #$03
          bcc   :-
          rts
setsptr:  lda   #<namebuf
          sta   sptr
          lda   #>namebuf
          sta   sptr+1
          rts
nonestr:  .byte 6,"(none)"
; ***
; get_nbp - get part of a name at ay and copy to sptr
; x = 0:name 1:type 2:zone, +$80 sub wildcard or default if none given
; return: (sptr) = string, x = length of it
.proc     get_nbp
          sty   sptr2
          sta   sptr2+1
          stx   stemp
          ldy   #$00
          sty   colon           ; init these
          sty   at
          lda   sptr            ; copy sptr to self-modifying wrtdest
          sta   wrtdest+1
          lda   sptr+1
          sta   wrtdest+2
          jsr   incdest         ; and move to first char position
          lda   (sptr2),y       ; length
          beq   notfound        ; zero, just give up
          sta   end
          ; find the delimiters
          tay
:         lda   (sptr2),y
          cmp   #':'
          bne   notcolon
          sty   colon
notcolon: cmp   #'@'
          bne   nxtdelim
          sty   at
nxtdelim: dey
          bne   :-
          ; now make sure that if @ is given, it is after :
          lda   at
          beq   :+
          cmp   colon
          bcc   bad
          ; now get the part requested
:         lda   stemp
          and   #$7f
          beq   getname
          cmp   #$01
          beq   gettype
getzone:  ldy   at
          beq   notfound
          cpy   end
          beq   notfound
          ; need to copy from at(+1) to end
docopy:   ldx   #$00
:         iny
          lda   (sptr2),y
          jsr   wrtdest
          cpy   end             ; was that the last char
          bcc   :-              ; nope, next char
          ldy   #$00
          txa
          sta   (sptr),y        ; save copied length
          clc
          rts                   ; and return
getname:  ldy   colon
          bne   :+
          ldy   at
          beq   :++
:         dey
          sty   end
:         ldy   end
          beq   notfound
          ldy   #$00            ; initially at pos 0
          ; need to copy from pos 1 to end
          beq   docopy          ; always
gettype:  ldy   colon
          beq   notfound        ; early out if no colon
          cpy   end
          beq   notfound
          ldy   at
          beq   :+              ; use end as-is
          dey                   ; otherwise end at pos before @
          sty   end
:         ldy   colon
          ; need to copy from colon(+1) to end
          bne   docopy          ; should be always
notfound: lda   stemp
          and   #$80
          bne   :+              ; if client asked for a default
          lda   #$00
          tay
          tax                   ; x is officially length of string result
          sta   (sptr),y        ; put a zero in destination
bad:      sec                   ; tell client we gave an empty string
          rts
:         ldx   #$01            ; length of default
          ldy   #$00
          txa
          sta   (sptr),y
          iny
          lda   stemp
          cmp   #$82            ; want default zone? ('*')
          bne   :++             ; nope
          lda   #'*'
:         sta   (sptr),y
          clc
          rts
:         lda   #'='            ; wildcard for name or type
          bne   :--             ; always        
wrtdest:  sta   $ffff
          inx                   ; inc count of copied chars
incdest:  inc   wrtdest+1
          bne   :+
          inc   wrtdest+2
:         rts
colon:    .byte $00
at:       .byte $00
end:      .byte $00
.endproc
; increment sptr by a
.proc     addsptr
          clc
          adc   sptr
          sta   sptr
          bcc   :+
          inc   sptr+1
:         rts
.endproc
; print pascal string at sptr
; leave sptr pointed at one past end
; of string
.proc     prpas
          ldy   #$00
          lda   (sptr),y        ; get length
          tax
next:     lda   #$01
          jsr   addsptr
          dex
          bpl   :+
          rts
:         lda   (sptr),y        ; get char
          ora   #$80            ; make printable
          jsr   cout
          bra   next
.endproc
; copy a pascal string to sptr offset by y
; a,x = source
; return: y = new offset after copied str
.proc     copystr
		      sta	  sptr2+1
          stx	  sptr2
		      sty	  stemp           ; save offset
		      ldy	  #$00
		      lda	  (sptr2),y       ; get number of chars
		      tax                   ; to copy
		      ldy	  stemp           ; get the offset
		      sta	  (sptr),y        ; store the length byte
		      inc	  stemp           ; increment the offset
		      inc	  sptr2           ; next source char
		      bne	  :+
		      inc	  sptr2+1
:         ldy	#0
          ; copy loop
:         phy
		      lda	  (sptr2),y
          ldy	  stemp
          sta	  (sptr),y
          inc   stemp
          ply
          iny
          dex
          bne   :-
          ldy   stemp
          rts
.endproc
          DX_end
