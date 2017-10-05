; %help
; iie.card -- Display info about or control the Apple //e Card for Mac LC.
;
; options:
;   -s <num>  Set speed, 0 = normal (1 MHz), 1 = fast (1.9 MHz),
;             anything else = option panel speed
;   -x        Show experimental info, uses undocumented features.
; %hend

curspeed  = xczpage

.pc02
.include  "davex-mg.inc"

          DX_start dx_mg_auto_origin ; load address
          DX_info $01,$12,dx_cc_iie_or_iigs,$00
          DX_ptab
          DX_parm 's',t_int1    ; set speed (0 = normal, 1 = fast, others = default)
          DX_parm 'x',t_nil     ; experimental info
          DX_end_ptab
          DX_desc "Control Apple //e LC PDS Card."
          DX_main
          ; davex has already identified the machine as a //e or IIgs
          sec
          jsr   checkmach       ; see if IIgs
          bcs   :+              ; not a IIgs
badiie:   lda   #$01
          jsr   xredirect
          jsr   xmess
          asc_hi "This program requires the Apple //e Card for Macintosh LC!"
          .byte $8d,$00
exiterr:  lda   #$ff
          jsr   xredirect
          jmp   xerr
:         lda   $fbc0           ; check for enhanced monitor
          and   #$fe
          cmp   #$e0            ; $e0 = enhanced, $e1 = debug
          bne   badiie
          lda   $fbdd           ; //e Card ID byte
          cmp   #$02
          bne   badiie
          lda   #'s'|$80        ; set speed?
          jsr   xgetparm_ch
          bcs   doinfo
          jsr   setspeed
          jmp   dispspeed
doinfo:   jsr   dispinfo
          lda   #'x'|$80        ; experimental?
          jsr   xgetparm_ch
          bcs   :+
          jsr   dispslot
:         jsr   dispspeed
          rts
.proc     dispinfo
          jsr   xmess
          asc_hi "Card revision: "
          .byte $00
          ldy   $fbbe
          iny
          lda   #$00
          jsr   xprdec_2
          jsr   xmess
          .byte $8d,$00
          rts
.endproc
.proc     dispspeed 
          jsr   xmess         
          asc_hi "CPU speed: "
          .byte $00
          lda   $c02b
          and   #%00000100      ; bit 2 = CPU speed
          sta   curspeed
          jsr   speedmsg
          lda   $c05c
          and   #%00000100      ; bit 2 = option panel speed
          cmp   curspeed
          beq   done
          pha
          jsr   xmess
          asc_hi " (option panel: "
          .byte $00
          pla
          jsr   speedmsg
          jsr   xmess
          asc_hi ")"
          .byte $00
done:     jsr   xmess
          .byte $8d,$00
          rts
.endproc
.proc     speedmsg
          bne   fast
          jsr   xmess
          asc_hi "normal"
          .byte $00
          rts
fast:     jsr   xmess
          asc_hi "fast"
          .byte $00
          rts
.endproc
.proc     setspeed
          lda   #%00000100      ; bit position of speed in $c05c and $c02b
          cpy   #$00            ; y reg will determine selected speed.  Is 0?
          beq   norm            ; set normal
          dey                   ; is 1?
          beq   fast            ; fast instead
          ; default fall through to option panel speed
          trb   $c02b           ; default to slow
          and   $c05c           ; bit 2 of $c05c reflects option panel setting
          ; fall through to tsb, which will do nothing if a = $00 (slow)
fast:     tsb   $c02b
          rts
norm:     trb   $c02b
          rts
.endproc
.proc     dispslot
          jsr   xmess
          asc_hi "Startup slot: "
          .byte $00
          .byte $02,$02         ; magic trick 1
          cmp   #$c8
          beq   chkscan
prslot:   and   #$0f
          tay
          dey
          lda   #$00
          jsr   xprdec_2
done:     jsr   xmess
          .byte $8d,$00
          rts
chkscan:  lda   #$c6            ; see if slot 7 is picked, less likely than scan
          .byte $02,$03         ; magic trick 2, introduces a visual anomaly if slot 7
          bne   prscan
          ; the following is to fix the screen up after it prints
          ; "UNABLE TO BOOT FROM STARTUP SLOT"
          ; if slot 7 is selected.
          lda   $28             ; save BASL
          pha
          lda   $29             ; save BASH
          pha
          ldx   #23             ; row 23
:         txa
          jsr   $fbc1           ; bascalc, preserves x
          ldy   #39
:         lda   ($28),y         ; read char from screen via BASL/BASH
          sta   ($28),y         ; write back, clearing message
          dey
          bpl   :-              ; if line not done
          dex
          bpl   :--             ; if screen not done
          pla
          sta   $29             ; restore BASH
          pla
          sta   $28             ; restore BASL
          lda   #$c8            ; slot 7+1
          bne   prslot
prscan:   jsr   xmess
          asc_hi "scan"
          .byte $00
          bra   done
.endproc
          DX_end
