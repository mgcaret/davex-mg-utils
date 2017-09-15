; %help
; dmem -- Display davex dynamic memory info.
;
; syntax:   dmem
; %hend

.pc02
.include  "davex-mg.inc"

          DX_start dx_mg_auto_origin ; load address
          DX_info $01,$12,dx_cc_any,$00
          DX_ptab
          DX_end_ptab
          DX_desc "Display DaveX dynamic memory info."
          DX_main
          jsr   xmess
          asc_hi "Dynamic memory available to external cmds:"
          .byte $8d,$8d,00
          ldx   #mli_close       ; free all
          jsr   xmmgr           ; ask for it          
          ; fall-through
.proc     showfree
          ldx   #mli_read        ; num free pages
          jsr   xmmgr           ; ask for it
          pha
          tay
          lda   #$00
          jsr   xprdec_2
          jsr   xmess
          asc_hi " page"
          .byte $00
          pla
          tay
          lda   #$00
          jsr   xplural
          jsr   xmess
          asc_hi " free"
          .byte  $8d
          asc_hi "Lowest page: $"
          .byte  $00
          ldx   #mli_gfinfo      ; lowest free page
          jsr   xmmgr
          jsr   $fdda           ; prbyte
          jsr   xmess
          .byte $8d,00
          rts
.endproc
          DX_end
