; %help
; afp.timezone - Set workstation AFP timezone.
;
; usage: afp.timezone w/e <0-24>
;
; Adjusts workstation time zone west or east relative to AFP server.
; 
; %hend

.pc02
.include  "davex-mg.inc"

sptr      = xczpage

prbyte		= $fdda
;cout      = $fded

pfxbuf    = filebuff2

          DX_start dx_mg_auto_origin ; load address
          DX_info $01,$12,dx_cc_iie_or_iigs,$00
          DX_ptab
          DX_parm $00,t_string    ; +/-
          DX_parm $00,t_int1      ; hours
          DX_end_ptab
          DX_desc "Set AFP time zone."
          DX_main
          cli                     ; appletalk requires interrupts
          ;bra   :+                ; DELETEME
          ATcall inforeq
          bcc   :+
          jmp   noatalk
:         lda   #$01              ; second parm
          jsr   xgetparm_n        ; davex ensures this is here
          cpy   #24
          bcs   badparms
          sty   timeflag
          lda   #$00
          jsr   xgetparm_n        ; zero length if not specified
          sta   sptr+1
          sty   sptr
          ldy   #$00
          lda   (sptr),y
          beq   badparms
          ; next two commented out, user might say west or east instead of w or e
          ;cmp   #$01
          ;bne   badparms
          iny
          lda   (sptr),y
          and   #$5f              ; upper case and strip high bit
          cmp   #'W'
          beq   dowest
          cmp   #'E'
          beq   doit
          jsr   cout
          ; fall through if not +
badparms: lda   #$01
          jsr   xredirect
          jsr   xmess
          asc_hi "Bad parameter(s)!"
          .byte $00
          bra   exiterr
;
dowest:   lda   #$80
          ora   timeflag
          sta   timeflag
doit:     
          ;lda   timeflag
          ;jsr   prbyte
          ;lda   #$8d
          ;jsr   cout
          ATcall tzreq
          bcs   tzerr
          rts                     ; buhbye
;
tzerr:    lda   #$01
          jsr   xredirect
          jsr   xmess
          asc_hi "Set time zone failed!"
          .byte $00
          bra   exiterr
noatalk:  lda   #$01
          jsr   xredirect
          jsr   xmess
          asc_hi "AppleTalk offline!"
          .byte $00
exiterr:  lda   #$ff
          jsr   xredirect
          jmp   xerr
;
inforeq:  .byte 0,2             ; sync GetInfo
          .word $0000           ; result code
          .dword $00000000      ; completion address
thisnet:  .word $0000           ; this network #
abridge:  .byte $00             ; local bridge
hwid:     .byte $00             ; hardware ID, IIgs only
romver:   .word $00             ; ROM version, IIgs only
nodenum:  .byte $00             ; node number
;
tzreq:    .byte 0,$30           ; sync FITimeZone
          .word $0000           ; result
timeflag: .byte $00             ; time flag (b6-0=hours,b7=1 subtract)
          DX_end
