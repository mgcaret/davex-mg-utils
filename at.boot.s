; %help
; at.boot - Boot over AppleTalk.
; %hend

; TODO:  If AT is not initialized, then:
; If //e:  Find and init workstation card and execute boot if it's not already
; initialized.
; If IIgs: Try Apple IIgs AT boot vectors.

.pc02
.include  "davex-mg.inc"

          DX_start dx_mg_auto_origin ; load address
          DX_info $01,$12,dx_cc_iie_or_iigs,$00
          DX_ptab
          DX_end_ptab
          DX_desc "Reboot over the network."
          DX_main
          cli                   ; appletalk requires interrupt
          ATcall inforeq
          bcs   noatalk
          ATcall boot
          lda   #$01
          jsr   xredirect
          jsr   xmess
          asc_hi "Network boot failed!"
          .byte $00
          bra   errexit          
noatalk:  lda   #$01
          jsr   xredirect
          jsr   xmess
          asc_hi "AppleTalk offline!"
          .byte $00
errexit:  lda   #$ff
          jsr   xredirect
          jmp   xerr
inforeq:  .byte 0,2             ; sync GetInfo
          .word $0000           ; result code
          .dword $00000000      ; completion address
thisnet:  .word $0000           ; this network #
abridge:  .byte $00             ; local bridge
hwid:     .byte $00             ; hardware ID, IIgs only
romver:   .word $00             ; ROM version, IIgs only
nodenum:  .byte $00             ; node number
boot:     .byte $00,$06         ; sync boot
          .word $0000           ; result code
          DX_end
