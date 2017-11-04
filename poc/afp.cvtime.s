; %help
; afp.cvtime -- Test AppleTalk time/date conversion.
;
; syntax:   afp.cvtime [-a int] [-s int] [-z]
;
; This program verifies unusual behavior in the AppleTalk card's handling
; of the ConvertTime call.
;
; The card "remembers" the seconds that were given to it by the last AFP
; to P8 time conversion, and uses them when converting from P8 to AFP.
;
; This program has a fixed time in it, use -a and -s to add and subtract
; seconds.
;
; Use -z to perform a conversion to Jan 1, 2000 12:00 AM between converting
; to and from, and note the difference in the converted forward vs converted
; back values.
; %hend

.pc02
.include  "davex-mg.inc"

prbyte    = $fdda

myptr     = xczpage

          DX_start $ae00 ; load address
          DX_info $01,$12,dx_cc_iie_or_iigs,$00
          DX_ptab
          DX_parm 'a',t_int3    ; add
          DX_parm 's',t_int3    ; subtract
          DX_parm 'z',t_nil     ; zero
          DX_end_ptab
          DX_desc "Display AppleTalk info."
          DX_main
          cli                   ; appletalk requires interrupt
          ; do this before error for testing
          ldy   #<From1
          lda   #>From1
          jsr   print4          ; print original From1
          jsr   mycr          
          ATcall inforeq
          bcc   :+
          jmp   noatalk
:         lda   #'a'|$80
          jsr   xgetparm_ch
          bcs   :+
          sta   toadd+1
          stx   toadd+2
          sty   toadd+3
:         lda   #'s'|$80
          jsr   xgetparm_ch
          bcs   :+
          sta   tosub+1
          stx   tosub+2
          sty   tosub+3
          ; big-endian add any specified # of seconds
:         clc
          ldx   #$03
:         lda   From1,x
          adc   toadd,x
          sta   From1,x
          dex
          bpl   :-
          ; big-endian subtract any specified # of seconds
          sec
          ldx   #$03
:         lda   From1,x
          sbc   tosub,x
          sta   From1,x
          dex
          bpl   :-
          ldy   #<From1
          lda   #>From1
          jsr   print4          ; print adjusted From1
          jsr   mycr
          ; Now perform network to P8 conversion
          ATcall Net2P8
          bcs   cnverr
          ldy   #<To1
          lda   #>To1
          jsr   print4          ; display converted-to bytes
          jsr   mycr
          ; Display in human-readable format
          ldy   To1
          lda   To1+1
          jsr   xpr_date_ay
          lda   #' '|$80
          jsr   cout
          ldy   To1+2
          lda   To1+3
          jsr   xpr_time_ay
          jsr   mycr
          lda   #'z'|$80
          jsr   xgetparm_ch
          bcs   :+
          ; This should reset the saved seconds number
          ATcall ResetSec
          bcs   cnverr
          ; Copy to reverse conversion
:         ldx   #$03
:         lda   To1,x
          sta   From2,x
          dex
          bpl   :-
          ; Perform P8 to network conversion
          ATcall P82Net
          bcs   cnverr
          ldy   #<To2
          lda   #>To2
          jsr   print4          ; display converted-back bytes
          jsr   mycr
          rts
cnverr:   lda   #$01
          jsr   xredirect
          jsr   xmess
          asc_hi "Conversion error!"
          .byte $8d,$00
          bra   exiterr
noatalk:  lda   #$01
          jsr   xredirect
          jsr   xmess
          asc_hi "AppleTalk offline!"
          .byte $8d,$00
exiterr:  lda   #$ff
          jsr   xredirect
          jmp   xerr
;
.proc     print4
          sta   myptr+1
          sty   myptr
          ldy   #$00
:         lda   (myptr),y
          jsr   prbyte
          iny
          cpy   #$04
          bne   :-
          ldy   myptr
          rts
.endproc
.proc     mycr
          lda   #$8d
          jmp   cout
.endproc
;
toadd:    .res 4,0              ; BIG ENDIAN
tosub:    .res 4,0              ; BIG ENDIAN
;
inforeq:  .byte 0,2             ; sync GetInfo
          .word $0000           ; result code
          .dword $00000000      ; completion address
thisnet:  .word $0000           ; this network #
abridge:  .byte $00             ; local bridge
hwid:     .byte $00             ; hardware ID, IIgs only
romver:   .word $00             ; ROM version, IIgs only
nodenum:  .byte $00             ; node number
; Convert time parameters - network to P8
Net2P8:   .byte 0,$34           ; sync ConvertTime
          .word $0000           ; result
          .byte $00             ; 0 = from AFP to ProDOS, 1 = reverse
From1:    .byte $21,$90,$14,$cd ; BIG ENDIAN, 4-Nov-17 6:03:57 AM GMT
To1:      .dword $00000000      ; P8 Time
; Convert time parameters - P8 to network
P82Net:   .byte 0,$34           ; sync ConvertTime
          .word $0000           ; result
          .byte $01             ; 1 = ProDOS to AFP
From2:    .dword $00000000      ; P8 Time
To2:      .dword $00000000      ; BIG ENDIAN
ResetSec: .byte 0,$34           ; sync ConvertTime
          .word $0000           ; result
          .byte $00             ; 0 = from AFP to ProDOS, 1 = reverse
          .dword $00000000      ; BIG ENDIAN, Jan 1, 2000 12:00 AM GMT
          .dword $00000000      ; P8 Time
          DX_end
