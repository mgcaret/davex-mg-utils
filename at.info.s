; %help
; at.info -- Display AppleTalk info.
;
; syntax:   at.info
;
; Displays AppleTalk node number, bridge node, and (for IIgs) hardware
; ID and ROM version.
;
; If AppleTalk is offline, will print the slot number of an installed
; Workstation Card.
; %hend

.pc02
.include  "davex-mg.inc"

cardptr   = xczpage

          DX_start $ae00 ; load address
          DX_info $01,$12,dx_cc_iie_or_iigs,$00
          DX_ptab
          DX_end_ptab
          DX_desc "Display AppleTalk info."
          DX_main
          cli                   ; appletalk requires interrupt
          ATcall inforeq
          bcs   noatalk
          lda   thisnet
          ldy   thisnet+1
          jsr   xprdec_2
          jsr   xmess
          asc_hi "."
          .byte $00
          lda   #$00
          ldy   nodenum
          jsr   xprdec_2
          ldy   abridge
          beq   :+
          phy
          jsr   xmess
          asc_hi ", bridge "
          .byte $00
          ply
          lda   #$00
          jsr   xprdec_2
:         lda   #$8d
          jsr   cout
          sec
          jsr   checkmach
          bcs   :+
          ; display IIgs specific
          jsr   xmess
          asc_hi "Hardware ID: "
          .byte $00
          lda   #$00
          ldy   hwid
          jsr   xprdec_2
          jsr   xmess
          asc_hi  ", ROM version "
          .byte $00
          lda   #$00
          ldy   romver
          jsr   xprdec_2
          lda   #$8d
          jsr   cout
:         rts
noatalk:  lda   #$01
          jsr   xredirect
          jsr   xmess
          asc_hi "AppleTalk offline!"
          .byte $00
          jsr   FindCard
          bcs   :+              ; no card
          and   #$0f            ; get slot #
          pha
          jsr   xmess
          asc_hi "Workstation Card in slot "
          .byte $00
          pla
          tay
          lda   #$00
          jsr   xprdec_2
          jsr   xmess
          .byte $8d,$00
:         lda   #$ff
          jsr   xredirect
          jmp   xerr
.proc     FindCard 
          lda   #$f9            ; offset to ID bytes
          sta   cardptr
          lda   #$c7            ; start at slot 7
          sta   cardptr+1
NextSlot: ldy   #$03
:         lda   (cardptr),y
          cmp   idtbl,y         ; check ID byte
          bne   NoMatch
          dey
          bpl   :-
          ldy   #$04
          lda   (cardptr),y
          beq   NoMatch	        ; Skip IIgs AppleTalk
          cmp   #$01            ; Workstation card?
          bne   NoCard          ; nope, something else
          clc
          lda   cardptr+1       ; get slot
          rts
NoMatch:  dec   cardptr+1
          lda   cardptr+1
          cmp   #$c0	;are we finished scanning the slots?
          bne   NextSlot
NoCard:   lda   #$00
          sec
          rts
idtbl:	  .byte "ATLK"          ; msb off
.endproc
inforeq:  .byte 0,2             ; sync GetInfo
          .word $0000           ; result code
          .dword $00000000      ; completion address
thisnet:  .word $0000           ; this network #
abridge:  .byte $00             ; local bridge
hwid:     .byte $00             ; hardware ID, IIgs only
romver:   .word $00             ; ROM version, IIgs only
nodenum:  .byte $00             ; node number
          DX_end
