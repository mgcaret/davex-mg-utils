; %help
; at.zones -- Display AppleTalk zones.
;
; syntax:   at.info
;
; Displays local AppleTalk zone and all known zones.
; %hend

.pc02
.include  "davex-mg.inc"

sptr      = xczpage
altbuf    = filebuff3     ; if no dynamic mem avail
altbufsz  = $04           ; pages

          DX_start dx_mg_auto_origin ; load address
          DX_info $01,$12,dx_cc_iie_or_iigs,$00
          DX_ptab
          DX_end_ptab
          DX_desc "Display AppleTalk zones."
          DX_main
          cli                   ; appletalk requires interrupts
          ATcall inforeq
          bcc   :+
          jmp   noatalk
          jsr   getatbuf        ; allocate buffer
          sta   bufp1+1
          sty   bufp1
          sta   bufp2+1
          sty   bufp2
          stx   buflen2+1
:         lda   abridge
          sta   zbridge         ; zone req needs it
          ATcall  myzone
          bcs   nozone
          lda   bufp1+1
          sta   sptr+1
          lda   bufp1
          sta   sptr
          ldy   #$00
          lda   (sptr),y
          beq   nozone
          jsr   xmess
          asc_hi "Local zone: "
          .byte $00
          jsr   prpas
          lda   #$8d
          jsr   cout
nozone:   ATcall getzones
          bcs   nozones         ; TODO: check for buffer overflow and display what we get
          lda   numzones+1
          ora   numzones
          beq   nozones         ; short circuit if no zones
          jsr   xmess
          asc_hi "Zone list:"
          .byte $8d,$00
          lda   bufp2+1         ; set up pointer
          sta   sptr+1
          lda   bufp2
          sta   sptr
przone:   ;lda   numzones+1
          ;ldy   numzones
          ;jsr   xprdec_2
          jsr   xmess
          asc_hi "  "
          .byte $00
          jsr   prpas           ; print zone name
          lda   #$8d
          jsr   cout
          lda   numzones
          sec
          sbc   #$01
          sta   numzones
          bcs   :+              ; no borrow
          dec   numzones+1
          bit   numzones+1      ; set N flag
          bmi   nozones
:         jsr   xcheck_wait
          bcs   nozones
          lda   numzones
          ora   numzones+1
          beq   nozones         ; if zero
          bra   przone 
nozones:  rts
noatalk:  lda   #$01
          jsr   xredirect
          jsr   xmess
          asc_hi "AppleTalk offline!"
          .byte $00
exiterr:  lda   #$ff
          jsr   xredirect
          jmp   xerr
          ; print pascal string at sptr
          ; leave sptr pointed at one past end
          ; of string
.proc     prpas
          ldy   #$00
          lda   (sptr),y        ; get length
          tax
next:     lda   sptr
          clc
          adc   #$01
          sta   sptr
          bcc   :+
          inc   sptr+1
:         dex
          bpl   :+
          rts
:         lda   (sptr),y        ; get char
          ora   #$80            ; make printable
          jsr   cout
          bra   next
.endproc
; allocate a big buffer for appletalk operations
; returns ay = start, x = size in pages
; tries to use dynamic mem for operations
; otherwise returns altbuf
.proc     getatbuf
          ldx   #mli_read
          jsr   xmmgr
          bcs   usealt
          cmp   #altbufsz
          bcc   usealt
          sta   tmp             ; save num pages
          ldx   #mli_open
          jsr   xmmgr           ; allocate all
          bcc   usealt          ; if error
          ldx   tmp             ; get num pages
          ldy   #$00            ; always on page boundary
          rts
usealt:   ldy   #<altbuf
          lda   #>altbuf
          ldx   altbufsz
          rts
tmp:      .byte $00
.endproc
inforeq:  .byte 0,2             ; sync GetInfo
          .word $0000           ; result code
          .dword $00000000      ; completion address
thisnet:  .word $0000           ; this network #
abridge:  .byte $00             ; local bridge
hwid:     .byte $00             ; hardware ID, IIgs only
romver:   .word $00             ; ROM version, IIgs only
nodenum:  .byte $00             ; node number
;
myzone:   .byte 0,$1a           ; sync GetMyZone
          .word $0000           ; result
          .dword $00000000      ; completion
bufp1:    .dword altbuf         ; buffer
          .byte 4,4             ; 4 times every 1 sec
          .word $0000           ; reserved
;
getzones: .byte 0,$1b           ; async GetZoneList
          .word $0000           ; result
          .dword $00000000      ; completion
buflen2:  .word altbufsz*$100   ; buffer len
bufp2:    .dword altbuf         ; buffer
zbridge:  .byte $00             ; bridge node
          .word $0001           ; start index
          .byte 8,16            ; 16 times every 2 secs
numzones: .word $0000           ; num zones returned
          .dword $0000000       ; reserved
          DX_end
