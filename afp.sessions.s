; %help
; afp.sessions - Display AFP sessions.
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
          DX_desc "Display AFP sessions."
          DX_main
          cli                   ; appletalk requires interrupts
          ATcall inforeq
          bcc   :+
          jmp   noatalk
:         jsr   getatbuf
          sta   bufp+1
          sty   bufp
          stx   buflen+1
          ATcall filsess
          bcs   nosess1
          lda   nument          ; number of entries returned
          bne   :+
          jsr   xmess
          asc_hi "No sessions."
          .byte $8d,$00
nosess1:  jmp   nosess
:         lda   bufp+1
          sta   sptr+1
          lda   bufp
          sta   sptr
          jsr   xmess
          asc_hi " session   vol id dev  volume name (* = user volume)"
          .byte $8d
          asc_hi " -------   ------ ---  --------------------------------"
          .byte $8d,$00
          ; loop to display sessions
dispsess: ldy   #$00
          sty   num+1
          sty   num+2
          lda   (sptr),y        ; ref num
          sta   num
          jsr   xprdec_pad
          lda   #' '+$80
          jsr   cout
          ldy   #$1e            ; offset of volume id
          lda   (sptr),y
          sta   num
          iny
          lda   (sptr),y
          sta   num
          jsr   xprdec_pad
          lda   #' '+$80
          jsr   cout
          ldy   #$01            ; offset of devnum
          lda   (sptr),y
          pha
          jsr   xprint_sd
          lda   #' '+$80
          jsr   cout
          pla
          and   #$01            ; isolate user volume flag
          beq   notuser
          lda   #'*'+$80
          bne   :+
notuser:  lda   #' '+$80 
:         jsr   cout
          lda   sptr+1
          pha                   ; save sptr
          lda   sptr
          pha 
          lda   #$02            ; offset to volume name
          jsr   addsptr
          jsr   prpas
          lda   #$8d
          jsr   cout
          pla                   ; restore sptr to where it was
          sta   sptr
          pla
          sta   sptr+1
          lda   #$20            ; size of entry
          jsr   addsptr         ; now point at next entry
          jsr   xcheck_wait     ; see if should pause or quit
          bcs   nosess          ; quit if esc pressed
          dec   nument
          bne   dispsess
nosess:   ;ldx   mli_close       ; free mem
          ;jsr   xmmgr           ; do it
          rts
noatalk:  lda   #$01
          jsr   xredirect
          jsr   xmess
          asc_hi "AppleTalk offline!"
          .byte $00
exiterr:  lda   #$ff
          jsr   xredirect
          jmp   xerr
;
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
filsess:  .byte 0,$2f           ; sync FIListSessions
          .word $0000           ; result code
buflen:   .word altbufsz*$100   ; buffer length
bufp:     .dword altbuf         ; buffer pointer
nument:   .byte $00             ; entries returned
          DX_end
