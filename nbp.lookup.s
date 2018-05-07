; %help
; nbp.lookup    [nbp-name]
;
; Perform NBP lookups.
;
; options:
;  nbp-name     Optional, name or partial name to lookup.
;
;  -v           Verbose mode, prints query and zone
;               info.
;
; Returns a list of addresses and associated NBP entries.
; or "No results." if none found.
; %hend

.pc02     ; appletalk requires it anyway
.include  "davex-mg.inc"

;ch        = $24                 ; cursor horizontal pos

sptr      = xczpage
sptr2     = sptr+2
stemp     = sptr2+2
verbose   = stemp+1             ; flag to show zone

zonebuf   = filebuff2           ; for local zone name
namebuf   = zonebuf+$0100       ; for building NBP name for lookup req
altbuf    = filebuff3           ; used if no dynamic mem avail
altbufsz  = $04                 ; pages

          DX_start dx_mg_auto_origin ; load address
          DX_info $02,$12,dx_cc_iie_or_iigs,$00
          DX_ptab
          DX_parm $00,t_string  ; name
          DX_parm 'v',t_nil     ; verbose
          DX_end_ptab
          DX_desc "Perform AppleTalk NBP lookups."
          DX_main
          cli                   ; appletalk requires interrupts
          ATcall inforeq
          bcc   :+
          jmp   noatalk         ; commented for debug
:         jsr   getatbuf        ; allocate buffer
          sta   bufp+1
          sty   bufp
          stx   buflen+1
          ; process command line parms
          stz   verbose
          lda   #'v'|$80        ; verbose
          jsr   xgetparm_ch
          bcs   :+
          inc   verbose
:         lda   #<namebuf
          sta   sptr
          lda   #>namebuf
          sta   sptr+1
          ; get name
          lda   #$00
          jsr   xgetparm_n
          ldx   #$80            ; name, return wildcard if not given
          jsr   get_nbp
          bcs   badnbp1
          inx
          txa
          jsr   addsptr
          ; get type
          lda   #$00
          jsr   xgetparm_n
          ldx   #$81            ; type, return wildcard if not given
          jsr   get_nbp
badnbp1:  bcs   badnbp
          inx
          txa
          jsr   addsptr
          ; get zone
          lda   #$00
          jsr   xgetparm_n
          ldx   #$82            ; type, return * if not given
          jsr   get_nbp
          bcs   badnbp
          ; sptr points at where we put the zone
          ldy   #$01
          lda   (sptr),y
          cmp   #'*'            ; were we given it?
          bne   doit
          ; get local zone if possible
          ATcall  myzone
          bcs   doit            ; error, just stick with what we got
          lda   zonebuf         ; if none
          beq   doit            ; attempt with default value
          ldx   #<zonebuf       ; copy returned zone in place of what we have
          lda   #>zonebuf
          ldy   #$00
          jsr   copystr
          ; now we have a complete name to look up
          ; rts                 ; DEBUG
doit:     lda   #<namebuf
          sta   sptr
          lda   #>namebuf
          sta   sptr+1
          lda   verbose
          beq   :+              ; don't print unless verbose
          jsr   prnbptup
          jsr   xmess
          .byte $8d,$00
:         ATcall lkupreq
          bcc   :+
          jmp   error           ; TODO: check for buffer overflow and display what we get
:         lda   matches
          bne   :+
          jsr   xmess
          asc_hi "No results."
          .byte $8d,$00
          rts
:         lda   bufp+1          ; set up pointer to response buffer
          sta   sptr+1
          lda   bufp
          sta   sptr
prent:    ;lda   #$00           ; this and next 2 debugging
          ;ldy   matches
          ;jsr   xprdec_2
          jsr   prnbpent
          jsr   xcheck_wait
          bcs   :+              ; if user pressed escape
          dec   matches
          bne   prent
:         rts
badnbp:   lda   #$01
          jsr   xredirect
          jsr   xmess
          asc_hi "Bad name."
          .byte $00
          bra   exiterr
noatalk:  lda   #$01
          jsr   xredirect
          jsr   xmess
          asc_hi "AppleTalk offline!"
          .byte $00
exiterr:  lda   #$ff
          jsr   xredirect
error:    jmp   xerr
;
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
; print an NBP entry at sptr
.proc     prnbpent
          ldy   #$00            ; offset into entry, net number low byte
          lda   (sptr),y        ; big end of network num
          pha
          iny
          lda   (sptr),y
          tay
          pla
          jsr   xprdec_2
          jsr   xmess
          asc_hi "."
          .byte $00
          ; print node
          ldy   #$02
          lda   (sptr),y
          tay
          lda   #$00
          jsr   xprdec_2
          jsr   xmess
          asc_hi ":"
          .byte $00
          ; print socket
          ldy   #$03
          lda   (sptr),y
          tay
          lda   #$00
          jsr   xprdec_2
          jsr   xmess
          asc_hi " "            ; print space in case output is not to screen.
          .byte $00
          lda   #20             ; Position NBP tuple on screen.
          sta   ch
          lda   #$05            ; offset to NBP tuple
          jsr   addsptr
          jsr   prnbptup
          jsr   xmess           ; CR
          .byte $8d,$00
          rts
.endproc
; print an NBP tuple at sptr
; leave sptr at byte just after tuple
.proc     prnbptup
          ; print name
          jsr   prpas
          jsr   xmess
          asc_hi ":"
          .byte $00
          ; print type
          jsr   prpas
          lda   verbose
          beq   skipzone        ; if not verbose, don't display @zone
          jsr   xmess
          asc_hi "@"
          .byte $00
          ; print zone
          jsr   prpas
          bra   :+
skipzone: ldy   #$00
          lda   (sptr),y        ; get length of zone name
          inc   a               ; account for length byte
          jsr   addsptr         ; and skip the lot
:         rts                   ; done
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
; allocate a big buffer for appletalk operations
; returns ay = start, x = size in pages
; tries to use dynamic mem for operations
; otherwise returns altbuf, which should usually
; be one of the file buffers.
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
myzone:   .byte 0,$1a           ; sync GetMyZone
          .word $0000           ; result
          .dword $00000000      ; completion
          .dword zonebuf        ; buffer, needs at least 33 bytes
          .byte 4,4             ; 4 times every 1 sec
          .word $0000           ; reserved
;
lkupreq:  .byte 0,$10           ; sync LookupName
          .word $0000           ; result
          .dword $00000000      ; completion
          .dword namebuf        ; name pointer
          .byte 4,4             ; 4 times every 1 sec
          .word $0000           ; reserved
buflen:   .word $0000           ; buffer length
bufp:     .dword $00000000      ; buffer pointer
          .byte $ff             ; max # matches
matches:  .byte $00             ; actual matches
          DX_end
