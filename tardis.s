; %help
; tardis - Get date/time from TimeLord server.
;
; options:
;  -v             Verbose mode, prints server and other info
;  -n <nbp-name>  Specify NBP query, default =:TimeLord@*
;  -p             Set ProDOS global page date/time
;  -y             Patch ProDOS year table to use returned year
;  -s <type>      Set a clock of <type>, current supported
;                 types: none
;  -e <hours>     Adjust time eastward
;  -w <hours>     Adjust time westward
; %hend

; Notes: AFP epoch is 12:00 AM January 1, 2000 GMT.
;        Mac epoch is 12:00 AM January 1, 1904 GMT.
; the difference is 3029529600 ($b492f400) seconds
; Mac epoch is returned by TimeLord.
; AFP epoch is expected by WS Card ConvertTime function.

; The general algorithm is:
; Request the date from TimeLord.
; Subtract the epoch difference from the response to get AFP time.
; Use the FIConvertTime call to convert to ProDOS date and time.
; That gives us month/day/year/hour/minute
; To get seconds:
;   Convert the ProDOS date and time back to AFP time.
;   This number should be <= the AFP time we got above because the seconds are assumed
;   to be 0.
;   Subtract to get the seconds.
;   In practice this does not work with the Apple II Workstation Card because its
;   FIConvertTime function is buggy.  It is sometimes off by 2 mins or more and the
;   reverse conversion gives garbage.
;   The IIgs this algo works perfect.
;   So for now we don't give seconds on a IIe.

; Should probably rewrite this to just do the math rather than use FIConverTime.

.pc02
.include  "davex-mg.inc"

;ch        = $24                 ; cursor horizontal pos
TL_OK      = 12
TL_BAD     = 10

entname   = filebuff2           ; buffer to build NBP entity name
NBPBuf    = filebuff3           ; buffer for NBP request
NBPBufSz  = $0400               ; size of the file buffer

P8DtTm    = $bf90
;prbyte    = $fdda

sptr      = xczpage
sptr2     = sptr+2
stemp     = sptr2+2
verbose   = stemp+1             ; verbose flag
seconds   = verbose+1           ; holds seconds
notgs     = seconds + 1

          DX_start dx_mg_auto_origin ; load address
          DX_info $03,$12,dx_cc_iie_or_iigs,$00
          DX_ptab
          DX_parm 'v',t_nil     ; verbose
          DX_parm 'n',t_string  ; name
          DX_parm 'z',t_string  ; zone
          DX_parm 'p',t_nil     ; set prodos time
          DX_parm 'y',t_nil     ; patch year table
          DX_parm 's',t_string  ; set clock
          DX_parm 'e',t_int1    ; east adjust
          DX_parm 'w',t_int1    ; west adjust
          DX_end_ptab
          DX_desc "Get time from TimeLord server."
          DX_main
          sec
          jsr   $FE1F           ; GS ID
          ror   notgs           ; notGS now negative if it's not a GS
          cli                   ; appletalk requires interrupts
          ATcall inforeq
          bcc   :+
          jmp   noatalk
:         stz   verbose
          lda   #'v'|$80        ; verbose
          jsr   xgetparm_ch
          bcs   :+
          inc   verbose
:         lda   #<entname
          sta   sptr
          lda   #>entname
          sta   sptr+1
          ; get name
          lda   #'n'|$80        ; NBP name param
          jsr   xgetparm_ch
          bcc   :+              ; was specified
          ; name param not given, use default
          lda   #<entname
          sta   sptr
          lda   #>entname
          sta   sptr+1
          ldy   #$00
          ldx   #<defname
          lda   #>defname
          jsr   copystr
          ldx   #<deftype
          lda   #>deftype
          jsr   copystr
          ldx   #<defzone
          lda   #>defzone
          jsr   copystr
          jmp   doit
:         ldx   #$80            ; name, return wildcard if not given
          jsr   get_nbp
          bcs   badnbp
          inx
          txa
          jsr   addsptr
          ; get type
          lda   #'n'|$80
          jsr   xgetparm_ch
          ldx   #$01            ; type, return empty if not given
          jsr   get_nbp
          bcc   :+
          ldy   #$00
          ldx   #<deftype
          lda   #>deftype
          jsr   copystr
          tya
          bra   :++             ; to addsptr
:         inx
          txa
:         jsr   addsptr
          ; get zone
          lda   #'n'+$80
          jsr   xgetparm_ch
          ldx   #$82            ; zone, return * if not given
          jsr   get_nbp
          bcc   doit
badnbp:   lda   #$01
          jsr   xredirect
          jsr   xmess
          asc_hi "Bad NBP name."
          .byte $00
          bra   exiterr1
doit:     lda   #<entname
          sta   sptr
          lda   #>entname
          sta   sptr+1
          lda   verbose
          beq   :+              ; don't print unless verbose
          jsr   prnbptup
          jsr   xmess
          .byte $8d,$00
          ; Locate TimeLord on network
:         ATcall lookup
          bcc   :+              ; no error, don't bail
notlord:  lda   #$01
          jsr   xredirect
          jsr   xmess
          asc_hi "No TimeLord found!"
          .byte $00
exiterr1: jmp   exiterr     
:         lda   matches         ; check # matches
          beq   notlord         ; no matches
          ; Copy found address/socket to request
          ldx   #3
:         lda   NBPBuf,x
          sta   ATPaddr,x
          dex
          bpl   :-
          lda   verbose
          beq   :+
          ; display NBP entry if verbose
          lda   bufp+1          ; set up pointer to response buffer
          sta   sptr+1
          lda   bufp
          sta   sptr
          jsr   prnbpent
          ; now make ATP request to TimeLord
:         lda   #$01
          sta   ATPbmap
          ATcall ATPparms
          bcc   :+
          jmp   notime          ; if error, bail now
          ; see if verbose
:         lda   #'v'|$80
          jsr   xgetparm_ch
          bcs   :++             ; skip if not verbose
          ; display raw returned data in hex in little-endian format
          ldx   #$03
:         lda   To,x
          phx
          jsr   prbyte
          plx
          dex
          bpl   :-
          ; display timelord status byte from user bytes
          lda   #'/'|$80
          jsr   cout
          lda   Status
          jsr   prbyte
          lda   #$8d
          jsr   cout
          ; check status
:         lda   Status
          cmp   #TL_OK
          bne   notime1
          ; now do big-endian subtraction of the base offset
          ; and simultaneously put the computed value in From
          sec
          ldx   #$03
:         lda   To,x
          sbc   Base,x
          sta   From,x
          dex
          bpl   :-
          ; Apply user-requested adjustments:
          lda   #'e'|$80
          jsr   xgetparm_ch
          bcs   ckwest
          cpy   #$01
          bcc   ckwest
          ; now do big-endian addition of eastward adjustment
:         clc
          ldx   #$03
:         lda   From,x
          adc   Hour,x
          sta   From,x
          dex
          bpl   :-
          dey
          bne   :--         
ckwest:   lda   #'w'|$80
          jsr   xgetparm_ch
          bcs   convert
          cpy   #$01
          bcc   convert
          ; do big-endian subtraction of westward adjustment
:         sec
          ldx   #$03
:         lda   From,x
          sbc   Hour,x
          sta   From,x
          dex
          bpl   :-
          dey
          bne   :--
          ; Use the WS card to convert value in From to ProDOS format in To
convert:  ATcall CvtParms
          bcs   notime1         ; bail if error
          ATcall ResetSec       ; sigh... weird firmware
          bcs   notime1
          bit   notgs
          stz   seconds
          bmi   setp8           ; skip calculate seconds if not IIgs
          ; calculate seconds
          ; Copy converted values to the other convert parm list
          ldx   #$03
:         lda   To,x
          sta   SecFrom,x
          dex
          bpl   :-
          ATcall SecParms       ; convert back
          bcc   :+
notime1:  jmp   notime          ; bail if error
:         lda   From+3
          sec
          sbc   SecTo+3
          sta   seconds
          ; set Prodos date/time if asked
setp8:    lda   #'p'|$80        ; set prodos date/time?
          jsr   xgetparm_ch
          bcs   nosetp8         ; skip if -p not given
          ; Copy converted values to the global page
          ldx   #$03
:         lda   To,x
          sta   P8DtTm,x
          dex
          bpl   :-
          ; patch year table?
nosetp8:  lda   #'y'|$80
          jsr   xgetparm_ch
          bcs   nosetyt
          lda   $c08b
          lda   $c08b
          ldx   #$03
:         lda   $d7b4,x         ; tdays table, at september
          cmp   tdaysv,x        ; that what ProDOS has?
          bne   setyterr        ; not the thunderclock driver
          dex
          bpl   :-
          lda   To+1            ; year (b7-1), upper bit of month (b0)
          lsr
          ldx   #6
:         sta   $d7b8,x         ; address of year table
          dex
          bpl   :-
          bit   $c082           ; ROM back
          bra   nosetyt
setyterr: bit   $c082           ; ROM back
          lda   #$01
          jsr   xredirect
          jsr   xmess
          asc_hi "Can't patch clock driver!"
          .byte $00
          bra   exiterr
nosetyt:  ; TODO: set NSC or ThunderClock or something
          ; Display date/time
          ldy   To
          lda   To+1
          jsr   xpr_date_ay
          jsr   xmess
          asc_hi " "
          .byte $00
          ldy   To+2
          lda   To+3
          jsr   xpr_time_ay
          bit   notgs
          bmi   nosec
          jsr   xmess
          asc_hi " +"
          .byte $00
          ldy   seconds
          cpy   #10
          bcs   :+
          lda   #'0'|$80
          jsr   cout            ; leading zero
:         ldy   seconds
          lda   #$00            ; zero high byte
          jsr   xprdec_2        ; print seconds
          jsr   xmess
          .byte "s"
          .byte $8d,$00
          rts
nosec:    lda   #$8d
          jsr   cout
          rts
notime:   lda   #$01
          jsr   xredirect
          jsr   xmess
          asc_hi "Error getting date/time!"
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
;
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
          ;lda   #20             ; Position NBP tuple on screen.
          ;sta   ch
          lda   #$05            ; offset to NBP tuple
          jsr   addsptr
          jsr   prnbptup
          jsr   xmess           ; CR
          .byte $8d,$00
          rts
.endproc
; print an NBP tuple at sptr
; leave sptr at byte just after tuple
; does not print zone unless verbose flag set
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
          sta   sptr2+1
          stx   sptr2
          sty   stemp           ; save offset
          ldy   #$00
          lda   (sptr2),y       ; get number of chars
          tax                   ; to copy
          ldy   stemp           ; get the offset
          sta   (sptr),y        ; store the length byte
          inc   stemp           ; increment the offset
          inc   sptr2           ; next source char
          bne   :+
          inc   sptr2+1
:         ldy #0
          ; copy loop
:         phy
          lda   (sptr2),y
          ldy   stemp
          sta   (sptr),y
          inc   stemp
          ply
          iny
          dex
          bne   :-
          ldy   stemp
          rts
.endproc
;
; table to validate the P8 tclock driver, 4 bytes at $d7b4
tdaysv:   .byte 242,20,51,81
;
inforeq:  .byte 0,2             ; sync GetInfo
          .word $0000           ; result code
          .dword $00000000      ; completion address
thisnet:  .word $0000           ; this network #
abridge:  .byte $00             ; local bridge
hwid:     .byte $00             ; hardware ID, IIgs only
romver:   .word $00             ; ROM version, IIgs only
nodenum:  .byte $00             ; node number
; some pointers & values for building names
defname:  .byte 1,"="           ; object
deftype:  .byte 8,"TimeLord"    ; type
defzone:  .byte 1,"*"           ; zone
; Base offset for epoch conversion, in big-endian order
;Base:     .byte $B4,$93,$56,$70 ; PDT
Base:     .byte $b4,$92,$f4,$00 ; GMT - set timezone or use adjust options
; Hours adjustment value
Hour:     .byte $00,$00,$0e,$10 ; 3600 seconds
; parameter list for NBPLookup
lookup:   .byte 0,16            ; sync NBPLookup
          .word $0000           ; result
          .dword $00000000      ; completion
          .dword entname        ; pointer to name to find
          .byte 4,4             ; 4 times, every 1 sec
          .word $0000           ; reserved
          .word NBPBufSz        ; buffer size
bufp:     .dword NBPBuf         ; buffer loc
          .byte 1               ; matches wanted
matches:  .byte $00             ; matches found
; ATP request parameters
ATPparms: .byte 0,18            ; sync SendATPReq
          .word $0000           ; result
          .dword $00000000      ; compl. addr
          .byte $00             ; socket #
ATPaddr:  .dword $00000000      ; destination address
          .word $0000           ; TID
          .word $0000           ; req buffer size
          .dword $00000000      ; req buffer addr
          .dword $00000000      ; user bytes, $00 = get time
          .byte $01             ; one response buffer
          .dword BDS            ; pointer to response BDS
          .byte $00             ; ATP flags
          .byte 4,4             ; try 4 times every 1/4 second
ATPbmap:  .byte $00             ; bitmap of blocks to recieve
          .byte $00             ; number of responses
          .res  6               ; 6 bytes reserved
; BDS for ATP request
BDS:      .word $0004           ; 4-byte buffer for basic response from TimeLord
          .dword To             ; Buffer pointer
Status:   .dword $00000000      ; returned user bytes, first byte = 12 if OK
          .word $0000           ; actual length
; Convert time parameters - P8 to network for calculating seconds
SecParms: .byte 0,$34           ; sync ConvertTime
          .word $0000           ; result
          .byte $01             ; 1 = ProDOS to AFP
SecFrom:  .dword $00000000
SecTo:    .dword $00000000
; Convert time to reset seconds to 0 because the firmware saves the seconds
; and reuses it when converting from P8 to network time.
ResetSec: .byte 0,$34           ; sync ConvertTime
          .word $0000           ; result
          .byte $00             ; 0 = from AFP to ProDOS, 1 = reverse
          .dword $00000000      ; BIG ENDIAN, Jan 1, 2000 12:00 AM GMT
          .dword $00000000      ; P8 Time
; Convert time parameters - network to P8
; note that ATP response is written to From
CvtParms: .byte 0,$34           ; sync ConvertTime
          .word $0000           ; result
CvtDir:   .byte $00             ; 0 = from AFP to ProDOS, 1 = reverse
From:     .dword $00000000
To:       .dword $00000000      ; initially contains time from ATP response
          DX_end
