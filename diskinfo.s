; %help
; diskinfo -- Display ProDOS disk/volume info.
;
; syntax:   diskinfo
; %hend

.p02
.include  "davex-mg.inc"

tptr      = xczpage             ; generic pointer
tmp       = tptr+2
bdflag    = tmp+1               ; block device flag, if slot has block dev
atpres    = bdflag+2            ; bit 7 = off if AT present
atflag    = atpres+1            ; bit 7 set if current device is appletalk
spflag    = atflag+1            ; zero if current device is smartport
slot      = spflag+1            ; slot address of device being processed
unit      = slot+2              ; unit number being analyzed
devno     = unit+1              ; device number being analyzed
drvr      = devno+1             ; drvr = driver address

sessbuf   = filebuff3
sessbufsz = $200
onlinebuf = filebuff2

DEVCNT    = $bf31               ; # of devices-1 (there must always be 1+)
DEVLST    = $bf32
DEVADR    = $bf10

          DX_start dx_mg_auto_origin ; load address
          DX_info $01,$12,dx_cc_any,$00
          DX_ptab
          DX_end_ptab
          DX_desc "Display disk/volume info."
          DX_main main
          
.proc     main
          lda   #$00
          sta   slot            ; init low byte
          lda   DEVCNT
          sta   devno           ; init dev number
          jsr   atcheck         ; look for appletalk & get sessions if present
:         ldx   devno
          jsr   doid            ; identify/get info for it
          jsr   printinfo       ; print the device info
          dec   devno           ; next device
          bpl   :-
          rts
.endproc

; See TN.PDOS.20 and 21
; expects device number (relative to DEVADR) in x
.proc     doid
          lda   DEVLST,x        ; retrieve unit number
          and   #$f0            ; mask of worthless bit
          sta   unit            ; and put in current device unit number
          sta   spflag          ; initially flag no smartport (spflag = nonzero)
          lsr                   ; convert unit number to driver table offset
          lsr
          lsr
          tax
          lda   DEVADR,x        ; get the driver address
          sta   drvr            ; and save it for later
          lda   DEVADR+1,x
          sta   drvr+1
          jsr   isslot          ; see if that driver is in a slot
          sta   slot+1          ; zeroed if not slot device
          beq   :+              ; and done
          jsr   maybeat         ; is appletalk device?
          ror                   ; move carry into high bit of A
          sta   atflag          ; and make it flag
          rol                   ; restore carry
          bcs   :+              ; and done if appletalk
          ldy   #$07            ; offset of smartport ID
          lda   (slot),y
          sta   spflag          ; otherwise set smartport flag as needed
:         rts
.endproc

; at entry, expect unit, flags, drvr, and (slot) to be filled
; and tptr pointed to the AFP session entry if it's an AppleTalk volume
.proc     printinfo
          lda   unit
          jsr   xprint_sd       ; print .sd
          jsr   prcolon
          jsr   prvolinfo       ; volume information
          jsr   prdevice        ; device info & slot if applicable
          jsr   prdrvaddr       ; print driver location
          jsr   prinfo          ; slot smartport/intelligent controller info
          jsr   my_crout        ; extra space between devices
          rts
.endproc

; at entry, expect unit, flags, drvr, and (slot) to be filled
; and tptr pointed to the AFP session entry if it's an AppleTalk volume
.proc     prdevice
          jsr   tab5
          bit   atflag
          bpl   :+
          jmp   printat
:         jsr   ismirr
          bcc   :+
          jsr   xmess
          asc_hi "mirrored "
          .byte $00
:         lda   slot+1
          beq   rambased        ; ain't a slot device
          jsr   isdiskii
          beq   diskii          ; disk II
          lda   spflag
          bne   p8dev
          jsr   xmess
          asc_hi "SmartPort "
          .byte $00
          jmp   :+
p8dev:    jsr   xmess
          asc_hi "ProDOS "
          .byte $00
:         jsr   xmess
          asc_hi "device"
          .byte $00
crdone:   jmp   prslot
diskii:   jsr   xmess
          asc_hi "Disk II"
          .byte $00
          jmp   crdone
rambased: lda   drvr+1
          cmp   #$ff
          bne   :+
          jsr   xmess
          asc_hi "ProDOS RAMdisk"
          .byte $00
          jmp   crdone
:         jsr   xmess
          asc_hi "RAM-based "
          .byte $00
          jmp   :--             ; "device"
.endproc

.proc     prslot
          lda   slot+1
          beq   :+              ; can't do for non-slot devs
          jsr   xmess
          asc_hi ", installed in slot "
          .byte $00
          lda   slot+1
          sec
          sbc   #$10            ; $Cn->$Bn
          jsr   cout
:         jsr   my_crout
          rts
.endproc

.proc     prinfo
          lda   slot+1
          bne   :+
done:     rts                   ; can't do for non-slot devs
:         bit   atflag
          bpl   done
          lda   spflag
          bne   :+
          jmp   prspinfo        ; go do smartport info
:         jsr   isdiskii
          beq   done
          jsr   tab5
          jsr   xmess
          asc_hi "Intelligent disk controller"
          .byte $00
          jsr   my_crtab5
          ldy   #$fe
          lda   (slot),y
          pha
          and   #%11001100      ; flags we are checking
          beq   :++++           ; if none set, skip to # volumes
          and   #%10000000      ; removable flag
          beq   :+
          jsr   xmess
          asc_hi "Removable "
          .byte $00
:         pla
          pha
          and   #%01000000      ; interruptible flag
          beq   :+
          jsr   xmess
          asc_hi "Interruptible "
          .byte $00
:         pla
          pha
          and   #%00001000      ; formattable flag
          beq   :+
          jsr   xmess
          asc_hi "Formattable "
          .byte $00
:         pla
          pha
          and   #%00000100      ; writable flag
          bne   :+              ; reverse logic
          jsr   xmess
          asc_hi "Read-only "
          .byte $00
          jsr   my_crtab5
:         pla                   ; now print # volumes
          and   #%00110000      ; volumes-1
          lsr
          lsr
          lsr
          lsr
          clc
          adc   #$01
          pha
          tay
          lda   #$01
          jsr   xprdec_2
          jsr   xmess
          asc_hi " volume"
          .byte $00
          pla
          tay
          lda   #$00
          jsr   xplural         ;pluralize
          jsr   my_crout        ; done
          rts
.endproc

.proc     prspinfo
          ldy   #$fb
          lda   (slot),y
          pha
          and   #%10000011      ; flags we care about
          bne   :+
          pla
          rts         
:         jsr   tab5
          jsr   xmess
          asc_hi "SP Flags: "
          pla
          pha
          and   #%10000000      ; extended flag
          beq   :+
          jsr   xmess
          asc_hi "Extended "
          .byte $00
:         pla
          pha
          and   #%00000010      ; SCSI flag
          beq   :+
          jsr   xmess
          asc_hi "SCSI "
          .byte $00
:         pla
          and   #%00000001      ; RAM card flag
          beq   :+
          jsr   xmess
          asc_hi "RAMdisk "
          .byte $00
:         jmp   my_crout
.endproc

.proc     prdrvaddr
          jsr   tab5
          jsr   xmess
          asc_hi "Driver"
          .byte $00
          lda   drvr+1
          and   #$f0
          cmp   #$c0
          bne   :+
          jsr   xmess
          asc_hi " in ROM"
          .byte $00
:         jsr   xmess
          asc_hi " at $"
          .byte $00
          lda   drvr+1
          jsr   prbyte
          lda   drvr
          jsr   prbyte
          jmp   my_crout
.endproc

.proc     prvolinfo
          ;jsr   tab5           ; not needed since it's the first item
          lda   unit
          beq   novol           ; make sure it is not zero
          sta   parm_on_line+1
          P8call $c5, parm_on_line
          bcs   novol           ; don't proceed if error
          lda   onlinebuf+1
          and   #$f0
          cmp   unit
          bne   novol           ; don't proceed if unit unmatched
          lda   onlinebuf+1
          and   #$0f
          beq   novol           ; don't proceed if volume name is 0-length
          sta   onlinebuf       ; store at preceding location
          inc   onlinebuf       ; account for '/' we are adding
          lda   #'/'
          sta   onlinebuf+1     ; and add it
          ldy   #<onlinebuf
          lda   #>onlinebuf
          jsr   xprint_path
          P8call $c4, parm_file_info
          bcs   donecr
          jsr   xmess
          asc_hi ", "
          .byte $00
          ldy   fi_blks
          lda   fi_blks+1
          jsr   xprdec_2
          jsr   xmess
          asc_hi " of "
          .byte $00
          ldy   fi_atype
          lda   fi_atype+1
          jsr   xprdec_2
          jsr   xmess
          asc_hi " blocks used "
          .byte $00
donecr:   jsr   my_crout
done:     rts
novol:    jsr   xmess
          asc_hi "No volume found"
          .byte $00
          jmp   donecr
parm_on_line:
          .byte $02
          .byte $F0
          .addr onlinebuf+1     ; offset by 1 to convert to path
parm_file_info:
          .byte $0a
          .addr onlinebuf
fi_acc:   .byte $00
fi_type:  .byte $00
fi_atype: .word $0000
fi_stype: .byte $00
fi_blks:  .word $0000
fi_mdate: .word $0000
fi_mtime: .word $0000
fi_cdate: .word $0000
fi_ctime: .word $0000
.endproc

; check to see if current device is a disk II (assuming it is a slot-based dev)
; return zero and Z set if so, otherwise nonzero and Z clear
.proc     isdiskii
          ldy   #$ff
          lda   (slot),y
          beq   :+
          clc
          adc   #$01            ; A=$00 if 13-sector Disk II, shouldn't happen
:         rts
.endproc

; check to see if current device's driver is in a slot
; return $Cn if so, $00 if otherwise, and Z flag set if so
.proc     isslot
          lda   drvr+1
          and   #$f0
          cmp   #$c0
          bne   :+              ; no, see if slot has block device
          lda   drvr+1
          rts
:         lda   #$00
          sta   tptr
          lda   unit
          and   #$70
          lsr
          lsr
          lsr
          lsr
          ora   #$c0
          sta   tptr+1
          ldx   #$02
          ldy   #$05
:         lda   (tptr),y        ; get slot firmware byte
          cmp   sltbytes,x
          bne   :+
          dey
          dey
          dex
          bpl   :-
          lda   tptr+1          ; it *is* a block dev after all
          rts
:         lda   #$00            ; flag not slot device
          rts
sltbytes: .byte $20,$00,$03
.endproc

; check to see if current device is a mirrored device
; return carry set if it is, clear otherwise
; if carry set, A contains actual slot number of device
.proc     ismirr
          lda   slot+1          ; high byte of slot pointer
          clc                   ; anticipate no slot
          beq   :++             ; and bail if not
          lda   drvr+1          ; get driver address high byte
          and   #$0f            ; mask off slot number
          cmp   #$c0            ; is it in slot space?
          beq   :+              ; yes, keep going
          clc                   ; flag not mirror
          bcc   :++             ; and go forth
:         sta   tmp
          lda   unit            ; %DSSSXXXX unit number
          lsr                   ; %0DSSSXXX
          lsr                   ; %00DSSSXX
          lsr                   ; %000DSSSX
          lsr                   ; %0000DSSS
          and   #$07            ; %00000SSS forget drive
          cmp   slot+1          ; compare unit to slot
          clc                   ; anticipate not
          beq   :+              ; if equal, it isn't
          sec                   ; flag it
:         rts
.endproc

.proc     printat
          jsr   prafp
          jsr   xmess
          asc_hi "session "
          .byte $00
          ldy   #$00
          lda   (tptr),y
          tay
          lda   #$00
          jsr   xprdec_2
          jsr   xmess
          asc_hi ", ID "
          .byte $00
          ldy   #$1f
          lda   (tptr),y
          tax
          dey
          lda   (tptr),y
          tay
          txa
          jsr   xprdec_2
          jsr   my_crout
          jsr   tab5
          jsr   prafp
          ldy   #$01            ; slot+drive and user flag
          lda   (tptr),y
          ror                   ; bit 0 -> carry
          bcc   :+
          jsr   xmess
          .byte "user "
          .byte $00
:         jsr   prvol
          jsr   prcolon
          lda   tptr            ; get tptr to AFP volume name
          clc                   ; into AY
          adc   #$02
          tay
          lda   tptr+1
          adc   #$00
          jmp   xprint_path     ; print pascal str in AY and leave
.endproc

.proc     prafp
          jsr   xmess
          asc_hi "AFP "
          .byte $00
          rts
.endproc

.proc     prvol
          jsr   xmess
          asc_hi "volume "
          .byte $00
          rts
.endproc

.proc     prcolon
          jsr   xmess
          asc_hi ": "
          .byte $00
          rts
.endproc

; see if current device is appletalk device
; return carry clear if it's not, carry set if it is and tptr
; is now pointed at the session entry for the device
.proc     maybeat
          bit   atpres
          bmi   :+
notat:    clc
          rts
.pushcpu
.pc02                           ; AT requires one anyway
:         lda   nument
          beq   notat
          sta   tmp
loop:     lda   #<sessbuf
          sta   tptr
          lda   #>sessbuf
          sta   tptr+1
          ldy   #$01
          lda   (tptr),y
          and   #$f0
          cmp   unit
          beq   :+
          dec   tmp
          beq   notat
          lda   #32
          clc
          adc   tptr
          sta   tptr
          bcc   loop
          inc   tptr+1
          bra   loop
:         sec
          rts
.popcpu
.endproc


.proc     tab5
          jsr   xmess
          asc_hi "     "
          .byte $00
          rts
.endproc

.proc     my_crtab5
          jsr   my_crout
          jmp   tab5
.endproc

.proc     my_crout
          jsr   xcheck_wait
          jmp   crout
.endproc

.proc     atcheck
          php
          cli                   ; allow interrupts for AT calls
          ATcall inforeq
          ror                   ; move carry into high bit
          sta   atpres          ; and use as flag
          rol                   ; put carry back
          bcs   :+              ; and exit if none
          ATcall filsess        ; get sessions
          bcc   :+
          lda   #$00
          sta   nument          ; ensure nument is 0 if error
:         plp
          rts
; param list for GetInfo
inforeq:  .byte 0,2             ; sync GetInfo
          .word $0000           ; result code
          .dword $00000000      ; completion address
thisnet:  .word $0000           ; this network #
abridge:  .byte $00             ; local bridge
hwid:     .byte $00             ; hardware ID, IIgs only
romver:   .word $00             ; ROM version, IIgs only
nodenum:  .byte $00             ; node number
; param list for FIListSessions
; note format returned by FIListSessions
; is nument 32-byte entries in the format:
; $00 session reference number
; $01 slot/drive
; $02-$1D volume name
; $13-$1F volume ID
; the max number of these under P8 is 14, so the max data returned
; is 14*32 = 448 bytes'
filsess:  .byte 0,$2f           ; sync FIListSessions
          .word $0000           ; result code
buflen:   .word sessbufsz       ; buffer length
bufp:     .dword sessbuf        ; buffer pointer
nument:   .byte $00             ; entries returned
.endproc
nument    = atcheck::nument

          DX_end
