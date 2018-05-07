; %help
; fastchip -- Control A2Heaven FastChip //e
;
; THIS PROGRAM IS BETA, SOME THINGS MAY NOT WORK!
;
; options:
;   -l        List speeds and exit
;   -s <num>  Set speed, 0-40, see -l for list of speed values
;             Setting speed to 0 (off) causes a reboot.
;   -p <str>  Set slot speeds, slot numbers in str are set to fast, rest slow
;             To set all slow, use -p 0.  Will not set Disk IIs to fast.
;   -a <num>  Set audio (speaker) delay, 0-4 (Off/Fast/Normal/Music/Hifi)
;   -j <num>  Set joystick delay, 0-2 (Off/Short/Long)
;   -b <num>  Set backlight, 0-5 (Off/Fade/Speed/R/G/B)
;   -y        Answer reboot y/n questions with yes
;
; Invalid values are silently ignored.  Settings that will cause a reboot
; will be confirmed unless -y is given.
;
; No options: List current settings
; %hend

.p02
.include  "davex-mg.inc"

UNLOCKV   = $6a
LOCKV     = $a6
CFG_SPEED = $00
CFG_SLOTS = $01
CFG_SPKR  = $02
CFG_JSTCK = $03
CFG_RFENA = $04
CFG_RFSLT = $05
CFG_RWENA = $06
CFG_LIGHT = $07

fcbase    = $c06a
fcenable  = fcbase+1
fcslots   = fcbase+2
fcspeed   = fcbase+3
fcregnum  = fcbase+4
fcregval  = fcbase+5

showall   = xczpage             ; if nonzero, don't show all settings
myidx     = showall+1           ; index variable
mytemp    = myidx+1             ; temp for use in setting/showing/misc
mytemp1   = mytemp+2            ; temp for Disk II check
savex     = mytemp1+2           ; place to save x reg
savey     = savex+1             ; place to save y reg

          DX_start dx_mg_auto_origin ; load address
          DX_info $01,$12,dx_cc_iie_or_iigs,$00
          DX_ptab
          DX_parm 'l',t_nil     ; list speeds
          DX_parm 's',t_int1    ; speed
          DX_parm 'p',t_string  ; slot speeds
          DX_parm 'a',t_int1    ; speaker delay
          DX_parm 'j',t_int1    ; joystick delay
          DX_parm 'b',t_int1    ; backlight
          DX_parm 'y',t_nil     ; force yes
          DX_end_ptab
          DX_desc "Control FastChip //e."
          DX_main
          ; let user list speeds even if not on a supported machine
          lda   #'l'|$80        ; list speed?
          jsr   xgetparm_ch
          bcs   :+
          jmp   listspeeds
          ; davex has already identified the machine as a //e or IIgs
:         sec
          jsr   checkmach       ; see if IIgs
          bcs   :+              ; not a IIgs
badiie:   lda   #$01
          jsr   xredirect
          jsr   xmess
          asc_hi "This program requires an Apple //e"
          .byte $8d,$00
exiterr:  lda   #$ff
          jsr   xredirect
          jmp   xerr
:         lda   $fbdd           ; //e Card ID byte
          cmp   #$02
          beq   badiie          ; not gonna work
; meat and potatoes now follow
          jsr   fcdetect
          bcc   doit
          lda   #$01
          jsr   xredirect
          jsr   xmess
          asc_hi "No FastChip detected!"
          .byte $8d,$00
          jmp   exiterr         ; comment out for basic emulator testing
doit:     lda   #$00
          sta   showall
          jsr   try_all
          lda   showall
          bne   :+
          jsr   show_all
:         rts

; Try all config setting options
.proc     try_all
          php
          sei
          jsr   fcunlock
          sta   fcenable
          jsr   try_speed
          jsr   try_slots
          jsr   try_speaker
          jsr   try_joystick
          jsr   try_backlight
          jsr   fclock
          plp
          rts
.endproc

.proc     try_speed
          lda   #'s'|$80
          jsr   xgetparm_ch
          bcs   done
          cpy   #0
          beq   disable
          cpy   #41             ; too big?
          bcs   done
doset:    lda   #CFG_SPEED
          sta   fcregnum        ; config reg
          sty   fcregval        ; value
noset:    jsr   show_speed
          inc   showall
done:     rts
disable:  lda   #'y'|$80
          jsr   xgetparm_ch
          bcc   :+
          jsr   ask_reboot
          beq   noset
:         ldy   #0
          beq   doset
.endproc

.proc     try_slots
          lda   #'p'|$80
          jsr   xgetparm_ch
          bcc   :+
          rts
:         sta   mytemp+1        ; save string pointer
          sty   mytemp
          ldy   #$00
          sty   myidx           ; we'll keep calculated value here
          lda   (mytemp),y      ; length of string
          beq   done            ; empty
          tay                   ; length into y
lp:       lda   (mytemp),y      ; get char
          and   #$7f
          cmp   #'1'
          bcc   next            
          cmp   #'7'+1
          bcs   next
          and   #$0f            ; get the bits containing slot #
          jsr   is_disk_ii      ; check if Disk II
          bcc   :+              ; nope, go ahead
          sty   savey           ; otherwise, warn user
          jsr   xmess
          asc_hi "Not setting fast Disk II for slot "
          .byte $00
          ldy   savey
          lda   (mytemp),y      ; get slot number back
          ora   #$80            ; make printable
          jsr   cout            ; print it
          lda   #$8d            ; and cr
          jsr   cout
          jmp   next            ; go to next number
:         tax
          lda   #$01
:         asl                   ; shift left for the slot
          dex
          bne   :-
          ora   myidx           ; combine with existing
          sta   myidx           ; and save
next:     dey
          bne   lp
          ldy   myidx           ; get computed value
          lda   #CFG_SPEED
          sta   fcregnum        ; config reg
          sty   fcregval        ; value
          ;lda   #$00            ; DEBUG
          ;jsr   xprdec_2        ; DEBUG
          jsr   show_slots
          inc   showall
done:     rts
.endproc

.proc     try_speaker
          lda   #'a'|$80
          jsr   xgetparm_ch
          bcs   done
          cpy   #05             ; too big?
          bcs   done
          lda   #CFG_SPKR
          sta   fcregnum        ; config reg
          sty   fcregval        ; value
          jsr   show_speaker
          inc   showall
done:     rts
.endproc

.proc     try_joystick
          lda   #'j'|$80
          jsr   xgetparm_ch
          bcs   done
          cpy   #03            ; too big?
          bcs   done
          lda   #CFG_JSTCK
          sta   fcregnum        ; config reg
          sty   fcregval        ; value
          jsr   show_joystick
          inc   showall
done:     rts
.endproc

.proc     try_backlight
          lda   #'b'|$80
          jsr   xgetparm_ch
          bcs   done
          cpy   #06            ; too big?
          bcs   done
          lda   #CFG_LIGHT
          sta   fcregnum        ; config reg
          sty   fcregval        ; value
          jsr   show_backlight
          inc   showall
done:     rts
.endproc

; Show all current settings
.proc     show_all
          jsr xmess
          asc_hi "FastChip //e"
          .byte $8d
          asc_hi "------------"
          .byte $8d,$00
          php
          sei
          jsr   fcunlock
          sta   fcenable
          jsr   show_speed
          jsr   show_slots
          jsr   show_speaker
          jsr   show_joystick
          jsr   show_backlight
          jsr   show_slinky
          jsr   show_aux
          jsr   fclock
          plp
          rts
.endproc

.proc     show_speed
          jsr   xmess
          asc_hi "Speed: "
          .byte $00
          lda   #CFG_SPEED
          sta   fcregnum        ; config reg
          lda   fcregval        ; get speed
          pha                   ; save it
          tay                   ; and put in y for printing
          lda   #$00            ; high byte for printing
          jsr   xprdec_2
          jsr   xmess
          asc_hi " = "
          .byte $00
          pla                   ; get speed back
          jsr   print_speed     ; print it
          bcs   :+              ; skip if non-number printed by print_speed
          jsr   xmess
          asc_hi " MHz"
          .byte $00
:         lda   #$8d
          jsr   cout
          rts
.endproc

.proc     show_slots
          jsr   xmess
          asc_hi "Slots: "
          .byte $00
          lda   #CFG_SLOTS
          sta   fcregnum
          lda   fcregval
          lsr                   ; slot zero bit unused
          sta   temp
          lda   #'1'|$80
          sta   myidx
lp:       jsr   cout            ; myidx reloaded at end of loop
          lsr   temp            ; get slot bit into carry
          bcc   :+              ; fast are marked with 1 bits
          lda   #'+'|$80        ; indicate fast
          bne   :++             ; always
:         lda   #' '|$80        ; indicate slow
:         jsr   cout            ; print fast/slow
          lda   #' '|$80        ; now get a space for separation
          jsr   cout            ; print it
          inc   myidx           ; next slot
          lda   myidx           ; get it
          cmp   #'8'|$80        ; done with slots?
          bcc   lp              ; nope, print this one
          jsr   xmess
          asc_hi "(+ = fast)"
          .byte $8d,$00
          rts
.endproc

.proc     show_speaker
          jsr   xmess
          asc_hi "Speaker: "
          .byte $00
          lda   #CFG_SPKR
          sta   fcregnum
          ldx   fcregval
          bne   :+
          jsr   pr_off
          lda   #$8d
          jsr   cout
          rts
:         dex
          bne   :+
          jsr   xmess
          asc_hi "Fast"
          .byte $8d,$00
          rts
:         dex
          bne   :+
          jsr   xmess
          asc_hi "Normal"
          .byte $8d,$00
          rts
:         dex
          bne   :+
          jsr   xmess
          asc_hi "Music"
          .byte $8d,$00
          rts
:         dex
          bne   :+
          jsr   xmess
          asc_hi "HiFi"
          .byte $8d,$00
          rts
:         jsr   pr_err
          lda   #$8d
          jsr   cout
          rts
.endproc

.proc     show_joystick
          jsr   xmess
          asc_hi "Joystick: "
          .byte $00
          lda   #CFG_JSTCK
          sta   fcregnum
          ldx   fcregval
          bne   :+
          jsr   pr_off
          lda   #$8d
          jsr   cout
          rts
:         dex
          bne   :+
          jsr   xmess
          asc_hi "Short"
          .byte $8d,$00
          rts
:         dex
          bne   :+
          jsr   xmess
          asc_hi "Long"
          .byte $8d,$00
          rts
:         jsr   pr_err
          lda   #$8d
          jsr   cout
          rts
.endproc

.proc     show_backlight
          jsr   xmess
          asc_hi "Backlight: "
          .byte $00
          lda   #CFG_LIGHT
          sta   fcregnum
          ldx   fcregval
          bne   :+
          jsr   pr_off
          lda   #$8d
          jsr   cout
          rts
:         dex
          bne   :+
          jsr   xmess
          asc_hi "Fade"
          .byte $8d,$00
          rts
:         dex
          bne   :+
          jsr   xmess
          asc_hi "Speed"
          .byte $8d,$00
          rts
:         dex
          bne   :+
          jsr   xmess
          asc_hi "Red"
          .byte $8d,$00
          rts
:         dex
          bne   :+
          jsr   xmess
          asc_hi "Green"
          .byte $8d,$00
          rts
:         dex
          bne   :+
          jsr   xmess
          asc_hi "Blue"
          .byte $8d,$00
          rts
:         jsr   pr_err
          lda   #$8d
          jsr   cout
          rts
.endproc

.proc     show_slinky
          jsr   xmess
          asc_hi "Slinky: "
          .byte $00
          lda   #CFG_RFENA
          sta   fcregnum
          lda   fcregval
          jsr   pr_onoff
          jsr   xmess
          asc_hi ", slot "
          .byte $00
          lda   #CFG_RFSLT
          sta   fcregnum
          ldy   fcregval
          lda   #$00
          jsr   xprdec_2
          lda   #$8d
          jsr   cout
          rts
.endproc

.proc     show_aux
          jsr   xmess
          asc_hi "Aux RAM: "
          .byte $00
          lda   #CFG_RWENA
          sta   fcregnum
          lda   fcregval
          jsr   pr_onoff
          lda   #$8d
          jsr   cout
          rts
.endproc

; detect fastchip //e.  If present, return carry clear
; otherwise return carry set.
.proc     fcdetect
          php
          sei
          jsr   fcunlock        ; unlock
          sta   fcenable        ; enable FC registers
          lda   fcenable        ; now see if it's there
          bpl   notfound        ; bit 7 set if present and enabled
          ldy   fcspeed         ; speed reg
          iny                   ; increment
          tya                   ; put in accum
          dey                   ; fix value back for later restore
          and   #$1f            ; at this point value is not what we read out
          sta   fcspeed         ; set speed reg
          eor   fcspeed         ; a should be 0 now
          sty   fcspeed         ; restore value
          bne   notfound        ; or not found
          sta   fcregnum        ; config register select, 0 = speed
          cpy   fcregval        ; config register data, saved speed
          bne   notfound        ; no match, no fastchip
          ; at this point we have a high degree of confidence that FC is there
          jsr   fclock          ; lock it back up
          plp                   ; restore IRQs
          clc                   ; indicate found
          rts
notfound: plp                   ; restore IRQs
          sec                   ; indicate not found
          rts
.endproc

.proc     fcunlock
          lda   #UNLOCKV
          sta   fcbase
          sta   fcbase
          sta   fcbase
          sta   fcbase
          rts
.endproc

.proc     fclock
          lda   #LOCKV
          sta   fcbase
          rts
.endproc

.proc     listspeeds
          lda   #$00
          sta   num+2           ; clear upper bytes of num
          sta   num+1
          lda   #41
          sta   myidx
spdlp:    dec   myidx           ; next index
          bmi   done
          lda   myidx           ; get it into accumulator
          sta   num             ; prepare to print
          pha                   ; save it
          and   #%11            ; mask off low bits
          php                   ; save Z flag
          tax
          lda   tabstops,x      ; get tab stop for table
          sta   ch              ; set cursor pos
          plp
          bne   :+              ; if 0, no cr
          lda   #$8d
          jsr   cout
:         ldy   #$01            ; field width-1
          jsr   xprdec_pady
          lda   #'='|$80
          jsr   cout
          pla                   ; get speed back
          jsr   print_speed     ; and print it
          lda   #' '|$80        ; in case output doesn't respect ch
          jsr   cout
          jmp   spdlp
done:     jsr   xmess
          .byte $8d,$00
          rts
tabstops: .byte 0,30,20,10
.endproc

; print fastchip speed in MHz from value in A
; returns carry clear if a number was printed
; carry set otherwise
.proc     print_speed
          cmp   #41
          bcs   invalid
          ora   #$00
          beq   off
          asl
          tax
          ldy   spdtab,x        ; MHz
          lda   spdtab+1,x      ; decimal
          pha                   ; save it
          lda   #$00
          jsr   xprdec_2        ; print MHz in AY
          lda   #'.'|$80
          jsr   cout
          pla                   ; get decimal back
          tay
          lda   #$00
          jsr   xprdec_2        ; print decimal in AY
          clc
          rts
invalid:  jsr   pr_err
          sec
          rts
off:      jsr   pr_off
          sec
          rts
          ; speed table... each pair is MHZ.decimal
spdtab:   .byte 0,0,0,2,0,3
          .byte 0,4,0,5,0,6
          .byte 0,7,0,8,0,9
          .byte 1,0,1,1,1,2
          .byte 1,3,1,4,1,5
          .byte 1,6,1,7,1,8
          .byte 1,9,2,0,2,1
          .byte 2,2,2,3,2,5
          .byte 2,6,2,7,2,9
          .byte 3,1,3,3,3,5
          .byte 3,8,4,1,4,55
          .byte 5,0,5,5,6,2
          .byte 7,1,8,3,10,0
          .byte 12,5,16,6
.endproc

.proc     ask_reboot
          lda   #$01
          jsr   xredirect
          jsr   xmess
          asc_hi "Apply setting and reboot"
          .byte $00
          lda   #'n'|$80
          jsr   xyesno2
          php
          lda   #$ff
          jsr   xredirect
          plp                   ; restore z flag
          rts
.endproc

.proc     pr_err
          jsr xmess
          asc_hi "Err!"
          .byte $00
          rts
.endproc

.proc     pr_onoff
          ora   #$00
          beq   pr_off
          cmp   #$01
          bne   pr_err
          jsr   xmess
          asc_hi "On"
          .byte $00
          rts
.endproc

.proc     pr_off
          jsr   xmess
          asc_hi "Off"
          .byte $00
          rts
.endproc

; check slot referenced by A, set carry if disk II, clear carry if not
; saves all regs
.proc     is_disk_ii
          pha
          stx   savex
          sty   savey
          ora   #$c0
          sta   mytemp1+1
          lda   #$00
          sta   mytemp1
          ldx   #$03
:         ldy   idloc,x
          lda   (mytemp1),y
          cmp   idval,x
          bne   :+
          dex
          bpl   :-
          sec
          bcs   getxy
:         clc
getxy:    ldx   savex
          ldy   savey
          pla
          rts
idloc:    .byte $01,$03,$05,$ff
idval:    .byte $20,$00,$03,$00
.endproc
          DX_end
