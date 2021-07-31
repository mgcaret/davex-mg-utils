; %help
; idemu -- Identify emulator.
;
; syntax:   idemu
;
; Uses official ways of detecting emulators if possible, otherwise uses the
; various idiosyncrasies that many emulators have regarding the ROM or I/O
; pages. 
; %hend

.p02
.include  "davex-mg.inc"

temp0     = xczpage

          DX_start dx_mg_auto_origin ; load address
          DX_info $02,$12,dx_cc_any,$00
          DX_ptab
          DX_end_ptab
          DX_desc "Identify emulator."
          DX_main
          jsr   idemu
          stx   temp0
          jsr   xprint_path
          lda   temp0
          beq   :+
          jsr   xmess
          asc_hi ", version "
          .byte $00
          lda   temp0
          jsr   xprint_ver
:         jsr   xmess
          .byte $8d,$00
          rts

; identify emulator
; return cs if no emulator found
; return cc if one is found
; ay = name pointer (even for none), x = version (if nonzero)
.proc     idemu
          lda   #$00
          sta   emuver
          sec
          jsr   $fe1f
          bcc   :+              ; skip to checking emubute if it's a IIgs
          ; okay, let's check for some obvious things first
          lda   $fbb3
          cmp   #$06            ; Apple IIe or better
          bne   :+
          lda   $fbdd
          cmp   #$02            ; LC PDS card or "IIe" for classic MacOS
          bne   :+
          lda   $fbde
          cmp   #$40            ; IIe for classic MacOS
          bne   iiecard         ; If not, it's Apple IIe Card for Mac.
          ldy   #<emIIe
          lda   #>emIIe
          bne   foundemu1
iiecard:  lda   $fbbe
          sta   emuver
          ldy   #<emPDS
          lda   #>emPDS
          bne   foundemu1
; Identify by emubyte value if possible
:         jsr   getemub
          bne   noemu           ; floating bus, not emu or a really good one
          cmp   #$00            ; yes this is needed
          bne   ckcatak         ; if not zero, go check for Catakig
          sta   emuver          ; zero out the version
          sec
          jsr   $fe1f           ; is GS?
          bcs   :+              ; nope, skip forward
          ldy   #<emGus         ; assume Gus if GS and ID 0
          lda   #>emGus
          bne   foundemu1
          ; check for Apple IIjs or jse
          ; which always have zeros from $c001-$c00f
:         lda   #$00
          ldx   #$0f
jslp:     lda   $c000,x
          bne   :+              ; find nonzero, move on
          dex
          bne   jslp
          stx   emuver          ; zero emuver
          ldy   #<emJS
          lda   #>emJS
          bne   foundemu1
:         ldy   #<emVII         ; Not one of those, just assume Virtual ][
          lda   #>emVII
          bne   foundemu1          
ckcatak:  cmp   #$ad            ; Catakig always has this in $c04f
          bne   :+
          lda   #$00
          sta   emuver          ; catakig doesn't have a version byte
          ldy   #<emCatak
          lda   #>emCatak
foundemu1:
          jmp   foundemu
:         cmp   #$fe            ; Bernie
          bne   :+
          ldy   #<emBernie
          lda   #>emBernie
          bne   foundemu
:         cmp   #$16            ; Sweet 16
          bne   :+
          ldy   #<emS16
          lda   #>emS16
          bne   foundemu
:         cmp   #$47            ; GSport
          bne   :+
          ldy   #<emGSport
          lda   #>emGSport
          bne   foundemu
:         cmp   #$4b            ; KEGS
          bne   :+
          ldy   #<emKEGS
          lda   #>emKEGS
          bne   foundemu
:         cmp   #$ab            ; AppleBlossom
          bne   :+
          ldy   #<emAB
          lda   #>emAB
          bne   foundemu
:         ; well, ID byte didn't match a known emu
          ; but we don't have floating bus at $c04f
          ; so it must be an emulator
unkemu:   lda   emubyte
          jsr   setemub
          ldy   #<emUnk
          lda   #>emUnk
foundemu: ldx   emuver
          clc
          rts
noemu:    ldy   #<emNone
          lda   #>emNone
          ldx   #$00
          sec
          rts

; get emubyte and version if possible
; returns with z clear if floating bus found instead
; if z set, a has emubute and y has emuver
; which are also saved in memory
getemub:  sta   $c04f           ; de-facto standard among IIgs emulators
          lda   $c04f           ; program ID
          ldy   $c04f           ; version
          sta   emubyte
          sty   emuver
          ldx   #$ff            ; now check ID again 255 times
:         sta   $c04f
          lda   $c04f
          cmp   emubyte         ; floating bus gonna float
          bne   :+              ; and if it does, we gotta nope
          dex
          bne   :-
:         rts
; set emubyte in unknown entry, enter with a = emubyte
setemub:  sed                   ; decimal mode trick for hex->ascii
          tax                   ; save it
          and   #$0f            ; low nibble
          cmp   #$0a
          adc   #$30
          ora   #$80            ; to Apple II
          sta   unkemub+1
          txa
          lsr                   ; high nibble
          lsr
          lsr
          lsr
          cmp   #$0a
          adc   #$30
          ora   #$80
          sta   unkemub
          cld         
          rts
; ***
emubyte:  .byte $00
emuver:   .byte $00
; ***
emNone:   pstr_hi "None identified"
emUnk:    pstr_hi "Unknown, emubyte = $XX"
unkemub = *-2
emPDS:    pstr_hi "Apple IIe Card"
emVII:    pstr_hi "Virtual ][ (probably)"
emIIe:    pstr_hi "IIe"
emGus:    pstr_hi "Gus (probably)"
emCatak:  pstr_hi "Catakig"
emBernie: pstr_hi "Bernie ][ the Rescue"
emS16:    pstr_hi "Sweet 16"
emGSport: pstr_hi "GSport"
emKEGS:   pstr_hi "KEGS"
emAB:     pstr_hi "AppleBlossom"
emJS:     pstr_hi "Apple IIjs or jse"
.endproc          
          
          DX_end