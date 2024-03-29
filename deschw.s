;*********************************************
;*********************************************
;
; External command for Davex
;
; deschw -- describe hardware
;
; Options:
;   -t   system type
;   -c   cards
;   -s   SmartPort
;
; 15-Oct-89 DAL ==> v1.2
;   Changed "//" to "II"
;   Added IIc+ identification
;     & fixed mismatched parens
;   Added IIe debugger ROM id
;
;*********************************************
;
; Converted to MPW IIgs 20-Sep-92 DAL
;
;*********************************************
; 27-Jul-17 MAG ==> v1.3
;   Added Apple IIe Card
;     for Mac LC identification
; 28-Jul-17 MAG ==> v1.3 (still)
;   Added CPU identification (6502, 65C02, 658xx)
;   Added emulator identification
;   Reduced repetition, reworded, saved a few bytes
;   Fix bug where garbage is printed if smartport
;     device name is 0 bytes long
;   Fix AppleTalk identification & enhanced
;   Fix case where an emulator doesn't execute
;     missing SMB0 as two-byte opcode
; 05-Aug-17 MAG ==> v1.3 (still)
;   Work around lazy IIe LC PDS Card SmartPort.
; 07-Aug-17 MAG ==> v1.3 (still)
;   Show Smartport ID type info (Extended/SCSI/RAM card)
;   Optional code to show SmartPort vendor and version (not working?) SmartPort TN #2
;   Support additional devices defined in SmartPort TN #4
;   Support subtype flags as described in SmartPort TN #7
; 14-Aug-17 MAG ==> v1.3 (still)
;   Reduce emulator detection to actual emulator for those supporting
;   EMUBYTE, or "probable" if no floating bus detected.  If you want more
;   specific identification, see idemu program.
; 10-Feb-19 MAG => v1.4
;   Detect VidHD

;.segment	"CODE_9000"

	.include "Common/2/Globals2.asm"
	.include "Common/2/Apple.Globals2.asm"
	.include "Common/2/Mli.Globals2.asm"
	.include "Common/Macros.asm"

OrgAdr	= $9000	;change as necessary (end below $B000)
.org	OrgAdr	; Makes the listing more readable, though it doesn't really org the code - the linker does that.

b_phy	= $5a
b_xce	= $fb
b_rep	= $c2
b_ldx	= $a2

;*********************************************
;
; SmartPort constants
;
sptSTATUS	= 0
sptREADBLK	= 1
sptWRITEBLK	= 2
sptFORMAT	= 3
sptCONTROL	= 4
sptINIT	= 5
sptOPEN	= 6
sptCLOSE	= 7
sptREAD	= 8
sptWRITE	= 9
;
;*********************************************
MyVersion	= $14
MinVersion	= $11
;*********************************************
	rts
	.byte $ee,$ee
	.byte MyVersion,MinVersion
	.byte %00000000	;hardware req
	.addr descr
	.addr OrgAdr
	.addr start
	.byte 0,0,0,0
; parameters here
	.byte $80+'t',t_nil
	.byte $80+'c',t_nil
	.byte $80+'s',t_nil
	.byte 0,0
descr:
	pstr "describe system hardware"

;*********************************************
; dum xczpage ;32 locations
slot	= xczpage	;ds.b 1
rom	= slot+1	;ds.b 2
Unit	= rom+2	;ds.b 1
scratch	= Unit+1	;ds.b 1
totalmem	= scratch+1	;ds.b 4
emubyte = totalmem+4
emuver  = emubyte+1
scratch2 = emuver+1
checkemu = scratch2+1 ; zero = need to check for emulator
iie_flag = checkemu+1 ; zero = running on Apple IIe
; dend
;
start:
  lda #$00
  sta checkemu
  sta iie_flag
	jsr xgetnump
	beq do_all
	lda #'t'+$80	;system Type
	jsr xgetparm_ch
	bcs no_type
	jsr my_crout
	jsr systemtype
no_type:	lda #'c'+$80	;Cards
	jsr xgetparm_ch
	bcs no_cards
	jsr my_crout
	jsr scanslots
no_cards:	lda #'s'+$80
	jsr xgetparm_ch
	bcs no_sp
	jsr my_crout
	jsr DescribeSP
no_sp:	rts
;
do_all:
	jsr systemtype
	jsr processor
	jsr emulation
	jsr my_crout
	jsr scanslots
	jsr my_crout
	jsr DescribeSP
	jsr my_crout
	rts
;
systemtype:
	jsr xmess
	asc "System:  "
	.byte 0
	sec
	jsr $fe1f
	bcc st_gs
	jmp IdentNotGS
;
st_gs:	jsr xmess


	asc "Apple IIgs, ROM version $"

	.byte 0
	clc
	.byte b_xce,b_rep,$30
	jsr $fe1f
	.byte b_phy
	sec
	.byte b_xce
	pla
	jsr $fdda
	jsr xmess


	asc "  (Machine ID=$"

	.byte 0
	pla
	jsr $fdda
	jsr xmess


	asc ")"

	.byte 0
	jsr my_crout
	clc
	.byte b_xce
	.byte b_rep,$30
	pha
	pha
	.byte b_ldx
	.addr $1d02	;TotalMem
	.byte $22,0,0,$E1
	pla
	sta totalmem
	pla
	sta totalmem+2
	sec
	.byte b_xce
	jsr xmess


	asc "Total RAM = "

	.byte 0
	ldx #10
div1K:	lsr totalmem+3
	ror totalmem+2
	ror totalmem+1
	ror totalmem
	dex
	bne div1K
	lda totalmem+1
	ldy totalmem
	jsr xprdec_2
	jsr xmess
	asc "K"
	.byte cr,0
	rts
;
IdentNotGS:
	lda $fbb3
	cmp #$ea
	bne not_2p3
	lda $fb1e
	cmp #$ad
	beq TwoPlus
	cmp #$8a
	beq Three
unknown:	jsr xmess
	asc "???"
	.byte 0
	rts
TwoPlus:	jsr xmess
	asc "Apple ][+"
	.byte 0
	rts
Three:	jsr xmess
	asc "Apple /// (in emulation mode)"
	.byte 0
	rts
;
not_2p3:
	lda $fbb3
	cmp #$06
	bne unknown
	lda $fbc0
	cmp #$ea
	bne Not2e1
	jsr apple2e
	jsr xmess


	asc "(unenhanced)"

	.byte 0
	rts
Not2e1:	cmp #$e0
	bne not2e2
	lda $fbdd
	cmp #$02        ; IIe for Power Mac also has this
	bne NotLC
	lda $fbde
	cmp #$01        ; IIe for Power Mac does *not* have this
	bne NotLC
	jsr apple2e
	jsr xmess


	asc "(LC PDS Card, version $"

	.byte 0
  lda $fbbe	; version # of LC PDS Card
  jsr $fdda
	jsr xmess
	asc ")"
	.byte $00
	dec checkemu
	rts

NotLC:
  jsr apple2e
	jsr xmess


	asc "(enhanced)"

	.byte 0
	rts
not2e2:
	cmp #$e1
	bne notDbgr
	jsr apple2e
	jsr xmess


	asc "(special ROMs)"

	.byte 0
	rts
notDbgr:
	cmp #$00
	bne unkn0
; it's a IIc
	lda $fbbf
	cmp #5
	beq IIcPlus
	jsr apple2c
	jsr xmess


	asc ", version "

	.byte 0
	lda $fbbf
	cmp #$ff
	beq TwoC1
	cmp #$00
	beq TwoC2
	cmp #$03
	beq TwoC3
	cmp #$04
	beq TwoC4
unkn0:	jmp unknown
IIcPlus:
  jsr apple2c
  jsr xmess


	asc " Plus"

	.byte 0
	rts
TwoC1:	jsr xmess


	asc "1 (ROM 255)"

	.byte 0
	rts
TwoC2:	jsr xmess


	asc "2: 3.5"

	.byte $a2


	asc " disk (ROM 0)"

	.byte 0
	rts
TwoC3:	jsr xmess


	asc "3: "

	.byte 0
memexp:
  jsr xmess
  asc "Mem. Expandable"
  .byte $00
	rts
TwoC4:	jsr xmess


	asc "4: Revised "

	.byte 0
	.pc02
	bra memexp
	.p02
apple2e:
	jsr xmess
	asc "Apple IIe "
	.byte $00
	rts
apple2c:
  jsr xmess
	asc "Apple IIc"
	.byte $00
	rts
;****************************************************
processor:
  jsr xmess
  .byte $8d
  asc "CPU: "
  .byte $00
  sed
  lda #$99
  clc
  adc #$01
  cld
  bmi p6502
  ; 65C02 or 8xx
  ldx #$00            ; see below
  clc
  .p816
  sep #%00000001      ; set carry, should be 2-byte NOP on the 'C02
  inx                 ; except when it isn't (Apple//jse)
                      ; so detect this with this inx that
                      ; gets skipped if sep #1 == nop ora zp,x
  stx scratch2
  .p02
  bcs p658xx
  ; we have a 65C02
  jsr xmess
  asc "65C02"
  .byte $00
  lda scratch2        ; is zero if broken
  beq brokec02
  .pc02
  stz scratch2
  smb0 scratch2       ; ora [d] on 8xx
  .p02
  lda scratch2
  bne procdone
  jsr xmess
  asc " (no x7/xF opcodes)"
  .byte $00
procdone:
  rts
p6502:
  jsr xmess
  asc "6502"
  .byte $00
  rts
p658xx:
  jsr xmess
  asc "658xx"
  .byte $00
  rts
brokec02:
  jsr xmess
  asc " (broken)"
  .byte $00
  rts
;****************************************************
emulation:
  bit checkemu        ; skip if we positively identified hardware (LC PDS card)
  bmi noemu1
  sta $c04f           ; get emubyte
  lda $c04f           ; Emulator ID
  ldy $c04f           ; The GS emulators (except Gus) all do version # here
  sta emubyte
  sty emuver
  ldx #$ff            ; now test 255 more times
: sta $c04f
  lda $c04f
  cmp emubyte         ; floating bus gonna float
  bne noemu1          ; not emulator if it changes
  dex
  beq econt
  bne :-
noemu1:
  rts
econt:
  cmp #$a0            ; a space... if we somehow got 255 of them
  beq noemu1          ; just assume we were lucky
  jsr xmess
  .byte $8d
  asc "Emulator: "
  .byte $00
  lda emubyte
  cmp #$fe
  bne notbernie
  jsr xmess
  asc "Bernie ][ the Rescue"
  .byte $00
  .p816
  jmp emuversion      ; out of range for bra
  .p02
notbernie:
  cmp #$16
  bne notsweet16
  jsr xmess
  asc "Sweet 16"
  .byte $00
  .p816
  bra emuversion
  .p02
notsweet16:
  cmp #$47
  bne notgsport
  jsr xmess
  asc "GSport or derivative"
  .byte $00
  .p816
  bra emuversion
  .p02
notgsport:
  cmp #$4b
  bne notkegs
  jsr xmess
  asc "KEGS or derivative"
  .byte $00
  .p816
  bra emuversion
  .p02
notkegs:
  cmp #$ab
  bne unkemu
  jsr xmess
  asc "Appleblossom"
  .byte $00
emuversion:
  jsr xmess
  asc ", version = $"
  .byte $00
  lda emuver
  jsr $fdda
noemu:
  rts
unkemu:
  jsr xmess
  asc "Probable ["
  .byte $00
  lda emubyte
  jsr $fdda
  lda emuver
  jsr $fdda
  jsr xmess
  asc "]"
  .byte $00
  rts
;****************************************************
scanslots:
	lda #1
	sta slot
ss1:	jsr scan1
	inc slot
	lda slot
	cmp #8
	bcc ss1

	rts
;
scan1:	jsr xmess


	asc "Slot "

	.byte 0
	lda slot
	ora #'0'+$80
	jsr cout
	jsr xmess


	asc ": "

	.byte 0
	lda slot
	ora #$c0
	sta rom+1
	lda #0
	sta rom
	jsr PrSlotDesc
	jsr my_crout
	jsr MaybeATLK
not_atlk:
	rts
;
; MaybeATLK - if ATLK found, return carry clear
; otherwise return carry set.
ATLKsig:	asc "ATLK"
;	.byte 0 unneeded
MaybeATLK:
	ldy #$F9	;check $CnF9
at_chk:	lda (rom),y
	cmp ATLKsig-$F9,y
	sec ; anticipate failure
	bne not_atlk
	iny
	cpy #$FD
	bcc at_chk
	jsr xmess


	asc "        AppleTalk "
	
	.byte $00
	ldy #$fd
	lda (rom),y
	bne notatgs
	jsr xmess
	asc "(IIgs)"
	.byte $00
	jmp :+
notatgs:
  cmp #$01
  bne notatws
  jsr xmess
	asc "Workstation"
	.byte $00
	jmp atcard
notatws:
  cmp #$02
  bne notatsrv
  jsr xmess
	asc "Server"
	.byte $00
atcard:
  jsr xmess
	asc " Card"
	.byte $00
	jmp :+
notatsrv:
  pha
  jsr xmess
	asc " (ID $"
	.byte $00
	pla
	jsr $fdda
	jsr xmess
	asc ")"
	.byte $00
  ; fall through
:	jsr xmess
	
	asc "; version="

	.byte 0
	ldy #$fe
	lda (rom),y
	pha
	lsr a
	lsr a
	lsr a
	lsr a
	jsr prnib
	lda #'.'+$80
	jsr cout
	pla
	jsr prnib
	ldy #$ff
	lda (rom),y
	jsr prbyte
	jsr my_crout
	clc
; not_atlk ; moved up
  rts
;
prnib:	and #$0F
	cmp #$0A
	bcc prn_dig
	adc #6
prn_dig:	adc #$B0
	jmp cout
;
SlotEmpty:	jsr xmess


	asc "empty"

	.byte 0
	rts
;
notPasc0:	jmp notPasc
PrSlotDesc:
	ldy slot
	lda bitpos,y
	and sltbyt
	beq SlotEmpty
	ldy #7
	lda (rom),y
	cmp #$18
	bne notPasc0
	ldy #$b
	lda (rom),y
	cmp #$1
	bne notPasc0
	ldy #5
	lda (rom),y
	cmp #$38
	beq :+
	cmp #$2C
	bne notPasc0
	; ViDHD
	jsr xmess
	asc "VidHD"
	.byte $00
	rts
: ldy #$0c
	lda (rom),y
	pha
	jsr xmess


	asc "Pascal ID = $"

	.byte 0
	pla
	pha
	jsr prbyte
	jsr xmess


	asc ": "

	.byte 0
	pla
	lsr a
	lsr a
	lsr a
	lsr a
	asl a
	tax
	lda PascTbl+1,x
	pha
	lda PascTbl,x
	pha
	rts
PascTbl:
	.addr ps0-1,ps1-1,ps2-1,ps3-1,ps4-1,ps5-1,ps6-1,ps7-1
	.addr ps8-1,ps9-1,ps10-1,ps0-1,ps0-1,ps0-1,ps0-1,ps0-1
;
ps0:	jsr xmess


	asc "???"

	.byte 0
	rts
ps1:	jsr xmess


	asc "printer"

	.byte 0
	rts
ps2:	jsr xmess


	asc "joystick/mouse"

	.byte 0
	rts
ps3:	jsr xmess


	asc "serial or parallel card"

	.byte 0
	rts
ps4:	jsr xmess
	asc "modem"
	.byte 0
	rts
ps5:	jsr xmess
	asc "sound/speech device"
	.byte 0
	rts
ps6:	jsr xmess
	asc "clock"
	.byte 0
	rts
ps7:	jsr xmess
	asc "disk/storage device"
	.byte 0
	rts
ps8:	jsr xmess
	asc "80-column card"
	.byte 0
	rts
ps9:	jsr xmess
	asc "network/bus interface"
	.byte 0
	rts
ps10:	jsr xmess
	asc "other"
	.byte 0
	rts
;
notPasc:
	jsr chk_clock
	bcs chk_sp
	jsr xmess
	asc "ThunderClock/compatible"
	.byte 0
	rts
chk_sp:	jsr chk_smport
	bcs chkb
	ldy #$fb ; smartport ID byte
	lda (rom),y
	pha
	and #%10000000
	beq :+
	jsr xmess
	asc "Extended "
	.byte 0
: pla
  pha
  and #%00000010
  beq :+
  jsr xmess
	asc "SCSI "
	.byte 0
: pla
  pha
  and #%00000001
  beq :+
  jsr xmess
	asc "RAM Card "
	.byte 0	
:	pla ; clean stack
  jsr xmess
	asc "SmartPort"
	.byte 0
	rts
chkb:	jsr chk_blkdev
	bcs notblk
; Is it a Disk II?
	ldy #$ff
	lda (rom),y
	bne notDiskII
	jsr xmess
	asc "5.25"
	.byte $A2
	asc " disk drive"
	.byte 0
	rts
notDiskII:
	jsr xmess
	asc "ProDOS block device"
	.byte 0
	rts
notblk:	jsr xmess
	asc "unknown card"
	.byte 0
	rts
;
bitpos:
	.byte %00000001
	.byte %00000010
	.byte %00000100
	.byte %00001000
	.byte %00010000
	.byte %00100000
	.byte %01000000
	.byte %10000000
;
; If ROM at (rom) is a ProDOS-recognized clock card,
; return CLC
;
chk_clock:	ldy #0
	lda (rom),y
	cmp #$08
	bne chks_no
	ldy #2
	lda (rom),y
	cmp #$28
	bne chks_no
	ldy #4
	lda (rom),y
	cmp #$58
	bne chks_no
	ldy #6
	lda (rom),y
	cmp #$70
	bne chks_no
	clc
	rts
;
; If ROM at (rom) is for a SmartPort, return CLC
;
chk_smport:
	jsr chk_blkdev
	bcs chks_no
	ldy #7
	lda (rom),y
	bne chks_no
	clc
	rts
chks_no:	sec
	rts
;
; If ROM at (rom) is for a block device, return CLC
;
chk_blkdev:
	ldy #1
	lda (rom),y
	cmp #$20
	bne chkb_no
	ldy #3
	lda (rom),y
	bne chkb_no
	ldy #5
	lda (rom),y
	cmp #$03
	bne chkb_no
	clc
	rts
chkb_no:	sec
	rts
;
; DescribeSP -- show all SmartPort information
;
DescribeSP:	lda #1
	sta slot
	lda #0
	sta rom
desSp1:	lda slot
	ora #$c0
	sta rom+1
	jsr chk_smport
	bcs chk_hiddenSP
	jsr Descr1SP	
desNotSP:	inc slot
	lda slot
	cmp #8
	bcc desSp1
	rts
; look for IIc Plus hidden smartport
chk_hiddenSP:	
	lda $fbbf
	cmp #5
	bne desNotSP
	lda slot
	cmp #6
	bne desNotSP
	lda $c64e
	cmp #$38 ; sec
	bne desNotSP
	lda $c651
	cmp #$18 ; clc
	bne desNotSP
	jsr xmess
	asc "IIc Plus hidden SmartPort in slot 6."
	.byte $8d,$00
	lda #$c6
	sta SpTrick+2
	lda #$51
	sta SpTrick+1
	jsr SpDescr
	jmp desNotSP
	
;
Descr1SP:
	jsr xmess


	asc "SmartPort controller found in slot "

	.byte 0
	lda slot
	ora #'0'+$80
	jsr cout
	jsr xmess
	.byte $80+'.',cr,0
; find the entry point
	ldy #$ff
	lda (rom),y
	clc
	adc #3
	sta SpTrick+1
	lda rom+1
	sta SpTrick+2
;
SpDescr:
	jsr SpStatus
	jsr EachStatus
	rts
;
; SpStatus -- get and print global status of a SmartPort chain
;
staterr:	jmp xProDOS_err
SpStatus:
	ldx #sptSTATUS
	lda #>GlobStat
	ldy #<GlobStat
	jsr CallSP
	bcs staterr
	jsr xmess


	asc "Number of devices: "

	.byte 0
	lda #0
	ldy NumDevs
	jsr xprdec_2
	.if 0 ; change to 1 to enable TN.SMPT.2 fields
	jsr my_crout
	jsr xmess
	asc "Vendor: "
	.byte $00
	ldy SpVendor
	lda SpVendor+1
	jsr xprdec_2
	jsr xmess
	asc " Version: "
	.byte $00
	lda SpVersion+1
	jsr xprint_ver
	lda SpVersion
	jsr $fdda
	.endif
	jmp my_crout
;
GlobStat:	.byte 3,0
	.addr gstat2
	.byte 0	;statcode
; http://www.1000bit.it/support/manuali/apple/technotes/smpt/tn.smpt.2.html
gstat2:
NumDevs:	.byte $00
SpIrqStatus: .byte $00
SpVendor: .word $0000
SpVersion: .word $0000
  .byte $00,$00 ; reserved
;
; EachStatus -- print stuff for every SmartPort device
;               in this chain
;
EachStatus:
	lda #1
	sta Unit
es1:	lda Unit
	cmp NumDevs
	beq es_go
	bcs es_done
es_go:	jsr StatOneUnit
	inc Unit
	jmp es1
es_done:	rts
;
StatOneUnit:
	jsr xmess


	asc "Unit #"

	.byte 0
	lda Unit
	sta UnitNum
	tay
	lda #0
	jsr xprdec_2
	jsr xmess


	asc ": "

	.byte 0
	lda $fbdd
	cmp #$02      ; LC IIe Card byte
	bne :+
	; work around lazy IIe LC PDS Card smartport
	; that doesn't write device type, subtype, or version
	; for hard disk partitions.  It doesn't bother to fill
	; them, so they are either uninitialized or match a
	; previous 3.5" disk.
	lda #$07      ; SCSI hard disk
	sta DevType
	lda #$00
	sta DevSubtype
	sta UnitVersion
	sta UnitVersion+1
	sta NumBlocks
	sta NumBlocks+1
	sta NumBlocks+2
	sta NameLen
:	ldx #sptSTATUS
	lda #>Stat1parms
	ldy #<Stat1parms
	jsr CallSP
	bcc statok
	jmp xProDOS_err
;
statok:
	lda StatByte
	jsr PrintStatByte
	jsr my_crout
;
	jsr xmess


	asc "         Blocks: "

	.byte 0
	lda NumBlocks+2
	ldx NumBlocks+1
	ldy NumBlocks
	sta xnum+2
	stx xnum+1
	sty xnum
	jsr xprdec_3
	jsr my_crout
;
	jsr xmess


	asc "         Device name: "

	.byte 0
	ldx #0
	ldy NameLen
	bne prname1
  ; no device name
  jsr xmess
  asc "(none)"
  .byte $00
  jmp :+
prname1:	lda NameLen+1,x
	ora #$80
	jsr cout
	inx
	dey
	bne prname1
:	jsr my_crout

;
	jsr PrintType
	jsr xmess


	asc ", subtype=$"

	.byte 0
	lda DevSubtype
	jsr $fdda
	jsr xmess


	asc ", version=$"

	.byte 0
	lda UnitVersion+1
	jsr $fdda
	lda UnitVersion
	jsr $fdda
	jsr my_crout
	lda DevSubtype
	bne :+
	rts
	; http://www.1000bit.it/support/manuali/apple/technotes/smpt/tn.smpt.7.html
:	pha
  jsr xmess
	asc "         "
	.byte $00
	pla
	pha
	and #%10000000
	beq :+
	jsr xmess
	asc "Extended.  "
	.byte $00
: pla
	pha
	and #%01000000
	beq :+
	jsr xmess
	asc "Disk-switch error supported.  "
	.byte $00
: pla
	pha
	and #%00100000
	beq :+
	jsr xmess
	asc "Non-removable.  "
	.byte $00	
:	pla ; clean stack
  jsr my_crout
	rts

;
PrintType:
	jsr xmess


	asc "         Type = "

	.byte 0
	lda #0
	ldy DevType
	jsr xprdec_2
	jsr xmess


	asc " ("

	.byte 0
	jsr prtype2
	jsr xmess


	asc ")"

	.byte 0
	rts

prtype2:
	lda DevType
	cmp #$10
	bcc less10
	lda #$10
less10:	asl a
	tax
	lda spTypes+1,x
	pha
	lda spTypes,x
	pha
	rts
;
PrintStatByte:
	sta scratch
	jsr sb7
	jsr xmess


	asc ", "

	.byte 0
	asl scratch
	asl scratch
	jsr ChkNot
	jsr xmess


	asc "online, "

	.byte 0
	asl scratch
	jsr ChkNot
	jsr xmess


	asc "write protected"

	.byte 0
	rts
;
sb7:	asl scratch
	bcc chardev
	jsr xmess
	asc "block device"
	.byte 0
	rts
chardev:	jsr xmess
	asc "character device"
	.byte 0
	rts
;
ChkNot:	asl scratch
	bcs notz
	jsr xmess
	asc "not "
	.byte 0
notz:	rts
;
Stat1parms:
	.byte 3
UnitNum:	.byte 0
	.addr Stat2
	.byte 3
Stat2:
StatByte:	.byte 0
NumBlocks:	.byte 0,0,0
NameLen:	.byte 0
	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DevType:	.byte 0
DevSubtype:	.byte 0
UnitVersion:	.addr 0
;**********************************************************
CallSP:	stx spCmd
	sta spParms+1
	sty spParms
SpTrick:	jsr $0000
spCmd:	.byte 0
spParms:	.addr 0
	rts
;
;
spTypes:	.addr spt0-1,spt1-1,spt2-1,spt3-1,spt4-1,spt5-1,spt6-1,spt7-1
	.addr spt8-1,spt9-1,spt10-1,spt11-1,spt12-1,spt13-1,spt14-1,spt15-1,spt16-1
spt0:	jsr xmess
	asc "RAM disk"
	.byte 0
	rts
spt1:	jsr xmess
	asc "3.5"
	.byte $a2
	asc " disk"
	.byte 0
	rts
spt2:	jsr xmess
	asc "ProFile-type hard disk"
	.byte 0
	rts
spt3:	jsr xmess
	asc "generic SCSI"
	.byte 0
	rts
spt4:	jsr xmess
	asc "ROM disk"
	.byte 0
	rts
spt5:	jsr xmess
	asc "SCSI CD-ROM"
	.byte 0
	rts
spt6:	jsr xmess
	asc "SCSI Tape or other SCSI sequential"
	.byte 0
	rts
spt7:	jsr xmess
	asc "SCSI hard disk"
	.byte 0
	rts
spt8:	jsr xmess
	asc "???"
	.byte 0
	rts
spt9:	jsr xmess
	asc "SCSI printer"
	.byte 0
	rts
spt10:	jsr xmess
	asc "5.25"
	.byte $a2
	asc " disk"
	.byte 0
	rts
spt11:	jsr xmess
	asc "???"
	.byte 0
	rts
spt12:	jsr xmess
	asc "???"
	.byte 0
	rts
; TN.SMPT.4: http://www.1000bit.it/support/manuali/apple/technotes/smpt/tn.smpt.4.html
spt13:	jsr xmess
	asc "Printer"
	.byte 0
	rts
spt14:	jsr xmess
	asc "Clock"
	.byte 0
	rts
spt15:	jsr xmess
	asc "Modem"
	.byte 0
	rts
spt16:	jsr xmess
	asc "???"
	.byte 0
	rts

;
my_crout:	jsr xcheck_wait
;bcc fine
;jmp xerr
fine:	jmp crout
.out .sprintf ("deschw ends at $%x", *)
.assert * < $b000, warning, "deschw overruns $b000"
