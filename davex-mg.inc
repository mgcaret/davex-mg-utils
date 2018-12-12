
; MG's davex macros
;
; A skeleton for using this:
;       DX_start $a000 ; load address
;       DX_info $01,$26,dx_cc_iie_or_iigs,$00
;       DX_ptab
;       DX_parm 's',t_path
;       DX_end_ptab
;       DX_desc "My command"
;       DX_main
;       <code>
;       DX_end
;       

; *********
.ifdef I_MG_DAVEX
.warning "davex-mg included more than once"
.else
I_MG_MAC    = 1
; *********

.include  "Common/2/Globals2.asm"
.include  "Common/2/Apple.Globals2.asm"
.include  "Common/2/MLI.Globals2.asm"
.include  "Common/Macros.asm"

;cout      = $fded               ; character out
checkmach	= $fe1f	              ; clears carry on IIgs, no effect on //e

p8_mli    = $bf00
mli_atlk  = $42

dx_cc_any = 0
dx_cc_40col = 1 << 7
dx_cc_80col = 1 << 6
dx_cc_iie_or_iigs = 1 << 5
dx_cc_iic   = 1 << 4
dx_cc_iigs  = 1 << 3
dx_cc_bit2  = 1 << 2
dx_cc_bit1  = 1 << 1
dx_cc_bit0  = 1 << 0

dx_mg_auto_origin = 0           ; value for auto origination
                                ; using utils/auto_origin.sh

dx_desc_none = 0

.macro    P8call    CallNum, PList
          jsr   p8_mli
          .byte CallNum
          .addr PList
.endmacro

.macro    ATcall    PList
          P8call  mli_atlk, PList
.endmacro

; if the Load address is zero, we don't export X_DX_LOAD
; so that later we can calculate an appropriate start addr
; and let the linker do it.
.macro    DX_start  Load,Resv
          .ifdef DID_DX_start
            .warning "DX_start used more than once"
          .else
            .export DID_YOU_DX_end
            .export X_DX_MACROS
            X_DX_MACROS = 1
            .if Load > 0
            .org  Load
            .export X_DX_LOAD
            .else
            .export X_DX_AUTO_LOAD
            X_DX_AUTO_LOAD = 1
            .endif
            X_DX_LOAD := *
            .ifblank Resv
            X_DX_RESV = 0
            .else
            X_DX_RESV = Resv
            .out .sprintf("Reserving %d bytes", Resv)
            .endif
            .export X_DX_RESV
ext_start:
          rts
          .byte $ee, $ee
            DID_DX_start = 1
          .endif
.endmacro

.macro    DX_info    CVer, DXVer, CCs, DAVer
          .ifdef DID_DX_start
          .byte CVer, DXVer, CCs    ; versions and characteristics
          .addr X_DX_DESC           ; description pointer to pascal str
          .addr X_DX_LOAD           ; load address
          .addr X_DX_EXEC           ; exec address
          .byte DAVer               ; minor version
          .byte $00, $00, $00       ; spares
          DID_DX_info = 1
          .else
            .warning "DX_info must happen after DX_start"
          .endif
.endmacro

.macro    DX_ptab
          .ifdef DID_DX_info
          ; start parameter table
          DID_DX_ptab = 1
          .else
            .warning "DX_ptab must come after DX_info"
          .endif
.endmacro

.macro    DX_parm   Char, Type
          .ifdef DID_DX_ptab
          .if   Char > 0
          .byte Char|$80,Type
          .else
          .byte Char,Type
          .endif
          .else
            .warning "DX_parm must come after DX_ptab"
          .endif
.endmacro

.macro    DX_end_ptab
          .ifdef DID_DX_info
          .byte $00,$00
          DID_DX_end_ptab = 1
          .else
            .warning "DX_end_ptab must come after DX_info"
          .endif
.endmacro

.macro    DX_desc   Desc
          .ifndef DID_DX_end_ptab
            .warning "DX_desc must come after DX_end_ptab"
          .endif
X_DX_DESC: .byte   .strlen(Desc)
          asc_hi    Desc
.assert   (* - ext_start) < 512, warning, "Description must end in the first 512 bytes"
.endmacro

.macro    DX_main   Addr
          .ifnblank Addr
            .out .sprintf("Note: main code address at %s", .string(Addr))
            X_DX_EXEC = Addr
          .else
            .ifconst *
              .out .sprintf("Note: implied main code at * = $%x", *)
            .else
              .out .sprintf("Note: implied main code at DX_main")
            .endif
            X_DX_EXEC = *
          .endif
.endmacro

.macro    DX_end
          .ifndef DID_DX_start
            .error "DX_start not used"
          .endif
          X_DX_END = *
          X_DX_SIZE = X_DX_END - X_DX_LOAD + X_DX_RESV
          DID_YOU_DX_end = 1
          .ifndef DID_DX_info
            .error "DX_info not used"
          .endif
          .ifndef X_DX_EXEC
            .error "DX_main not used"
          .endif
          .ifndef X_DX_DESC
            .out "Note: no DX_end_ptab used, hope that's okay"
          .endif
          .ifndef X_DX_DESC
            .out "Note: no description provided"          
          X_DX_DESC := 0
          .endif
          .ifndef X_DX_AUTO_LOAD
            .out .sprintf("Code ends at $%x, size $%x (%x resv)", X_DX_END, X_DX_SIZE, X_DX_RESV)
            .assert * < $b000, error, "Code is past $b000!"
            .if (X_DX_END+X_DX_RESV) < $af00
              .out "Consider moving code start to place end shortly before $b000"
              .out .sprintf("Perhaps at $%x", ($b000-X_DX_SIZE)&$ff00)
            .endif
            .if (X_DX_END+X_DX_RESV) > $afff
              .out "Code overruns available space, consider moving code start"
              .out .sprintf("Perhaps at $%x", ($b000-X_DX_SIZE)&$ff00)
              .error "Cannot continue"
            .endif
          .else
            .out .sprintf("Auto load address, size $%x", X_DX_SIZE)
          .endif
.endmacro

; *********
.endif
; *********
