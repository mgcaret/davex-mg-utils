; %help
; Davex Forth interpeter.
;
; syntax: dxforth
;
; This is a Forth system implementing the Forth 2012 Core Word Set
; a subset of the Core Extensions word set, the Exception word set,
; and select words from other word sets.
; 
; Additionally, words supporting Davex and ProDOS are provided.
;
; See the full documentation (not written yet) for complete information.
; %hend

; This is a byte-addressed, direct-threaded, 16-bit Forth.  The stack
; can't be on the zero page due to Davex only giving us 32 bytes there.
; so it's a bit slower than some Forths due to that.

; PSP = X register
; RSP = 6502 stack
; IP = self-modified in fetch_IP
; DLAST = xczpage+0,1 - last dictionary word defined, HBYTE=0, none
; CHERE = xczpage+2,3 - next address to be compiled
; WR = xczpage+4,5 - working register
; XR = xczpage+6,7 - working register
; YR = xczpage+8,9 - working register
; ZR = xczpage+10,11 - working register
; ZSTATE = xczpage+12,13 - compilation state
; ZBASE = xczpage+14,15 - system number base
; ZACC = xczpage+16..19 - mixed-precision multiply accumulator
; SPTMP = xcpage+20,21 - place to save X register for operations that use it
; HIMEM = SPTMP+22,23 - highest available memory address+1
; RSSVE = xczpage+24,25 - saves the 6502 stack pointer upon system entry
; IHERE = xczpage+26,27 - start of user dictionary space
; INBUF = xczpage+28,29 - location of input buffer
; TRADR = xczpage+$1E - trace address (when assembled with tracing enabled)

; ***** Options & Debugging *****
.define TRACE 0
.define TRACE_WORDS 0

; ***** Firmware *****
COut      = $fded
COut1     = $fdf0
TabV      = $fb5b
PrByte    = $fdda
Home      = $fc58
VTab      = $fc22
;KeyIn     = $fd1b            ; do not use for davex
PrntAX    = $f941

; ***** Zero Page *****

CH      = $24
CV      = $25

.globalzp xczpage             ; davex should do this
DLAST   = xczpage
CHERE   = DLAST+2
WR      = CHERE+2             ; working register
XR      = WR+2                ; second working reg
YR      = XR+2                ; for saving Y register, usually
ZR      = YR+2                ; used for searching dict
ZSTATE  = ZR+2                ; nonzero = compiling
ZBASE   = ZSTATE+2
ZACC    = ZBASE+2             ; 4 bytes
ZACC1   = ZACC
ZACC2   = ZACC1+2
SPTMP   = ZACC+4              ; for primitives to save data stack ptr for certain ops
HIMEM   = SPTMP+2             ; maybe for memory management

RSSAV   = xczpage+$18         ; for stuff we don't re-init
IHERE   = RSSAV+2             ; start of user-defined dictionary words
INBUF   = IHERE+2             ; location of input buffer

TRADR   = xczpage+$1E

.assert (HIMEM+2)<=RSSAV,error,"fix zpage stuff!"

; ***** Constants *****

.define PSTK_SZ $80           ; size of parameter stack, some optimizations when $80
PSTK    = $AF00               ; covered by the reserve
PSTKL   = PSTK
PSTKH   = PSTK+PSTK_SZ

.define PAD_SIZE 128              ; (for reference, not directly used yet)
                                  ; minimum required by standard is 84
.define WORD_SIZE 35              ; minimum required by standard is 33, but
                                  ; the pictured numeric output words share
                                  ; it and is required to be
                                  ; 2*(cell size in bits=1)+2 = 34 bytes, plus
                                  ; we want a count byte at the beginning

; compiler constants
.define opJSR $20                 ; JSR opcode
.define opJMP $4C                 ; JMP opcode
.define opRTS $60                 ; RTS opcode
.define opNOP $EA                 ; NOP opcode

; flags for words
.define F_IMMED %01000000         ; immediate
.define F_CONLY %00100000         ; compile-only
.define F_SMUDG %00010000         ; smudged, invisible in search

; Control chars
AFF       = $0C
ACR       = $0D

; Non-keyboard/eval source IDs (high byte)
.define SRC_REDIR $FE                   ; Input redirected
.define SRC_ARG   $FD                   ; File given on command line

; Misc
P8_ER_RNG = $FE                   ; range of P8 errors for exception numbers

.macro  ENTER
        jsr   enter
.endmacro

.macro  EXIT
        .addr exit_next
.endmacro

.macro  CODE
        .addr exit_code
.endmacro

.macro  NEXT
        jmp next
.endmacro

; optimization for the common case where
; a native word would end with jsr pushay followed by NEXT
.macro  PUSHNEXT
        jmp next::fast_num
.endmacro

.macro  RUN
        jmp next::run
.endmacro

.macro  NLIT num
  .if num & $FF00
    .word LIT::xt
  .endif
  .byte   <(num)             ; can't do .word because ca65 won't allow negative
  .byte   >(num)
.endmacro

.macro  SLIT str
  .local  target,addr
  .addr   _JUMP
  .addr   target
addr:
  .byte   str
target:
  NLIT    addr
  NLIT    .strlen(str)
.endmacro

.macro  CSLIT str
  .local  target,addr
  .addr   _JUMP::xt
  .addr   target
addr:
  .byte   .strlen(str)
  .byte   str
target:
  NLIT    addr
.endmacro

; dictionary macros

; Dictionary format:
; Bytes Purpose
; 2     Link to previous
; 1     flags & Name length, high bit always set
; n     Name (low ASCII)
; m     Code field (direct threaded, CFA is eliminated)
; ...   next entry or HERE

; 
print_dict .set 1
;
.macro  dstart
  __dstart = 0
  .define   l_dword __dstart
.endmacro

; define a headerless word
; fname is there so that a word can be switched back and
; forth between a headerless and normal.  Flags are irrelevant
; because headerless words can't be used by the user.
.macro  hword dname,fname,flags
  .ifnblank flags
    .out "Warning: flags used on headerless word"
  .endif
  .ifdef c_dword
    .error .sprintf("%s definition not closed",.string(c_dword))
  .endif
  .ifdef c_hword
    .error .sprintf("%s definition not closed",.string(c_hword))
  .endif
  .if print_dict
    .if .const(*)
      .out .concat(fname, .sprintf(" (headerless) starts at $%x", *))
    .endif
  .endif
  .define c_hword dname
  .proc dname
  xt:
  .if TRACE_WORDS
    trace fname
  .endif
.endmacro

.macro  dword dname,fname,flags
  .ifdef c_dword
    .error .sprintf("%s definition not closed",.string(c_dword))
  .endif
  .ifdef c_hword
    .error .sprintf("%s definition not closed",.string(c_hword))
  .endif
  .if print_dict
    .if .const(*)
      .out .concat(fname, .sprintf(" starts at $%x", *))
    .endif
  .endif
  .define c_dword dname
  .proc dname
  .addr l_dword
  .ifblank flags
    .byte $80+.strlen(fname)
  .else
    .byte ($80|flags)+.strlen(fname)
  .endif
  .byte fname
  xt:
  .if TRACE_WORDS
    jsr trace_word
  .endif
  ;.if print_dict
  ;  .out .concat(fname, .sprintf(" entry at $%x", xt))
  ;.endif
.endmacro

.macro  dwordq dname,fname,flags
  .charmap  $27,$22 ; temporarily map ' -> "
  dword dname,fname,flags
  .charmap  $27,$27 ; undo mapping
.endmacro

.macro  dchain dname
  .ifdef l_dword
  .undefine l_dword
  .endif
  .define l_dword dname
.endmacro

.macro  eword
  .endproc
  .ifdef c_dword
    dchain c_dword
    .undefine c_dword
  .endif
  .ifdef c_hword
    .undefine c_hword
  .endif
.endmacro

.macro  dconst dname,fname,value,flags
  dword dname,fname,flags
    ldy   #<value
    lda   #>value
    PUSHNEXT
  eword
.endmacro

.macro  hconst dname,fname,value,flags
  hword dname,fname,flags
    ldy   #<(value)
    lda   #>(value)
    PUSHNEXT
  eword
.endmacro

.macro  dvar dname,fname,value,flags
  dword dname,fname,flags
    jsr   pushda
    val: .word value
  eword
.endmacro

.macro  hvar dname,fname,value,flags
  hword dname,fname,flags
    jsr   pushda
    val: .word value
  eword
.endmacro

.macro dvalue dname,fname,value,flags
  dword dname,fname,flags
    jsr   pushconst
    val: .word value
  eword
.endmacro

.macro hvalue dname,fname,value,flags
  hword dname,fname,flags
    jsr   pushconst
    val: .word value
  eword
.endmacro


.macro dend
  ELAST = l_dword
.endmacro

.macro    trace name
  .if TRACE_WORDS
          jsr   tr_save_regs
          jsr   xmess
          .byte '{',name,'}',$00
          jsr   tr_rest_regs
  .endif
.endmacro

.p02
.include  "davex-mg.inc"

          ;DX_start dx_mg_auto_origin,$100 ; load address & top reserve
          DX_start $8E00,$100 ; load address & top reserve for stack
version:  DX_info $01,$12,dx_cc_any,$00
          DX_ptab
          DX_parm 0,t_path
          DX_end_ptab
          DX_desc "Forth interpreter."
          DX_main
          ; first init the immutables
          tsx
          stx   RSSAV                   ; save 6502 stack ptr
          ldx   #mli_read
          jsr   xmmgr                   ; get # free pages
          cmp   #$03                    ; need at least 3 pages...
          bcc   nomem
          tax                           ; work around mmgr bug
          dex
          txa
          ldx   #mli_open
          jsr   xmmgr
          bcs   nomem
          sta   INBUF+1
          sta   IHERE+1
          inc   IHERE+1                 ; make room for input buffer
          lda   #$00
          sta   SOURCE_ID
          sta   SOURCE_ID+1
          jsr   xgetparm_n              ; see if file given
          sta   WR+1
          sty   WR
          ldy   #$00
          lda   (WR),y
          beq   :+
          lda   #SRC_ARG
          sta   SOURCE_ID+1             ; flag initial source ID
:         ldx   #$00                    ; init stack ptr
          stx   INBUF
          stx   IHERE
          jmp   _cold                   ; now cold-start the interpreter
nomem:    jsr   xmess
          .byte "Not enough memory!",$0D,$00
          jmp   xerr

.if TRACE
.proc     _dtrace
          txa
          pha
          lda   TRADR+1
          jsr   PrByte
          lda   TRADR
          jsr   PrByte
          pla
          tax
          rts
.endproc
.endif

.if TRACE_WORDS
.proc     trace_word
          jsr   tr_save_regs
          tsx
          pla
          tay
          pla
          txs
          sta   TRADR+1
          sty   TRADR
          ; TRADR now points at the last byte of the JSR
          jsr   dec_tradr           ; to middle byte
          jsr   dec_tradr           ; to first byte
:         jsr   dec_tradr           ; last byte of name, or flags+len if anon
          bpl   :-
          and   #$0F
          beq   anon                ; anonymous
          sta   check
          lda   #'{'
          jsr   _emit
          ldy   #$00
:         iny
          lda   (TRADR),y
          cmp   #' '
          bcc   nono
          jsr   _emit
nono:     cpy   #$00                ; self-modified
check = * - 1
          bcc   :-
          lda   #'}'
          jsr   _emit
          jsr   tr_rest_regs
          rts
anon:     jsr   xmess
          .byte "{noname}",$00
          ; fall-through to tr_rest_regs
.endproc

.proc tr_rest_regs
          lda   #$00
psave = * - 1
          pha
          lda   #$00
asave = * - 1
          ldx   #$00
xsave = * - 1
          ldy   #$00
ysave = * - 1
          plp
          rts
.endproc

.proc tr_save_regs
          php
          sta   tr_rest_regs::asave
          stx   tr_rest_regs::xsave
          sty   tr_rest_regs::ysave
          pla
          sta   tr_rest_regs::psave
          rts
.endproc

.proc     dec_tradr
          lda   TRADR
          bne   :+
          dec   TRADR+1
:         dec   TRADR
          ldy   #$00
          lda   (TRADR),y
          rts
.endproc
.endif

; inner interpreter entry
; pops caller from 6502 stack and initializes IP with it
; saving previous IP on 6502 stack.
.proc     enter
.if TRACE
          txa
          pha
          jsr   xmess
          .byte $8D,"[ENTER]",$00
          jsr   xcheck_wait
          pla
          tax
.endif
          lda   IP
          sta   WR
          lda   IP+1
          sta   WR+1
          pla
          sta   IP
          pla
          sta   IP+1
          lda   WR+1
          pha
          lda   WR
          pha
          ; fall-through
.endproc

; fetch and execute next instruction
.proc     next
          jsr   fetch_IP          ; fetch low byte
          tay
          jsr   fetch_IP
.if TRACE
          beq   fast_num1
.else
          beq   fast_num
.endif
run:      sta   target+1
          sty   target
.if TRACE
          sta   TRADR+1
          sty   TRADR
          txa
          pha
          lda   #'>'
          jsr   _emit
          jsr   _dtrace
          pla
          tax   
.endif
          jmp   *                 ; self-modified, dispatch xt
target = * - 2
.if TRACE
fast_num1:
          sta   TRADR+1
          sty   TRADR
          txa
          pha
          lda   #'^'
          jsr   _emit
          pla
          tax
          lda   TRADR+1
          ldy   TRADR
.endif
fast_num:
          jsr   pushay            ; throw on stack
          jmp   next              ; and immediately do next insn
.endproc

.proc     fetch_IP
          inc   IP
          bne   :+
          inc   IP+1
:         
.if TRACE
          lda   IP
          sta   TRADR
          lda   IP+1
          sta   TRADR+1
          txa
          pha
          lda   #':'
          jsr   _emit
          jsr   _dtrace
          pla
          tax
.endif
          lda   *                 ; self-modified
IP = * - 2
          rts
.endproc
IP      = fetch_IP::IP

; exit thread.  restore previous IP
; and resume execution at Forth IP
.proc     exit_next
          .if TRACE
          txa
          pha
          jsr   xmess
          .byte "[EXIT]",$8D,$00
          jsr   xcheck_wait
          pla
          tax
          .endif
          pla                     ; and restore IP of caller's caller
          sta   IP
          pla
          sta   IP+1
          NEXT
.endproc

; exit thread, restore previous IP
; and resume 6502 after last executed IP
.proc     exit_code
          ldy   IP                ; save IP
          sty   YR
          lda   IP+1
          sta   YR+1
.if   TRACE
          sta   TRADR+1
          sty   TRADR
          txa
          pha
          jsr   xmess
          .byte ">CODE:",$00
          jsr   _dtrace
          pla
          tax
.endif
          pla                     ; restore previous IP
          sta   IP
          pla
          sta   IP+1
          lda   YR+1              ; old IP on 6502 stack
          pha
          lda   YR
          pha
          rts                     ; rts resumes execution at IP+1
.endproc

; ***** stack primitives *****

.proc     peekay
          jsr   popay
          inx
          rts
.endproc

; flags reflect the high byte of the popped word
.proc     popay
          dex
          bmi   stku_err
          ldy   PSTKL,x
          lda   PSTKH,x
.if TRACE
          sta   TRADR+1
          sty   TRADR
          txa
          pha
          lda   #'/'
          jsr   _emit
          jsr   _dtrace
          pla
          tax
          ldy   PSTKL,x
          lda   PSTKH,x
.endif
          rts
.endproc

.proc     popwr
          jsr   popay
          sta   WR+1
          sty   WR
          rts
.endproc

.proc     popxr
          jsr   popay
          sta   XR+1
          sty   XR
          rts
.endproc

; stack pop routines for davex routines
; ( d -- ) -> A(high) XY(low) = d truncated to 24 bits
; X will be saved in SPTMP
.proc     popaxy
          jsr   popay     ; high cell
          sty   YR+1      ; to by A later
          jsr   popay     ; low cell
          stx   SPTMP
          tax
          lda   YR+1
          rts
.endproc

.proc     stku_err
          ldx   #$00
          ldy   #<-4
          lda   #>-4
          jmp   _throway
.endproc

.proc     stko_err
          ldx   #PSTK_SZ-8          ; leave enough room for ops
          ldy   #<-3
          lda   #>-3
          jmp   _throway
.endproc

; push AY onto stack, preserves contents of AY
.proc     pushay
.if TRACE
          sta   TRADR+1
          sty   TRADR
.endif
          pha
          sta   PSTKH,x
          tya
          sta   PSTKL,x
          pla
          inx
.if PSTK_SZ=$80
          bmi   stko_err
.else
          cpx   #PSTK_SZ
          bcs   stko_err
.endif
.if TRACE
          pha
          lda   #'#'
          jsr   _emit           ; these must preserve regs 'cept A
          jsr   _dtrace
          pla
.endif
          rts
.endproc

; preserves AY
.proc     pusha
.if TRACE
          sta   TRADR
.endif
          pha
          sta   PSTKL,x
          lda   #$00
          sta   PSTKH,x
          pla
          inx
.if PSTK_SZ=$80
          bmi   stko_err
.else
          cpx   #PSTK_SZ
          bcs   stko_err
.endif
.if TRACE
          pha
          lda   #$00
          sta   TRADR+1
          lda   #'#'
          jsr   _emit
          jsr   _dtrace
          pla
.endif
          rts
.endproc

; ***** Interpretation Helpers *****

; push word data address
; this is the default routine used by CREATE
; call via JSR, pops return stack entry, pushes data addr onto stack, and
; exits via next
.proc     pushda
          pla                     ; get low byte
          clc
          adc   #$01
          tay
          pla
          adc   #$00              ; in case page crossed
          PUSHNEXT
.endproc

; push constant
; pushes the word following the JSR onto the stack
; and exits via next, this is also used by VALUE
.proc     pushconst
          pla             ; low byte
          clc
          adc   #$01      ; account for RTS PC-1
          sta   WR
          pla
          adc   #$00
          sta   WR+1
          ldy   #$01
          lda   (WR),y
          pha
          dey
          lda   (WR),y
          tay
          pla
          PUSHNEXT
.endproc

; ***** Compilation Helpers *****

.proc     cworday
          pha
          tya
          ldy   #$00
          sta   (CHERE),y
          iny
          pla
          sta   (CHERE),y
          jsr   inchere
          ; fall-through
.endproc

.proc     inchere
          inc   CHERE
          bne   :+
          inc   CHERE+1
:         rts
.endproc

.proc     cbytea
          ldy   #$00
          sta   (CHERE),y
          jmp   inchere
.endproc

; ***** Math Library *****
; save X before calling any of these
; use YR and ZR for the operands, ZACC for the results

; ZACC(32)=ZR(16)*YR(16)
; adapted from https://www.llx.com/~nparker/a2/mult.html
.proc     _umult
          lda   #0
          sta   ZACC+2
          ldx   #16
l1:       lsr   YR+1
          ror   YR
          bcc   l2
          tay
          clc
          lda   ZR
          adc   ZACC+2
          sta   ZACC+2
          tya
          adc   ZR+1
l2:       ror
          ror   ZACC+2
          ror   ZACC+1
          ror   ZACC
          dex
          bne   l1
          sta   ZACC+3
          rts
.endproc

; ZR rem ZACC1=ZR/YR 
; ibid.
.proc     _udiv
          lda   #0
          sta   ZACC1
          sta   ZACC1+1
          ldx   #16
l1:       asl   ZR
          rol   ZR+1
          rol   ZACC1
          rol   ZACC1+1
          lda   ZACC1
          sec
          sbc   YR
          tay
          lda   ZACC1+1
          sbc   YR+1
          bcc   l2
          sta   ZACC1+1
          sty   ZACC1
          inc   ZR
l2:       dex
          bne   l1
          rts
.endproc

; ZACC(16) rem ZR(16)=ZR(32)/YR(16)
; adapted from Garth Wilson's routines
; N=0:YR 1:YR+1 2:ZR 3:ZR+1 4:ZACC 5:ZACC+1 6:ZACC+2 7:ZACC+3
.proc     _umdiv
          sec
          lda   ZR
          sbc   YR
          lda   ZR+1
          sbc   YR+1
          bcc   :+              ; no overflow
          ldy   #<-11
          lda   #>-11
          jmp   _throway        ; result out of range
:         ldx   #$11
loop:     rol   ZACC
          rol   ZACC+1
          dex
          bne   :+
          rts
:         rol   ZR
          rol   ZR+1
          lda   #$00
          sta   ZACC+3          ; carry
          rol   ZACC+3
          sec
          lda   ZR
          sbc   YR
          sta   ZACC+2
          lda   ZR+1
          sbc   YR+1
          tay
          lda   ZACC+3
          sbc   #$0
          bcc   loop
          lda   ZACC+2
          sta   ZR
          sty   ZR+1
          bcs   loop
.endproc

; ***** DICTIONARY *****

dstart

; push a compiled literal at IP on the stack
; headerless native
hword     LIT,"LIT"
          jsr   fetch_IP
          tay
          jsr   fetch_IP
          PUSHNEXT
eword

; directly compile a cell literal from IP to (HERE)
hword     COMP_LIT,"COMP_LIT"
          jsr   fetch_IP
          tay
          jsr   fetch_IP
          jsr   cworday
          NEXT
eword

; directly compile a char literal from IP to (HERE)
hword     COMP_CLIT,"COMP_CLIT"
          jsr   fetch_IP
          jsr   cbytea
          NEXT
eword

; Programming-Tools 15.6.2.0830
; quit intepreter ( -- )
dword     BYE,"BYE"
          tya
          ldx   RSSAV
          txs
          tay
          lda   #mli_close
          jsr   xmmgr             ; free all mem
          rts
eword

; backing value for SOURCE-ID
hvar      dSOURCEID,"$SOURCE-ID",0
SOURCE_ID = dSOURCEID::val

; backing values for string buffers
hconst    SBUF1,"SBUF1",filebuff3
hconst    SBUF2,"SBUF2",filebuff3+256
hvar      CSBUF,"CSBUF",filebuff3

; non-standard
; coldstart interpreter ( * -- )
; resets to built-in dictionary, clear stack, etc.
dword     COLD,"COLD"
          jmp   _cold
eword

; Core 6.1.2250
; really a variable, but address is constant
dconst    STATE,"STATE",ZSTATE

; non-standard
hconst    DMEMTOP,"$MEMTOP",X_DX_LOAD

; non-standard
; really a variable, but address is constant
hconst    DHIMEM,"$HIMEM",HIMEM

.proc     _emit
          ora   #$80
          jmp   COut              ; it must preserve al registers
.endproc

; Core 6.1.1320
dword     EMIT,"EMIT"
          jsr   popay
          tya
          jsr   _emit
          NEXT
eword

; ( c-addr u -- )
; Consume c-addr and u, applying routine at (ZR), inited from AY,
; to every char of the string.
; When (ZR) is called, next char is in A.  (ZR) may trash 
; any registers except X, and must not touch WR and XR
; when it's called, Y=0 and XR=address of string
.proc     string_op_ay
          sta   ZR+1
          sty   ZR
op:       jsr   popwr             ; length into WR
          jsr   popxr             ; address into XR
          lda   WR                ; now calculate ending pos into WR
          clc
          adc   XR
          sta   WR
          lda   WR+1
          adc   XR+1
          sta   WR+1
lp:       lda   WR
          cmp   XR
          bne   :+
          lda   WR+1
          cmp   XR+1
          bne   :+
          rts
:         ldy   #$00        ; here in case (XR) trashes it
          lda   (XR),y
          jsr   docall
          inc   XR
          bne   :+
          inc   XR+1
:         jmp   lp
docall:   jmp   (ZR)
.endproc
string_op = string_op_ay::op ; user sets ZR instead

; Core 6.1.2310
dword     TYPE,"TYPE"
          ldy   #<_emit
          lda   #>_emit
          jsr   string_op_ay
          NEXT
eword
          
; Core 6.1.1370
; ( xt -- * )
dword     EXECUTE,"EXECUTE"
          jsr   popay
          RUN
eword

; headlerless word to implement branches
hword     _JUMP,"_JUMP"
jump2:    jsr   fetch_IP
          tay
          jsr   fetch_IP
          ; we need to be at one less than the given target
          cpy   #$00
          bne   :+
          sec
          sbc   #$01
:         dey
go:       sta   IP+1
          sty   IP
          NEXT
eword


; headlerless word to implement control flow
hword     _SKIP,"_SKIP"
skip2:    jsr   fetch_IP
          jsr   fetch_IP
          NEXT
eword

; headerless word to implement state-smartness
; if interpreting, jumps, if compiling, skips
hword     _SMART,"_SMART"
          lda   ZSTATE
          ora   ZSTATE+1
          beq   _JUMP::xt
          bne   _SKIP::xt
eword

; headlerless word to implement control flow
hword     _SKIP2,"_SKIP2"
          jsr   fetch_IP
          jsr   fetch_IP
          jmp   _SKIP::skip2
eword

.if 0 ; may not need this
; headlerless word to implement control flow
hword     _SKIPJUMP,"SKIPJUMP"
          jsr   fetch_IP
          jsr   fetch_IP
          jmp   _JUMP::jump2
eword
.endif

; Core 6.1.0150
; ( n -- ) compile word into dictionary
dword     COMMA,","
          jsr   popay
          jsr   cworday
          NEXT
eword

; Core 6.1.0860
; ( c -- ) compile char into dictionary
dword     CCOMMA,"C,"
          jsr   popay
          tya
          jsr   cbytea
          NEXT
eword

; helper
.proc     wrplus2
          lda   WR
          clc
          adc   #$02
          sta   WR
          lda   WR+1
          adc   #$00
          sta   WR+1
          rts
.endproc

; Core 6.1.0650
; ( adr -- n ) get number n from adr
dword     FETCH,"@"
          jsr   popwr
fetch2:   jsr   fetchay
          PUSHNEXT        
fetchay:  ldy   #$01          ; need to re-use
          lda   (WR),y
          pha
          dey
          lda   (WR),y
          tay
          pla
          rts
eword

; Core 6.1.0350
; ( addr -- x1 ) - store x2,x1 at addr,addr+cell
dword     TWOFETCH,"2@"
          jsr   popwr
          jsr   FETCH::fetchay
          jsr   pushay
          jsr   wrplus2
          jmp   FETCH::fetch2
eword

; Core 6.1.0870
; ( adr - c ) get char c from adr
dword     CFETCH,"C@"
          jsr   popwr
          ldy   #$00
          lda   (WR),y
          jsr   pusha
          NEXT
eword

; Core 6.1.0010
; ( x addr ) - store n at addr
dword     STORE,"!"
          jsr   popwr           ; pop addr into WR
store2:   jsr   popay           ; pop n
          jsr   storeay         ; need to re-use
          NEXT
storeay:  pha                   ; save high byte of n
          tya                   ; store low byte first
          ldy   #$00
          sta   (WR),y
          pla                   ; get high byte back
          iny
          sta   (WR),y
          rts
eword

; Core 6.1.0310
; ( x1 x2 addr ) - store x2,x1 at addr,addr+cell
dword     TWOSTORE,"2!"
          jsr   popwr
          jsr   popay
          jsr   STORE::storeay
          jsr   wrplus2
          jmp   STORE::store2
eword

; Core 6.1.0010
; ( c adr ) - store char c at addr
dword     CSTORE,"C!"
          jsr   popwr
          jsr   popay
          tya
          ldy   #$00
          sta   (WR),y
          NEXT
eword

; Core 6.1.1290
dword     DUP,"DUP"
          jsr   peekay
          PUSHNEXT
eword

; Core 6.1.0630
dword     QDUP,"?DUP"
          jsr   peekay
          cmp   #$00
          bne   :+
          cpy   #$00
          bne   :+
          NEXT
:         PUSHNEXT
eword

; Core 6.1.0580
dword     PtoR,">R"
          jsr   popay
          pha
          tya
          pha
          NEXT
eword

; Core ext 6.2.0340
; Must be primitive
dword     TWOPtoR,"2>R"
          jsr   _swap
          jsr   popay
          pha
          tya
          pha
          jmp   PtoR::xt
eword

; Non-standard helper
; ( x1 .. xn n -- n | r: -- xn .. x1 )
; copy x1-xn to return stack, leave n on param stack, n <= 255
; must be primitive, note not in the same order as TWOPtoR
hword     NPtoR,"N>R"
          jsr   popay             ; get n
          sty   YR
          sty   YR+1              ; save n 
          cpy   #$00              ; just in case
          beq   done
:         jsr   popay
          pha
          tya
          pha
          dec   YR
          bne   :-          
done:     lda   #$00
          ldy   YR+1
          PUSHNEXT
eword


; Core 6.1.2060
dword     RtoP,"R>"
          pla
          tay
          pla
          PUSHNEXT
eword

; Core ext 6.2.0410
; must be a primitive
dword     TWORtoP,"2R>"
          pla
          tay
          pla
          jsr   pushay
          pla
          tay
          pla
          jsr   pushay
          jsr   _swap
          NEXT
eword

; Non-standard helper
; ( r: -- xn .. x1 | n -- x1 .. xn n | )
; copy x1-xn to parameter stack, leave n on top of param stack, n <= 255
; must be primitive, note not in the same order as TWORtoP
hword     NRtoP,"N>R"
          jsr   popay             ; get n
          sty   YR
          sty   YR+1              ; save n
          cpy   #$00              ; just in case
          beq   done
:         pla
          tay
          pla
          jsr   pushay
          dec   YR
          bne   :-          
done:     lda   #$00
          ldy   YR+1
          PUSHNEXT
eword


; Core 6.1.2070
dword     RCOPY,"R@"
          stx   SPTMP
          tsx
          pla
          tay
          pla
          txs
          ldx   SPTMP
          PUSHNEXT
eword

; Non-standard
.if 0
dword     RSPat,"RSP@"
          stx   SPTMP
          tsx
          txa
          tay
          lda   #$01
          ldx   SPTMP
          PUSHNEXT
eword
.endif

; non-standard
dword     RDROP,"RDROP"
          pla
          pla
          NEXT
eword

; non-standard helper
hword     RPICK,"RPICK"
          jsr   popay
          tya
          asl
          sta   WR
          stx   SPTMP
          tsx
          txa
          sec             ; +1
          adc   WR
          tax
          lda   $100,x
          tay
          lda   $101,x
          ldx   SPTMP
          PUSHNEXT
eword

; headerless helper
; get the 2nd entry from the return stack
hword     RPLUCK,"RPLUCK"
          pla
          sta   WR
          pla
          sta   WR+1
          pla
          tay
          pla
          jsr   pushay
          lda   WR+1
          pha
          lda   WR
          pha
          NEXT
eword

; more complicated due to the split stack
.proc     _swap
          jsr   popay
          pha
          tya
          pha
          jsr   peekay
          jsr   pushay
          dex
          dex
          pla
          tay
          pla
          jsr   pushay
          inx
          rts
.endproc

; Core 6.1.2260
dword     SWAP,"SWAP"
          jsr   _swap
          NEXT
eword

; Core 6.1.1260
dword     DROP,"DROP"
          jsr   popay
          NEXT
eword

.proc     _over
          jsr   popay
          jsr   popay
          inx                         ; we know there are 2 values above SP
          inx
          jsr   pushay
          rts
.endproc

; Core 6.1.1990
dword     OVER,"OVER"
          jsr   _over
          NEXT
eword

; Core ext 6.2.1930
dword     NIP,"NIP"
          ENTER
          .addr SWAP::xt
          .addr DROP::xt
          EXIT
eword

; Core ext 6.2.2300
dword     TUCK,"TUCK"
          ENTER
          .addr SWAP::xt
          .addr OVER::xt
          EXIT
eword

; Core 6.1.0390
dword     TWODUP,"2DUP"
          jsr   _over
          jsr   _over
          NEXT
eword

; Core ext 6.2.0415
; ( -- x1 x2 ) ( R: x1 x2 -- x1 x2 )
; must be primitive
dword     TWORCOPY,"2R@"
          stx   SPTMP       ; save data stack ptr
          tsx               ; save 6502 stack ptr
          pla               ; pop x2
          tay
          pla
          sta   WR+1        ; save x2 in WR
          sty   WR
          pla               ; pop x1
          tay
          pla
          txs               ; restore 6502 stack
          ldx   SPTMP       ; restore data stack
          jsr   pushay      ; push x1
          lda   WR+1        ; get x2
          ldy   WR          ; push x2
          PUSHNEXT
eword


; Core 6.1.2160
dword     ROT,"ROT"
          ENTER
          .addr PtoR::xt
          .addr SWAP::xt
          .addr RtoP::xt
          .addr SWAP::xt
          EXIT
eword

; Non-standard
dword     NROT,"-ROT"
          ENTER
          .addr ROT::xt
          .addr ROT::xt
          EXIT
eword

; Core 6.1.0430
dword     TWOSWAP,"2SWAP"
          ENTER
          .addr PtoR::xt
          .addr NROT::xt
          .addr RtoP::xt
          .addr NROT::xt
          EXIT
eword

; Core 6.1.0400
dword     TWOOVER,"2OVER"
          ENTER
          .addr TWOPtoR::xt
          .addr TWODUP::xt
          .addr TWORtoP::xt
          .addr TWOSWAP::xt
          EXIT
eword

; Core 6.1.0370
dword     TWODROP,"2DROP"
          jsr   popay
          jsr   popay
          NEXT
eword

; Core 6.1.0250
dword     ZEROLT,"0<"
          jsr   popay
          and   #$80
          beq   :+
          lda   #$ff
:         tay
          PUSHNEXT          
eword

; Core ext 6.2.1485
dword     FALSE,"FALSE"
          lda   #$00
          tay
          PUSHNEXT
eword

; Core ext 6.2.2298
dword     TRUE,"TRUE"
          lda   #$ff
          tay
          PUSHNEXT
eword

; Core 6.1.0270
dword     ZEROQ,"0="
          jsr   popay             ; flags reflect A reg
          bne   FALSE::xt
          tya
          bne   FALSE::xt
          beq   TRUE::xt          ; always
eword

; Core ext 6.2.0280
dword     ZEROGT,"0>"
          jsr   popay
          bmi   FALSE::xt
          bne   TRUE::xt
          tya
          bne   TRUE::xt
          PUSHNEXT                ; 0 if we got here
eword

; Core 6.1.0530
dword     EQUAL,"="
          jsr   _cmpcom
          bne   FALSE::xt
          cpy   WR
          bne   FALSE::xt
          beq   TRUE::xt
eword

; Core 6.1.2340
dword     ULT,"U<"
          jsr   _cmpcom
          bcc   TRUE::xt
          bne   FALSE::xt
          cpy   WR
          bcc   TRUE::xt
          bcs   FALSE::xt
eword

; Core ext 6.2.2350
dword     UGT,"U>"
          jsr   _cmpcom
          bcc   FALSE::xt
          bne   TRUE::xt
          cpy   WR
          beq   FALSE::xt
          bcs   TRUE::xt
          bcc   FALSE::xt
eword

; Core 6.1.0480
dword     SLT,"<"
          jsr   _stest
          bcc   FALSE::xt
          bcs   TRUE::xt
eword

; Core 6.1.0540
dword     SGT,">"
          jsr   _stest
          beq   FALSE::xt
          bcc   FALSE::xt
          bcs   TRUE::xt        ; always
eword

; Common routines for comparisons, appearing after them
; so that we can use relative branches
; all the unsigned comparisons begin this way
; ( u1 u2 -- )
.proc     _cmpcom
          jsr   popwr           ; u2 to WR
          jsr   popay           ; u1 to AY
          cmp   WR+1            ; compare u1h to A
          rts
.endproc

; ( n1 n2 -- ) 16 bit signed comparison
; C and Z flags reflect the same comparison results as the 8-bit
; CMP instruction (Z means equal, C means >=
.proc     _stest
          jsr   popxr
          jsr   popwr
          lda   WR+1
          eor   XR+1
          bpl   same          ; same-sign compare, good to go
          lda   WR            ; otherwise do unsigned compare
          cmp   XR            ; and note that opposite-signed #s can't be equal
          lda   WR+1
          sbc   XR+1
          bvs   :+
          eor   #$80
:         sec                 ; Make sure Z flag is cleared
          rol                 ; move comparison result into carry
          rts          
same:     lda   WR+1
          cmp   XR+1
          bcc   done          ; if less than or not equal, done
          bne   done
          lda   WR
          cmp   XR
done:     rts  
.endproc

; Core 6.1.1650
; ( -- w )
dword     HERE,"HERE"
          lda   CHERE+1
          ldy   CHERE
          PUSHNEXT
eword

; non-standard
; ( -- w )
dword     LAST,"LAST"
          lda   DLAST+1
          ldy   DLAST
          PUSHNEXT
eword

dvar      OLDHERE,"OLDHERE",0

; Core 6.1.1380
dword     DEXIT,"EXIT",F_CONLY
          jmp   exit_next
eword

; _IF <falsejump> truecode
; headerless word compiled by IF
; jumps if the top of stack is false, otherwise
; skips jump addr and continues execution
hword     _IF,"_IF"
          jsr   popay             ; flags represent A reg
          bne   :+
          tya
          bne   :+
          jmp   _JUMP::xt
:         jmp   _SKIP::xt
eword

; _IFFALSE <truejump> falsecode
; jumps if the top of stack is truthy, otherwise
; skips jump addr and continues execution
hword     _IFFALSE,"_IFFALSE"
          jsr   popay             ; flags represent A reg
          bne   :+
          tya
          bne   :+
          jmp   _SKIP::xt
:         jmp   _JUMP::xt
eword
          
; Core 6.1.1700
dword     IF,"IF",F_IMMED|F_CONLY
          ENTER
          .addr COMP_LIT::xt
          .addr _IF::xt               ; compile _IF
          .addr HERE::xt              ; save to resolve later
          .addr COMP_LIT::xt
          .addr controlmm             ; compile unresolved
          EXIT
eword

; Core 6.1.1310
; ( orig1 -- orig2 )
dword     ELSE,"ELSE",F_IMMED|F_CONLY
          ENTER
          .addr COMP_LIT::xt
          .addr _JUMP::xt             ; compile JUMP
          .addr HERE::xt              ; (o1 -- o1 o2 )
          .addr COMP_LIT::xt
          .addr controlmm             ; compile unresolved
          .addr SWAP::xt              ; (o1 o2 -- o2 o1 )
          .addr HERE::xt              ; (o2 o1 -- o2 o1 addr )
          .addr SWAP::xt              ; (o2 o1 addr -- o2 addr o1 )
          .addr STORE::xt             ; (o2 o1 addr -- o2 ) resolve IF
          EXIT
eword

; Core 6.1.2270
; (orig -- )
dword     THEN,"THEN",F_IMMED|F_CONLY
          ENTER
          .addr HERE::xt              ; ( o1 -- o1 addr )
          .addr SWAP::xt              ; ( o1 addr -- addr o1 )
          .addr STORE::xt             ; ( o1 addr -- )
          EXIT
eword

; Core 6.1.0760
dword     BEGIN,"BEGIN",F_IMMED|F_CONLY
          ENTER
          .addr HERE::xt
          EXIT
eword

; Core 6.1.2430
; ( C: dest -- orig dest )
dword     WHILE,"WHILE",F_IMMED|F_CONLY
          ENTER
          .addr COMP_LIT::xt          ; compile IF
          .addr  _IF::xt
          .addr HERE::xt              ; orig = new unresolved
          .addr SWAP::xt              ; underneath top
          .addr COMP_LIT::xt          ; compile unresolved
          .addr controlmm
          EXIT
eword

; Core 6.1.2390

dword     UNTIL,"UNTIL",F_IMMED|F_CONLY
          ENTER
          .addr COMP_LIT::xt
          .addr _IF::xt               ; compile
          .addr COMMA::xt             ; compile false branch destination
          EXIT
eword

; Core 6.1.2140
; ( C: orig dest -- )
dword     REPEAT,"REPEAT",F_IMMED|F_CONLY
          ENTER
          .addr COMP_LIT::xt
          .addr _JUMP::xt
          .addr COMMA::xt       ; compile _JUMP dest
          .addr HERE::xt        ; ( C: orig -- orig here )
          .addr SWAP::xt        ; ( ... -- here orig )
          .addr STORE::xt       ; resolve orig
          EXIT
eword

; Core ext 6.2.0700
dword     AGAIN,"AGAIN",F_IMMED|F_CONLY
          ENTER
          .addr COMP_LIT::xt
          .addr _JUMP::xt
          .addr COMMA::xt
          EXIT
eword

; Core 6.1.0750
; really a variable, but the address of the var is constant
dconst    BASE,"BASE",ZBASE

; Core ext 6.2.1660
dword     HEX,"HEX"
          ldy   #<16
          lda   #>16
          sty   ZBASE
          sta   ZBASE+1
          NEXT
eword

; Core 6.1.1170
dword     DECIMAL,"DECIMAL"
          ldy   #<10
          lda   #>10
          sty   ZBASE
          sta   ZBASE+1
          NEXT
eword

.if 0
; non-standard
dword     OCTAL,"OCTAL"
          ldy   #<8
          lda   #>8
          sty   ZBASE
          sta   ZBASE+1
          NEXT
eword
.endif

.if 0
; non-standard
dword     BINARY,"BINARY"
          ldy   #<2
          lda   #>2
          sty   ZBASE
          sta   ZBASE+1
          NEXT
eword
.endif

.proc     _invertay
          pha
          tya
          eor   #$FF
          tay
          pla
          eor   #$FF
          rts
.endproc

; Core 6.1.1720
; optimized for space
dword     INVERT,"INVERT"
          jsr   popay
          jsr   _invertay
          PUSHNEXT
eword

.proc     _negateay
          pha
          tya
          eor   #$FF
          clc
          adc   #$01
          tay
          pla
          eor   #$FF
          adc   #$00
          rts
.endproc

; Core 6.1.1910
; optimized for space
dword     NEGATE,"NEGATE"
          jsr   popay
          jsr   _negateay
          PUSHNEXT
eword

; Non-standard
; ( d f -- d' ) if f < 0 then negate
hword     QNEGATE,"?NEGATE"
          jsr   popay
          and   #$80
          beq   :+
          jmp   NEGATE::xt
:         NEXT
eword

; Core 6.1.0690
dword     ABS,"ABS"
          lda   PSTKH-1,x
          bmi   NEGATE::xt
          NEXT
eword

; Double-Number 8.6.1.1230
dword     DNEGATE,"DNEGATE"
          jsr   popay               ; high cell
          pha
          tya
          pha
          jsr   popay               ; low cell
          jsr   _negateay
          php
          jsr   pushay
          plp
          pla
          eor   #$FF
          adc   #$00
          tay
          pla
          eor   #$FF
          adc   #$00
          PUSHNEXT
eword

; Double-Number 6.1.0690
dword     DABS,"DABS"
          lda   PSTKH-1,x
          bmi   DNEGATE::xt
          NEXT
eword

          
; Core 6.1.0290
dword     INCR,"1+"
          cpx   #$01
          bcc   stku2
          inc   PSTKL-1,x
          bne   :+
          inc   PSTKH-1,x
:         NEXT
stku2:    jmp   stku_err
eword
stku2 = INCR::stku2

; Core 6.1.0300
dword     DECR,"1-"
          cpx   #$01
          bcc   stku2
          lda   PSTKL-1,x
          bne   :+
          dec   PSTKH-1,x
:         dec   PSTKL-1,x
          NEXT
eword

.proc     m2parm
          cpx   #$02
          bcc   stku2
          dex
          lda   PSTKL-1,x
          rts
.endproc

; Core 6.1.0120
; would be faster if we could have the stack on the ZP...
dword     PLUS,"+"
          jsr   m2parm
          clc
          adc   PSTKL,x
          sta   PSTKL-1,x
          lda   PSTKH-1,x
          adc   PSTKH,x
          sta   PSTKH-1,x
          NEXT
eword

; Core 6.1.0160
dword     MINUS,"-"
          jsr   m2parm
          sec
          sbc   PSTKL,x
          sta   PSTKL-1,x
          lda   PSTKH-1,x
          sbc   PSTKH,x
          sta   PSTKH-1,x
          NEXT
eword

; Core 6.1.1130
dword     PSTORE,"+!"
          ENTER
          .addr DUP::xt
          .addr FETCH::xt
          .addr ROT::xt
          .addr PLUS::xt
          .addr SWAP::xt
          .addr STORE::xt
          EXIT
eword

; (n1 n2 -- ) n2->YR n1->ZR
.proc     _setup2
          jsr   popay
          sta   YR+1
          sty   YR
          jsr   popay
          sta   ZR+1
          sty   ZR
          rts
.endproc

; (n1 n2 n3 -- ) n3->YR n2(hw)->ZR n1(lw)->ZACC
.proc     _setup3
          jsr   _setup2
          jsr   popay
          sta   ZACC+1
          sty   ZACC
          rts
.endproc

; (n1 n2 -- ) n2->abs->YR n1->abs->ZR
; for division, divisor (remainder) sign stored in dsign
; result sign stored in rsign
.proc     _setup2_signed
          jsr   popay
          sta   rsign
          sta   dsign             ; divisor sign for symmetric division
          bpl   :+                ; popay sets sign correctly
          jsr   _negateay
:         sta   YR+1
          sty   YR
          jsr   popay
          sta   fsign
          pha
          eor   rsign             ; compute result sign
          sta   rsign
          pla
          bpl   :+
          jsr   _negateay
:         sta   ZR+1
          sty   ZR
          rts
rsign:    .byte $00               ; result sign
dsign:    .byte $00               ; dividend sign
fsign:    .byte $00               ; divisor sign
.endproc

.proc     _multcommon
          jsr   _setup2
nosetup:  txa
          pha
          jsr   _umult
          pla
          tax
          rts
.endproc

.proc     _smultcommon
          jsr   _setup2_signed
          jmp   _multcommon::nosetup
.endproc

; Core 6.1.2360
dword     UMMULT,"UM*"
          jsr   _multcommon
push:     lda   ZACC+1
          ldy   ZACC
          jsr   pushay
          lda   ZACC+3
          ldy   ZACC+2
          PUSHNEXT
eword

; Core 6.1.1810
dword     MMULT,"M*"
          jsr   _smultcommon
          bit   _setup2_signed::rsign
          bpl   UMMULT::push        ; just push if result not negative
          lda   ZACC+1
          ldy   ZACC
          jsr   _negateay           ; negate the low word
          php                       ; and save carry
          jsr   pushay
          plp                       ; restore carry
          lda   ZACC+2              ; negate high word
          eor   #$FF
          adc   #$00
          tay
          lda   ZACC+3
          eor   #$FF
          adc   #$00
          PUSHNEXT 
eword

; Core 6.1.0090
dword     MULT,"*"
          ENTER
          .addr MMULT::xt
          .addr DROP::xt
          EXIT
eword

.proc     _divcommon
          jsr   _setup2
signed:   lda   YR
          ora   YR+1
          bne   :+
divzero:  ldy   #<-10
          lda   #>-10
          jmp   _throway
:         txa
          pha
          jsr   _udiv
          pla
          tax
          rts
.endproc

.proc     _udivmod
          jsr   _divcommon
push:     lda   ZACC1+1           ; remainder
          ldy   ZACC1
          jsr   pushay
quot:     lda   ZR+1              ; quotient
          ldy   ZR
          jsr   pushay
          rts
.endproc

; Core 6.1.2370
dword     UMDIVMOD,"UM/MOD"
          jsr   _setup3
          lda   YR
          ora   YR+1
          beq   _divcommon::divzero
          txa
          pha
          jsr   _umdiv
          pla
          tax
          lda   ZR+1
          ldy   ZR
          jsr   pushay
          lda   ZACC+1
          ldy   ZACC
          PUSHNEXT
eword

.proc     _sdivcommon
          jsr   _setup2_signed          
          jmp   _divcommon::signed ; go do signed division
.endproc

; non-standard, 16-bit toward-zero signed division
dword     SDIVREM,"S/REM"
          jsr   _sdivcommon
sames:    lda   ZACC+1                ; get remainder
          ldy   ZACC
          bit   _setup2_signed::rsign ; result sign
          bpl   :+
          jsr   _negateay
:         bit   _setup2_signed::dsign ; remainder sign
          bpl   :+
          jsr   _negateay
:         jsr   pushay
          lda   ZR+1
          ldy   ZR
          bit   _setup2_signed::rsign ; quotient sign
          bpl   :+
          jsr   _negateay
:         PUSHNEXT
eword

; non-standard, 16-bit floored signed division
dword     FDIVMOD,"F/MOD"
          jsr   _sdivcommon
          lda   _setup2_signed::rsign ; result sign
          bpl   SDIVREM::sames        ; if not negative
          lda   ZACC+1
          ldy   ZACC
          jsr   _invertay
          bit   _setup2_signed::fsign ; divisor sign = remainder sign
          bpl   :+                    ; already negative
          jsr   _negateay
:         jsr   pushay
          lda   ZR+1
          ldy   ZR
          jsr   _invertay
          PUSHNEXT
eword

; Core 6.1.0240
; implemented as resolved deferred word so that it may be changed
; from floored to symmetric
dword     DIVMOD,"/MOD"   
          jmp   FDIVMOD::xt
eword

; Core 6.1.1890
dword     MOD,"MOD"
          ENTER
          .addr DIVMOD::xt
          .addr DROP::xt
          EXIT
eword

; Core 6.1.0230
dword     DIV,"/"
          ENTER
          .addr DIVMOD::xt
          .addr SWAP::xt
          .addr DROP::xt
          EXIT
eword

.proc     logcom1
          jsr   popwr
          jsr   popay
          rts
.endproc

; Core 6.1.0720
dword     LAND,"AND"
          jsr   logcom1
          and   WR+1
          pha
          tya
          and   WR
com2:     tay
          pla
          PUSHNEXT
eword
logcom2 = LAND::com2

; Core 6.1.1980
dword     LOR,"OR"
          jsr   logcom1
          ora   WR+1
          pha
          tya
          ora   WR
          jmp   logcom2
eword

; Core 6.1.2450
dword     LXOR,"XOR"
          jsr   logcom1
          eor   WR+1
          pha
          tya
          eor   WR
          jmp   logcom2
eword

; Core 6.1.2214
dword     SMDIVREM,"SM/REM"
          ENTER
          .addr TWODUP::xt
          .addr LXOR::xt
          .addr PtoR::xt
          .addr OVER::xt
          .addr PtoR::xt
          .addr ABS::xt
          .addr PtoR::xt
          .addr DABS::xt
          .addr RtoP::xt
          .addr UMDIVMOD::xt
          .addr SWAP::xt
          .addr RtoP::xt
          .addr QNEGATE::xt
          .addr SWAP::xt
          .addr RtoP::xt
          .addr QNEGATE::xt
          EXIT
eword

hword     SIGNUM,"SIGNUM"
          ENTER
          .addr DUP::xt
          .addr ZEROLT::xt
          .addr SWAP::xt
          .addr ZEROGT::xt
          .addr MINUS::xt
          EXIT
eword

; Core 6.1.1561
dword     FMDIVMOD,"FM/MOD"
          ENTER
          .addr DUP::xt
          .addr PtoR::xt
          .addr SMDIVREM::xt
          .addr OVER::xt
          .addr SIGNUM::xt
          .addr RCOPY::xt
          .addr SIGNUM::xt
          .addr NEGATE::xt
          .addr EQUAL::xt
          .addr _IF::xt
          .addr _else
          .addr DECR::xt
          .addr SWAP::xt
          .addr RtoP::xt
          .addr PLUS::xt
          .addr SWAP::xt
          EXIT
_else:    .addr RDROP::xt
          EXIT
eword

; Non standard
; implemented as resolved deferred word so that it may be changed
; from floored to symmetric for derived words
dword     MDIVMOD,"M/MOD"
          jmp   FMDIVMOD::xt
eword

; Core 6.1.0110
dword     MULTDIVMOD,"*/MOD"
          ENTER
          .addr PtoR::xt
          .addr MMULT::xt
          .addr RtoP::xt
          .addr MDIVMOD::xt
          EXIT
eword

; Core 6.1.0100
dword     MULTDIV,"*/"
          ENTER
          .addr MULTDIVMOD::xt
          .addr NIP::xt
          EXIT
eword

; Davex
; read key ( c1 -- c2 )
; c1 = char to place under cursor
; c2 = key that is read
dword     XKEY,"XKEY"
          jsr   popay
          tya
          stx   SPTMP
          jsr   xrdkey
          and   #$7F
          ldx   SPTMP
          jsr   pusha
          NEXT
eword

; Core 6.1.1750
dword     KEY,"KEY"
          ENTER
          NLIT  ' '
          .addr XKEY::xt
          EXIT
eword

; Facility 10.6.1.1755
dword     KEYQ,"KEY?"
          lda   $C000
          and   #$80
          beq   :+
          lda   #$FF
:         tay
          PUSHNEXT
eword

; Facility 10.6.1.1755
dword     PAGE,"PAGE"
          lda   #AFF
          jsr   _emit
          NEXT
eword

; non-standard
dword     HTAB,"HTAB"
          jsr   popay
          sty   CH
          NEXT
eword

; non-standard
dword     VTAB,"VTAB"
          jsr   popay
          tya
          jsr   TabV          ; preserves x
          NEXT
eword

; Facility 10.6.1.
dword     ATXY,"AT-XY"
          ENTER
          .addr VTAB::xt
          .addr HTAB::xt
          EXIT
eword

; Non-standard in 2012, former standard
; note this is NOT a dconst because INBUF
; isn't set until run-time!
hword     TIB,"TIB"
          lda   INBUF+1
          ldy   INBUF
          PUSHNEXT
eword

; non-standard, current input buffer
hvar      CIB,"CIB",0

; Core ext 6.2.2218
dword     SOURCEID,"SOURCE-ID",0
          ENTER
          .addr dSOURCEID::xt
          .addr FETCH::xt
          EXIT
eword

; Core 6.1.0560
dvar      PIN,">IN",0

; Non-standard, # of chars in input buffer
hvar      NIN,"#IN",0

; Non-standard
; return false if there is no more input
; true if there is
hword     INQ,"IN?"
          ENTER
          .addr PIN::xt
          .addr FETCH::xt
          .addr NIN::xt
          .addr FETCH::xt
          .addr UGT::xt
          .addr ZEROQ::xt
          EXIT
eword

; Core 6.1.2216
; address & content length of source input buffer
dword     SOURCE,"SOURCE"
          ENTER
          .addr   CIB::xt
          .addr   FETCH::xt
          .addr   NIN::xt
          .addr   FETCH::xt
          EXIT
eword

; Non-standard
; Headerless helper to compute current input buffer char address
hword     INPTR,"INPTR"
          ENTER
          .addr PIN::xt
          .addr FETCH::xt
          .addr CIB::xt
          .addr FETCH::xt
          .addr PLUS::xt
          EXIT
eword

; Non-standard
; headerless helper to increment the input pointer
hword     INC_INPTR,"INPTR+"
          ENTER
          NLIT 1
          .addr PIN::xt
          .addr PSTORE::xt
          EXIT
eword

; Non-standard
; read current input ( -- c )
hword     GETCH,"GETCH"
          ENTER
          .addr INPTR::xt
          .addr CFETCH::xt
          .addr INC_INPTR::xt
          EXIT
eword

; Davex
; return redirect status
; ( -- f1 f2 ) -- f1 is input redirect status, f2 is output redirect status
dword     REDIRECTQ,"REDIRECT?"
          stx   SPTMP
          lda   #$00
          jsr   xredirect
          ldx   SPTMP
          pha
          and   #%01000000
          jsr   :+
          pla
          and   #%10000000
          jsr   :+
          NEXT
          beq   :+
          lda   #$FF
:         tay
          jsr   pushay
          rts
eword

; Non-standard helper to set input source to keyboard or redirect
hword     SETKBD,"STKBD"
          ENTER
          .addr TIB::xt
          .addr CIB::xt
          .addr STORE::xt
dokbd:    NLIT  0
doany:    .addr dSOURCEID::xt
          .addr STORE::xt
          EXIT
eword

; Davex
dconst    XMAXLEN,"MAXLEN",(maxlen)

; ( c-addr n1 -- n2 )
; get up to n1 chars from the user's keyboard into the buffer
; at c-addr, returning in n2 the # of characters accepted
; n1 should not be greater than MAXLEN
; since davex returns a counted string, we will convert it in situ
.proc     _accept
          trace "_accept"
          jsr   popay                 ; pop n1
          sty   XR                    ; save max length
          jsr   popwr                 ; pop c-addr
          lda   WR                    ; now use c-addr minus 1
          bne   :+
          dec   WR+1
:         dec   WR
          ldy   #$00
          lda   (WR),y                ; grab byte where length will go
          pha                         ; and save it
          stx   SPTMP                 ; save PSP
          lda   WR+1
          ldy   WR
          ldx   XR
          inx                         ; account for length byte
          jsr   xgetln2               ; AY=buffer, X=max length
          ldx   SPTMP                 ; restore PSP
          ldy   #$00                  ; now get returned length
          lda   (WR),y
          sta   XR                    ; and save it
          pla                         ; restore byte where length went
          sta   (WR),y
          lda   XR
          jmp   pusha
.endproc

; Core 6.1.0695
dword     ACCEPT,"ACCEPT"
          jsr   _accept
          NEXT
eword

hword     dREFILL,"$REFILL"
          lda   SOURCE_ID
          ora   SOURCE_ID+1
          beq   keyboard
          lda   SOURCE_ID+1
          cmp   #SRC_REDIR
          beq   keyboard              ; redirected simulates keyboard input
          cmp   #SRC_ARG
          beq   filearg
          ldy   #<-57
          lda   #>-57
          jmp   _throway              ; to be implemented later, potentially
filearg:  ldy   #$00
:         stx   SPTMP
          sty   YR
          lda   SOURCE_ID
          jsr   xfman_read
          ; jsr   _emit               ; uncomment for input echo
          ldy   YR
          ldx   SPTMP
          bcs   filerr
          and   #$7F
          cmp   #ACR
          beq   :+
          sta   (INBUF),y
          iny
          bne   :-                    ; go until 256 chars if no CR
          lda   #$01                  ; $0100
          bne   :++                   ; always
:         lda   #$00                  ; $00yy
:         jsr   pushay
          jmp   accepted              ; accepted, go ahead
filerr:   cmp   #$4C
          bne   noteof
          cpy   #$00
          bne   accepted              ; got some chars before EOF, go interpret
          tya
          jsr   pushay                ; FALSE onto stack
          jmp   SETKBD::xt            ; and switch to keyboard input
noteof:   jmp   _throwp8              ; throw ProDOS error
keyboard: stx   SPTMP                 ; set source ID to reflect keyboard
          lda   #$00
          jsr   xredirect             ; or redirection depending on status
          ldx   SPTMP                 ; of redirection
          and   #%01000000
          beq   :+
          lda   #SRC_REDIR
:         sta   SOURCE_ID+1
          lda   INBUF+1
          ldy   INBUF
          jsr   pushay
          lda   #maxlen
          jsr   pusha
          jsr   _accept               ; accept input
          jsr   peekay                ; get length
          tya                         ; into a
          beq   accepted              ; do nothing on empty buffer
          dey                         ; account for zero-based index
lp:       lda   (INBUF),y             ; mask off all high bits
          and   #$7F
          sta   (INBUF),y
          dey
          cpy   #$FF                  ; can't use minus or zero
          bne   lp
accepted: ENTER
          .addr NIN::xt               ; #IN
          .addr STORE::xt             ; write count
          NLIT  0                     ; now reset >IN
          .addr PIN::xt
          .addr STORE::xt             ; 0 >IN !
          .addr TRUE::xt              ; and always return true
          EXIT
eword

; Core ext 6.2.2125
dword     REFILL,"REFILL"
          jmp   dREFILL::xt
eword

; make dictionary entry for word at WR, length in XR
; returns with position of new word in AY
.proc     _mkdict
          ldy   XR
          beq   badword
          cpy   #$10
          bcs   badword
          lda   CHERE+1               ; save HERE for return
          pha
          lda   CHERE
          pha
          lda   DLAST+1               ; get LAST word
          ldy   DLAST
          jsr   cworday               ; compile link
          lda   XR
          ora   #$80                  ; default flags+length
          jsr   cbytea
          ldy   #$00
:         cpy   XR
          beq   done
          lda   (WR),y
          jsr   _wconva               ; normalize to upper case
          sty   YR
          jsr   cbytea                ; compile byte (wrecks Y)
          ldy   YR
          iny
          bne   :-
done:     pla                         ; get old HERE
          tay
          pla
          rts
badword:  ldy   #<-19                 ; definition name too long
          lda   #>-19
          jmp   _throway
.endproc

; Convert to upper case
.proc     _wconva
          and   #$7F
          cmp   #'z'+1                ; upper case conversion
          bcs   :+                    ; not standard...
          cmp   #'a'
          bcc   :+
          and   #$DF
:         rts
.endproc

; search dictionary for word at WR, length in XR
; if found, AY != 0 and carry set
; otherwise carry clear and AY=0
.proc     _search
          trace "_search"
          lda   DLAST+1               ; TODO: move this out if search order
          ldy   DLAST                 ; words are implemented
olp:      sta   ZR+1
          sty   ZR
          ora   ZR
          beq   notfnd
          ldy   #$02                  ; offset of len+flags
          lda   (ZR),y
          and   #F_SMUDG              ; see if smudged (invisible)
          bne   snext
          lda   (ZR),y                ; otherwise next...
          and   #$0F                  ; mask in length
          cmp   XR
          bne   snext
          lda   ZR
          clc
          adc   #$03                  ; offset to name start
          sta   chkchr
          lda   ZR+1
          adc   #$00
          sta   chkchr+1
          ldy   XR
          dey                         ; from 1-based to 0-based
ilp:      lda   (WR),y
          jsr   _wconva               ; normalize (non-standard)
          cmp   *,y                   ; self-modified
chkchr = * - 2
          bne   snext                 ; nope
          dey
          bpl   ilp
          sec                         ; loop end, found it!
done:     lda   ZR+1
          ldy   ZR
          rts
notfnd:   clc
          bcc   done
snext:    ldy   #$01                  ; get pointer to next word
          lda   (ZR),y                ; into AX
          pha
          dey
          lda   (ZR),y
          tay
          pla
          jmp   olp
.endproc

; non-standard
hword     DSEARCH,"$SEARCH"
          jsr   popxr
          jsr   popwr
          lda   XR+1
          eor   XR
          beq   none
          jsr   _search
          bcs   :+
none:     lda   #$00
          tay
:         PUSHNEXT
eword

; with word head in AY
; find code address and put in AY
; set S and V flags to reflect immediate and compile-only flags
; return carry set always
.proc     _code
          sta   ldlen+1
          tya
          clc
          adc   #$02
          sta   ldlen
          bcc   :+
          inc   ldlen+1
:         lda   *                     ; self-modified
ldlen = * - 2
          sta   flags
          and   #$0F                  ; mask length
          sec                         ; extra one byte to line up
          adc   ldlen                 ; add back into low byte
          tay
          lda   ldlen+1
          adc   #$00
          asl   flags
          bit   flags
          sec
          rts
flags:    .byte $00
.endproc

; Non-standard
hword     DFLAGS,"$FLAGS"
          ENTER
          .addr DUP::xt
          .addr ZEROQ::xt
          .addr _IF::xt
          .addr ok
          EXIT
ok:       NLIT  2
          .addr PLUS::xt
          EXIT
eword

; Non-standard
hword     DXT,"$XT"
          jsr   popay
          cmp   #$00
          bne   :+
          cpy   #$00
          beq   done
:         jsr   _code
done:     PUSHNEXT          
eword

.proc     searcherr
          ldy   #<-13                 ; undefined word
          lda   #>-13
          jmp   _throway
.endproc

; Core 6.1.0550
dword     rBODY,">BODY"
          jsr   popwr
          clc
jmponly:  ldy   #$00
          lda   (WR),y
          bcs   ckjmp
          ldy   #$03
          cmp   #opJSR
          beq   :+
ckjmp:    ldy   #$01
          cmp   #opJMP
          beq   :+
          ldy   #<-31           ; not a word for which a body may be found
          lda   #>-31
          jmp   _throway
:         tya
          clc
          adc   WR
          tay
          lda   WR+1
          adc   #$00
          PUSHNEXT
eword
; headerless get body of JMP only
hword     _rJMP,">JMP"
          jsr   popwr
          sec
          bcs   rBODY::jmponly
eword

.proc     _cold
          trace "_cold"
          lda   #<BYE::xt
          sta   IP
          lda   #>BYE::xt
          sta   IP+1
          lda   #$00
          ldx   #$17              ; np ZP,y
:         sta   xczpage,x         ; clear system stuff
          dex
          bpl   :-
          lda   #10
          sta   ZBASE
          lda   IHERE+1
          sta   CHERE+1
          lda   IHERE
          sta   CHERE
          lda   #<ELAST
          sta   DLAST
          lda   #>ELAST
          sta   DLAST+1
          lda   #<X_DX_LOAD
          sta   HIMEM
          lda   #>X_DX_LOAD
          sta   HIMEM+1
          dec   HIMEM+1
          ldy   #$FE
          tya
          sta   (HIMEM),y
          iny
          sta   (HIMEM),y
          inc   HIMEM+1
          lda   SOURCE_ID+1
          cmp   #SRC_ARG
          bne   :+
          jmp   FQUIT_xt
; greetings!
:         lda   #$00
          jsr   xredirect         ; determine if I/O is redirected
          and   #%11000000        ; mask in bits
          bne   abort             ; and skip greeting if redirected
          jsr   xmess
          .byte "MG's Davex Forth ",$00
          lda   version
          jsr   xprint_ver
          jsr   xmess
          .byte $8D,$00
; non-exception abort
abort:    ldx   #$00              ; init data stack pointer
          jmp   QUIT_xt
.endproc
_abort = _cold::abort

; see if last word needs forgetting due to exception
; in the middle of defining it
.proc     _patch
          trace "_patch"
          lda   DLAST             ; see if last word needs forgetting
          sta   WR
          lda   DLAST+1
          sta   WR+1
          ldy   #$02
          lda   (WR),y            ; get smudge bit
chk = * - 2
          and   #F_SMUDG          ; mask off smudge flag
          bne   :+                ; fix up if smudged
          rts                     ; otherwise do nothing
:         ldy   #$01
          lda   (WR),y
          sta   DLAST+1
          dey
          lda   (WR),y
          sta   DLAST
          lda   OLDHERE::val+1
          sta   CHERE+1
          lda   OLDHERE::val
          sta   CHERE
          rts
.endproc

; non-standard helper to return address of WORD buffer, which
; starts at 16 past HERE
hword     WORDBUF,"WORDBUF"
          ENTER
          .addr HERE::xt
          NLIT  16
          .addr PLUS::xt
          EXIT
eword

; Core ext 6.2.2000
; PAD is immediately after WORDBUF
dword     PAD,"PAD"
          ENTER
          .addr WORDBUF::xt
          NLIT  WORD_SIZE
          .addr PLUS::xt
          EXIT
eword

; Core 6.1.2070
dword     RFETCH,"R@"
          stx   SPTMP
          tsx
          pla
          sta   WR
          pla
          txs
          ldx   SPTMP
          ldy   WR
          PUSHNEXT
eword

; Core 6.1.2170
dword     StoD,"S>D"
          jsr   peekay
          and   #$80
          beq   :+
          lda   #$FF
:         tay
          jsr   pushay
          NEXT
eword

; non-standard
; ( x1 x2 -- d1 d2 )
dword     TWOStoD,"2S>D"
          ENTER
          .addr PtoR::xt
          .addr StoD::xt
          .addr RtoP::xt
          .addr StoD::xt
          EXIT
eword

; Core 6.1.0770
dword     BL,"BL"
          lda   #' '
          jsr   pusha
          NEXT
eword

; Core 6.1.2220
dword     SPACE,"SPACE"
          lda   #' '              ; asm version 1 byte shorter
          jsr   _emit
          NEXT
eword

; Core 6.1.0990
dword     CR,"CR"
          lda   #ACR              ; asm version 1 byte shorter
          jsr   _emit
          NEXT
eword

; helper to convert digit to char
.proc     _tochar
          clc
          adc   #'0'
          cmp   #'9'+1
          bcc   :+
          adc   #6
          cmp   #'Z'
          bcc   :+
          adc   #6
:         rts
.endproc

; routine to convert char to digit
.proc     _todigit
          pha
          lda   ZBASE               ; cheating, no high byte!
          cmp   #36
          pla
          bcs   :+
          jsr   _wconva
          sec
:         sbc   #'0'                ; 0-9 conversion
          bmi   bad
          cmp   #10                 ; if less than 10 we are good
          bcc   good
          sbc   #7                  ; A-Z conversion
          bmi   bad
          cmp   #37
          bcc   good                ; good if less than 37
          sbc   #7                  ; a-z conversion
          bmi   bad
good:     sec
          rts
bad:      clc
          rts
.endproc

; routine to convert a number at WR len XR
; start by initializing current number to 0
; then for each digit left-to-right, multiply the number 
; by the current radix in BASE and adding the digit
; return carry set if conversion was successful
; and number in AY
.proc     _parsenum
          trace "_parsenum"
          clc
          ror   mflag
          ldy   #$00                ; clear y and use it to
          sty   YR                  ; init 2nd multiplicand
          sty   YR+1                ; which also accumulates total
pnum2:    stx   SPTMP               ; data SP gonna get trashed
          lda   ZBASE
          sta   ZR                  ; ZR undisturbed by multiply
          lda   ZBASE+1             ; so first multiplicand will be base
          sta   ZR+1                
          ;ldy   #$00
          sty   nflag
lp:       sty   XR+1                ; save Y
          jsr   _umult              ; since _umult kills it
          lda   ZACC                ; copy results to 2nd multiplicand
          sta   YR
          lda   ZACC+1
          sta   YR+1
          ldy   XR+1                ; get Y back
          lda   (WR),y              ; now grab a char to convert
          and   #$7F                ; strip high bit
          cmp   #'-'
          beq   minus
          jsr   _todigit            ; convert to digit
          bcc   bad
          cmp   ZBASE               ; make sure smaller than base
          bcs   bad                 ; (cheating by not checking high byte)
          clc                       ; now add to accumulating value
          adc   YR
          sta   YR
          bcc   :+
          inc   YR+1
:         iny                       ; count # of digits processed
          sty   XR+1                ; and save count for interested parties
          cpy   XR                  ; and see if we are done
          bcc   lp                  ; (if not, keep going)
done:     lda   YR+1                ; and return the #
          ldy   YR
          ldx   SPTMP
          bit   nflag
          bpl   :+
          jsr   _negateay
:         sec
          rts
bad:      bit   mflag
          bmi   done
          ldx   SPTMP
          clc
          rts
minus:    bit   mflag
          bmi   bad
          cpy   #$00
          bne   bad
          ror   nflag               ; carry is set
          iny
          bne   lp
nflag:    .byte $00
mflag:    .byte $00
.endproc

; Core 6.1.0570
dword     GNUMBER,">NUMBER"
          jsr   popxr
          jsr   popwr
          jsr   popay
          sta   ZR+1
          sty   ZR
          ldy   #$00
          sec
          ror   _parsenum::mflag
          jsr   _parsenum::pnum2
          jsr   pushay
          lda   WR
          clc
          adc   XR+1
          tay
          lda   WR+1
          adc   #$00
          jsr   pushay
          lda   XR
          sec
          sbc   XR+1
          jsr   pusha
          NEXT
eword

; backing variable for pictured numeric output
hvar      dPPTR,"$PPTR",0

; Core 6.1.0490
dword     PBEGIN,"<#"
          ENTER
          .addr WORDBUF::xt
          NLIT  WORD_SIZE
          .addr PLUS::xt
          .addr dPPTR::xt
          .addr STORE::xt
          EXIT
eword

; Core 6.1.1670
; ( c -- ), put c into pictured numeric output buffer
dword     PHOLD,"HOLD"
          ENTER
          .addr dPPTR::xt         ; Current pictured output pointer var
          .addr FETCH::xt         ; get the saved address
          .addr DECR::xt          ; move to next lower address
          .addr DUP::xt           ; make a second copy
          .addr dPPTR::xt
          .addr STORE::xt         ; write back to pointer var
          .addr CSTORE::xt        ; write character to location
          EXIT 
eword

; Core 6.1.2210
dword     PSIGN,"SIGN"
          jsr   popay
          and   #$80
          beq   :+
          lda   #'-'
          jsr   pusha
          jmp   PHOLD::xt
:         NEXT
eword

; non-standard, unsigned divide 32-bit by 16-bit
; leaving 32-bit quotient and 16-bit remainder
; ( ud u -- u-rem ud-quot )
; borrowed from sixtyforth
dword     UMLDIVMOD,"UML/MOD"
          ENTER
          .addr PtoR::xt
          .addr RCOPY::xt
          NLIT 0
          .addr SWAP::xt
          .addr UMDIVMOD::xt
          .addr RtoP::xt
          .addr SWAP::xt
          .addr PtoR::xt
          .addr UMDIVMOD::xt
          .addr RtoP::xt
          EXIT
eword

; Core 6.1.0030
dword     PNUM,"#"
          ENTER
          .addr BASE::xt
          .addr FETCH::xt
          .addr UMLDIVMOD::xt   ; divide by BASE
          .addr ROT::xt         ; put remainder in front
          CODE
          jsr   popay           ; get remainder
          tya                   ; only low byte is practical
          jsr   _tochar         ; convert to ASCII
          jsr   pusha           ; and back onto stack
          jmp   PHOLD::xt       ; then put in output buffer
eword

; Core 6.1.0050
dword     PNUMS,"#S"
          ENTER
another:  .addr PNUM::xt
          .addr TWODUP::xt
          .addr LOR::xt
          .addr _IFFALSE::xt      ; is zero?
          .addr another           ; nope, do another digit
          EXIT
eword

; Core 6.1.0040
; ( xd -- c-addr u )
dword     PDONE,"#>"
          ENTER
          .addr TWODROP::xt       ; drop remaining quotient
getstr:   .addr dPPTR::xt         ; c-addr
          .addr FETCH::xt
          .addr WORDBUF::xt       ; now compute u
          NLIT  WORD_SIZE
          .addr PLUS::xt
          .addr dPPTR::xt
          .addr FETCH::xt
          .addr MINUS::xt
          EXIT
eword

; general number formatter for standard number output words
; it's slow, but it does it all and saves space with the words that follow
; ( d u1 f -- c-addr u2 ) u1 = field size f = true if signed output desired
hword     DFMT,"DFMT"
          ENTER
          .addr SWAP::xt
          .addr PtoR::xt
          .addr PBEGIN::xt
          .addr _IF::xt             ; check f
          .addr us1                 ; unsigned if f is false
          .addr DUP::xt             ; duplicate cell with sign
          .addr NROT::xt            ; and put it behind d
          .addr DABS::xt
          .addr _SKIP2::xt          ; skip next 2 words
us1:      NLIT  0                   ; no sign printed
          .addr NROT::xt
          .addr PNUMS::xt           ; perform conversion
          .addr ROT::xt             ; get sign back to front
          .addr PSIGN::xt           ; add sign if needed
          .addr PDONE::xt
          .addr RtoP::xt            ; get field size back
          .addr MINUS::xt           ; if less than 0, have to add blanks
lp:       .addr DUP::xt
          .addr ZEROLT::xt          ; is less than 0?
          .addr _IF::xt
          .addr fielddn             ; nope, done
          .addr INCR::xt            ; increment
          .addr BL::xt
          .addr PHOLD::xt           ; hold a blank
          .addr _JUMP::xt           ; and go back to lp
          .addr lp
fielddn:  .addr TWODROP::xt         ; drop c-addr and leftovers
          .addr _JUMP::xt
          .addr PDONE::getstr       ; and return c-addr u for result
eword

; Double-number 8.6.1070
dword     DDOTR,"D.R"
          ENTER
dosdotr:  NLIT 1                    ; signed
dodotr:   .addr DFMT::xt
          .addr TYPE::xt
          EXIT
eword

; Double-number 8.6.1060
dword     DDOT,"D."
          ENTER
          NLIT  0                   ; field size
          .addr DDOTR::xt
          .addr SPACE::xt
          EXIT
eword

; Core ext 6.2.2330
dword     UDOTR,"U.R"
          ENTER
          .addr PtoR::xt
          NLIT  0                   ; unsigned S>D
          .addr RtoP::xt
          NLIT  0                   ; want unsigned
          .addr _JUMP::xt
          .addr DDOTR::dodotr
eword

; Core ext 6.2.0210
dword     DOTR,".R"
          ENTER
          .addr PtoR::xt
          .addr StoD::xt
          .addr RtoP::xt
          .addr _JUMP::xt
          .addr DDOTR::dosdotr
eword

; Core 6.1.2320
dword     UDOT,"U."
.if 0
          ; faster
          ENTER
          NLIT  0                   ; unsigned S>D
          .addr PBEGIN::xt
          .addr PNUMS::xt
          .addr PDONE::xt
          .addr TYPE::xt
          .addr SPACE::xt
          EXIT
.else
          ; smaller
          ENTER
          NLIT  0
          .addr UDOTR::xt
          .addr SPACE::xt
          EXIT
.endif
eword

; Core 6.1.0180
dword     DOT,"."
.if 0
          ; faster
          ENTER
          .addr StoD::xt
          .addr _IF::xt
          .addr pos
          NLIT  '-'
          .addr EMIT::xt
          .addr ABS::xt
pos:      .addr UDOT::xt
          EXIT
.else
          ; smaller
          ENTER
          NLIT  0
          .addr DOTR::xt
          .addr SPACE::xt
          EXIT
.endif
eword

; Core 6.1.1900
dword     MOVE,"MOVE"
          jsr   _swap
          jsr   popay
          sta   YR+1
          sty   YR
          ldy   #<func
          lda   #>func
          jsr   string_op_ay
          NEXT
func:     sta   (YR),y
          inc   YR
          bne   :+
          inc   YR+1
:         rts
eword

; non-standard
; ( c-addr1 u c-addr2 -- ) place string at (c-addr1,u) in counted form
; at c-addr 2
dword     PLACE,"PLACE"
          ENTER
          .addr TWODUP::xt
          .addr STORE::xt
          .addr INCR::xt
          .addr SWAP::xt
          .addr MOVE::xt
          EXIT
eword

; Davex
dword     PRP8ERR,".P8_ERR"
          jsr   popay
          tya
          stx   SPTMP
          jsr   xProDOS_er
          ldx   SPTMP
          NEXT
eword

.proc     _message
          jsr   peekay
          cmp   #P8_ER_RNG
          beq   PRP8ERR::xt
          stx   SPTMP
          jsr   xmess
          .byte "Msg #",$00
          ldx   SPTMP
          ENTER
          .addr DOT::xt
          EXIT
.endproc

; Non-standard
; This appears as a deferrable
; to be overridden by something more helpful later
dword     MESSAGE,"MESSAGE"
          jmp   _message
eword

; Exception 9.6.1.0875
; push exception stack frame onto stack and execute token
dword     CATCH,"CATCH"
          jsr   popwr         ; remove xt from stack
          inc   flag          ; flag catch active
          lda   IP+1          ; save IP on stack
          pha
          lda   IP
          pha
          lda   rstk          ; save old catch return stack if any
          pha
          txa                 ; save data stack pointer
          pha
          stx   SPTMP
          tsx                 ; save return stack pointer
          stx   rstk
          ldx   SPTMP
          lda   WR+1          ; put xt back on stack
          ldy   WR
          jsr   pushay
          ENTER
          .addr EXECUTE::xt
          CODE
          ; if we got here, no exception
          lda   #$00
          sta   WR
          sta   WR+1
          pla                 ; drop old data stack ptr
fixup:    pla
          sta   rstk
          pla
          sta   IP            ; restore previous IP
          pla
          sta   IP+1
          dec   flag
          lda   WR+1
          ldy   WR
          PUSHNEXT
flag:     .byte $00
rstk:     .byte $00
eword

; Exception 9.6.1.2275
dword     THROW,"THROW"
          jsr   peekay
          ora   #$00
          bne   :+
          tya
          bne   :+
          dex                 ; peek told us there was at least one item
          NEXT
:         jsr   popwr
ithrow:
.if TRACE
          txa
          pha
          lda   WR
          sta   TRADR
          lda   WR+1
          sta   TRADR+1
          jsr   xmess
          .byte "[THROW,",$00
          jsr   _dtrace
          lda   #']'
          jsr   _emit
          pla
          tax
.endif
          lda   CATCH::flag   ; see if active CATCH
          beq   uncaught
          ldx   CATCH::rstk   ; restore prior return stack ptr
          txs
          pla                 ; restore prior data stack ptr
          tax
          jmp   CATCH::fixup  ; now "return" from catch
uncaught: lda   #$FF
          cmp   WR+1
          bne   :+
          lda   WR
          cmp   #<-1
          beq   abort
          cmp   #<-2
          beq   abort
:         stx   SPTMP
          jsr   xmess
          .byte " Uncaught: ",$00
          ldx   SPTMP
          cpx   #PSTK_SZ-10   ; check space left in parameter stack
          bcc   :+            ; and reserve enough to handle the
          ldx   #PSTK_SZ-10   ; error if needed
:         lda   WR+1
          ldy   WR
          jsr   pushay
          ENTER
          .addr MESSAGE::xt
          CODE
          jmp   QUIT_xt
abort:    jmp   _abort
eword

; Throw an exceptiopn because of a ProDOS 8 error.
.proc     _throwp8
          tay
          lda   #P8_ER_RNG
          ; fall through to _throway
.endproc

; this word bypasses the stack ops and executes throw
; the contents of AX should *not* be zero
.proc     _throway
          sta   WR+1
          sty   WR
          jmp   THROW::ithrow
.endproc

; non-standard parse helper
hword     ISSPC,"ISSPACE?"
          ENTER
          .addr BL::xt
          .addr INCR::xt
          .addr ULT::xt
          EXIT
eword          

; non-standard parse helper
hword     ISNOTSPC,"ISNOTSPACE?"
          ENTER
          .addr ISSPC::xt
          .addr ZEROQ::xt
          EXIT
eword

; Core ext 6.2.2020
; ( "name" -- c-addr u )
dword     PARSE_NAME,"PARSE-NAME"
          ENTER
l1:       .addr INQ::xt           ; is there input?
          .addr _IF::xt           ; 
          .addr none              ; if not, just return empty-handed
          .addr GETCH::xt         ; get char ( -- c )
          .addr ISSPC::xt         ; is it a space? ( -- tf )
          .addr _IFFALSE::xt      ; or, rather if not ( tf -- )
          .addr l1                ; do loop if it is
          .addr INPTR::xt         ; ( -- c-addr )
          .addr DECR::xt          ; fixup because INPTR is 1 ahead now
          NLIT  0                 ; and we have 1 char ( -- c-addr u=1 )
l2:       .addr INQ::xt           ; is there input?
          .addr _IF::xt
          .addr e1                ; if not, exit
          .addr INCR::xt          ; ( c-addr u -- c-addr u=u+1 ) count non-spaces
          .addr GETCH::xt         ; ( c-addr u -- c-addr u c )
          .addr ISSPC::xt         ; ( c-addr u c -- c-addr u n )
          .addr _IF::xt           ; ( c-addr u n -- c-addr u tf )
          .addr l2                ; not a space, keep parsing
e1:       EXIT
none:     .addr INPTR::xt
          NLIT  0
          EXIT
eword

; Core ext 6.2.2020
dword     PARSE,"PARSE"
          ENTER
          .addr PtoR::xt          ; save delimeter
          .addr INPTR::xt         ; get current input address
          NLIT 0                  ; and start with a count of 0
l1:       .addr INQ::xt           ; is there input available?
          .addr _IF::xt
          .addr e1                ; false branch exits
          .addr GETCH::xt         ; get the next char
          .addr RCOPY::xt         ; and copy the delimiter from return stack
          .addr EQUAL::xt         ; is it the same char?
          .addr _IF::xt
          .addr i1                ; false branch increments count and continues loop
e1:       .addr RDROP::xt
          EXIT
i1:       .addr INCR::xt
          .addr _JUMP::xt
          .addr l1          
eword

; Core 6.1.2450
; ( char "<chars>ccc<char>" -- c-addr )
dword     WORD,"WORD"
          ENTER
          .addr PARSE::xt         ; ( char -- c-addr u ) parse the word
          .addr DUP::xt           ; dup count
          NLIT  WORD_SIZE         ; max size of word space
          .addr ULT::xt           ; unsigned <
          .addr _IF::xt           ; was it less than?
          .addr bad               ; nope, error
          .addr DUP::xt           ; dup length again
          .addr WORDBUF::xt       ; address of word buf
          .addr CSTORE::xt        ; store length
          .addr WORDBUF::xt       ; wordbuf again
          .addr INCR::xt          ; +1
          .addr SWAP::xt          ; make sure stack is ( c-addr u )
          .addr MOVE::xt          ; move the data
          .addr WORDBUF::xt       ; and put word buffer address on stack
          EXIT
bad:      NLIT  -18               ; "parsed string overflow"
          .ADDR THROW::xt         ; never returns
eword

; Core 6.1.0895
dword     CHAR,"CHAR"
          ENTER
          .addr PARSE_NAME::xt
          .addr DROP::xt
          .addr CFETCH::xt
          EXIT     
eword

; helper for words that must parse and find
; a dictionary entry
hword     PARSEFIND,"$WORD"
          ENTER
          .addr PARSE_NAME::xt
          .addr DSEARCH::xt
          .addr DUP::xt
          .addr _IF::xt
          .addr exc
          EXIT
exc:      .addr DROP::xt
          NLIT -13
          .addr THROW::xt
eword

; Core 6.1.2520
dword     CCHAR,"[CHAR]",F_CONLY|F_IMMED
          ENTER
          .addr CHAR::xt
          .addr COMMA::xt         ; compile fast literal
          EXIT
eword

; helper
.proc     _parsenametowrxr
          ENTER
          .addr PARSE_NAME::xt
          CODE
          jsr   popxr
          jmp   popwr
.endproc

; Core 6.1.0070
dword     FIND,"'"
          ENTER
          .addr PARSEFIND::xt
          .addr DXT::xt
          EXIT
eword

; Core 6.1.2510
dword     CFIND,"[']",F_CONLY|F_IMMED
          ENTER
          .addr FIND::xt          ; find xt
          .addr COMP_LIT::xt
          .addr LIT::xt           ; compile LIT
          .addr COMMA::xt         ; compile xt as literal
          EXIT
eword

; Headerless helper to make a new dictionary entry
hword     MKENTRY,"MKENTRY"
          ENTER
          .addr PARSE_NAME::xt
          .addr HERE::xt          ; if successfully parsed, set OLDHERE
          .addr OLDHERE::xt
          .addr STORE::xt
          CODE
          jsr   popxr
          jsr   popwr
          jsr   _mkdict
          sta   DLAST+1
          sty   DLAST
          NEXT
eword

; Core 6.1.1000
dword     CREATE,"CREATE"
          ENTER
          .addr MKENTRY::xt
          NLIT  opJSR
          .addr CCOMMA::xt
          .addr LIT::xt
          .addr pushda
          .addr COMMA::xt
          EXIT
eword

; Core ext 6.2.1173
dword     DEFER,"DEFER"
          ENTER
          .addr MKENTRY::xt
          NLIT  opJMP
          .addr CCOMMA::xt
          .addr LIT::xt
          .addr _undefined
          .addr COMMA::xt
          EXIT
eword

; Core ext 6.2.1177
dword     DEFERAT,"DEFER@"
          ENTER
          .addr _rJMP::xt
          .addr FETCH::xt
          EXIT
eword

; Core 6.1.2500
dword     STATEI,"[",F_CONLY|F_IMMED
          lda   #$00
          sta   ZSTATE
          sta   ZSTATE+1
          NEXT
eword

; Core 6.1.2540
dword     STATEC,"]"
          ldy   #$01
          sty   ZSTATE
          dey
          sty   ZSTATE+1
          NEXT
eword

; Core ext 6.1.1175
dword     DEFERSTO,"DEFER!"
          ENTER
          .addr _rJMP::xt
          .addr STORE::xt
          EXIT
eword

; Core 6.1.0450
dword     COLON,":"
          ENTER
          .addr MKENTRY::xt
          NLIT  opJSR
          .addr CCOMMA::xt
          .addr LIT::xt
          .addr enter
          .addr COMMA::xt
          .addr LAST::xt
          NLIT 2
          .addr PLUS::xt
          .addr DUP::xt
          .addr CFETCH::xt
          NLIT   F_SMUDG                ; smudge it
          .addr LOR::xt
          .addr SWAP::xt
          .addr CSTORE::xt
          .addr STATEC::xt
          EXIT
eword

; Core ext 6.2.0455
; compile an anonymous definition
dword     NONAME,":NONAME"
          ENTER
          .addr HERE::xt
          NLIT  opJSR
          .addr CCOMMA::xt
          .word LIT::xt
          .addr enter
          .addr COMMA::xt
          .addr STATEC::xt
          EXIT
eword

; Core 6.1.0460
dword     SEMI,";",F_IMMED|F_CONLY
          ENTER
          .addr COMP_LIT::xt
          .addr exit_next
dosemi:   .addr LAST::xt
          NLIT 2
          .addr PLUS::xt
          .addr DUP::xt
          .addr CFETCH::xt
          NLIT   F_SMUDG               ; unsmudge it
          .addr INVERT::xt
          .addr LAND::xt
          .addr SWAP::xt
          .addr CSTORE::xt
          .addr STATEI::xt
          EXIT
eword

; Headerless helper word for DOES> and ;CODE
hword     SEMIS,"SEMIS"
          ENTER
          .addr COMP_LIT::xt
          .addr exit_code
          .addr _JUMP
          .addr SEMI::dosemi
eword

; Core part of DOES> implementation
; modify the most recent CREATEed definition to jsr
; to the address immediately following whoever
; JSRed to this.
.proc     SDOES
          pla
          clc
          adc   #$01
          sta   ZR
          pla
          adc   #$00
          sta   ZR+1
          ldy   DLAST
          lda   DLAST+1
          jsr   _code
          sta   WR+1
          sty   WR
          ldy   #$00
          lda   (WR),y
          cmp   #$20
          bne   csmm
          iny
          lda   ZR
          sta   (WR),y
          iny
          lda   ZR+1
          sta   (WR),y
          NEXT
csmm:     ldy   #<-22             ; control structure mismatch
          lda   #>-22
          jmp   _throway
.endproc
controlmm = SDOES::csmm

; Core 6.1.1250
; DOES> is... complicated
; when a colon def compiles DOES>, the DOES> closes the definition
; with semis and compiles the following to the word:
; jsr SDOES ( see above )
; jsr ENTER
; RPLUCK INCR
; and then goes back into compile mode until ;
; this has the effect that when the word containing DOES> is executed
; it replaces the effect of the most recently-defined word (provided it was
; created by CREATE) with new effects, namely the word will push
; it's data address onto the stack and execute the code following DOES>
; e.g. : MKARRAY CREATE CELLS ALLOT DOES> SWAP CELLS + ;
; 2 MKARRAY FOO -> OK
; 0 FOO U. -> 35120 OK
; 1 FOO U. -> 35122 OK
dword     DOES,"DOES>",F_CONLY|F_IMMED
          ENTER
          .addr SEMIS::xt       ; close current definition for code
          .addr COMP_CLIT::xt
          .byte opJSR           ; C: jsr
          .addr COMP_LIT::xt
          .addr SDOES           ; C: (jsr) SDOES
          .addr COMP_CLIT::xt
          .byte opJSR           ; C: jsr
          .addr COMP_LIT::xt
          .addr enter           ; C: (jsr) ENTER
          .word COMP_LIT::xt
          .addr RPLUCK::xt      ; C: RPLUCK
          .word COMP_LIT::xt
          .addr INCR::xt        ; C: INCR
          NLIT  2
          .addr STATE::xt
          .addr STORE::xt
          EXIT
eword

; Core 6.1.1200
dword     DEPTH,"DEPTH"
          txa
          tay
          lda   #$00
          PUSHNEXT
eword

; Core ext 6.2.2030
dword     PICK,"PICK"
          jsr   popay
          sty   XR
          txa
          sec
          sbc   XR
          bcc   :+
          stx   SPTMP
          tax
          jsr   popay
          ldx   SPTMP
          PUSHNEXT
:         jmp   stku_err
eword

; Tools 15.6.1.0220
; I thought about DEPTH 1- 0 DO I PICK . -1 +LOOP
; but it doesn't save anything
dword     DOTS,".S"
.if 1
          ; secondary version, uses pictured numeric output
          ; 17 bytes shorter than native
          ENTER
          NLIT  '{'
          .addr EMIT::xt
          .addr SPACE::xt
          .addr DEPTH::xt
          .addr DUP::xt
          .addr DOT::xt
          NLIT  ':'
          .addr EMIT::xt
          .addr SPACE::xt
          .addr DUP::xt
          .addr _IF::xt
          .addr done            ; early out for empty stack
lp:       .addr DECR::xt
          .addr DUP::xt
          .addr PtoR::xt
          .addr PICK::xt
          .addr DOT::xt
          .addr RtoP::xt
          .addr DUP::xt
          .addr _IFFALSE::xt
          .addr lp
done:     .addr DROP::xt
          NLIT  '}'
          .addr EMIT::xt
          EXIT
.else
          ; native version, uses DaveX functions
          stx   SPTMP
          jsr   xmess
          .byte "{ ",$00
          lda   #$00
          ldy   SPTMP
          jsr   xprdec_2
          jsr   xmess
          .byte " : ",$00
          ldx   #$00
lp:       cpx   SPTMP
          bcc   :+
          lda   #'}'
          jsr   _emit
          NEXT
:         ldy   PSTKL,x
          lda   PSTKH,x
          bpl   :+
          pha
          lda   #'-'
          jsr   _emit
          pla
          jsr   _negateay
:         stx   XR
          jsr   xprdec_2
          ldx   XR
          lda   #' '
          jsr   _emit
          inx
          jmp   lp
.endif
eword
DOTS_xt   = DOTS::xt

; Non-standard, but useful
dword     ZEROSP,"0SP"
          ldx   #$00
          NEXT
eword

; Exception ext 9.6.2.0670
dword     ABORT,"ABORT"
          ENTER
          ;.addr ZEROSP::xt
          NLIT  -1
          .addr THROW::xt
          EXIT
eword

; Non-standard
dword     ABORTBANG,"ABORT!",F_IMMED
          jmp   ABORT::xt
eword

; headerless word implementing text interpreter
hword     INTERPRET,"INTERPRET"
loop:     ENTER
          .addr INQ::xt           ; is there input?
          .addr _IF::xt           ; ( tf -- )
          .addr done              ; done if none
          .addr PARSE_NAME::xt    ; otherwise parse next word
          CODE
          jsr   popxr
          jsr   popwr
          lda   XR+1
          eor   XR
          beq   loop              ; if length is 0, loop back
          jsr   _search
          bcc   trynum
          jsr   _code             ; get code address & flags
          php                     ; save flags
          jsr   pushay
          plp
          bvs   conly             ; compile-only
          bmi   execute           ; immediate
          lda   ZSTATE
          ora   ZSTATE+1
          beq   execute
compile:  ldy   #<COMMA::xt
          lda   #>COMMA::xt
          jsr   pushay
execute:  ENTER
          .addr EXECUTE::xt
          CODE
lp2:      jmp   loop
done:     EXIT
trynum:   jsr   _parsenum
          bcc   badword
          jsr   pushay
          lda   ZSTATE
          ora   ZSTATE+1
          beq   lp2
          jsr   peekay
          ora   #$00
          beq   compile           ; fast literal
          ldy   #<LIT::xt         ; otherwise compile literal
          lda   #>LIT::xt
          jsr   cworday
          jmp   compile
badword:  ldy   #$00
pr:       cpy   XR
          bcs   notfnd
          lda   (WR),y
          jsr   _emit
          iny
          bne   pr
notfnd:   lda   #'?'
          jsr   _emit
          ldy   #<-13
          lda   #>-13
barf:     jmp   _throway
conly:    php
          lda   ZSTATE
          ora   ZSTATE+1
          bne   :+
          plp
          dex               ; drop xt from stack
          ldy   #<-14
          lda   #>-14
          bne   barf
:         plp
          bmi   execute
          bpl   compile
eword
_undefined = INTERPRET::notfnd

; Core ext 6.2.2182
; TODO: if/when file words are implemented, this has to deal with them
; as well, and some words that use it (EVALUATE) will need to be modified
dword     SAVEINPUT,"SAVE-INPUT"
          ENTER
          .addr SOURCE::xt      ; put CIB and #IN on stack
          .addr PIN::xt
          .addr FETCH::xt       ; put >IN on stack
          NLIT  3
          EXIT
eword

; Core ext 6.2.2148
dword     RESTOREINPUT,"RESTORE-INPUT"
          ENTER
          .addr DROP::xt
          .addr PIN::xt
          .addr STORE::xt
          .addr NIN::xt
          .addr STORE::xt
          .addr CIB::xt
          .addr STORE::xt
          EXIT
eword

; Core 6.1.1360
; Save the current input source to the return stack
; set input up for string to evaluate, then put it all back
dword     EVALUATE,"EVALUATE"
          ENTER
          .addr SOURCEID::xt      ; puts one item on stack
          .addr SAVEINPUT::xt     ; puts n+1 items on stack, with n at the top
          .addr INCR::xt          ; and add one for source ID
          .addr NPtoR::xt         ; save on return stack
          .addr PtoR::xt          ; and save the count on return stack
          NLIT  -1
          .addr dSOURCEID::xt
          .addr STORE::xt         ; set source ID to -1
          NLIT  0
          .addr PIN::xt           ; set >IN to 0
          .addr STORE::xt
          .addr NIN::xt
          .addr STORE::xt         ; string length to #IN         
          .addr CIB::xt
          .addr STORE::xt         ; string addr to CIB
          .addr INTERPRET::xt     ; interpret from there until nothing left
          .addr RtoP::xt          ; get count back
          .addr NRtoP::xt         ; and pull them off the return stack
          .addr DECR::xt          ; account for what we added
          .addr RESTOREINPUT::xt  ; restore input spec
          .addr dSOURCEID::xt
          .addr STORE::xt         ; and input source ID
          EXIT
eword

.proc     _status
          lda   SOURCE_ID
          ora   SOURCE_ID+1
          bne   :+
          lda   #ACR
          jsr   _emit
:         NEXT
.endproc

dword     STATUS,"STATUS"
          jmp   _status
eword

; Core 6.1.2050
; Empty the return stack, store zero in SOURCE-ID if it is present, make the 
; user input device the input source, and enter interpretation state. Do not 
; display a message. Repeat the following:
; * Accept a line from the input source into the input buffer, set >IN to zero,
; and interpret.
; * Display the implementation-defined system prompt if in interpretation
; state, all processing has been completed, and no ambiguous condition exists.
dword     QUIT,"QUIT"
          lda   #$00              ; enter interpreting state
          sta   ZSTATE
          sta   ZSTATE+1
          stx   SPTMP
          ldx   RSSAV             ; clear return stack
          txs
          ldx   SPTMP
          jsr   _patch            ; forget most recent def if smudged
          ENTER                   ; outer interpreter
source0:  .addr SETKBD::xt        ; set keyboard source
lp:       .addr STATUS::xt        ; display status (default: CR if source ID=0)
          .addr REFILL::xt        ; get input (TODO, before this SOURCE-ID should reflect redirection)
          .addr _IF::xt           ; did we get any?
          .addr source0           ; if not, set source to keyboard, go again
          .addr INTERPRET::xt     ; otherwise, interpret what we got
          .addr SOURCEID::xt      ; what source?
          .addr _IFFALSE::xt      ; something other than keyboard?
          .addr lp                ; yes, don't print any prompts
          .addr REDIRECTQ::xt     ; I/O redirected?
          .addr DROP::xt          ; nobody cares about poor output redirection :(
          .addr _IFFALSE::xt      ; not redirecting?
          .addr lp                ; we are!  don't do prompt
          .addr SPACE::xt         ; otherwise, a space
          .addr _SMART::xt        ; compiling?
          .addr interp            ; no, do normal prompt
          SLIT  "[OK]"            ; otherwise do compiling prompt
          .addr TYPE::xt
          .addr _JUMP::xt
          .addr lp
interp:   SLIT  "OK"
          .addr TYPE::xt
          .addr _JUMP::xt
          .addr lp
eword
QUIT_xt = QUIT::xt

; headerless word to do first QUIT when there is a file on the command line
hword     FQUIT,"FQUIT"
          lda   SOURCE_ID         ; already have file refnum?
          beq   :+                ; if not go ahead and set it up
          jmp   _cold::abort      ; otherwise abort
:         lda   #$00              ; enter interpreting state
          sta   ZSTATE
          sta   ZSTATE+1
          ldx   RSSAV             ; clear return stack
          txs
          jsr   xgetparm_n
          jsr   xfman_open
          bcc   :+
          jmp   xProDOS_err       ; totally bomb if file not available
:         sta   SOURCE_ID
          ldx   #$00              ; clear parameter stack
          ENTER
          .addr TIB::xt
          .addr CIB::xt
          .addr STORE::xt
          .addr _JUMP::xt
          .addr QUIT::lp
eword
FQUIT_xt = FQUIT::xt

; Core 6.1.0080
dword     RPAREN,"(",F_IMMED
          ENTER
          NLIT  ')'
          .addr PARSE::xt
          EXIT
eword

; Tools 15.6.1.0600
dword     VIEW,"?"
          ENTER
          .addr FETCH::xt
          .addr DOT::xt
          EXIT
eword

; Core ext 6.2.0200
dword     DOTPAREN,".(",F_IMMED
          ENTER
          NLIT  ')'
          .addr PARSE::xt
          .addr TYPE::xt
          EXIT
eword

; Davex
dconst    CBUFF,"CATBUFF",catbuff
; Davex
dconst    FBUFF,"FBUFF",filebuff
; Davex
dconst    FBUFF2,"FBUFF2",filebuff2
; Davex
dconst    FBUFF3,"FBUFF3",filebuff3

; Davex
dword     DOTFTYPE,".FTYPE"
          jsr   popay
          tya
          stx   SPTMP
          jsr   xprint_ftype
          ldx   SPTMP
          NEXT
eword

; Davex
dword     DOTACCESS,".ACCESS"
          jsr   popay
          tya
          stx   SPTMP
          jsr   xprint_access
          ldx   SPTMP
          NEXT
eword


; Davex
dword     U3PERCENT,"3U%"
          jsr   popaxy
          sta   num+2
          stx   num+1
          sty   num
          ldx   SPTMP
          jsr   popaxy
          jsr   xpercent
          ldx   SPTMP
          jsr   pusha
          NEXT
eword

dword     UPERCENT,"U%"
          ENTER
          .addr TWOStoD::xt
          .addr U3PERCENT::xt
          EXIT
eword

; Davex
dword     DOTSD,".SD"
          jsr   popay
          tya
          stx   SPTMP
          jsr   xprint_sd
          ldx   SPTMP
          NEXT
eword

; Davex
dword     CSTYPE,"CSTYPE"
          jsr   popay
          stx   SPTMP
          jsr   xprint_path
          ldx   SPTMP
          NEXT
eword

; Davex
dword     BUILD_LOCAL,"BUILD_LOCAL"
          jsr   popay
          stx   SPTMP
          jsr   xbuild_local
          ldx   SPTMP
          PUSHNEXT
eword

; Davex
dword     PREDIRECT,"+REDIRECT"
          stx   SPTMP
          lda   #$FF
redir:    jsr   xredirect
          ldx   SPTMP
          NEXT
eword

; Davex
dword     MREDIRECT,"-REDIRECT"
          stx   SPTMP
          lda   #$00
          beq   PREDIRECT::redir
eword

; Davex
dword     YESNO,"Y/N"
          stx   SPTMP
          jsr   xyesno
yn2:      beq   :+
          lda   #$FF
:         tay
          ldx   SPTMP
          PUSHNEXT
eword

; Davex
dword     YESNO2,"Y/N2"
          jsr   popay
          tya
          stx   SPTMP
          jsr   xyesno2
          jmp   YESNO::yn2
eword

; Davex
dword     BELL,"BELL"
          stx   SPTMP
          jsr   xbell
          ldx   SPTMP
          NEXT
eword

; Davex
dword     PRDATE,".DATE"
          jsr   popay
          stx   SPTMP
          jsr   xpr_date_ay
          ldx   SPTMP
          NEXT
eword

; Davex
dword     PRTIME,".TIME"
          jsr   popay
          stx   SPTMP
          jsr   xpr_time_ay
          ldx   SPTMP
          NEXT
eword

; Davex
.proc     dircommon
          stx   SPTMP
          jsr   xpush_level
          ldx   SPTMP
          rts
.endproc

; Davex
dword     TDIR,"<DIR"
          jsr   dircommon
          jsr   popay
          stx   SPTMP
          jsr   xdir_setup
          ldx   SPTMP
          NEXT
eword

; Davex
dword     TTDIR,"<<DIR"
          jsr   dircommon
          jsr   popay
          stx   SPTMP
          jsr   xdir_setup2
          ldx   SPTMP
          NEXT
eword

; Davex
dword     DIRP,"DIR+"
          stx   SPTMP
          jsr   xread1dir
          ldx   SPTMP
          bcs   :+
          ldy   #<catbuff
          lda   #>catbuff
done:     PUSHNEXT
:         lda   #$00
          tay
          beq   done
eword

; Davex
dword     DIRT,"DIR>"
          stx   SPTMP
          jsr   xdir_finish
          ldx   SPTMP
          NEXT
eword

; Davex
dword     CHKWAIT,"WAIT?"
          stx   SPTMP
          jsr   xcheck_wait
          ldx   SPTMP
          lda   #$00
          bcc   :+
          lda   #$ff
:         tay
          PUSHNEXT
eword

; Davex
dword     IOPOLL,"IOPOLL"
          jsr   xpoll_io            ; all regs preserved
          NEXT
eword

; Davex
dword     DIRTY,"DIRTY"
          stx   SPTMP
          jsr   xdirty
          ldx   SPTMP
          NEXT
eword

; Davex
dword     PRVER,".VER"
          jsr   popay
          tya
          stx   SPTMP
          jsr   xprint_ver
          ldx   SPTMP
          NEXT
eword

; Davex
; ( c -- ay x true ) or ( c -- false )
dword     XINFO,"XINFO"
          jsr   popay
          stx   SPTMP
          tya
          tax
          jsr   xshell_info
          stx   XR
          ldx   SPTMP
          bcs   bad
          sta   YR+1
          sty   YR
          jsr   pushay
          lda   XR
          jsr   pusha
          lda   #$FF
          bne   :+
bad:      lda   #$00
:         tay
          PUSHNEXT
eword

.proc     xpmgr_do
          stx   SPTMP
          jsr   xpmgr
command:  .byte $00
parm1:    .word $0000
parm2:    .word $0000
          ldx   SPTMP
          NEXT
.endproc

.proc     xpmgr_begin
          sta   xpmgr_do::command
          lda   #opNOP
          sta   xpmgr_do::parm2+1 ; for one-parm commands, the common case
          sta   xpmgr_do::parm2
          jsr   _swap
          jsr   popay
          sta   xpmgr_do::parm1+1
          sty   xpmgr_do::parm1
          rts
.endproc

; Davex - append one counted string to another
dword     CAPPENDS,"CS+CS"
          lda   #pm_appay
          jsr   xpmgr_begin
          jsr   popay
          jmp   xpmgr_do
eword

; Davex - append one character to counted string
dword     CAPPEND,"CS+"
          lda   #pm_appch
          jsr   xpmgr_begin
          jsr   popay
          tya
          jmp   xpmgr_do
eword

; Davex - remove path segment
dword     CDROP,"CS/-"
          lda   #pm_up
          jsr   xpmgr_begin
          jmp   xpmgr_do
eword

; Davex - add / if none in string
dword     CSLASH,"CS+/"
          lda   #pm_slashif
          jsr   xpmgr_begin
          jmp   xpmgr_do
eword

; Davex - copy counted string from PARM1 to PARM2
dword     CSMOVE,"CSMOVE"
          lda   #pm_copy
          jsr   xpmgr_begin
          jsr   popay
          sta   xpmgr_do::parm2+1
          sta   xpmgr_do::parm2
          jmp   xpmgr_do
eword

; ProDOS
dword     P8MLI,"MLI"
          jsr   popay
          sta   parmlist+1
          sty   parmlist
          jsr   popay
          sty   callnum
          stx   SPTMP
          jsr   mli
callnum:  .byte $00
parmlist: .addr $0000
chkerr1:  ldx   SPTMP           ; other words enter here to restore SP first
chkerr:   bcc   :+              ; check for error, throw it if present
          jmp   _throwp8
:         NEXT
eword

; Core 6.1.0710
dword     ALLOT,"ALLOT"
          jsr   popay
          pha
          tya
          clc
          adc   CHERE
          sta   CHERE
          pla
          adc   CHERE+1
          sta   CHERE+1
          NEXT
eword

; Core ext 6.2.0825
dword     BUFFER,"BUFFER:"
          ENTER
          .addr CREATE::xt
          .addr ALLOT::xt
          EXIT
eword

; Core 6.1.0880
dword     CELLP,"CELL+"
          ENTER
          NLIT  2
          .addr PLUS::xt
          EXIT
eword

; Core 6.1.0890
dword     CELLS,"CELLS"
          ENTER
          NLIT 2
          .addr MULT::xt
          EXIT
eword

; Core 6.1.0897
dword     CHARP,"CHAR+"
          jmp   INCR::xt
eword

; Core ext 6.2.0945
; in our case, the semantics of COMPILE, and ,
; are the same
dword     COMPILEC,"COMPILE,"
          jmp   COMMA::xt
eword

; Core 6.1.0950
dword     CONSTANT,"CONSTANT"
          ENTER
          .addr MKENTRY::xt
          .addr COMP_CLIT
          .byte opJSR
          .addr COMP_LIT
          .addr pushconst
          .addr COMMA::xt       ; compile value
          EXIT
eword

; Core ext 6.2.2405
dword     VALUE,"VALUE"
          jmp   CONSTANT::xt
eword

; Core ext 6.2.2295
dword     TO,"TO",F_IMMED
          ENTER ; interpretation
          .addr PARSEFIND::xt
          .addr DXT::xt
          .addr rBODY::xt
          .addr _SMART::xt
          .addr interp
          .addr COMP_LIT::xt  ; compilation semantics
          .addr LIT::xt       ; compile literal
          .addr COMMA::xt     ; compile address of VALUE / LOCAL
          .addr COMP_LIT::xt  ; we get to do a neat trick here
interp:   .addr STORE::xt     ; and re-use the interpretation store
          EXIT
eword

; Core 6.1.0980
dword     COUNT,"COUNT"
          ENTER
          .addr DUP::xt
          .addr INCR::xt
          .addr SWAP::xt
          .addr CFETCH::xt
          EXIT
eword

; Core 6.1.1550
dword     WFIND,"FIND"
          ENTER
          .addr DUP::xt         ; ( c-addr -- c-addr c-addr )
          .addr COUNT::xt       ; ( c-addr -- c-addr c-addr u )
          .addr DSEARCH::xt     ; ( c-addr -- c-addr 0|xt )
          .addr DUP::xt         ; ( c-addr 0|xt -- c-addr 0|xt 0|xt )
          .addr _IF::xt         ; ( c-addr 0|xt 0|xt -- c-addr 0|xt )
          .addr notfound        ; if ( c-addr 0 -- )
          .addr NIP::xt         ; otherwise it's ( c-addr xt -- ), drop c-addr
          CODE                  ; do some native work
          jsr   popay
          jsr   _code
          php
          jsr   pushay
          lda   #$00            ; -1 = immediate flag
          ldy   #$01
          plp
          bmi   :+              ; yep, immediate
          jsr   _negateay       ; otherwise change to -1
:         jsr   pushay          ; and push it
          NEXT
notfound: EXIT
eword

; headerless helper to compile a string
hword     CSTRING,"CSTRING"
          ldy   #<cbytea
          lda   #>cbytea
          jsr   string_op_ay
          NEXT
eword

; swap the current interpretation string buffer
; and return it
hword     NEXTSBUF,"NEXTSBUF"
          ENTER
          .addr CSBUF::xt
          .addr FETCH::xt
          .addr SBUF1::xt
          .addr EQUAL::xt
          .addr _IF::xt
          .addr gobuf1
          .addr SBUF2::xt
          .addr _SKIP::xt           ; skip next instruction
gobuf1:   .addr SBUF1::xt
          .addr DUP::xt
          .addr CSBUF::xt
          .addr STORE::xt
          EXIT
eword

hword     CSCOMM,"CSCOMM"
          ENTER
          .addr COMP_LIT::xt
          .addr _JUMP::xt           ; C: _JUMP
          .addr HERE::xt            ; ( -- a ) resolve address
          .addr COMP_LIT::xt
          .addr controlmm           ; C: <f>(unresolved)
          .addr HERE::xt            ; ( a -- a b )
          .addr SWAP::xt            ; ( a -- b a ) so we can resolve a first
          NLIT  '"'                 ; parse delimiter
          .addr PARSE::xt           ; ( b a -- b a c-addr u ) 
          EXIT
eword

; Core ext 6.2.0855
; need to compile the following sequence:
; _JUMP <f> <string> f:PUSH <c-addr> PUSH <u> 
dwordq    SQ,"S'",F_IMMED
          ENTER
          .addr _SMART::xt          ; smart word
          .addr interp
          .addr CSCOMM::xt          ; ( ... -- b a c-addr u )
          .addr SWAP::xt            ; ( b a c-addr u -- b a u c-addr )
          .addr OVER::xt            ; ( ... -- b a u c-addr u )
          .addr CSTRING::xt         ; ( ... -- b a u ) compile string into program
          .addr SWAP::xt            ; ( ... -- b u a )
          .addr HERE::xt            ; ( ... -- b u a h )
          .addr SWAP::xt            ; ( ... -- b u h a )
          .addr STORE::xt           ; ( ... -- b u )  resolve <f>
          .addr SWAP::xt            ; ( ... -- u b ) compile addr first
          .addr COMP_LIT::xt
          .addr LIT::xt             ; C: LIT
          .addr COMMA::xt           ; ( ... -- u ) compile b as c-addr
          .addr COMP_LIT::xt
          .addr LIT::xt             ; C: LIT
          .addr COMMA::xt           ; ( ... -- u ) compile u
          EXIT
interp:   .addr NEXTSBUF::xt        ; go to next string buffer
          .addr DUP::xt             ; make extra copy
          NLIT  '"'
          .addr PARSE::xt           ; ( ... -- c-addr1 caddr1 c-addr2 u )
          .addr PtoR::xt
          .addr SWAP::xt
          .addr RCOPY::xt
          .addr MOVE::xt
          .addr RtoP::xt
          EXIT     
eword

; Core ext 6.2.0855
dwordq    CQ,"C'",F_IMMED
          ENTER
          .addr _SMART::xt
          .addr interp
          .addr CSCOMM::xt          ; ( ... -- b a c-addr u )
          .addr DUP::xt             ; ( ... -- b a c-addr u u )
          .addr CCOMMA::xt          ; ( ... -- b a c-addr u ) compile copy of u
          .addr CSTRING::xt         ; ( ... -- b a ) compile string into program
          .addr HERE::xt            ; ( ... -- b a h )
          .addr SWAP::xt            ; ( ... -- b h a )
          .addr STORE::xt           ; ( ... -- b ) resolve jump
          .addr COMP_LIT::xt
          .addr LIT::xt             ; C: LIT
          .addr COMMA::xt           ; ( ... -- b ) compile b as c-addr
          EXIT
interp:   NLIT  '"'
          .addr PARSE::xt
          .addr NEXTSBUF::xt
          .addr PLACE::xt
          .addr CSBUF::xt
          .addr FETCH::xt
          EXIT
eword

; Core 6.1.0190
; interpretation semantics defined like .(
dwordq    DOTQ,".'",F_IMMED
          ENTER
          .addr _SMART::xt
          .addr interp
          .addr SQ::xt              ; get msg addr on stack
          .addr COMP_LIT::xt
          .addr TYPE::xt            ; display it
          EXIT
interp:   NLIT '"'
          .addr PARSE::xt
          .addr TYPE::xt
          EXIT
eword

; word compiled by ABORT",
hword     _ABORTQ,"_ABORT'"
          ENTER
          .addr ROT::xt             ; move param after string
          .addr _IF::xt
          .addr noabort
          NLIT  CATCH::flag
          .addr CFETCH::xt
          .addr _IF::xt
          .addr dotype              ; if catch flag set, do not type
          .addr TWODROP::xt
          .addr _SKIP::xt
dotype:   .addr TYPE::xt
          .addr ZEROSP::xt
          NLIT  -2
          .addr THROW::xt
noabort:  .addr TWODROP::xt         ; drop string
          EXIT          
eword


; Exception ext 9.6.2.0680
dwordq    ABORTQ,"ABORT'",F_CONLY|F_IMMED
          ENTER
          .addr SQ::xt              ; compile string
          .addr COMP_LIT::xt
          .addr _ABORTQ::xt
          EXIT
eword

; Core 6.1.1540
; ( c-addr u char -- ) - fill u chars (bytes) at c-addr with char
dword     FILL,"FILL"
          jsr   popay
fchary:   sty   char
          ldy   #<func
          lda   #>func
          jsr   string_op_ay
          NEXT
func:     lda   #$FF                ; self-modified
char = * - 1
          sta   (XR),y
          rts
eword

; Core ext 6.2.1350
dword     ERASE,"ERASE"
          ldy   #$00
          jmp   FILL::fchary
eword

; String 17.6.1.0780
dword     BLANK,"BLANK"
          ldy   #' '
          jmp   FILL::fchary
eword


; Core 6.1.1710
; TODO: de-dup shared code with COLON
; and future COMPILE-ONLY
dword     IMMEDIATE,"IMMEDIATE"
          ENTER
          .addr LAST::xt
          NLIT 2
          .addr PLUS::xt
          .addr DUP::xt
          .addr CFETCH::xt
          NLIT  F_IMMED
          .addr LOR::xt
          .addr SWAP::xt
          .addr CSTORE::xt
          EXIT
eword

; Core 6.1.1780
dword     LITERAL,"LITERAL",F_CONLY|F_IMMED
          jsr   peekay
          cmp   #$00
          beq   fastlit
          ENTER
          .addr COMP_LIT::xt
          .addr LIT::xt
          .addr COMMA::xt
          EXIT
fastlit:  jmp   COMMA::xt
eword

; helper function to perform a function in ZR
; XR times
.proc     _iter
lp:       lda   XR
          ora   XR+1
          bne   :+
          rts
          lda   XR
          bne   :+
          dec   XR+1
:         dec   XR
          jsr   doit
          jmp   lp
doit:     jmp   (ZR)
.endproc

.proc     _shiftcom1
          sta   ZR+1
          sty   ZR
          jsr   popxr
          jsr   popwr
          jmp   _iter
.endproc

.proc     _shiftcom2
          lda   WR+1
          ldy   WR
          PUSHNEXT
.endproc

; Core 6.1.1805
dword     LSHIFT,"LSHIFT"
          ldy   #<goleft
          lda   #>goleft
          jsr   _shiftcom1
          jmp   _shiftcom2
goleft:   asl   WR
          rol   WR+1
          rts
eword

; Core 6.1.2230
dword     SPACES,"SPACES"
          ldy   #<doit
          lda   #>doit
          sta   ZR+1
          sty   ZR
          jsr   popxr
          jsr   _iter
          NEXT
doit:     lda   #' '
          jmp   _emit
eword

; Core 6.1.0330
dword     TWOMULT,"2*"
          jsr   popwr
          jsr   LSHIFT::goleft
          jmp   _shiftcom2
eword

; Core 6.1.2162
dword     RSHIFT,"RSHIFT"
          ldy   #<goright
          lda   #>goright
          jsr   _shiftcom1
          jmp   _shiftcom2
goright:  lsr   WR+1
          ror   WR
          rts
eword

; Core 6.1.0330
dword     TWODIV,"2/"
          jsr   popwr
          jsr   RSHIFT::goright
          jmp   _shiftcom2
eword

.proc     _marker
          ENTER
          .addr HERE::xt
          .addr LAST::xt
          .addr CREATE::xt
          .addr COMMA::xt       ; compile LAST first
          .addr COMMA::xt       ; then HERE
          CODE
          jsr   SDOES
          ENTER
          .addr RPLUCK::xt
          .addr INCR::xt
          CODE
          jsr   popwr
          ldy   #$00
          lda   (WR),y
          sta   DLAST
          iny
          lda   (WR),y
          sta   DLAST+1
          iny
          lda   (WR),y
          sta   CHERE
          iny
          lda   (WR),y
          sta   CHERE+1
          NEXT
.endproc

; Core 6.2.1850
dword     MARKER,"MARKER"
          jmp   _marker
eword

; Tools 15.6.1.2465
dword     WORDS,"WORDS"
          sta   SPTMP
          lda   DLAST
          sta   WR
          lda   DLAST+1
          sta   WR+1
lp:       lda   WR
          ora   WR+1
          bne   :+
done:     lda   SPTMP
          NEXT
:         lda   WR+1
          jsr   PrByte
          lda   WR
          jsr   PrByte
          lda   #' '
          jsr   _emit
          ldy   #$02
          lda   (WR),y
          and   #$0F
          beq   nxt
          clc
          tax
pr:       iny
          lda   (WR),y
          jsr   _emit
          dex
          bne   pr
nxt:      lda   #ACR
          jsr   _emit
          jsr   xcheck_wait
          bcs   done
          ldy   #$00
          lda   (WR),y
          pha
          iny
          lda   (WR),y
          sta   WR+1
          pla
          sta   WR
          jmp   lp
eword

; Core 6.1.2033
dword     POSTPONE,"POSTPONE",F_CONLY|F_IMMED
          ENTER
          .addr PARSEFIND::xt
          .addr DXT::xt
          .addr COMMA::xt
          EXIT
eword

; Core 6.1.2410
dword     VARIABLE,"VARIABLE"
          ENTER
          .addr CREATE::xt
          NLIT  2
          .addr ALLOT::xt
          EXIT
eword

; Core ext 6.2.2395
dword     UNUSED,"UNUSED"
          ENTER
          .addr DHIMEM::xt
          .addr FETCH::xt
          .addr HERE::xt
          .addr MINUS::xt
          EXIT
eword

; Core 6.1.2120
dword     RECURSE,"RECURSE",F_CONLY|F_IMMED
          ENTER
          .word LAST::xt
          .word DXT::xt
          .word COMMA::xt
          EXIT
eword

; Core 6.1.1880
dword     MIN,"MIN"
          ENTER
          .word TWODUP::xt
          .word SGT::xt
com:      .word _IF::xt
          .word noswap
          .word SWAP::xt
noswap:   .word DROP::xt
          EXIT
eword

; Core 6.1.1870
dword     MAX,"MAX"
          ENTER
          .word TWODUP::xt
          .word SLT::xt
          .word _JUMP::xt
          .word MIN::com
eword

; Core ext 6.2.2440
; ( test low high ) true if test is within low (inclusive) and high (exclusive)
; required for loop checks
dword     WITHIN,"WITHIN"
          ENTER
          .addr OVER::xt
          .addr MINUS::xt
          .addr PtoR::xt
          .addr MINUS::xt
          .addr RtoP::xt
          .addr ULT::xt
          EXIT
eword

; Headerless helper to put the top two stack entries in numeric order
dword   ORDER,"ORDER"
        ENTER
        .addr TWODUP::xt
        .addr MAX::xt
        .addr PtoR::xt
        .addr MIN::xt
        .addr RtoP::xt
        EXIT
eword

; and now the do ... loop stuff, here's the architecture:
; a loop is compiled as such
; |_DO _JUMP leave-addr|(1)word word word word|_LOOP|-PLOOP _JUMP (1)| UNLOOP
; where the first group is applied by DO, dropping _JUMP address on the
; stack. Any instance of LEAVE will jump here.  leave-addr is resolved when LOOP
; /+LOOP are compiled, which put 1 _PLOOP/_PLOOP, followed by _UNLOOP, with the effect that
; leave jumps to the _JUMP following _DO and 
; when executing:
; _DO puts the loop control parameters on the Rstack, and finishes with a jmp
; to _SKIP2 to skip the flow control structure.
; any LEAVE will jump back to the _JUMP, which will jump forward to the UNLOOP
; and finally, _LOOP/_PLOOP will increment/offset the index and compare it to
; the ending value using WITHIN and will either fall through the to the UNLOOP
; or jump back to the beginning of the loop

; run-time semantics for DO, must be primitive or account for ENTER on rstack
; ( -- limit index )(R: -- leave_address index limit )
hword     _DO,"_DO"
          lda   IP+1            ; put leave target
          pha                   ; onto the stack
          lda   IP
          pha
          jsr   popay           ; get index
          pha
          tya
          pha
          jsr   popay           ; get limit
          pha
          tya
          pha
          jmp   _SKIP2::xt      ; skip LEAVE's target
eword

; Core 6.1.1240
dword     DO,"DO",F_IMMED|F_CONLY
          ENTER
          .addr COMP_LIT::xt
          .addr _DO::xt       ; compile execution semantics
          .addr HERE::xt      ; ( C: -- do-sys ) address for LEAVE
          .addr COMP_LIT::xt
          .addr _JUMP::xt     ; LEAVE will jump here
          .addr COMP_LIT::xt
          .addr controlmm     ; LOOP/+LOOP will jump to do-sys+4, after this word
          EXIT
eword

; Core 6.1.2380
; Really, it's 3RDROP
dword     UNLOOP,"UNLOOP",F_CONLY
          pla
          pla
          pla
          pla
          pla
          pla
          NEXT
eword

; run-time semantics for +LOOP
; with increment on stack and (R: index limit )
; leaves new loop parms on return stack
; if the new index is in the termination range,
; exits via _SKIP, otherwise exits via _JUMP
; WR: increment
; XR: computed next index
; YR: limit
; ZR: computed limit bounds
hword     _PLOOP,"_+LOOP"
          jsr   popwr         ; increment to WR
          pla                 ; get limit from return stack
          sta   YR            ; put limit in YR
          clc
          adc   WR            ; add increment to get upper bound low byte 
          sta   ZR            ; to put in ZR
          pla                 ; get the high byte
          sta   YR+1          ; limit in YR
          adc   WR+1          ; add high byte of increment
          sta   ZR+1          ; and put in ZR
          pla                 ; now get current index low byte
          clc
          adc   WR            ; add increment
          sta   XR            ; new index low byte to XR
          pla                 ; high byte
          adc   WR+1          ; high byte of increment
          sta   XR+1          ; into XR
          pha                 ; and new index back on return stack
          lda   XR            ; high byte then low byte
          pha
          tay                 ; low byte to Y
          lda   XR+1          ; high byte to A
          jsr   pushay        ; and put new index on forth stack
          lda   YR+1          ; finally put limit back on return stack
          pha                 ; high byte
          lda   YR            ; then low byte
          pha
          tay                 ; low byte to Y
          lda   YR+1          ; get high byte
          jsr   pushay        ; and limit on forth stack
          lda   ZR+1          ; now limit bound into AY
          ldy   ZR
          jsr   pushay        ; limit bound on forth stack
          ENTER
          .addr ORDER::xt     ; ensure within range is ordered low->high
          .addr WITHIN::xt    ; ( test lower upper -- flag )
          CODE
          jsr   popay         ; y = FF if within loop term range, $00 if not
          tya
          beq   :+            ; if not within range, go do jump
          jmp   _SKIP::xt     ; otherwise skip
:         jmp   _JUMP::xt
eword

; Core 6.1.0140
; compilation semantics for +LOOP
dword     PLOOP,"+LOOP",F_IMMED|F_CONLY
          ENTER
          .addr COMP_LIT::xt
          .addr _PLOOP::xt
          .addr DUP::xt       ; dup do-sys
          NLIT  4
          .addr PLUS::xt      ; get target of loop jump
          .addr COMMA::xt     ; compile as target of loop
          .addr COMP_LIT::xt
          .addr UNLOOP::xt    ; compile in an UNLOOP (skipped by LEAVE)
          NLIT  2
          .addr PLUS::xt      ; add 2 to get address we need to resolve
          .addr HERE::xt      ; we'll set jump to target HERE
          .addr SWAP::xt      ; get things into position
          .addr STORE::xt     ; and resolve all LEAVES
          EXIT                ; whew!
eword

; Core 6.1.1800
; compilation semantics for LOOP
dword     LOOP,"LOOP",F_IMMED|F_CONLY
          ENTER
          .addr COMP_LIT::xt
          .word 1
          .addr PLOOP::xt
          EXIT
eword

; Core 6.1.1800
dword     LEAVE,"LEAVE",F_CONLY
          pla                   ; drop loop control vars
          pla
          pla
          pla
          pla                   ; get leave address from return stack
          tay
          pla
          jmp   _JUMP::go       ; and jump
eword

; Core 6.1.1680
dword     IX,"I",F_CONLY
          ENTER
          NLIT 2
          .addr RPICK::xt
          EXIT
eword

; Core 6.1.1730
dword     JX,"J",F_CONLY
          ENTER
          NLIT 4
          .addr RPICK::xt
          EXIT
eword

.if 0
; non-standard
dword     KX,"K",F_CONLY
          ENTER
          NLIT 6
          .addr RPICK::xt
          EXIT
eword
.endif

; Back to non-loop stuff

; Core ext 6.2.2535
dword     BACKSLASH,"\",F_IMMED
          ENTER
          .addr NIN::xt
          .addr FETCH::xt
          .addr PIN::xt
          .addr STORE::xt
          EXIT
eword

; The following words are implemented as no-ops because they are
; inapplicable to this system.  They are implemented as JMPs
; so that they can potentially be resolved as deferred words.

; but first, here's where they will all point initially
hword     NO_OP,"NO_OP"
          NEXT
eword

; Core 6.1.0705
; alignment is not required on this platform
dword     ALIGN,"ALIGN"
          jmp   NO_OP
eword

; Core 6.1.0706
; alignment is not required on this platform
dword     ALIGNED,"ALIGNED"
          jmp   NO_OP
eword

; Core 6.1.0898
; chars are byte-sized
dword     CHARS,"CHARS"
          jmp   NO_OP
eword

.proc     _environmentq
          ENTER
          .addr TWODROP::xt
          .addr FALSE::xt
          EXIT
.endproc

; Core 6.1.1345
; ENVIRONMENT? always returns false (unknown) by default
; but implemented as a deferred word
dword     ENVIRONMENTQ,"ENVIRONMENT?"
          jmp _environmentq
eword


; the following words are not implemented per the Forth 2012 standard
; because they are obsolete.  They can be enabled if desired.
.if 0
; Core ext 6.2.2530
dword     CCOMPILE,"[COMPILE]",F_CONLY|F_IMMED
          ENTER
          .addr FIND::xt
          .addr COMMA::xt
          EXIT
eword
.endif


; must come after all dictionary words
dend

          DX_end