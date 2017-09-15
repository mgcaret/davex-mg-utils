; %help
; afp.userprefix - Display or set the AFP user prefix.
;
; options:   -s <path>  Set to <path>, normally only if not already set.
;            -f         Force setting even if it is already set.
;            -c         Set current prefix to user prefix.
;
; Note that setting the user prefix uses undocumented features of the
; AppleTalk stack.
; %hend

.pc02
.include  "davex-mg.inc"

sptr      = xczpage
setflag   = sptr+2

pfxbuf    = filebuff2

          DX_start dx_mg_auto_origin ; load address
          DX_info $01,$12,dx_cc_iie_or_iigs,$00
          DX_ptab
          DX_parm 's',t_path      ; path to set
          DX_parm 'f',t_nil       ; force setting
          DX_parm 'c',t_nil       ; set prefix
          DX_end_ptab
          DX_desc "Display or set AFP User Prefix."
          DX_main
          cli                     ; appletalk requires interrupts
          ATcall inforeq
          bcc   :+
          jmp   noatalk
:         stz   setflag
          lda   #'s'|$80
          jsr   xgetparm_ch
          bcs   :+                ; set not requested
          inc   setflag
:         lda   #'f'|$80          
          jsr   xgetparm_ch
          bcs   getit             ; not being forced to set
          lda   setflag
          beq   :+                ; user said -f without -s, go to error
          jmp   setpfx            ; -s and -f, set prefix
:         lda   #$01
          jsr   xredirect
          jsr   xmess
          asc_hi "-f without -s!"
          .byte $00
          bra   exiterr
getit:    stz   dirflag           ; make sure we are getting, not setting
          ATcall pfxreq           ; get user prefix
          bcc   :+                ; if no error
          lda   setflag           ; error, see if we should try setting it?
          beq   pfxerr            ; nope, error out
          bra   tryset            ; otherwise, try setting
:         lda   #<pfxbuf          ; set sptr to prefix buffer
          sta   sptr
          lda   #>pfxbuf
          sta   sptr+1
          ldy   #$00              ; index of length byte
          lda   (sptr),y          ; check length
          beq   :+                ; no prefix, see if we should set it
          stz   setflag           ; have a prefix already, so flag don't set
          jsr   prpas             ; and print
:         lda   setflag           ; user wants prefix set?
          beq   :+                ; no, so skip doing it
tryset:   jmp   setpfx            ; go to the set routine
:         lda   #'c'|$80
          jsr   xgetparm_ch       ; change to wanted?
          bcs   :+                ; nope, skip to the end
          P8call $c6,p8pfxreq     ; enact the change
          bcc   :+                ; all good, skip error
          jmp   xProDOS_err       ; if something went pear-shaped
:         rts                     ; buhbye
pfxerr:   lda   #$01
          jsr   xredirect
          jsr   xmess
          asc_hi "Get/set user prefix error!"
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
;
setpfx:   lda   #'s'|$80          ; get the pointer to option
          jsr   xgetparm_ch
          bcs   badpath           ; shouldn't happen here!!!
          sty   sptr              ; set sptr to check length
          sty   parm1             ; and parm1 for copy
          sta   sptr+1
          sta   parm1+1
          lda   (sptr),y          ; check length
          beq   badpath           ; and if zero, it must be bad
          jsr   xpmgr             ; copy from user option to pfxbuf
          .byte pm_copy           ; davex function
parm1:    .addr pfxbuf            ; source, initially something not entirely insane
          .addr pfxbuf            ; destination
          P8call $c4,fileinfo     ; P8 GET_FILE_INFO
          bcc   :+                ; no error, continue
          jmp   xProDOS_err       ; something went pear-shaped
:         lda   fitype            ; get file type
          cmp   #$0f              ; is directory?
          bne   badpath           ; nope... error out
          lda   #$aa              ; undocumented, found in LOGON program
          sta   dirflag
          ATcall pfxreq           ; set user prefix
          bcc   :+                ; all good, skip error
          jmp   pfxerr            ; sigh... error out
:         stz   setflag           ; avoid infinite loop
          jmp   getit             ; wind back to get/print/maybe change dir
badpath:  lda   #$01
          jsr   xredirect
          jsr   xmess
          asc_hi "Bad new prefix!"
          .byte $00
          bra   exiterr
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
pfxreq:   .byte 0,$2a           ; sync FIUserPrefix
          .word $0000           ; result
dirflag:  .byte $00             ; direction flag (bit 7)
          .dword pfxbuf         ; buffer (at least 64 bytes)
;
p8pfxreq: .byte $01
          .addr pfxbuf
;
fileinfo: .byte $0a
          .addr pfxbuf          ; buffer
          .byte $00             ; access
fitype:   .byte $00             ; want to see $0f
          .word $0000           ; aux type
          .byte $00             ; storage type
          .word $0000           ; blocks used
          .word $0000           ; mod date
          .word $0000           ; mod time
          .word $0000           ; create date
          .word $0000           ; create time
          DX_end
