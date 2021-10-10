SYS_READ        equ 0
SYS_WRITE       equ 1
SYS_EXIT        equ 60
STDIN           equ 0
STDOUT          equ 1
EXIT_SUCCESS    equ 0
EXIT_FAILURE    equ 1

MAX_BYTE_VALUE      equ 0xFF        ; Byte with all bits lighted.
UTF8_MAX_BYTES      equ 4           ; Max length in bytes of UTF-8 char.
UTF8_BITMASK_AND    equ 00111111b   ; Bitmask for and operation on UTF-8 bytes.
UTF8_BITMASK_CHAR   equ 10000000b   ; Bitmask for bytes in UTF-8 char.
UTF8_MIN_2_BYTES    equ 0x80        ; Minimal value for 2 bytes UTF-8 char.
UTF8_MIN_3_BYTES    equ 0x800       ; Minimal value for 3 bytes UTF-8 char.
UTF8_MIN_4_BYTES    equ 0x10000     ; Minimal value for 4 bytes UTF-8 char.
UTF8_MAX_VALUE      equ 0x10FFFF    ; Maximal value for UTF-8 char.

MODULO              equ 0x10FF80
ARGUMENT_SHIFT      equ 0x80        ; Value by which polynomial is shifted.

; Macro for setting value in register to 0.
; %1 is modified.
%macro reset 1
    xor     %1, %1
%endmacro

; Macro for checking if value in register is zero.
; Only flags are modified..
%macro is_zero 1
    test    %1, %1
%endmacro

; Macro for performing modulo operation. Calculate %1 modulo %2 and save it in %1.
; %1, rax, rdx are modified.
%macro modulo 2
    mov     rax, %1
    reset   rdx
    mov     %1, %2
    div     %1
    mov     %1, rdx     ; Return remainder.
%endmacro


section .data
    bufferSize:         equ 32768
    lastChar:           dq bufferSize   ; Last read char in input buffer.
    inputCurrentChar:   dq bufferSize   ; Place for next char to read in buffer.
    outputCurrentChar:  dq 0            ; Place for next char to write in buffer.


section .bss
    inputBuffer     resb bufferSize
    outputBuffer    resb bufferSize


section .text
    global _start

; Function gets char from buffer and read to buffer from stdin if needed.
; al - returned char,
; rdx - 1 if any char is read else 0.
; rax, rdx, rdi, rsi are modified.
get_char:
    mov     rax, [lastChar]
    cmp     [inputCurrentChar], rax
    jnb     .read_buffer
.end:
    mov     rdx, [inputCurrentChar]
    inc     qword [inputCurrentChar]    ; Move to next place in buffer.

    reset   rax
    mov     al, [inputBuffer + rdx]
    mov     rdx, 1
    ret

.read_buffer:
    push    rcx                         ; Save rcx.
    mov     rax, SYS_READ
    mov     rdi, STDIN
    mov     rsi, inputBuffer
    mov     rdx, bufferSize
    syscall
    pop     rcx

    cmp     rax, 0
    jl      error                       ; Syscall returned error.

    is_zero rax                         ; Check if read any byte.
    jz      .eof

    mov     [lastChar], rax             ; Reset buffer values.
    mov     qword [inputCurrentChar], 0
    jmp     .end
.eof:
    reset   rdx                         ; EOF so no char is read.
    ret

; Function inserts char into buffer and prints buffer to stdout if needed.
; rdi - char to be inserted.
; rax, rdx, rdi, rsi are modified.
put_char:
    mov     rax, [outputCurrentChar]
    mov     [outputBuffer + rax], dil
    inc     qword [outputCurrentChar]               ; Move to next place in buffer.
    cmp     qword [outputCurrentChar], bufferSize   ; Check if it is end of buffer.
    je      .write_buffer
    ret

.write_buffer:
    push    rcx                             ; Save rcx.
    mov     rax, SYS_WRITE
    mov     rdi, STDOUT
    mov     rsi, outputBuffer
    mov     rdx, bufferSize
    syscall
    pop     rcx

    cmp     rax, 0
    jl      error                           ; Syscall returned error.

    mov     qword [outputCurrentChar], 0    ; Reset buffer values.
    ret

; Function gets char in UTF-8 format from buffer and checks if it is ASCII.
; rax - returned char value.
; rax, rcx, rdx, rdi, rsi, r12 are modified.
get_utf8_char:
    call    get_char

    is_zero rdx
    jz      end                     ; EOF reached.

    reset   rdx
    bt      rax, 7
    jnc     .end                    ; Read ASCII char.

    ; r12 - value of read char.
    movzx   r12, al
    reset   rcx
    ; rdx - position of bit to check.
    mov     rdx, 6
    ; rcx will be number of bytes that are not first in char.
.check_char_type:                   ; Check how many bytes has char.
    inc     rcx
    dec     rdx
    btr     r12, rdx                ; Check if next bit is lighted and reset it.
    jc      .check_char_type        ; Char had lighted bit, so it's one byte longer.

    cmp     rcx, UTF8_MAX_BYTES
    ja      error                   ; Char has too many bytes.

    and     r12, UTF8_BITMASK_AND   ; Now it's value of first byte without leading ones.

    push    rcx                     ; Save rcx.

.loop:
    shl     r12, 6                  ; Make place for values of next byte of char.

    call    get_char
    is_zero rdx
    jz      error                   ; Char ends unexpectedly.

    ; Check if byte is correct and  its char value.
    btr     rax, 7
    jnc     error                   ; Byte is in wrong format (without leading one).
    btr     rax, 6
    jc      error                   ; Byte is in wrong format (ligthed second bit).

    add     r12, rax                ; Add next byte value to char value.
    loop    .loop

    mov     rax, r12
    mov     rdx, 1

    pop     rcx                     ; Restore information about char length.

    cmp     rcx, 3
    jz      .check_if_good_4_bytes

    cmp     rcx, 2
    jz      .check_if_good_3_bytes

    cmp     rcx, 1
    jz      .check_if_good_2_bytes

    jmp     error                   ; Char value is to small.

.check_if_good_4_bytes:
    cmp     r12, UTF8_MIN_4_BYTES
    jb      error                   ; A shorter write method was possible.
    jmp     .end

.check_if_good_3_bytes:
    cmp     r12, UTF8_MIN_3_BYTES
    jb      error                   ; A shorter write method was possible.
    jmp     .end

.check_if_good_2_bytes:
    cmp     r12, UTF8_MIN_2_BYTES
    jb      error                   ; A shorter write method was possible.

.end:
    cmp     r12, UTF8_MAX_VALUE
    ja      error                   ; Char is too big.

    ret


; Function gets char in UTF-8 format from buffer and checks if it is ASCII.
; rdi - value of char to be putted.
; rax, rcx, rdx, rsi, r8, r12 are modified.
put_utf8_char:
    cmp     rdi, UTF8_MIN_2_BYTES
    jae     .not_ascii              ; Check if it's in UTF-8 format.

    ; Char can be printed in ASCII format.
    call    put_char
    ret

.not_ascii:
    ; rcx will be number of bytes that are not first in char.
    mov     rcx, 1

    cmp     rdi, UTF8_MIN_3_BYTES
    jb      .first_char
    inc     rcx

    cmp     rdi, UTF8_MIN_4_BYTES
    jb      .first_char
    inc rcx
.first_char:            ; Section for printing first byte of char.
    ; r8 is first byte of char with only leading ones.
    ; rax is number of first byte's bits that are not leading ones.
    mov     r8, MAX_BYTE_VALUE
    mov     rax, 7
    sub     rax, rcx

    mov     r12, rcx    ; Save rcx value.

    mov     rcx, rax
    shl     r8, cl      ; Get first byte of char without char values.

    ; rax is position in char value of last bit to put in first byte.
    mov     rax, 6
    mul     r12
    mov     rdx, rdi

    mov     rcx, rax
    shr     rdx, cl     ; Get char value coded in first byte.
    mov     rax, rdx
    add     rax, r8     ; Get first byte of char.

    push    rdi
    mov     dil, al
    call    put_char    ; Print first char.
    pop     rdi

    mov     rcx, r12    ; Restore rcx value.
.next_char:             ; Section for printing next bytes of char.
    ; rdx is position in char value of last bit to put in next byte.
    lea     rdx, [rcx - 1]
    imul    rdx, 6
    mov     rax, rdi

    mov     r12, rcx                ; Save rcx.

    mov     rcx, rdx
    shr     rax, cl
    and     al, UTF8_BITMASK_AND    ; Get value coded in next byte of char.
    add     al, UTF8_BITMASK_CHAR   ; Get next byte of char.

    push    rdi
    mov     dil, al
    call    put_char
    pop     rdi

    mov     rcx, r12                ; Restore rcx.

    loop    .next_char
    ret

; Function parses string to number using Horner's schema.
; rdi - pointer to string,
; rax - returned value of string,
; rax, rdx, rdi, r12 are modified.
parse_to_int:
    ; r12 - temporary value to store output.
    reset   r12
.loop:
    ; Multiply by 10;
    mov     rax, r12
    lea     rax, [rax + rax * 4]
    add     rax, rax
    mov     r12, rax

    reset   rdx
    mov     dl, [rdi]       ; Get next char.

    cmp     rdx, '0'
    jb      error           ; Invalid char.
    cmp     rdx, '9'
    ja      error           ; Invalid char.

    sub     rdx, '0'        ; Change to digit.
    add     r12, rdx        ; Add digit to output value.
    modulo  r12, MODULO

    inc     rdi
    cmp     byte [rdi], 0
    jne     .loop           ; Do until meet null byte.

    mov     rax, r12
    ret

; Function parses all polynomial coefficients from string to their numeric values.
; Input of function is located on stack (number of coefficients is its first element and then sequence of coefficients).
; r13 - position of number of coefficients.
; rcx - number of coefficients.
; rax, rbx, rcx, rdx, rdi, r12 are modified.
prepare_coefficients:
    lea     rbx, [r13 + 8]  ; Get first argument.
.loop:                      ; Change all stack elements to ints.
    push    rcx             ; Save rcx.
    mov     rdi, [rbx]
    call    parse_to_int
    pop     rcx             ; Restore rcx.
    mov     [rbx], rax      ; Change pointer to string to number value of this string.
    add     rbx, 8          ; Move to next argument
    loop    .loop

    ret

; Function takes polynomial argument and calculate its value modulo MODULO using Horner's schema.
; Input of function is located on stack (number of coefficients is its first element and then sequence of coefficients).
; r13 - position of number of coefficients,
; rdi - polynomial argument.
; rax, rbx, rcx, rdx, rdi, r12 are modified.
polynomial:
    sub     rdi, ARGUMENT_SHIFT         ; Shift polynomial argument

    ; rcx - number of coefficients
    ; rbx - pointer to last coefficient
    mov     rcx, [r13]
    mov     rbx, 8
    imul    rbx, rcx
    add     rbx, r13

    ; r12 - temporary value of polynomial.
    reset   r12
.loop:
    mov     rax, rdi
    mul     r12
    mov     r12, rax
    modulo  r12, MODULO
    add     r12, qword [rbx]
    modulo  r12, MODULO

    sub     rbx, 8                      ; Go to previous coefficient.
    loop    .loop

    lea     rax, [r12 + ARGUMENT_SHIFT] ; Shift polynomial value.
    ret


; rax, rbx, rcx, rdx, rdi, rsi, rsp, r8, r12, r13 are modified.
_start:
    pop     rcx
    dec     rcx

    is_zero rcx
    jz      error                       ; Program has no arguments.

    mov     [rsp], rcx                  ; Change first cmd argument to arguments size.

    ; Save registers to enable careless using them in program.
    push    r12
    push    r13
    push    rbx

    lea     r13, [rsp + 24]             ; Set pointer to polynomial data.

    call    prepare_coefficients
main_loop:                              ; Infinite loop until reach EOF.
    call    get_utf8_char

    is_zero rdx
    jz      .write                      ; ASCII char which is not processed by polynomial.

    mov     rdi, rax
    call    polynomial                  ; Calculate polynomial value.
.write:
    mov     rdi, rax
    call    put_utf8_char

    jmp     main_loop


; Function to call when program should end successfully.
; rdi is modified.
end:
    mov     rdi, EXIT_SUCCESS   ; Set exit status.
    jmp     exit

; Function to call when program should end with error.
; rdi is modified.
error:
    mov     rdi, EXIT_FAILURE   ; Set exit status.

; Function ends execution of program.
; rdi - exit status.
; rax, rdx, rdi, rsi are modified.
; rbx, r12, r13 are restored, so program didn't modify them.
exit:
    ; Restore saved registers.
    pop     rbx
    pop     r13
    pop     r12

    cmp     qword [outputCurrentChar], 0
    jb      .end                ; Buffer is empty so there is no need for printing it.

    ; Print buffer if its not empty.
    push    rdi
    mov     rax, SYS_WRITE
    mov     rdi, STDOUT
    mov     rsi, outputBuffer
    mov     rdx, [outputCurrentChar]
    syscall
    pop     rdi

    cmp     rax, 0
    jnl     .end

    mov     rdi, EXIT_FAILURE   ; Syscall returned error.
.end:
    mov     rax, SYS_EXIT
    syscall
