.386
IDEAL
MODEL small
STACK 100h
DATASEG

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;CONSTANTS;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BLOCK_SIZE db 16
MAP_WIDTH db 8
MAP_HEIGHT db 8
MAP db 1,1,1,1,1,1,1,1
    db 1,0,0,0,0,0,0,1
    db 1,0,0,0,0,0,0,1
    db 1,0,0,0,0,0,0,1
    db 1,0,0,0,0,0,0,1
    db 1,0,0,0,0,1,0,1
    db 1,0,0,0,0,0,0,1
    db 1,1,1,1,1,1,1,1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;VARIABLES;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
keyboardState db 7 dup(08h) ; w, a, s, d, dot, comma, esc


backBuffer db 320*200 dup(1)


CODESEG

proc change_handler_and_run_main
    xor     ax, ax
    mov     es, ax

    cli                              ; interrupts disabled
    push    [word ptr es:9*4+2]      ; save old keyboard (9) ISR address - interrupt service routine(ISR)
    push    [word ptr es:9*4]
	                                 ; put my keyboard (9) ISR address: procedure irq1isr
    mov     [word ptr es:9*4], offset keyboard_handler
	                                 ; put cs in ISR address
    mov     [es:9*4+2], cs
    sti                               ; interrupts enabled

    call    main                     ; program that use the interrupt  lines 43 - 83

    xor ax, ax
    mov es, ax
    cli                               ; interrupts disabled
    pop     [word ptr es:9*4]         ; restore ISR address
    pop     [word ptr es:9*4+2]
    sti                               ; interrupts enabled

    ret
endp change_handler_and_run_main


proc keyboard_handler
    pusha
    in al, 60h

    xor bh, bh
    mov bl, al
    and bl, 7Fh ; bl = scancode

    ; si = keyboardState offset
    check_W:
        cmp bl, 11h
        jne check_A
        mov si, 0
        jmp press_check
    check_A:
        cmp bl, 1Eh
        jne check_S
        mov si, 1
        jmp press_check
    check_S:
        cmp bl, 1Fh
        jne check_D
        mov si, 2
        jmp press_check
    check_D:
        cmp bl, 20h
        jne check_dot
        mov si, 3
        jmp press_check
    check_dot:
        cmp bl, 34h
        jne check_comma
        mov si, 4
        jmp press_check
    check_comma:
        cmp bl, 33h
        jne check_esc
        mov si, 5
        jmp press_check
    check_esc:
        cmp bl, 1h
        jne press_check
        mov si, 6
        jmp press_check

    press_check:
        push cx
        mov cx, 7
        shr al, cl
        pop cx
        xor al, 1  ; al = 1 if pressed 0 if released
        mov ah, 0
        ;;;;;;;;;; save state ;;;;;;;;;;
        add si, offset keyboardState
        mov [si], al

    in      al, 61h
    mov     ah, al
    or      al, 80h
    out     61h, al
    mov     al, ah
    out     61h, al
    mov     al, 20h
    out     20h, al

    popa
    iret
endp keyboard_handler


proc delay
    pusha

    mov cx, 02h
    mov dx, 093E0h
    mov ah, 86h
    int 15h

    popa
    ret
endp delay


proc draw_keyboard_buffer
    pusha

    mov di, 80 * 40 + 40
    mov bx, offset keyboardState
    mov cx, 0
    draw_kbf_loopstart:
        mov bx, offset keyboardState
        add bx, cx
        mov dh, [bx]
        mov dl, 0Ah
        add dh, 48
        mov [es:di], dx
        add di, 2
        inc cx
        cmp cx, 7
        jne draw_kbf_loopstart
    popa
    ret
endp draw_keyboard_buffer


proc main
    pusha

    finit
    mov di, 0B800h
    mov es, di

    main_loopstart:
        call draw_keyboard_buffer
        mov bx, offset keyboardState
        add bx, 6
        call delay
        mov al, [bx]
        cmp al, 1
        jne main_loopstart

    popa
    ret
endp main

start:
	mov ax, @data
	mov ds, ax
	
    call change_handler_and_run_main

exit:
    mov ax, 4C00h
    int 21h
END start
