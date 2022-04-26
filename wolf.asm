.386
IDEAL
MODEL small
STACK 100h
DATASEG

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;CONSTANTS;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

TWO_PI dd ? ; set up once in main
ZERO dd 0.0

PLAYER_WIDTH dw 6
PLAYER_HEIGHT dw 6
PLAYER_SPEED dd 1.0
PLAYER_TURN_SPEED dd 0.1

BLOCK_SIZE dw 16
MAP_WIDTH dw 8
MAP_HEIGHT dw 8
MAP db 1,1,1,1,1,1,1,1
    db 1,0,0,0,0,0,0,1
    db 1,0,0,0,0,0,0,1
    db 1,0,0,0,0,0,0,1
    db 1,0,0,0,0,0,0,1
    db 1,0,0,0,0,1,0,1
    db 1,0,0,0,0,0,0,1
    db 1,1,1,1,1,1,1,1

DRAW_DIRECTION_SCALE dd 8.0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;LOCAL;;;VARIABLES;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;---draw_line---;
draw_line_half_width dw ?
;---------------;

;---draw_player---;
draw_player_fpu_out dw ?
;-----------------;

;---cast_ray---; ds:62
cast_ray_a_tan dd 0.123
cast_ray_is_looking_up dw 0
cast_ray_fpu_io dw ?
cast_ray_ray_y dd ?
cast_ray_ray_x dd ?
cast_ray_accuracy_number dd 0.0001
cast_ray_storage dw ?
;--------------;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;GLOBAL;;;VARIABLES;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

player_x dw 36h
player_y dw 36h
player_angle dd 4.5

keyboard_state db 7 dup(0) ; w, a, s, d, dot, comma, esc


back_buffer db 320*200 dup(1)


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

    ; si = keyboard_state offset
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
        add si, offset keyboard_state
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

; mult(x, y) -> x * y
; y needs to be 4 bits
proc mult
    push bp
    mov bp, sp
    pusha

    mov ax, [bp+6]
    mov cx, [bp+4]
    mul cx

    mov [bp+6], ax

    popa
    pop bp
    ret 2
endp mult

; divd(x, y) -> x / y
proc divd
    push bp
    mov bp, sp
    pusha

    mov ax, [bp+6]
    mov cx, [bp+4]
    div cx

    mov [bp+6], ax

    popa
    pop bp
    ret 2
endp divd

; cos_angle_times_speed() -> ST(0) = cos(player_angle) * player_speed
proc cos_angle_times_speed
    fld [dword ptr player_angle]
    fcos ; cos(player_angle)
    fld [dword ptr PLAYER_SPEED]
    fmulp ; cos(player_angle) * player_speed

    ret
endp cos_angle_times_speed

; sin_angle_times_speed() -> ST(0) = sin(player_angle) * player_speed
proc sin_angle_times_speed
    fld [dword ptr player_angle]
    fsin ; sin(player_angle)
    fld [dword ptr PLAYER_SPEED]
    fmulp ; cos(player_angle) * player_speed
    
    ret
endp sin_angle_times_speed

; to_pos(x, y) -> y * 320 + x
proc to_pos
    push bp
    mov bp, sp
    pusha

	mov dx, 0
    mov ax, [bp+4] ; y
    mov cx, 320
    mul cx ; y * 320

    add ax, [bp+6] ; y * 320 + x

    mov [bp+6], ax

    popa
    pop bp

    ret 2
endp to_pos

; at_map(x, y) -> map[y][x]
proc at_map

    push bp
    mov bp, sp
    pusha

    mov bx, offset MAP ; mapStart
    mov cx, [bp+4] ; x
    mov ax, [bp+6] ; y
    mov di, [offset MAP_WIDTH]

    mul di     ; ax = y * mapWidth
    add ax, cx ; ax = y * mapWidth
    add ax, bx ; ax = pos

    cmp bx, ax
    je at_mapIs00
    at_mapLoopstart:
        inc bx
        cmp bx, ax
        jne at_mapLoopstart

    at_mapIs00:

    mov ax, 0
    mov al, [bx]
    mov [bp+6], ax

    popa
    pop bp

    ret 2
endp at_map

; clear_screen() -> None
proc clear_screen
    push bp
    mov bp, sp
    pusha

    mov bx, offset back_buffer
    add bx, 320 * 200
    mov ax, offset back_buffer

    mov edx, 08080808h

    sub bx, 4

    clearLoopstart:
        mov [bx], edx
        sub bx, 4
        cmp bx, ax
        jne clearLoopstart

    mov [bx], edx

    popa
    pop bp
    ret
endp clear_screen

; draw_rect(x, y, width, height, color) -> None
proc draw_rect
    push bp
    mov bp, sp
    pusha

    mov cx, [bp+6] ; height
    draw_rect_collumn:
        dec cx
        mov ax, [bp+8] ; width
        draw_rect_line:
            dec ax
            mov di, ax
            add di, [bp+12]
            push di
            mov di, cx
            add di, [bp+10]
            push di
            call to_pos
            pop si ; pos
            add si, offset back_buffer
            mov dx, [bp+4]
            mov [si], dl
            cmp ax, 0
            jne draw_rect_line
        cmp cx, 0
        jne draw_rect_collumn

    popa
    pop bp
    ret 10
endp draw_rect

; draw_map() -> None
proc draw_map
    push bp
    mov bp, sp
    pusha

    mov ax, 0 ; vertical counter
    mov cx, 0 ; horizontal counter

    draw_map_vertical_loopstart:
        draw_map_horizontal_loopstart:

            push cx
            mov dx, [offset BLOCK_SIZE]
            push dx
            call mult
            pop dx ; x * block_size
            inc dx ; x * block_size + 1
            push dx ; ->rect x

            push ax
            mov dx, [offset BLOCK_SIZE]
            push dx
            call mult
            pop dx ; y * block_size
            inc dx ; y * block_size + 1
            push dx ; ->rect y

            mov dx, [offset BLOCK_SIZE]
            sub dx, 2 ; BLOCK_SIZE - 2
            push dx ; ->rect width
            push dx ; ->rect height

            push cx
            push ax
            call at_map
            pop bx ; map[y][x]

            cmp bx, 1
            je draw_map_is_wall
            jmp draw_map_no_wall

            draw_map_is_wall:
                mov dx, 0 ; color = black
                jmp draw_map_skip
            draw_map_no_wall:
                mov dx, 0Fh ; color = white
            draw_map_skip:

            push dx ; ->rect color

            call draw_rect

            inc cx
            cmp cx, [offset MAP_WIDTH]
            jb draw_map_horizontal_loopstart

        mov cx, 0
        inc ax
        cmp ax, [offset MAP_WIDTH]
        jb draw_map_vertical_loopstart

    popa
    pop bp
    ret
endp draw_map

; draw_player() -> None
proc draw_player
    push bp
    mov bp, sp
    pusha

    mov ax, [offset player_y] ; player y
    mov cx, [offset player_x] ; player x
    mov bx, [offset PLAYER_WIDTH] ; player width
    mov dx, [offset PLAYER_HEIGHT] ; player height

    mov di, [offset PLAYER_WIDTH]
    shr di, 1 ; player_width / 2

    mov si, [offset PLAYER_HEIGHT]
    shr si, 1 ; player_height / 2

    sub cx, di
    sub ax, si

    push cx ; ->rect x
    push ax ; ->rect y
    push bx ; ->rect width
    push dx ; ->rect height
    mov ax, 0Ch ; player color (red)
    push ax ; ->rect color
    call draw_rect

    ; player_x + cos(player_angle) * player_speed * scale
    call cos_angle_times_speed ; cos(player_angle) * player_speed
    fld [dword ptr DRAW_DIRECTION_SCALE]
    fmulp ; cos(player_angle) * player_speed * scale
    fild [word ptr player_x]
    faddp ; player_x + cos(player_angle) * player_speed * scale
    fistp [word ptr draw_player_fpu_out]
    mov bx, [word ptr draw_player_fpu_out]

    ; player_y + sin(player_angle) * player_speed * scale
    call sin_angle_times_speed ; sin(player_angle) * player_speed
    fld [dword ptr DRAW_DIRECTION_SCALE]
    fmulp ; sin(player_angle) * player_speed * scale
    fild [word ptr player_y]
    faddp ; player_y + sin(player_angle) * player_speed * scale
    fistp [word ptr draw_player_fpu_out]
    mov di, [word ptr draw_player_fpu_out]

    mov dx, 1 ; half of width/height
    sub bx, dx
    sub di, dx

    ; ax, bx, di
    push bx ; ->rect x
    push di ; ->rect y
    mov ax, 2 ; width, height
    push ax ; ->rect width
    push ax ; ->rect height
    mov ax, 0Ch ; color (red)
    push ax ; ->rect color
    call draw_rect

    popa
    pop bp
    ret
endp draw_player

; draw_pressed_keys() -> None
proc draw_pressed_keys
    push bp
    mov bp, sp
    pusha

    mov ax, 200 ; x
    mov cx, 100 ; y
    mov bx, offset keyboard_state
    mov dx, 10 ; width, height
    draw_pressed_loopstart:
        push ax
        push cx
        push dx
        push dx
        cmp [byte ptr bx], 1
        jne pressed_no
        pressed_yes:
            mov si, 0Fh
            jmp pressed_skip
        pressed_no:
            mov si, 00h
        pressed_skip:
        push si ; color
        call draw_rect
        inc bx
        add ax, 10
        cmp ax, 240
        jne draw_pressed_loopstart

    popa
    pop bp
    ret
endp draw_pressed_keys

proc delay
    pusha

    mov cx, 02h
    mov dx, 093E0h
    mov ah, 86h
    int 15h

    popa
    ret
endp delay

; memcpy(size, src, dst) -> None
proc memcpy
    push bp
    mov bp, sp
    pusha

    mov si, [bp+6] ; src
    mov di, [bp+4] ; dst
    mov cx, [bp+8] ; size

    shr cx, 2

    cpy1:
        cmp cx, 0
        je cpy2

        mov eax, [ds:si]
        mov [es:di], eax
        add si, 4
        add di, 4
        dec cx
        jmp cpy1
    cpy2:
        mov ax, [bp+4]

    popa
    pop bp

    ret 6
endp memcpy

; wait_for_VSync() -> None
proc wait_for_VSync
    pusha
    waitSync:
        mov dx, 03DAh
    waitEnd:
        in al, dx
        test al, 8
        jnz waitEnd
    waitStart:
        in al, dx
        test al, 8
        jz waitStart

    popa
    ret
endp wait_for_VSync

; move_player() -> None
proc move_player
    push bp
    mov bp, sp
    pusha

    mov bx, offset keyboard_state

    cmp [byte ptr bx], 1
        jne move_player_w_skip

        ; player_x + cos(player_angle) * player_speed
        fild [word ptr player_x]
        call cos_angle_times_speed ; cos(player_angle) * player_speed
        
        faddp ; player_x + cos(player_angle) * player_speed
        fistp [word ptr player_x]

        ; player_y + sin(player_angle) * player_speed
        fild [word ptr player_y]
        call sin_angle_times_speed
        
        faddp ; player_y + cos(player_angle) * player_speed
        fistp [word ptr player_y]

    move_player_w_skip:

    inc bx
    cmp [byte ptr bx], 1
        jne move_player_a_skip

        sub [offset player_x], ax

    move_player_a_skip:

    inc bx
    cmp [byte ptr bx], 1
        jne move_player_s_skip
        ; player_x + cos(player_angle) * player_speed
        fild [word ptr player_x]
        call cos_angle_times_speed ; cos(player_angle) * player_speed
        fsubp ; player_x + cos(player_angle) * player_speed
        fistp [word ptr player_x]

        ; player_y + sin(player_angle) * player_speed
        fild [word ptr player_y]
        call sin_angle_times_speed
        fsubp ; player_y + cos(player_angle) * player_speed
        fistp [word ptr player_y]

    move_player_s_skip:

    inc bx
    cmp [byte ptr bx], 1
        jne move_player_d_skip

        add [offset player_x], ax

    move_player_d_skip:

    inc bx
    cmp [byte ptr bx], 1
        jne move_player_dot_skip

        fld [dword ptr offset player_angle]
        fld [dword ptr offset PLAYER_TURN_SPEED]
        fsubp

        fld [dword ptr ZERO]

        fcomp
        fnstsw ax
        sahf
        jb move_player_dot_skip_reset_angle
            fld [dword ptr TWO_PI]
            faddp
        move_player_dot_skip_reset_angle:

        fstp [dword ptr offset player_angle]

    move_player_dot_skip:

    inc bx
    cmp [byte ptr bx], 1
        jne move_player_comma_skip

        fld [dword ptr offset player_angle]
        fld [dword ptr offset PLAYER_TURN_SPEED]
        faddp

        fld [dword ptr TWO_PI]

        fcomp
        fnstsw ax
        sahf
        jb move_player_comma_skip_reset_angle
            fld [dword ptr TWO_PI]
            fsubp
        move_player_comma_skip_reset_angle:

        fstp [dword ptr offset player_angle]

    move_player_comma_skip:

    popa
    pop bp
    ret
endp move_player

; cast_rays() -> None
proc cast_rays
    push bp
    mov bp, sp
    pusha

    ; si = dof
    mov si, 0

    fld [dword ptr offset player_angle] ; player_angle
    fptan
    fmulp ; tan(player_angle) #works
    mov bx, 1
    mov [offset cast_ray_fpu_io], bx
    fild [word ptr offset cast_ray_fpu_io]
    fchs ; ST(0) = -1 ST(1) = tan(player_angle)
    fdivrp ; -1 / tan(player_angle)
    fstp [dword ptr offset cast_ray_a_tan]

    ; check if player looking up or down
    fldpi
    fld [dword ptr offset player_angle]
    fcompp
    fnstsw ax
    sahf ; cmp player_angle, PI
    jb cast_ray_looking_down
    sahf
    je cast_ray_looking_left_or_right
    fld [dword ptr offset ZERO]
    fld [dword ptr offset player_angle]
    fcompp
    fnstsw ax
    sahf
    je cast_ray_looking_left_or_right

    ; cx = jumpX
    ; ax = jumpY

    cast_ray_looking_up:
        mov ax, [offset player_y]
        shr ax, 4 ; change if block size changes
        shl ax, 4 ; change if block size changes
        mov [offset cast_ray_fpu_io], ax
        fild [word ptr offset cast_ray_fpu_io]
        fld [dword ptr offset cast_ray_accuracy_number]
        fsubp ; ((player_y>>6)<<6)-0.0001
        fstp [dword ptr offset cast_ray_ray_y]
        ; #works

        mov bx, offset cast_ray_a_tan

        fild [word ptr offset player_y]
        fld [dword ptr offset cast_ray_ray_y]
        fsubp ; player_y - ray_y
        fld [dword ptr offset cast_ray_a_tan]
        fmulp ; (player_y - ray_y) * a_tan
        fild [word ptr offset player_x]
        faddp ; (player_y - ray_y) * a_tan + player_x
        fstp [dword ptr offset cast_ray_ray_x]

        mov ax, [offset BLOCK_SIZE]
        neg ax ; ax = -BLOCK_SIZE

        mov cx, [offset BLOCK_SIZE]
        mov [offset cast_ray_fpu_io], cx
        fild [word ptr offset cast_ray_fpu_io]
        fld [dword ptr offset cast_ray_a_tan]
        fmulp ; BLOCK_SIZE * a_tan
        fistp [word ptr offset cast_ray_fpu_io]
        mov cx, [offset cast_ray_fpu_io] ; cx = jumpX = BLOCK_SIZE * a_tan

        jmp cast_ray_skip1
    cast_ray_looking_down:
        mov ax, [offset player_y]
        shr ax, 4 ; change if block size changes
        shl ax, 4 ; change if block size changes
        add ax, [offset BLOCK_SIZE] ; ((player_y>>6)<<6)+64
        mov [offset cast_ray_fpu_io], ax
        fild [word ptr offset cast_ray_fpu_io]
        fstp [dword ptr offset cast_ray_ray_y]

        fild [word ptr offset player_y]
        fld [dword ptr offset cast_ray_ray_y]
        fsubp ; player_y - ray_y
        fld [dword ptr offset cast_ray_a_tan]
        fmulp ; (player_y - ray_y) * a_tan
        fild [word ptr offset player_x]
        faddp ; (player_y - ray_y) * a_tan + player_x
        fstp [dword ptr offset cast_ray_ray_x]

        mov ax, [offset BLOCK_SIZE] ; ax = jumpY = BLOCK_SIZE

        mov cx, [offset BLOCK_SIZE]
        neg cx ; cx = -BLOCK_SIZE
        mov [offset cast_ray_fpu_io], cx
        fild [word ptr offset cast_ray_fpu_io]
        fld [dword ptr offset cast_ray_a_tan]
        fmulp ; -BLOCK_SIZE * a_tan
        fistp [word ptr offset cast_ray_fpu_io]
        mov cx, [offset cast_ray_fpu_io] ; cx = jumpX = -BLOCK_SIZE * a_tan

        jmp cast_ray_skip1
    
    cast_ray_looking_left_or_right:
        fild [word ptr offset player_x]
        fstp [dword ptr offset cast_ray_ray_x] ; ray_x = player_x
        fild [word ptr offset player_y]
        fstp [dword ptr offset cast_ray_ray_y] ; ray_y = player_y
        mov si, 8 ; dof = 8
        jmp cast_ray_skip2
    cast_ray_skip1:

    ; ax=jumpY | bx=mapX | cx=jumpX | dx=mapY | di=free | si=dof
    cast_ray_loopstart:
        fld [dword ptr offset cast_ray_ray_x]
        fistp [word ptr offset cast_ray_fpu_io]
        ; bx = mapX | dx = mapY
        mov bx, [offset cast_ray_fpu_io]
        shr bx, 4 ; bx = mapX = ray_x / 16 | change if block size changes
        fld [dword ptr offset cast_ray_ray_y]
        fistp [word ptr offset cast_ray_fpu_io]
        mov dx, [offset cast_ray_fpu_io]
        shr dx, 4 ; dx = mapY = ray_y / 16 | change if block size changes
        
        push dx
        mov di, [offset MAP_WIDTH]
        push di
        call mult
        pop di ; map_y * MAP_WIDTH
        add di, bx ; map_y * MAP_WIDTH + map_x
        cmp di, 0
        jl cast_ray_in_map_bounds_or_clear_tile_skip

        push di
        ;----------------------;
        push [word ptr offset MAP_WIDTH]
        push [word ptr offset MAP_HEIGHT]
        call mult
        pop di
        mov [offset cast_ray_storage], di
        ;----------------------;
        pop di
        cmp di, ax
        jge cast_ray_in_map_bounds_or_clear_tile_skip
        push bx
        push dx
        call at_map
        pop di
        cmp di, 0
        je cast_ray_in_map_bounds_or_clear_tile_skip
            mov si, 8
            jmp cast_ray_in_loop_skip
        cast_ray_in_map_bounds_or_clear_tile_skip:
            ; ray_x += jumpX | ray_y += jumpY | dof += 1
            fld [dword ptr offset cast_ray_ray_x]
            mov [offset cast_ray_fpu_io], cx
            faddp
            fstp [dword ptr offset cast_ray_ray_x]

            fld [dword ptr offset cast_ray_ray_y]
            mov [offset cast_ray_fpu_io], ax
            faddp
            fstp [dword ptr offset cast_ray_ray_y]

            inc si
        cast_ray_in_loop_skip:

        cmp si, 8
        jb cast_ray_loopstart


    mov bx, offset cast_ray_a_tan ;deleteme
    ; draw_rect(ray_x, ray_y, width, width, 2 (green))
    fld [dword ptr offset cast_ray_ray_x]
    fistp [word ptr offset cast_ray_fpu_io]
    mov cx, [offset cast_ray_fpu_io] ; ray_x
    fld [dword ptr offset cast_ray_ray_y]
    fistp [word ptr offset cast_ray_fpu_io]
    mov ax, [offset cast_ray_fpu_io] ; ray_y
    mov bx, [offset PLAYER_WIDTH]
    mov di, 2 ; green

    push cx
    push ax
    push bx
    push bx
    push dx
    call draw_rect

    cast_ray_skip2:

    popa
    pop bp
    ret
endp cast_rays

proc main
    pusha

    finit

    mov di, 0A000h
    mov es, di

    mov ax, 13h
    int 10h

    fldpi
    fldpi
    faddp
    fstp [dword ptr TWO_PI]

    mov di, 1

    main_loopstart:

        ; game logic ;
        call move_player
        call cast_rays

        ; game graphics ;
        call clear_screen

        call draw_map
        call draw_player

        inc di
        cmp di, 100
        jne skip123
            mov di, 1
        skip123:

        call draw_pressed_keys

        ; swap buffers ;
        call wait_for_VSync
        mov ax, 320*200
        push ax
        mov ax, offset back_buffer
        push ax
        mov ax, 0
        push ax
        call memcpy
        ; swap buffers ;

        mov bx, offset keyboard_state + 6
        mov al, [bx]
        cmp al, 1
        jne main_loopstart


    mov al, 03h
    int 10h

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
