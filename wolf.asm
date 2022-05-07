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
HALF dd 0.5
ONE dd 1.0
DEG2RAD dd 0.0174533 ; multiply degrees by this for rads
BIG_NUMBER dd 10000000.0

PLAYER_WIDTH dw 6
PLAYER_HEIGHT dw 6
PLAYER_SPEED dd 0.7
PLAYER_TURN_SPEED dd 0.1
WALL_WIDTH dw 5
BLOCK_SIZE dw 16
MAP_WIDTH dw 8
MAP_HEIGHT dw 8
dw ?
dw ?
dw ?
MAP db 1,1,1,1,1,1,1,1
    db 1,0,0,0,0,0,0,1
    db 1,0,0,0,0,1,1,1
    db 1,0,0,0,0,0,0,1
    db 1,0,0,0,0,0,1,1
    db 1,1,0,0,0,1,1,1
    db 1,1,1,0,0,1,1,1
    db 1,1,1,1,1,1,1,1

DRAW_DIRECTION_SCALE dd 8.0


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;LOCAL;;;VARIABLES;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;---draw_menu---;
file_name db 'mmenu.bmp'
filehandle 	dw ?
Header 		db 54 dup (0)
ScrLine 	db 320 dup (0)
ErrorMsg 	db 'Error', 13, 10,'$'
;---------------;

;---move_player---;
;-----------------;

;---draw_player---;
draw_player_fpu_out dw ?
;-----------------;

;---cast_ray---; size of 0x36 or 54.0
cast_ray_a_tan dd ? ; ds:76
cast_ray_negative_tan dd ?
cast_ray_is_looking_up dw 0 ; ds:7E
cast_ray_fpu_io dw ?
cast_ray_ray_y dd ?
cast_ray_ray_x dd ? ; ds:86
cast_ray_jump_y dd ?
cast_ray_jump_x dd ? ; ds:8E
cast_ray_ray_angle dd ?
cast_ray_accuracy_number dd 1.0
cast_ray_horizontal_ray_x dd ?
cast_ray_horizontal_ray_y dd ?
cast_ray_vertical_ray_x dd ? ; ds:A4
cast_ray_vertical_ray_y dd ?
cast_ray_horizontal_ray_length dd ?
cast_ray_vertical_ray_length dd ?
cast_ray_final_ray_x dd ? ; ds:AA
cast_ray_final_ray_y dd ?
cast_ray_final_ray_length dd ?
cast_ray_is_vertical_hit dw 0
cast_ray_wall_height dd ? ; ds:C4
cast_ray_wall_offset dd ?
;--------------;

;---cast_sight_rays---;
cast_sight_rays_fpu_io dd ?
cast_sight_rays_current_angle dd ?
;---------------------;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;GLOBAL;;;VARIABLES;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

player_x dd 36.0
player_y dd 36.0
player_angle dd 0.5

keyboard_state db 8 dup(0FFh) ; w, a, s, d, dot, comma, esc, space

back_buffer db 320*200 dup(0)


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
    sti                               ; interrupts enabledב

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
        jne check_space
        mov si, 6
        jmp press_check
    check_space:
        cmp bl, 39h
        jne press_check
        mov si, 7
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

; round_down() -> None
; sets ST(0) to floor(ST(0))
proc round_towards_zero
    pusha
    fld [dword ptr offset ZERO] ; ST(0) = 0
                                ; ST(1) = x
    fcomp
    fnstsw ax
    sahf
    ja round_towards_zero_bellow_zero

    round_towards_zero_above_zero:
        fld [dword ptr offset HALF]
        fsubp
        frndint
        jmp round_towards_zero_skip
    round_towards_zero_bellow_zero:
        fld [dword ptr offset HALF]
        faddp
        frndint
    round_towards_zero_skip:

    popa
    ret
endp round_towards_zero

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


proc OpenFile ;file name offset as a parameter
; Open file
	push bp
	mov bp, sp
	mov ah, 3Dh
	xor al, al
	mov dx, [bp + 4]
	int 21h
	jc openerror
	mov [filehandle], ax
	pop bp
	ret 2
openerror:
	mov dx, offset ErrorMsg
	mov ah, 9h
	int 21h
	pop bp
	ret 2
endp OpenFile

proc CloseFile
mov ah,3Eh
mov bx, [filehandle]
int 21h
ret
endp CloseFile

proc ReadHeader
; Read BMP file header, 54 bytes
	mov ah,3fh
	mov bx, [filehandle]
	mov cx,54
	mov dx,offset Header
	int 21h
	ret
endp ReadHeader

proc ReadPalette
; Read BMP file color palette, 256 colors * 4 bytes (400h)
	mov ah,3fh
	mov cx,400h
	mov dx,offset back_buffer
	int 21h
	ret
endp ReadPalette

proc CopyPal
; Copy the colors palette to the video memory
; The number of the first color should be sent to port 3C8h
; The palette is sent to port 3C9h
	mov si,offset back_buffer
	mov cx,256
	mov dx,3C8h
	mov al,0
; Copy starting color to port 3C8h
	out dx,al
; Copy palette itself to port 3C9h
	inc dx
PalLoop:
; Note: Colors in a BMP file are saved as BGR values rather than RGB.
	mov al,[si+2] ; Get red value.
	shr al,2 ; Max. is 255, but video palette maximal
; value is 63. Therefore dividing by 4.
	out dx,al ; Send it.
	mov al,[si+1] ; Get green value.
	shr al,2
	out dx,al ; Send it.
	mov al,[si] ; Get blue value.
	shr al,2
	out dx,al ; Send it.
	add si,4 ; Point to next color.
; (There is a null chr. after every color.)
	loop PalLoop
	ret
endp CopyPal

proc CopyBitmap
; BMP graphics are saved upside-down.
; Read the graphic line by line (200 lines in VGA format),
; displaying the lines from bottom to top.
	mov ax, 0A000h
	mov es, ax
	mov cx,200
	PrintBMPLoop:
	push cx
; di = cx*320, point to the correct screen line
	mov di,cx
	shl cx,6
	shl di,8
	add di,cx
	; Read one line
	mov ah,3fh
	mov cx,320
	mov dx,offset ScrLine
	int 21h
; Copy one line into video memory
	cld ; Clear direction flag, for movsb
	mov cx,320
	mov si,offset ScrLine

	rep movsb ; Copy line to the screen
 ;rep movsb is same as the following code:
 ;mov es:di, ds:si
 ;inc si
 ;inc di
 ;dec cx
 ;loop until cx=0
	pop cx
	loop PrintBMPLoop
	ret
endp CopyBitmap


proc PrintBmp ;offset of file name as a parameter
; Process BMP file
;---Used Registers--->
	push ax bx cx dx si di es
;-------------------->
	push bp
	mov bp, sp
	push [word ptr bp + 18]
	call OpenFile
	call ReadHeader
	call ReadPalette
	call CopyPal
	call CopyBitmap
	call CloseFile
	pop bp
;---Used Registers--->
	pop es di si dx cx bx ax
;-------------------->
	ret 2
endp PrintBmp

proc PrintBmp2 ;offset of file name as a parameter
; Process BMP file
    push bp
	mov bp, sp
;---Used Registers--->
	push ax bx cx dx si di es
;-------------------->
	push [word ptr bp + 4]
	call OpenFile
	;call ReadHeader
	;call ReadPalette
	;call CopyPal
	call CopyBitmap
	call CloseFile
;---Used Registers--->
	pop es di si dx cx bx ax
;-------------------->
    pop bp
	ret 2
endp PrintBmp2

; clear_screen() -> None
proc clear_screen
    push bp
    mov bp, sp
    pusha

    mov bx, offset back_buffer
    add bx, 320 * 100
    mov ax, offset back_buffer

    mov edx, 01010101h

    sub bx, 4

    clearLoopstart:
        mov [bx], edx
        sub bx, 4
        cmp bx, ax
        jne clearLoopstart

    mov [bx], edx


    mov bx, offset back_buffer
    add bx, 320 * 200
    mov ax, offset back_buffer
    add ax, 320 * 100

    mov edx, 02020202h

    sub bx, 4

    clearLoopstart2:
        mov [bx], edx
        sub bx, 4
        cmp bx, ax
        jne clearLoopstart2

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
            cmp si, 320 * 200
            ja draw_rect_out_of_bounds_skip
                add si, offset back_buffer
                mov dx, [bp+4]
                mov [si], dl
            draw_rect_out_of_bounds_skip:
            cmp ax, 0
            jne draw_rect_line
        cmp cx, 0
        jne draw_rect_collumn

    popa
    pop bp
    ret 10
endp draw_rect

; draw_player() -> None
;proc draw_player
;    push bp
;    mov bp, sp
;    pusha

;    mov ax, [offset player_y] ; player y
;    mov cx, [offset player_x] ; player x
;    mov bx, [offset PLAYER_WIDTH] ; player width
;    mov dx, [offset PLAYER_HEIGHT] ; player height

;    mov di, [offset PLAYER_WIDTH]
;    shr di, 1 ; player_width / 2

 ;   mov si, [offset PLAYER_HEIGHT]
;    shr si, 1 ; player_height / 2

;    sub cx, di
;    sub ax, si

;    push cx ; ->rect x
;    push ax ; ->rect y;
;    push bx ; ->rect width
;    push dx ; ->rect height
;    mov ax, 0Ch ; player color (red)
;    push ax ; ->rect color
;    call draw_rect

    ; player_x + cos(player_angle) * player_speed * scale
;    call cos_angle_times_speed ; cos(player_angle) * player_speed
;    fld [dword ptr DRAW_DIRECTION_SCALE]
;    fmulp ; cos(player_angle) * player_speed * scale
;    fild [word ptr player_x]
;    faddp ; player_x + cos(player_angle) * player_speed * scale
;    fistp [word ptr draw_player_fpu_out]
;    mov bx, [word ptr draw_player_fpu_out]

    ; player_y + sin(player_angle) * player_speed * scale
;    call sin_angle_times_speed ; sin(player_angle) * player_speed
;    fld [dword ptr DRAW_DIRECTION_SCALE]
;    fmulp ; sin(player_angle) * player_speed * scale
;    fild [word ptr player_y]
;    faddp ; player_y + sin(player_angle) * player_speed * scale
;    fistp [word ptr draw_player_fpu_out]
;    mov di, [word ptr draw_player_fpu_out]

;    mov dx, 1 ; half of width/height
;    sub bx, dx
;    sub di, dx

    ; di = player_y + sin(player_angle) * player_speed * scale
    ; bx = player_x + sin(player_angle) * player_speed * scale

;    mov cx, [offset player_x]
;    mov ax, [offset player_y]

    ; ax, bx, di
;    push bx ; ->rect x
;    push di ; ->rect y
;    mov ax, 2 ; width, height
;    push ax ; ->rect width
;    push ax ; ->rect height
;    mov ax, 0Ch ; color (red)
;    push ax ; ->rect color
;    call draw_rect

;    popa
;    pop bp
;    ret
;endp draw_player

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
        cmp ax, 280
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


proc memcpy2
    push bp
    mov bp, sp
    pusha

    mov si, [bp+6]
    mov di, [bp+4]
    mov cx, [bp+8]

    shr cx, 2

    cpy12:
        cmp cx, 0
        je cpy22

        mov eax, [es:si]
        mov [ds:di], eax
        add si, 4
        add di, 4
        dec cx
        jmp cpy12
    cpy22:
        mov ax, [bp+4]

    popa
    pop bp

    ret 6
endp memcpy2

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
        fld [dword ptr player_x]
        call cos_angle_times_speed ; cos(player_angle) * player_speed
        
        faddp ; player_x + cos(player_angle) * player_speed
        fstp [dword ptr player_x]

        ; player_y + sin(player_angle) * player_speed
        fld [dword ptr player_y]
        call sin_angle_times_speed
        
        faddp ; player_y + cos(player_angle) * player_speed
        fstp [dword ptr player_y]

    move_player_w_skip:

    inc bx
    cmp [byte ptr bx], 1
    jne move_player_a_skip

        ;sub [offset player_x], ax

    move_player_a_skip:

    inc bx
    cmp [byte ptr bx], 1
    jne move_player_s_skip
        ; player_x + cos(player_angle) * player_speed
        fld [dword ptr player_x]
        call cos_angle_times_speed ; cos(player_angle) * player_speed
        fsubp ; player_x + cos(player_angle) * player_speed
        fstp [dword ptr player_x]

        ; player_y + sin(player_angle) * player_speed
        fld [dword ptr player_y]
        call sin_angle_times_speed
        fsubp ; player_y + cos(player_angle) * player_speed
        fstp [dword ptr player_y]

    move_player_s_skip:

    inc bx
    cmp [byte ptr bx], 1
    jne move_player_d_skip

        ;add [offset player_x], ax

    move_player_d_skip:

    inc bx
    cmp [byte ptr bx], 1
        jne move_player_dot_skip

        fld [dword ptr offset player_angle]
        fld [dword ptr offset PLAYER_TURN_SPEED]
        faddp

        fld [dword ptr TWO_PI]

        fcomp
        fnstsw ax
        sahf
        ja move_player_comma_skip_reset_angle
            fld [dword ptr TWO_PI]
            fsubp
        move_player_comma_skip_reset_angle:

        fstp [dword ptr offset player_angle]

    move_player_dot_skip:

    inc bx
    cmp [byte ptr bx], 1
        jne move_player_comma_skip

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

    move_player_comma_skip:

    popa
    pop bp
    ret
endp move_player

; cast_ray(iteration, [float] *offset) -> None
proc cast_ray
    push bp
    mov bp, sp
    pusha

    ; zero all variables
    mov bx, offset cast_ray_a_tan
    mov di, offset cast_ray_wall_offset + 4
    cast_ray_zero_loopstart:
        inc bx
        cmp bx, di
        jbe cast_ray_zero_loopstart

    fld [dword ptr offset BIG_NUMBER]
    fld ST(0)

    ; set to big numbers by default
    fstp [dword ptr offset cast_ray_horizontal_ray_length]
    fstp [dword ptr offset cast_ray_vertical_ray_length]

    ; si = dof
    mov si, 0

    mov bx, [bp+4]
    fld [dword ptr offset player_angle]
    fld [dword ptr bx]
    faddp
    fstp [dword ptr offset cast_ray_ray_angle]


    fld [dword ptr ZERO]
    fld [dword ptr offset cast_ray_ray_angle]
    fcompp
    fnstsw ax
    sahf ; cmp ray_angle, 0
    ja cast_ray_ray_angle_not_bellow_zero
        fld [dword ptr offset TWO_PI]
        fld [dword ptr offset cast_ray_ray_angle]
        faddp ; ray_angle + 2 * PI
        fstp [dword ptr offset cast_ray_ray_angle]
    cast_ray_ray_angle_not_bellow_zero:

    fld [dword ptr offset TWO_PI]
    fld [dword ptr offset cast_ray_ray_angle]
    fcompp
    fnstsw ax
    sahf ; cmp ray_angle, 2 * PI
    jb cast_ray_ray_angle_not_above_two_pi
        fld [dword ptr offset cast_ray_ray_angle]
        fld [dword ptr offset TWO_PI]
        fsubp ; ray_angle - 2 * PI
        fstp [dword ptr offset cast_ray_ray_angle]
    cast_ray_ray_angle_not_above_two_pi:

    fld [dword ptr offset cast_ray_ray_angle] ; player_angle
    fptan
    fmulp ; tan(player_angle) #works
    mov bx, 1
    mov [offset cast_ray_fpu_io], bx
    fild [word ptr offset cast_ray_fpu_io]
    fchs ; ST(0) = -1 ST(1) = tan(player_angle)
    fdivrp ; -1 / tan(player_angle)
    fstp [dword ptr offset cast_ray_a_tan]

    fld [dword ptr offset cast_ray_ray_angle]
    fptan
    fmulp ; tan(player_angle)
    fchs ; -tan(player_angle)
    fstp [dword ptr offset cast_ray_negative_tan]

    ; check if player looking up or down
    fldpi
    fld [dword ptr offset cast_ray_ray_angle]
    fcompp
    fnstsw ax
    sahf ; cmp player_angle, PI
    jb cast_ray_horizontal_looking_down
    sahf
    je cast_ray_horizontal_looking_left_or_right
    fld [dword ptr offset ZERO]
    fld [dword ptr offset cast_ray_ray_angle]
    fcompp
    fnstsw ax
    sahf
    je cast_ray_horizontal_looking_left_or_right

    ; cx = jumpX
    ; ax = jumpY

    cast_ray_horizontal_looking_up:
        ;mov ax, [offset player_y]
        fld [dword ptr offset player_y]
        fistp [word ptr offset cast_ray_fpu_io]
        mov ax, [offset cast_ray_fpu_io]

        shr ax, 4 ; change if block size changes
        shl ax, 4 ; change if block size changes
        mov [offset cast_ray_fpu_io], ax
        fild [word ptr offset cast_ray_fpu_io]
        fld [dword ptr offset cast_ray_accuracy_number]
        fsubp ; ((player_y>>6)<<6)-0.0001
        fstp [dword ptr offset cast_ray_ray_y]
        ; #works

        mov bx, offset cast_ray_a_tan

        fld [dword ptr offset player_y]
        fld [dword ptr offset cast_ray_ray_y]
        fsubp ; player_y - ray_y
        fld [dword ptr offset cast_ray_a_tan]
        fmulp ; (player_y - ray_y) * a_tan
        fld [dword ptr offset player_x]
        faddp ; (player_y - ray_y) * a_tan + player_x
        fstp [dword ptr offset cast_ray_ray_x]

        fild [word ptr offset BLOCK_SIZE]
        fchs ; -BLOCK_SIZE
        fst [dword ptr offset cast_ray_jump_y]

        ; ST(0) = jumpY
        fchs ; -jumpY
        fld [dword ptr offset cast_ray_a_tan]
        fmulp ; -jumpY * -1 / tan(ray_angle)
        fstp [dword ptr offset cast_ray_jump_x]

        jmp cast_ray_looking_up_or_down_skip
    cast_ray_horizontal_looking_down:
        ;mov ax, [offset player_y]
        fld [dword ptr offset player_y]
        fistp [word ptr offset cast_ray_fpu_io]
        mov ax, [offset cast_ray_fpu_io]

        shr ax, 4 ; change if block size changes
        shl ax, 4 ; change if block size changes
        add ax, [offset BLOCK_SIZE] ; ((player_y>>6)<<6)+64
        mov [offset cast_ray_fpu_io], ax
        fild [word ptr offset cast_ray_fpu_io]
        fstp [dword ptr offset cast_ray_ray_y]

        fld [dword ptr offset player_y]
        fld [dword ptr offset cast_ray_ray_y]
        fsubp ; player_y - ray_y
        fld [dword ptr offset cast_ray_a_tan]
        fmulp ; (player_y - ray_y) * a_tan
        fld [dword ptr offset player_x]
        faddp ; (player_y - ray_y) * a_tan + player_x
        fstp [dword ptr offset cast_ray_ray_x]

        fild [word ptr offset BLOCK_SIZE]
        ; BLOCK_SIZE
        fst [dword ptr offset cast_ray_jump_y]

        ; ST(0) = jumpY
        fchs ; -jumpY
        fld [dword ptr offset cast_ray_a_tan]
        fmulp ; -jumpY * -1 / tan(ray_angle)
        fstp [dword ptr offset cast_ray_jump_x]

        jmp cast_ray_looking_up_or_down_skip
    
    cast_ray_horizontal_looking_left_or_right:
        fld [dword ptr offset player_x]
        fstp [dword ptr offset cast_ray_ray_x] ; ray_x = player_x
        fld [dword ptr offset player_y]
        fstp [dword ptr offset cast_ray_ray_y] ; ray_y = player_y
        mov si, 8 ; dof = 8
        jmp cast_ray_horizontal_looking_left_or_right_skip
    cast_ray_looking_up_or_down_skip:

    ; ax=free | bx=mapX | cx=free | dx=mapY | di=free | si=dof
    cast_ray_horizontal_loopstart:
        fld [dword ptr offset cast_ray_ray_x]
        fistp [word ptr offset cast_ray_fpu_io] ; (int)ray_x
        mov bx, [offset cast_ray_fpu_io]
        shr bx, 4 ; bx = mapX = ray_x / 16 | change if block size changes
        fld [dword ptr offset cast_ray_ray_y]
        fistp [word ptr offset cast_ray_fpu_io] ;(int)ray_y
        mov dx, [offset cast_ray_fpu_io]
        shr dx, 4 ; dx = mapY = ray_y /16 | change if block size changes

        ; bx = mapX | dx = mapY
        cmp bx, 0
        jl cast_ray_horizontal_out_of_bounds_skip

        cmp dx, 0
        jl cast_ray_horizontal_out_of_bounds_skip

        cmp bx, [word ptr offset MAP_WIDTH]
        jge cast_ray_horizontal_out_of_bounds_skip

        cmp dx, [word ptr offset MAP_HEIGHT]
        jge cast_ray_horizontal_out_of_bounds_skip

        push bx
        push dx
        call at_map
        pop di ; map[y][x]
        cmp di, 0
        jne cast_ray_horizontal_wall_tile

        cast_ray_horizontal_clear_tile:

            fld [dword ptr offset cast_ray_ray_x]
            fld [dword ptr offset cast_ray_jump_x]
            faddp ; ray_x + jump_x
            fstp [dword ptr offset cast_ray_ray_x]

            fld [dword ptr offset cast_ray_ray_y]
            fld [dword ptr offset cast_ray_jump_y]
            faddp ; ray_y + jump_y
            fstp [dword ptr offset cast_ray_ray_y]

            jmp cast_ray_horizontal_in_loop_skip
        cast_ray_horizontal_wall_tile:
            mov si, 8
            jmp cast_ray_horizontal_in_loop_skip
        cast_ray_horizontal_out_of_bounds_skip:
            mov si, 9 ; tell function that it went out of bounds
        cast_ray_horizontal_in_loop_skip:
        inc si

        cmp si, 8
        jb cast_ray_horizontal_loopstart

    cast_ray_horizontal_looking_left_or_right_skip:

    ;mov eax, [offset cast_ray_ray_x]
    ;mov [offset cast_ray_horizontal_ray_x], eax

    ;mov eax, [offset cast_ray_ray_y]
    ;mov [offset cast_ray_horizontal_ray_y], eax
    cmp si, 0Ah ; did it go out of bounds?
    je cast_ray_horizontal_set_skip
    fld [dword ptr offset cast_ray_ray_x]
    fstp [dword ptr offset cast_ray_horizontal_ray_x]

    fld [dword ptr offset cast_ray_ray_y]
    fstp [dword ptr offset cast_ray_horizontal_ray_y]

    fld [dword ptr offset cast_ray_horizontal_ray_x]
    fld [dword ptr offset player_x]
    fsubp ; ray_x - player_x
    fabs ; abs(ray_x - player_x)
    fld ST(0)
    fmulp ; abs(ray_x - player_x) ^ 2

    fld [dword ptr offset cast_ray_horizontal_ray_y]
    fld [dword ptr offset player_y]
    fsubp ; ray_y - player_y
    fabs ; abs(ray_y - player_y)
    fld ST(0)
    fmulp ; abs(ray_y - player_y) ^ 2

    faddp ; abs(ray_x - player_x) ^ 2 + abs(ray_y - player_y) ^ 2
    fsqrt ; sqrt(abs(ray_x - player_x) ^ 2 + abs(ray_y - player_y) ^ 2)
    fstp [dword ptr offset cast_ray_horizontal_ray_length]
    cast_ray_horizontal_set_skip:

    ;--- vertical lines ---;
    mov si, 0

    ; looking left
    fldpi
    fld [dword ptr offset HALF]
    fmulp ; pi * 0.5
    fld [dword ptr offset cast_ray_ray_angle]
    fcompp
    fnstsw ax
    sahf ; cmp player_angle, 0.5 * PI
    jb cast_ray_not_looking_left_skip

    fldpi
    fld [dword ptr offset HALF]
    fmulp ; pi * 0.5
    fldpi
    faddp ; pi * 1.5
    fcompp
    fnstsw ax
    sahf
    ja cast_ray_not_looking_left_skip
        ; mov ax, [offset player_x]
        fld [dword ptr offset player_x]
        fistp [word ptr offset cast_ray_fpu_io]
        mov ax, [offset cast_ray_fpu_io]

        shr ax, 4 ; change if block_size changes
        shl ax, 4 ; change if block size changes
        mov [offset cast_ray_fpu_io], ax
        fild [word ptr offset cast_ray_fpu_io]
        fld [dword ptr offset cast_ray_accuracy_number]
        fsubp ; ((player_x >> 4) << 4) - accuracy_number
        fstp [dword ptr offset cast_ray_ray_x]

        fld [dword ptr offset player_x]
        fld [dword ptr offset cast_ray_ray_x]
        fsubp ; player_x - ray_x
        fld [dword ptr offset cast_ray_negative_tan]
        fmulp ; (player_x - ray_x) * -tan(ray_angle)
        fld [dword ptr offset player_y]
        faddp ; (player_x - ray_x) * -tan(ray_angle) + player_y
        fstp [dword ptr offset cast_ray_ray_y] 

        fild [word ptr offset BLOCK_SIZE]
        fchs ; -BLOCK_SIZE
        fst [dword ptr offset cast_ray_jump_x]

        ; ST(0) = jumpX
        fchs ; -jumpX
        fld [dword ptr offset cast_ray_negative_tan]
        fmulp ; -jumpX * -tan(ray_angle)
        fstp [dword ptr offset cast_ray_jump_y]
    cast_ray_not_looking_left_skip:

    ; looking right
    fldpi
    fld [dword ptr offset HALF]
    fmulp ; pi * 0.5
    fld [dword ptr offset cast_ray_ray_angle]
    fcompp
    fnstsw ax
    sahf ; cmp player_angle, pi * 0.5
    jb cast_ray_looking_right

    fldpi
    fld [dword ptr offset HALF]
    fmulp
    fldpi
    faddp ; pi * 1.5
    fld [dword ptr offset cast_ray_ray_angle]
    fcompp
    fnstsw ax
    sahf ; cmp player_angle, pi * 1.5
    ja cast_ray_looking_right
    jmp cast_ray_not_looking_right
    cast_ray_looking_right:
        ;mov ax, [offset player_x]
        fld [dword ptr offset player_x]
        fistp [word ptr offset cast_ray_fpu_io]
        mov ax, [offset cast_ray_fpu_io]

        shr ax, 4 ; change if block size changes
        shl ax, 4 ; change if block size changes
        add ax, [offset BLOCK_SIZE]
        mov [offset cast_ray_fpu_io], ax
        fild [word ptr offset cast_ray_fpu_io]
        ; ((player_x >> 6) << 6) + BLOCK_SIZE
        fstp [dword ptr offset cast_ray_ray_x]

        fld [dword ptr offset player_x]
        fld [dword ptr offset cast_ray_ray_x]
        fsubp ; player_x - ray_x
        fld [dword ptr offset cast_ray_negative_tan]
        fmulp ; (player_x - ray_x) * -tan(ray_angle)
        fld [dword ptr offset player_y]
        faddp ; (player_x - ray_x) * -tan(ray_angle) + player_y
        fstp [dword ptr offset cast_ray_ray_y]

        fild [word ptr offset BLOCK_SIZE]
        ; BLOCK_SIZE
        fst [dword ptr offset cast_ray_jump_x]

        ; ST(0) = jumpX
        fchs ; -jumpX
        fld [dword ptr offset cast_ray_negative_tan]
        fmulp ; -jumpX * -tan(ray_angle)
        fstp [dword ptr offset cast_ray_jump_y]

    cast_ray_not_looking_right:

    ; draw_rect(ray_x, ray_y, width, height, 5)
    fld [dword ptr offset cast_ray_ray_x]
    fistp [word ptr offset cast_ray_fpu_io]
    mov ax, [offset cast_ray_fpu_io]
    fld [dword ptr offset cast_ray_ray_y]
    fistp [word ptr offset cast_ray_fpu_io]
    mov cx, [offset cast_ray_fpu_io]
    mov bx, [offset PLAYER_WIDTH] ; width
    mov di, 6 ; purple

    fldpi
    fld [dword ptr offset HALF]
    fmulp ; pi * 0.5
    fld [dword ptr offset cast_ray_ray_angle]
    fcompp
    fnstsw ax
    sahf ; cmp player_angle, pi * 0.5
    je cast_ray_vertical_looking_straight_up_or_down

    fldpi
    fldpi
    fld [dword ptr offset HALF]
    fmulp ; pi * 0.5
    faddp ; pi * 1.5
    fld [dword ptr offset cast_ray_ray_angle]
    fcompp
    fnstsw ax
    sahf ; cmp player_angle, pi * 1.5
    je cast_ray_vertical_looking_straight_up_or_down

    jmp cast_ray_vertical_not_looking_straight_up_or_down
    cast_ray_vertical_looking_straight_up_or_down:
        fld [dword ptr offset player_x]
        fstp [dword ptr offset cast_ray_ray_x] ; ray_x = player_x
        fld [dword ptr offset player_y]
        fstp [dword ptr offset cast_ray_ray_y]
        jmp cast_ray_vertical_looking_up_or_down_skip
    cast_ray_vertical_not_looking_straight_up_or_down:

    cast_ray_vertical_loopstart:
        fld [dword ptr offset cast_ray_ray_x]
        fistp [word ptr offset cast_ray_fpu_io] ; (int)ray_x
        mov bx, [offset cast_ray_fpu_io]
        shr bx, 4 ; bx = mapX = ray_x >> 4 ; change if block size changes
        fld [dword ptr offset cast_ray_ray_y]
        fistp [word ptr offset cast_ray_fpu_io] ; (int)ray_y
        mov dx, [offset cast_ray_fpu_io]
        shr dx, 4 ; dx = mapY = ray_y /16 | change if block size changes

        cmp bx, 0
        jl cast_ray_vertical_out_of_bounds_skip

        cmp dx, 0
        jl cast_ray_vertical_out_of_bounds_skip

        cmp bx, [word ptr offset MAP_WIDTH]
        jge cast_ray_vertical_out_of_bounds_skip

        cmp dx, [word ptr offset MAP_HEIGHT]
        jge cast_ray_vertical_out_of_bounds_skip

        push bx
        push dx
        call at_map
        pop di ; map[y][x]
        cmp di, 0
        jne cast_ray_vertical_wall_tile

        cast_ray_vertical_clear_tile:

            fld [dword ptr offset cast_ray_ray_x]
            fld [dword ptr offset cast_ray_jump_x]
            faddp ; ray_x + jump_x
            fstp [dword ptr offset cast_ray_ray_x]

            fld [dword ptr offset cast_ray_ray_y]
            fld [dword ptr offset cast_ray_jump_y]
            faddp
            fstp [dword ptr offset cast_ray_ray_y]

            jmp cast_ray_vertical_in_loop_skip
        cast_ray_vertical_wall_tile:
            mov si, 8
            jmp cast_ray_vertical_in_loop_skip
        cast_ray_vertical_out_of_bounds_skip:
            mov si, 9
        cast_ray_vertical_in_loop_skip:
        inc si

        cmp si, 8
        jb cast_ray_vertical_loopstart

    cast_ray_vertical_looking_up_or_down_skip:

    ;mov eax, [offset cast_ray_ray_x]
    ;mov [offset cast_ray_vertical_ray_x], eax

    ;mov eax, [offset cast_ray_ray_y]
    ;mov [offset cast_ray_vertical_ray_y], eax
    cmp si, 0Ah
    je cast_ray_vertical_set_skip
    fld [dword ptr offset cast_ray_ray_x]
    fstp [dword ptr offset cast_ray_vertical_ray_x]

    fld [dword ptr offset cast_ray_ray_y]
    fstp [dword ptr offset cast_ray_vertical_ray_y]

    fld [dword ptr offset cast_ray_vertical_ray_x]
    fld [dword ptr offset player_x]
    fsubp ; ray_x - player_x
    fabs ; abs(ray_x - player_x)
    fld ST(0)
    fmulp ; abs(ray_x - player_x) ^ 2

    fld [dword ptr offset cast_ray_vertical_ray_y]
    fld [dword ptr offset player_y]
    fsubp ; ray_y - player_y
    fabs ; abs(ray_y - player_y)
    fld ST(0)
    fmulp ; abs(ray_y - player_y) ^ 2

    faddp ; abs(ray_x - player_x) ^ 2 + abs(ray_y - player_y) ^ 2
    fsqrt ; sqrt(abs(ray_x - player_x) ^ 2 + abs(ray_y - player_y) ^ 2)
    fstp [dword ptr offset cast_ray_vertical_ray_length]
    cast_ray_vertical_set_skip:

    ;--------------------drawing--------------------;
    mov ax, 0
    mov [cast_ray_is_vertical_hit], ax
    fld [dword ptr offset cast_ray_vertical_ray_length]
    fld [dword ptr offset cast_ray_horizontal_ray_length]
    fcompp
    fnstsw ax
    sahf ; cmp horizontal_length, vertical_length
    jb cast_ray_horizontal_length_shorter
    cast_ray_vertical_length_shorter:
        ;mov ax, [offset cast_ray_vertical_ray_x]
        ;mov cx, [offset cast_ray_vertical_ray_y]
        ;mov [offset cast_ray_final_ray_x], ax
        ;mov [offset cast_ray_final_ray_y], cx
        
        mov bx, offset cast_ray_vertical_ray_x
        fld [dword ptr offset cast_ray_vertical_ray_x]
        fstp [dword ptr offset cast_ray_final_ray_x]
    
        fld [dword ptr offset cast_ray_vertical_ray_y]
        fstp [dword ptr offset cast_ray_final_ray_y]

        fld [dword ptr offset cast_ray_vertical_ray_length]
        fstp [dword ptr offset cast_ray_final_ray_length]

        mov bx, offset cast_ray_is_vertical_hit
        mov [word ptr offset cast_ray_is_vertical_hit], 1

        jmp cast_ray_shorter_skip
    cast_ray_horizontal_length_shorter:
        ;mov ax, [offset cast_ray_horizontal_ray_x]
        ;mov cx, [offset cast_ray_horizontal_ray_y]
        ;mov [offset cast_ray_final_ray_x], ax
        ;mov [offset cast_ray_final_ray_y], cx

        fld [dword ptr offset cast_ray_horizontal_ray_x]
        fstp [dword ptr offset cast_ray_final_ray_x]

        fld [dword ptr offset cast_ray_horizontal_ray_y]
        fstp [dword ptr offset cast_ray_final_ray_y]

        fld [dword ptr offset cast_ray_horizontal_ray_length]
        fstp [dword ptr offset cast_ray_final_ray_length]
    cast_ray_shorter_skip:


    fld [dword ptr offset cast_ray_final_ray_x]
    fistp [word ptr offset cast_ray_fpu_io]
    mov ax, [offset cast_ray_fpu_io]

    fld [dword ptr offset cast_ray_final_ray_y]
    fistp [word ptr offset cast_ray_fpu_io]
    mov cx, [offset cast_ray_fpu_io]

    ;push ax
    ;push cx
    ;mov dx, 4
    ;push dx
    ;push dx
    ;mov di, 1h
    ;push di
    ;call draw_rect

    ; 04h = dark red
    ; 0Ch = light red


    ; wall_height
    ; ((BLOCK_SIZE * 320) / ray_length) / cos(player_angle - ray_angle)
    mov bx, offset cast_ray_wall_height
    fild [word ptr offset BLOCK_SIZE]
    mov [word ptr offset cast_ray_fpu_io], 320
    fild [word ptr offset cast_ray_fpu_io]
    fmulp ; BLOCK_SIZE * 320

    fld [dword ptr offset cast_ray_final_ray_length]
    fdivp ; (BLOCK_SIZE * 320) / ray_length

    fld [dword ptr offset player_angle]
    fld [dword ptr offset cast_ray_ray_angle]
    fsubp
    fcos ; cos(player_angle - ray_angle)

    fdivp ; ((BLOCK_SIZE * 320) / ray_length) / cos(player_angle - ray_angle)

    mov ax, 2
    mov [offset cast_ray_fpu_io], ax
    fild [word ptr offset cast_ray_fpu_io]
    fdivp
    fstp [dword ptr offset cast_ray_wall_height]

    ; wall_offset
    ; 160 - wall_height / 2
    fld [dword ptr offset cast_ray_wall_height]
    mov [word ptr offset cast_ray_fpu_io], 2
    fild [word ptr offset cast_ray_fpu_io]
    fdivp ; wall_height / 2
    mov [word ptr offset cast_ray_fpu_io], 100
    fild [word ptr offset cast_ray_fpu_io]
    fsubrp ; 160 - wall_height / 2
    fstp [dword ptr offset cast_ray_wall_offset]

    mov ax, [bp+6] ; iteration
    push ax
    mov ax, [offset WALL_WIDTH]
    push ax
    call mult ; i * wall_width <-rect

    fld [dword ptr cast_ray_wall_offset]
    fistp [dword ptr offset cast_ray_fpu_io]
    mov ax, [offset cast_ray_fpu_io]
    push ax ; (int)wall_offset <-rect
    mov ax, [offset WALL_WIDTH]
    push ax ; wall_width <-rect
    fld [dword ptr offset cast_ray_wall_height]
    fistp [dword ptr offset cast_ray_fpu_io]
    mov ax, [offset cast_ray_fpu_io]
    push ax ; wall_height <-rect
    cmp [word ptr offset cast_ray_is_vertical_hit], 0
    je cast_ray_is_vertical_hit_skip1
        mov ax, 0Ch
        jmp cast_ray_is_vertical_hit_skip2
    cast_ray_is_vertical_hit_skip1:
        mov ax, 04h
    cast_ray_is_vertical_hit_skip2:
    push ax
    call draw_rect

    ;-----------------------------------------------;


    popa
    pop bp
    ret 4
endp cast_ray

; cast_sight_rays() -> None
; cast 60 rays from player_angle - 30 degs to 
; player_angle + 30 degs
proc cast_sight_rays
    push bp
    mov bp, sp
    pusha
    mov bx, offset cast_sight_rays_fpu_io ;deleteme

    mov ax, -30
    mov [offset cast_sight_rays_fpu_io], ax
    fild [word ptr offset cast_sight_rays_fpu_io]
    fld [dword ptr offset DEG2RAD]
    fmulp ; -30 * deg2rad
    fstp [dword ptr offset cast_sight_rays_current_angle]
    mov di, 0

    cast_sight_rays_loopstart:
        push di ; iteration
        mov bx, offset cast_sight_rays_current_angle
        push bx
        call cast_ray

        mov [offset cast_sight_rays_fpu_io], ax
        fild [word ptr offset cast_sight_rays_fpu_io]
        fld [dword ptr offset DEG2RAD]
        fmulp ; current_offset * deg2rad
        fstp [dword ptr offset cast_sight_rays_current_angle]
        inc di
        inc ax
        cmp ax, 33
        jle cast_sight_rays_loopstart

    popa
    pop bp
    ret
endp cast_sight_rays

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

    mov di, 0

    yuiop:

    mov ax, offset file_name
    push ax
    call PrintBmp

    menu_loopstart:

        mov bx, offset keyboard_state + 6
        mov al, [bx]
        cmp al, 1
        je main_skip

        mov bx, offset keyboard_state
        add bx, 7 ; space
        cmp [word ptr bx], 1
        jne menu_loopstart

    mov ax, 13h
    int 10h

    main_loopstart:
        call move_player

        call clear_screen

        call cast_sight_rays

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
    
    main_skip:

    mov al, 03h
    int 10h

    popa
    ret
endp main

start:
	mov ax, @data
	mov ds, ax

    mov bx, offset cast_ray_vertical_ray_x
	
    call change_handler_and_run_main

exit:
    mov ax, 4C00h
    int 21h
END start
