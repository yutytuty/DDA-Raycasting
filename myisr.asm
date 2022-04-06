; Author - Maryanovsky Alla
; My own keyboard interrupt + test
; You may press a s, e, d buttons for move the star on the display
IDEAL
MODEL small
STACK 100h

DATASEG

CODESEG
;=====================================================================
;   Start code
;=====================================================================
;
start:
  	mov ax, @data                    ; start address of segment data
	mov ds, ax
	call change_handler              ; put my own keyboard interrupt
	jmp exit
proc delay
    push si
	push cx
	
	mov si, 0FFFFH
odd:
    mov cx, 5H
odin:
    loop odin
	dec si
	jnz odd
	
	pop cx
	pop si
	ret
endp delay	
;============================================================================	
;      Procedures
;============================================================================	
proc put_star
; paints star on the display
    push ax

	mov ah, 0Ah             ; color        
	mov al, '*'            ; we'll put one star on the screen
	
	mov [es:di], ax        ; [es:di] - logical address; es*16 + di = 20 bit physical address	
	
	pop ax
	ret
endp put_star
	
proc clear_star
; clear star from the display
    push ax

	mov ah, 0              ; color        
	mov al, ' '            ; we'll put one star on the screen
	
	mov [es:di], ax        ; [es:di] - logical address; es*16 + di = 20 bit physical address	
	
	pop ax
	ret
endp clear_star

;======================================================================	
proc change_handler
    xor     ax, ax
    mov     es, ax

    cli                              ; interrupts disabled
    push    [word ptr es:9*4+2]      ; save old keyboard (9) ISR address - interrupt service routine(ISR)
    push    [word ptr es:9*4]
	                                 ; put my keyboard (9) ISR address: procedure irq1isr
    mov     [word ptr es:9*4], offset my_isr
	                                 ; put cs in ISR address
    mov     [es:9*4+2],        cs
    sti                               ; interrupts enabled

    call    my_program                     ; program that use the interrupt  lines 43 - 83

    cli                               ; interrupts disabled
    pop     [word ptr es:9*4]         ; restore ISR address
    pop     [word ptr es:9*4+2]
    sti                               ; interrupts enabled

    ret
endp change_handler	
;=====================================================================
proc my_program   
; main program
; moves the star
; 'a' - down, 'w' - left, 'e' - up, 's' - right
	mov ax, es                        ; save es
	push ax

	mov ax, 0b800h                    ; start address of text video memory
	                                  ; 80 columns * 25 rows * 2 bytes per character:
						              ; low byte = character code; high byte = attribute (background+color)
	mov es, ax
	
	mov di,  (13*80+39)*2             ; address on the middle of display
	

main_loop:                            ; none end loop: scan array kbdbuf
	call put_star  
    call delay	
    call clear_star	
	call delay
	
    mov si,0
	mov cx,4
	
check_buttons:
    cmp [byte ptr cs:esc_key], 0       ; if clicked ?
	jne toret                          ; yes ---> end the program
    mov     al, [cs:kbdbuf + si]       ;scan array of clickes

	cmp al,0
	je cont
toright:	
	cmp si,3
	jne toup
	add di,2
	jmp cont
toup:
    cmp si,2
    jne toleft
    sub di, 160	
	jmp cont
toleft:
    cmp si,1
    jne todown
    sub di, 2
	jmp cont
todown:	
    cmp si,0
    jne cont
    add di, 160	
cont:	
	inc si
	loop check_buttons
    jmp main_loop
    

toret:
    pop ax
	mov es, ax
    ret
endp my_program
;==============================================================

;==============================================================
proc my_isr               
 ; my isr for keyboard   
	push    ax
	push    bx
    push    cx
    push    dx
	push    di
	push    si
        

                        ; read keyboard scan code
    in      al, 60h

                        ; update keyboard state
    xor     bh, bh
    mov     bl, al
    and     bl, 7Fh     ; bx = scan code
	cmp bl, 11h         ; if click on w (index 1 in array kbdbuf)
	jne check1
	mov bl,2
	jmp end_check
	
check1:
	cmp bl, 1eh		    ; if click on a (index 0 in array kbdbuf)
	jne check2
	mov bl,1
	jmp end_check
	
check2:
	cmp bl, 20h		    ; if click on d (index 2 in array kbdbuf)
	jne check3
	mov bl,3
	jmp end_check
	
check3:
	cmp bl, 1fh		    ; if click on s (index 3 in array kbdbuf)
	jne check4
	mov bl,0
	jmp end_check
	
check4:
    cmp bl, 1h		    ; if click on esc
	jne end_check
	mov [byte ptr cs:esc_key], 1
	
end_check:
    push cx
	mov cx, 7
    shr al, cl              ; al = 0 if pressed, 1 if released
	pop cx
    xor al, 1               ; al = 1 if pressed, 0 if released
    mov     [cs:kbdbuf+bx], al  ; save pressed buttons in array kbdbuf
	
	
                                ; send EOI to XT keyboard
    in      al, 61h
    mov     ah, al
    or      al, 80h
    out     61h, al
    mov     al, ah
    out     61h, al

                                ; send EOI to master PIC
    mov     al, 20h
    out     20h, al
	
    pop     si
    pop     di                       ;
    pop     dx
    pop     cx
    pop     bx
    pop     ax
   
    iret
endp my_isr
;==========================================================
; kbdbuf - array of 4 numbers: index 0 - down, index 1 - left, index 2 - up, index 3 - right
 ; numbers value 1 - key pressed
;==========================================================
kbdbuf      db 4 dup (0)
esc_key  db 0
exit:
   
	mov ax, 4c00h
	int 21h
END start