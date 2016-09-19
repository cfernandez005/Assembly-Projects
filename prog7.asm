;  ************************************************************* 
;  Chris Fernandez
;  COMSC-260 Spring 2013
;  Date: 4/28/14
;  Assignment #7
;  Did program compile? Yes
;  Did program produce correct results? Yes
;
;  Estimate of time in hours to complete assignment: 12 seasons and a fort night tis was
;                                                      a hardy one indeed
;  
;  Short description of what program does: Takes values from a bitmap, prints them, and then
;    performs calulations with its isolated bits
;     
;  *************************************************************


.386                     ;identifies minimum CPU for this program

.MODEL flat,stdcall      ;flat - protected mode program
                         ;stdcall - enables calling of MS_windows programs

;allocate memory for stack
;(default stack size for 32 bit implementation is 1MB without .STACK directive 
;  - default works for most situations)

.STACK 4096              ;allocate 4096 bytes (1000h) for stack

;Irvine32.lib contains the code for DumpRegs and many other Irvine
;functions

INCLUDELIB /Irvine/Irvine32.lib

;***************************MACROS*******************************

mPrtStr  MACRO  arg1              ;arg1 is replaced by the name of string to be displayed
		 push   edx				  ;save edx
         mov    edx, offset arg1  ;address of str to display should be in edx
         call   WriteString       ;display 0 terminated string
         pop    edx				  ;restore edx
ENDM

mPrtChar MACRO arg1               ;arg1 is replaced by the character to be displayed
         push eax                 ;save eax
         mov al, arg1             ;char to print in al
         call WriteChar           ;print char to console
         pop eax                  ;restore eax
ENDM

;*************************PROTOTYPES*****************************

ExitProcess PROTO, dwExitCode:DWORD  ;from Win32 api not Irvine

ReadChar PROTO                       ;Irvine code for getting a single char from keyboard
				                     ;Character is stored in the al register.
			                         ;Can be used to pause program execution until key is hit.

WriteString PROTO                    ;Irvine code to write null-terminated string to output
                                     ;EDX points to string

WriteChar PROTO                      ;write the character in al to the console

WriteDec PROTO                       ;converts the value stored in EAX into a decimal and prints

;*************************CONSTANTS****************************

    LF equ 0Ah                          ;declares a ASCII line feed
	operand1Pos     equ 0               ;operand 1 bit starting position constant
	operandBits     equ 14              ;operand size in bits constant
	operand2Pos     equ 18              ;operand 2 bit starting position constant
	operatorPos     equ 14              ;operator sign bit starting position constant
	operatorBits    equ 4               ;operator sign size in bits constant
    $parm1 equ dword ptr [ebp + 8]      ;stack parameter 1
    $parm2 equ dword ptr [ebp + 12]     ;stack parameter 2

;************************DATA SEGMENT***************************

.data

    titleMsg           byte "Program 7, by Chris Fernandez",LF,LF,0    ;declares a title string

                      ;|<----op1--->||op||<----op2--->|
                      ;01234567890123456789012345678901
    bitmap    dword    10000001001111010111101000001101b        ;declares values of a bitmap
	          dword    11001011101011010100000010001101b
			  dword    01011010101111010111001010001001b
			  dword    01001010101111010100000010101001b
			  dword    00000000000000010111111110011001b

			  dword    11111111111111101011111111111111b
			  dword    00000000001101101000000000000001b
			  dword    00000000000000101000000000000000b
			  dword    00000000000000101000001010111101b
			  dword    11000101000101101000000000000000b

			  dword    10000010001111111111101010001001b
			  dword    11001010101111111100000010001001b
			  dword    00000000000000111111111110010001b

			  dword    11111111111111110100000011111011b
			  dword    11110011110101110101111100111011b

			  dword    10000000001101101110000000000101b
			  dword    11111111111111101111111111111111b
    ARR_SIZE	equ ($ - bitmap)                                ;an ARR_SIZE constant of bitmap

;************************CODE SEGMENT****************************

.code

main PROC
    mPrtStr    titleMsg              ;prints titleMsg string
    mov        esi, 0                ;initalizes esi to 0
    mov        edi, 0                ;initalizes edi to 0

loopTop:                             ;begins while loop
    cmp        esi, ARR_SIZE         ;compares esi to bitmap ARR_SIZE constant
    jae        done                  ;if above or equal, jumps to done
    push       bitmap[edi]           ;pushes bitmap value at index esi onto the stack
    call       DspBin                ;calls function DspBin
    mPrtChar   LF                    ;prints a line feed
    
    mov        eax, bitmap[edi]      ;moves bitmap at index edi into eax
    mov        cl,  operand1Pos      ;populates cl with operand1Pos constant
    mov        ch,  operandBits      ;populates ch with operandBits constant
    call       GetBits               ;calls function GetBits
    call       WriteDec              ;prints value in eax as a decimal
	mov        ebx, eax              ;saves eax in ebx

    mPrtChar   ' '                   ;prints a space
    mov        eax, bitmap[edi]      ;moves bitmap at index edi into eax
    mov        cl,  operatorPos      ;populates cl with operatorPos constant
    mov        ch,  operatorBits     ;populates ch with operatorBits constant
    call       GetBits               ;calls function GetBits
    or         al, 20h               ;signs the 6th bit in al
    call       WriteChar             ;prints the value stored in al as a character
    mov        edx, eax              ;saves eax in edx

    mPrtChar   ' '                   ;prints a space
    mov        eax, bitmap[edi]      ;moves bitmap at index edi into eax
    mov        cl,  operand2Pos      ;populates cl with operand2Pos constant
    mov        ch,  operandBits      ;populates ch with operandBits constant
    call       GetBits               ;calls function GetBits
    call       WriteDec              ;prints value in eax as a decimal
	mov        ecx, eax              ;saves eax in ecx
    mPrtChar   ' '                   ;prints a space
    mPrtChar   '='                   ;prints an equals sign
    mPrtChar   ' '                   ;prints a space

    cmp        dl, 100101b           ;compares dl to 100101b value
    jne        multThis              ;if not equal, jumps to multThis

	push       ecx                   ;pushes ecx onto the stack
    push       ebx                   ;pushes ebx onto the stack
	call       doDivShiftBits        ;calls doDivShiftBits function
	mov        eax, edx              ;populates eax with edx
    jmp        next                  ;jumps to next

multThis:                            ;begins multThis jump
    cmp        dl, 101010b           ;compares dl to 101010b value
    jne        divThis               ;if not equal, jumps to divThis
    
    push       ebx                   ;pushes ebx onto the stack
    push       ecx                   ;pushes ecx onto the stack
    call       doMultShiftBits       ;calls doMultShiftBits function
    jmp        next                  ;jumps to next

divThis:                             ;begins divThis jump
    cmp        dl, 101111b           ;compares dl to 101111b value
    jne        subThis               ;if not equal, jumps to subThis

	push       ecx                   ;pushes ecx onto the stack
    push       ebx                   ;pushes ebx onto the stack
	call       doDivShiftBits        ;calls doDivShiftBits function
    jmp        next                  ;jumps to next

subThis:                             ;begins subThis jump
    cmp        dl, 101101b           ;compares dl to 101101b value
    jne        addThis               ;if not equal, jumps to addThis

    sub        ebx, ecx              ;subtracts ecx from ebx
    mov        eax, ebx              ;moves ebx into eax
    jmp        next                  ;jumps to next

addThis:                             ;begins addThis jump
    add        ebx, ecx              ;adds ecx to ebx
	mov        eax, ebx              ;moves ebx into eax
    jmp        next                  ;jumps to next

next:                                ;begins next jump
    call       WriteDec              ;prints calculated answer as a decimal
    mPrtChar   LF                    ;prints a line feed
    mPrtChar   LF                    ;prints a line feed
    add        esi, 4                ;increases esi by 4
    add        edi, 4                ;increases edi by 4
    jmp        loopTop               ;jumps to loopTop

done:
    call   ReadChar                  ;pauses comand prompt screen
    INVOKE ExitProcess, 0            ;closes comand prompt

main ENDP


;************** DspBin - display a Dword in binary including leading zeros
;
; ENTRY –operand1, the number to print in binary, is on the stack
;
; For Example if parm1 contained contained AC135h the following would print:
; 00000000000010101100000100110101
; For Example if parm1 contained 000Ah the following would print:
; 00000000000000000000000000001010
; EXIT - None
; REGS - EAX,EBX,ECX,FLAGS
;
; to call DspBin:
; push 1111000110100b ;number to print in binary is on the stack
; call DspBin ; 00000000000000000001111000110100 should print
; Note: leading zeros do print
; Note; at the end of this function use ret 4 (instead of just ret) to remove the parameter from the stack
; Do not use add esp, 4 in the main function.
;--------------

DspBin Proc
    push    ebp                  ;pushes ebp on the stack
    mov     ebp, esp             ;saves the stack pointer
	push    ecx                  ;pushes ecx on the stack
	push    ebx                  ;pushes ebx on the stack
    
    mov     ecx, 32              ;populates ecx with 32
    mov     ebx, $parm1          ;populates ebx with parm1
loopTop:                         ;begins loop loopTop
    mov     al, 0                ;populates al with 0
    shl     ebx, 1               ;shifts ebx by one bit to the left
    rcl     al, 1                ;rotates the carry flag into al from the left
    or      al, 30h              ;signs bit 110000 in al
    call    WriteChar            ;prints value in al as a char
    loop    loopTop              ;decreases ecx by 1 and jumps to loopTop

	pop     ebx                  ;pops ebx from the stack
	pop     ecx                  ;pops ecx from the stack
    pop     ebp                  ;pops ebp from the stack
    ret     4                    ;removes parm1 from the stack and returns to main
DspBin ENDP


;************** doMultShiftBits – Dword multiplication by shifting and adding
;
; ENTRY - operand 1 is on the stack
; operand 2 is on the stack
; EXIT - EAX = result (EAX = op1 * op2)
; REGS - EAX,EBX,EDX,FLAGS
;
; doMultShiftBits will multiply 2 Dwords together using shifting and addition.
;
; Note: mul or imul is not to be used. If you use mul or imul no credit will
; be given for this function.
;
; Note: the product is assumed to fit in eax and no error checking is done.
;
; note: Before calling doMultShiftBits push operand 2 onto the stack and then push operand 1.
; After calling doMultShiftBits remove the parameters from the stack by adding 8 to esp
; to call doMultShiftBits
; push 15 ;32 bit operand2
; push 3 ;32 bit operand1
; call doMultShiftBits ;15 * 3 = 45 (answer in eax)
;
; Note; at the end of this function use ret 8 (instead of just ret) to remove the parameters from the stack.
; Do not use add esp, 8 in the main function.
;--------------

doMultShiftBits Proc
    push    ebp                      ;pushes ebp on the stack
    mov     ebp, esp                 ;saves the stack pointer
	push    ebx                      ;pushes ebx on the stack
	push    ecx                      ;pushes ecx on the stack
    
    mov     ebx, $parm2              ;populates ebx with parm2
    mov     ecx, $parm1              ;populates ecx with parm1
    mov     eax, 0                   ;initalizes eax to 0
looptop:                             ;begins loop loopTop
    cmp     ecx, 0                   ;compares ecx to 0
    je      done                     ;if equal, jumps to done

    test    cl, 0b                   ;test the value ot the right most bit in cl
    jnz     continue                 ;if difference not zero, jumps to continue
    add     eax, ebx                 ;adds ebx to eax
    jmp     continue                 ;jumps to continue

continue:                            ;begins continue jump
    shr     ecx, 1                   ;shifts ecx by one bit to the right
    shl     ebx, 1                   ;shifts ebx by one bit to the left
    jmp     loopTop                  ;jumps to loopTop

done:                                ;begins done jump
    pop     ecx                      ;pops ecx from the stack
    pop     ebx                      ;pops ebx from the stack
    pop     ebp                      ;pops ebp from the stack
    ret     8                        ;removes both parm 1 and 2 from the stack and returns to main
doMultShiftBits ENDP


;************** doDivShiftBits – unsigned Dword division using shifting and subtraction
;
; ENTRY – operand 1 is on the stack (dividend)
; operand 2 is on the stack (divisor)
; EXIT - EAX = quotient EDX = remainder
; REGS - EAX,EBX,ECX,EDX,EDI,FLAGS
;
; doDivShiftBits will divide 2 unsigned Dwords using shifting and subtraction.
;
; Note: idiv or div is not to be used. If you use idiv or div no credit will
; be given for this function.
;
; Note: this function checks for divide by 0 problem as outlined below.
;
; Note: The quotient is assumed to fit in eax.
;
; note: this function uses the method outlined in divShift.pdf to do division using shifting and subtraction
;
; note: Before calling doDivShiftBits push operand 2(divisor) onto the stack and then push operand 1(dividend).
;
; to call doDivShiftBits
; push 4 ;32 bit operand2 (Divisor)
; push 23 ;32 bit operand1 (Dividend)
; call doDivShiftBits ;23 /4 = 5 R3(5 = quotient in eax,3 = remainder in edx )
;
; Note; at the end of this function use ret 8 (instead of just ret) to remove the parameters from the stack.
;--------------

doDivShiftBits Proc
    push    ebp                       ;pushes ebp on the stack
    mov     ebp, esp                  ;saves the stack pointer
    push    ebx                       ;pushes ebx on the stack
	push    ecx                       ;pushes ecx on the stack
	push    edi                       ;pushes edi on the stack

	mov     edi, $parm2               ;populates edi with parm2
	mov     ebx, $parm1               ;populates ebx with parm1
	mov     ecx, 32                   ;populates ecx with 32
	mov     edx, 0                    ;initalizes edx to 0 
	mov     eax, 0                    ;initalizes eax to 0

	cmp     edi, 0                    ;compares edi to 0
	jnz     loopTop                   ;if difference not zero, jump to loopTop
	int     0                         ;thows out a "divide by zero" error

loopTop:                              ;begins loop loopTop
    cmp     ecx, 0                    ;compares ecx to 0
	jz      done                      ;if difference zero, jumps to done

    shl     ebx, 1                    ;shift ebx by one bit to the left
	rcl     edx, 1                    ;rotates the carry flag into edx from the left
	shl     eax, 1                    ;shifts eax by one bit to the left
	cmp     edi, edx                  ;compares edi and edx
	jbe     subTemp                   ;if below or equal, jumps to subTemp
	dec     ecx                       ;decreases ecx by 1
	jmp     loopTop                   ;jumps to loopTop

subTemp:                              ;begins subTemp jump
    sub     edx, edi                  ;subtracts edi from edx
	or      eax, 1b                   ;signs bit one in eax
	dec     ecx                       ;decreases ecx by 1
	jmp     loopTop                   ;jumps to loopTop
    
done:                                 ;begins done jump
	pop     edi                       ;pops edi from the stack
	pop     ecx                       ;pops ecx from the stack
	pop     ebx                       ;pops ebx from the stack
    pop     ebp                       ;pops ebp from the stack
    ret     8                         ;removes both parm 1 and 2 from the stack and returns to main
doDivShiftBits ENDP


;******************** GetBits – isolate and extract bits from a 32 bit bitmap
; ENTRY – EAX = bitmap to extract bits from
; CH = number of bits to extract
; CL = starting bit position of bits to extract
; EXIT - EAX = extracted bits
; REGS - EAX,ECX

GetBits Proc
    push    ecx                ;save ecx register

    shl     eax, cl            ;shift bits to isolate to left edge
    mov     cl, 32             ;total bits to work with is 32
    sub     cl, ch             ;how far to shift right? 32 minus number of bits to extract
    shr     eax, cl            ;shift bits to extract to right edge - 
                               ;fills in with zeros on the left isolating the bits we wish to extract

    pop     ecx                ;restore ecx register
    ret                        ;returns to main
GetBits ENDP


END main