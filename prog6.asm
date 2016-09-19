;  ************************************************************* 
;  Chris Fernandez
;  COMSC-260 Spring 2013
;  Date: 4/14/14
;  Assignment #6
;  Did program compile? Yes
;  Did program produce correct results? Yes
;
;  Estimate of time in hours to complete assignment: 10 hours, this took me a looooong loooooooooong 
;     loooooooooooong time....i lost a lot of sleep over this program
;  
;  Short description of what program does: This program takes dword values from two arrays and
;     either multiplies, adds, subtracts, divides, or moduluses them together
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

;*************************CONSTANTS****************************

    LF equ 0Ah                       ;declares a ASCII line feed
    $parm1 equ dword ptr [ebp + 8]   ;stack parameter 1
    $parm2 equ dword ptr [ebp + 12]  ;stack parameter 2

;************************DATA SEGMENT***************************

.data

    titleMsg             byte "Program 6, by Chris Fernandez",LF,LF,0    ;declares a title string
    negativeMsg          byte "***negative overflow error***",LF,0       ;declares a negativeMsg string
    positiveMsg          byte "***positive overflow error***",LF,0       ;declares a positiveMsg string
    multiplicationMsg    byte "***multiplication overflow***",LF,0       ;declares a multiplicationMsg string
    equals               byte  " = ",0                                   ;declares an equals string

    operand1    dword -1062741823,2147483647,2147483647,0,-2147483648,-2147482600, 2147483600,-94567,-2147483648,4352687,-15678 ;declares an operand1 array
    operators	byte	'*','+','%','/','+','-','-','/','-','+','%'                                                             ;declares an operators array
    operand2    dword 2,10,-5625,543,-8,675,-200,383,150,9786,23                                                                ;declares an operand2 array
    ARR_SIZE	equ ($ - operand2)                                                                                              ;declares an ARR_SIZE constant

;************************CODE SEGMENT****************************

.code

main PROC
    
    mPrtStr    titleMsg              ;prints titleMsg string
    mov        esi, 0                ;initializes esi to 0
    mov        ebx, 0                ;initializes ebx to 0

loopTop:
    cmp        esi, ARR_SIZE         ;compars esi to ARR_SIZE constant
    jae        done                  ;if equal or greater jumps to done

    mov        eax, operand1[esi]    ;moves operand1[offset] into eax
    call       DspDword              ;prints a dword dec in eax
    mPrtChar   ' '                   ;prints a space
    mPrtChar   operators[ebx]        ;prints operators[offset]
    mPrtChar   ' '                   ;prints a space
    mov        eax, operand2[esi]    ;moves operand2[offset] into eax
    call       DspDword              ;prints a dword dec in eax
    mPrtStr    equals                ;prints equals string

    push       operand2[esi]         ;pushes operand2[offset] onto the stack
    push       operand1[esi]         ;pushes operand1[offset] onto the stack

    ;ifElse.asm::example 9 - example of series of if/else where only first true statement executes
    ;and the rest of the if/elses will be skipped
    cmp        operators[ebx], '*'   ;compares operator[offset] to '*'
    jne        addThis               ;if not equal jumps to addThis

    call       doMult                ;calls doMult function
    jo         OVARmult              ;if overflow flag jumps to OVARmult
    jmp        printAns              ;jumps to printAns

addThis:                             ;starts jump addThis
    cmp        operators[ebx], '+'   ;compares operator[offset] to '+'
    jne        subThis               ;if not equal jumps to subThis

    call       doAdd                 ;calls doAdd function
    jo         OVAAAR                ;if overflow flag jumps to OVAAAR
    jmp        printAns              ;jumps to printAns

subThis:                             ;starts jump subThis
    cmp        operators[ebx], '-'   ;compares operator[offset] to '-'
    jne        divThis               ;if not equal jumps to disThis

    call       doSub                 ;calls doSub function
    jo         OVAAAR                ;if overflow flag jumps to OVAAAR
    jmp        printAns              ;jumps to printAns

divThis:                             ;starts divThis jump
    cmp        operators[ebx], '/'   ;compares operator[offset] to '/'
    jne        modThis               ;if not equal jumps to modThis

    call       doDiv                 ;calls doDiv function
    jmp        printAns              ;jumps to printAns

modThis:                             ;starts modThis jump
    call       doDiv                 ;calls doDiv function
    mov        eax, edx              ;moves edx into eax
    jmp        printAns              ;jumps to printAns

OVAAAR:                              ;
    test       al, 80h               ;test for 80h for signed value
    jnz        Negative              ;if not zero jumps to Negative

    mPrtStr    positiveMsg           ;prints positiveMsg string
    jmp        next                  ;jumps to next

Negative:                            ;starts Negative jump
    mPrtStr    negativeMsg           ;prints negativeMsg string
    jmp        next                  ;jumps to next

OVARmult:                            ;starts OVARmult jump
    mPrtStr    multiplicationMsg     ;prints multiplicationMsg string
    jmp        next                  ;jumps to next

printAns:                            ;starts jump printAns
    call       DspDword              ;prints a dword dec in eax
    mPrtChar   LF                    ;prints a line feed
    jmp        next                  ;jumps to next

next:                                ;starts next jump
    add        esi, 4                ;adds 4 to esi
    inc        ebx                   ;increases ebx by 1
    jmp        loopTop               ;jumps to the top of the loop

done:                                ;starts jump done
    call   ReadChar                  ;pauses comand prompt screen
    INVOKE ExitProcess, 0            ;closes comand prompt

main ENDP

;************** doMult - signed dword multiplication
;
; ENTRY - operand 1 and operand 2 are on the stack
;
; EXIT - EDX:EAX = result (operand 1 * operand 2)
; (for this assignment the product is assumed to fit in EAX and EDX is ignored)
;
; REGS - EBX, EAX
;
; note: Before calling doMult push operand 2 onto the stack and then push operand 1.
;
; to call doMult in main function:
; push 3 ;32 bit operand2
; push 6 ;32 bit operand1
; call doMult ;6 * 3 = 18 (answer in eax)
;
; Remove parameters by using ret 8 rather than just ret at the end of this function
;--------------

doMult PROC
    push     ebp                    ;pushes ebp on the stack
    mov      ebp, esp               ;save stack pointer

    mov      eax, $parm1            ;moves parm1 into eax
    imul     $parm2                 ;multiplies eax by parm2

    pop      ebp                    ;pops ebp from the stack
    ret      8                      ;returns the function
doMult ENDP


;************** doAdd - dword addition
;
; ENTRY – operand 1 and operand 2 are on the stack
;
; EXIT - EAX = result (operand 1 + operand 2) (any carry is ignored so the answer must fit in 32 bits)
; REGS - EBX, EAX
;
; note: Before calling doAdd push operand 2 onto the stack and then push operand 1.
;
;
; to call doAdd in main function:
; push 13 ;32 bit operand2
; push 7 ;32 bit operand1
; call doAdd ;7 + 13 = 20 (answer in eax)
;
; Remove parameters by using ret 8 rather than just ret at the end of this function
;
;--------------

doAdd PROC
    push    ebp                    ;pushes ebp on the stack
    mov     ebp, esp               ;save stack pointer

    mov     eax, $parm1            ;moves parm1 into eax
    add     eax, $parm2            ;adds parm2 to eax

    pop     ebp                    ;pops ebp from the stack
    ret     8                      ;returns the function
doAdd ENDP


;************** doSub - dword subtraction
;
; ENTRY - operand 1 and operand 2 are pushed on the stack
;
; EXIT -EAX = result (operand 1 - operand 2)
; REGS - EBX, EAX
;
; note: Before calling doSub push operand 2 onto the stack and then push operand 1.
;
; to call doSub in main function:
; push 4 ;32 bit operand2
; push 6 ;32 bit operand1
; call doSub ;6 – 4 = 2 (answer in eax)
;
; Remove parameters by using ret 8 rather than just ret at the end of this function
;--------------

doSub PROC
    push    ebp                    ;pushes ebp on the stack
    mov     ebp, esp               ;save stack pointer

    mov     eax, $parm1            ;moves parm1 into eax
    sub     eax, $parm2            ;subtracts parm2 from eax

    pop     ebp                    ;pops ebp from the stack
    ret     8                      ;returns the function
doSub ENDP


;************** doDiv - signed dword / dword division
;
; ENTRY - operand 1(dividend) and operand 2(divisor) are on the stack
;
; EXIT - EAX = quotient
; EDX = remainder
; REGS - EBX, EAX
;
; note: Before calling doDiv push operand 2(divisor) onto the stack and then push operand 1(dividend).
;
; to call doDiv in main function:
; push 2 ;32 bit operand2 (Divisor)
; push 7 ;32 bit operand1 (Dividend)
; call doDiv ;7 / 2 = 3 R1(3 = quotient in eax,1 = remainder in edx )
;
; Remove parameters by using ret 8 rather than just ret at the end of this function
;--------------

doDiv PROC
    push    ebp                    ;pushes ebp on the stack
    mov     ebp, esp               ;save stack pointer

    mov     eax, $parm1            ;moves parm1 into eax
    cdq                            ;converts double to quad
    idiv    $parm2                 ;divides eax by parm2

    pop     ebp                    ;pops ebp from the stack
    ret     8                      ;returns the function
doDiv ENDP


;************** DspDword - display DWORD in decimal
;
;    ENTRY - eax contains unsigned number to display
;    EXIT - none
;    REGS - EAX, EBX, EDX, ECX, EFLAGS
;
;    To call DspDword: populate eax with number to print then call DspDword
;
;        mov eax, 1234
;        call DspDword
;       
;-------------- Input Parameters

DspDword PROC
    push     ebp                    ;save ebp to stack
    mov      ebp, esp                ;save stack pointer
    sub      esp, 12                ;allocate 12 bytes for byte array
                                    ;note: must be an increment of 4
                                    ;otherwise WriteChar will  not work
    push     ecx                    ;save ecx to stack
    push     ebx                    ;save ebx to stack
    push     edx                    ;save edx to stack
    push     eax                    ;save eax to stack

    mov      ecx, -1                 ;ecx = offset of beginning of byte array from ebp 
    mov      ebx, 10                 ;ebx = divisor for peeling off decimal digits

    ;Modification for negative numbers
    OR         eax, eax              ;find first digit
    jns        next_digit            ;
    mPrtChar   '-'                   ;
    neg        eax                   ;

;each time through loop peel off one digit (division by 10),
;convert the digit to ascii and store the digit in the stack
;in reverse order: 1234 stored as 4321

next_digit:
    mov      edx, 0                 ; before edx:eax / 10, set edx to 0 
    div      ebx                    ; eax = quotient = dividend shifted right
                                    ; edx = remainder = digit to print.
                                    ; next time through loop quotient becomes
                                    ; new dividend.
    add      dl, '0'                ; convert digit to ascii character: i.e. 1 becomes '1'
    mov      [ebp + ecx], dl          ; Save converted digit to buffer on stack
    dec      ecx                    ; Move down in stack to next digit position
    cmp      ecx, -10               ; only process 10 digits
    jge      next_digit             ; repeat until 10 digits on stack
                                    ; since working with a signed number, use jge not jae

    inc      ecx                    ; when above loop ends we have gone 1 byte too far
                                    ; so back up one byte

;loop to display all digits stored in byte array on stack
DISPLAY_NEXT_DIGIT:      
    cmp      ecx, -1                ; are we done processing digits?
    jg       DONE10                 ; repeat loop as long as edi <= -1
    mPrtChar byte ptr[ebp + ecx]    ; print digit
    inc      ecx                    ; ecx = ecx + 1: if ecx = -10 then ecx + 1 = -9
    jmp      DISPLAY_NEXT_DIGIT     ; repeat

DONE10:
    pop      eax                    ; eax restored to original value
    pop      edx              ; edx restored to original value
    pop      ebx              ; ebx restored to original value
    pop      ecx              ; ecx restored to original value

    mov      esp, ebp               ;restore stack pointer which removes local byte array
    pop      ebp                    ;restore ebp to original value
    ret
DspDword endp

END main