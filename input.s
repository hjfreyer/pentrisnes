
; Constants
UP_BUTTON       = $0800
DOWN_BUTTON     = $0400
LEFT_BUTTON     = $0200
RIGHT_BUTTON    = $0100

DIR_BUTTON      = LEFT_BUTTON | RIGHT_BUTTON | DOWN_BUTTON

INPUT_ROTATE    = $8000
INPUT_COUNTER   = $4000
INPUT_D_HELD    = $2000


D_PAD_PRESSED   = $1000
D_PAD_HELD      = $2000

; PRE_HELD        

DAS_INITIAL_DELAY  = $0010
DAS_REPEAT      = $0006

.BSS
PrevInput:      .res 2
; ButtonHeld      .res 2
DasTimer:         .res 2
; DasRepeat       .res 2

.CODE

.proc InitInput
    .a8

    stz PrevInput
    ; stz ButtonHeld

    ; lda 

    rts
.endproc


.proc DoInput
    .a16

    ; read joypad 1
    ; check whether joypad is ready
WaitForJoypad:
    lda HVBJOY                      ; get joypad status
    and #$0001                      ; check whether joypad still reading...
    bne WaitForJoypad               ; ...if not, wait a bit more

    lda JOY1L                       ; Read controller status

    bit #DIR_BUTTON                  ; Check if any direction button is pressed.
    beq MoveEnd

    ; Push a move delta onto the stack based on which button is pushed.
    bit #LEFT_BUTTON
    bne LeftPressed

    bit #RIGHT_BUTTON
    bne RightPressed

    ; Down must have been pressed.
    pea $20
    jmp DeltaPushed

RightPressed:
    pea $0001
    jmp DeltaPushed

LeftPressed:
    pea -1
    
DeltaPushed:
    lda PrevInput                   ; Check if any was pressed last frame.
    bit #DIR_BUTTON
    beq NewlyPressed

    ; If here, a dir button is being held.
    lda DasTimer
    beq DoDas

    dec
    sta DasTimer

    pla

    jmp MoveEnd

DoDas:
    jsr TryMove
    pla

    lda #DAS_REPEAT
    sta DasTimer

    jmp MoveEnd

NewlyPressed:
    jsr TryMove
    pla

    lda #DAS_INITIAL_DELAY
    sta DasTimer


MoveEnd:
    lda JOY1L                       ; Read controller status
    sta PrevInput                   ; Store it in PrevInput



    ; lda PrevInput                   ; Load last frame's status.
    ; eor #$ffff
    ; and $01, S                      ; A = current status - last frame's status (i.e., new buttons)

    ; bit #RIGHT_BUTTON               ; If right is pressed, move right, etc.
    ; bne MoveRight

    ; lda #INIT_DAS_DELAY


; MoveRight:


        ; bit #LEFT_BUTTON
        ; bne MoveLeft

        ; bit #DOWN_BUTTON
        ; bne MoveDown

        ; bit #UP_BUTTON
        ; bne MoveUp

        ; jmp MoveEnd

; MoveLeft:
;         pea -1
;         jsr TryMove
;         pla
;         jmp MoveEnd

; MoveRight:
;         pea $0001
;         jsr TryMove
;         pla
;         jmp MoveEnd

; MoveDown:
;         pea $20
;         jsr TryMove
;         pla
;         jmp MoveEnd

; MoveUp:
;         jsr TryRotate
;         jmp MoveEnd

; MoveEnd:

    ; pla                             ; Pop controller input
    ; sta PrevInput                   ; Store it in PrevInput
    rts

.endproc
