
; Constants
UP_BUTTON       = $0800
DOWN_BUTTON     = $0400
LEFT_BUTTON     = $0200
RIGHT_BUTTON    = $0100
A_BUTTON        = $0080
B_BUTTON        = $8000

DIR_BUTTON      = LEFT_BUTTON | RIGHT_BUTTON | DOWN_BUTTON

INPUT_STATUS_ROTATE     = $8000
INPUT_STATUS_COUNTER    = $4000
INPUT_STATUS_D_HELD     = $2000
INPUT_STATUS_D_PRESSED  = $1000

D_PAD_PRESSED   = $1000
D_PAD_HELD      = $2000

DAS_INITIAL_DELAY  = $0010
DAS_REPEAT      = $0006

.BSS
PrevInput:      .res 2
DasTimer:         .res 2

.CODE

.proc InitInput
    .a16

    stz PrevInput

    rts
.endproc


.proc DoInput
    .a16
    
    pha             ; InputStatus
    pha             ; Move Delta

    jsr ReadInput

    ; Handle rotation.
    lda $03, S      ; Load input status
    bit #INPUT_STATUS_ROTATE
    bne Rotate
    jmp RotateEnd

    ; TODO: Counter rotation.

Rotate:
    jsr TryRotate

RotateEnd:

    ; Handle translation.
    lda $03, S      ; Load input status
    bit #INPUT_STATUS_D_PRESSED
    bne DoPressed
    
    bit #INPUT_STATUS_D_HELD
    bne DoHeld

    jmp TranslateEnd

DoPressed:
    jsr TryMove             ; Try to move by the delta already on the stack.

    lda #DAS_INITIAL_DELAY  ; Reset DAS timer to initial delay.
    sta DasTimer
    jmp TranslateEnd

DoHeld:
    lda DasTimer        ; If the button is held and the DAS timer expired...
    beq DoDas           ; do the DAS thing.

    dec                 ; Otherwise decrement the timer and continue.
    sta DasTimer
    jmp TranslateEnd

DoDas:
    jsr TryMove         ; Try to move by the delta already on the stack.
    lda #DAS_REPEAT     ; Reset the DAS timer to the "repeat" interval.
    sta DasTimer

TranslateEnd:
    lda JOY1L                       ; Read controller status
    sta PrevInput                   ; Store it in PrevInput

    pla                             ; Clean up the stack.
    pla
    rts

.endproc

; Reads input from the joypad and does some processing.
;
; Stack:
; - $05  Input Status out (See INPUT_STATUS_*)
; - $03  Effective Move delta out.
.proc ReadInput
    .a16
    InputStatus = $05
    MoveDelta  = $03

    lda #$0000                      ; Initialize outputs.
    sta InputStatus, S
    sta MoveDelta, S
    
    ; check whether joypad is ready
WaitForJoypad:
    lda HVBJOY                      ; get joypad status
    and #$0001                      ; check whether joypad still reading...
    bne WaitForJoypad               ; ...if not, wait a bit more

    ; First read the rotation status.

    lda PrevInput                   ; Load last frame's status.
    eor #$FFFF                      ; bitwise NOT
    and JOY1L                       ; A = current status - last frame's status (i.e., new buttons)

    bit #B_BUTTON
    bne Rotate
    bit #A_BUTTON
    bne Counter
    jmp RotationEnd

Rotate:
    lda InputStatus, S
    ora #INPUT_STATUS_ROTATE
    sta InputStatus, S
    jmp RotationEnd

Counter:
    lda InputStatus, S
    ora #INPUT_STATUS_COUNTER
    sta InputStatus, S

RotationEnd:
    ; Next, see which direction we're moving.

    lda JOY1L                       ; Read controller status

    bit #DIR_BUTTON                 ; Check if any direction button is pressed.
    beq End                         ; If not, we're done.

    ; Set a move delta based on which button is pushed.
    bit #LEFT_BUTTON
    bne LeftPressed

    bit #RIGHT_BUTTON
    bne RightPressed

    ; Down must have been pressed.
    lda #$0020
    jmp DeltaInA

RightPressed:
    lda #$0001
    jmp DeltaInA

LeftPressed:
    lda #$FFFF

DeltaInA:
    sta MoveDelta, S

    ; Next, see if we're pressing or holding.

    lda PrevInput                   ; Check if any was pressed last frame.
    bit #DIR_BUTTON
    beq NotHolding

    lda InputStatus, S              ; Indicate holding.
    ora #INPUT_STATUS_D_HELD
    sta InputStatus, S

    jmp End

NotHolding:
    lda InputStatus, S              ; Indicate pressing.
    ora #INPUT_STATUS_D_PRESSED
    sta InputStatus, S

End:
    lda JOY1L                       ; Read controller status
    sta PrevInput                   ; Store it in PrevInput
    rts
.endproc
