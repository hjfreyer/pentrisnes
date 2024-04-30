; -----------------------------------------------------------------------------
;   File: game.s
;   Description: Main game logic.
; -----------------------------------------------------------------------------

.include "mmap.s"
.include "macros.s"

;----- Assembler Directives ----------------------------------------------------
.p816                           ; tell the assembler this is 65816 code
.smart
;-------------------------------------------------------------------------------

.include "globals.s"
.include "init.s"
.include "header.s"

.CODE

;-------------------------------------------------------------------------------
;   After the ResetHandler will jump to here
;-------------------------------------------------------------------------------
.proc   GameLoop
        wai                     ; wait for NMI / V-Blank
        ; .byte $42, $00          ; debugger breakpoint

        A16

        ; Reset the board enclosure.
        lda #$0001
.repeat 20, Row
.repeat (32-12), Col

        sta TilemapMirror + (Row * 32 + Col + 12) * 2 
.endrep
.endrep

.repeat (32-20), Row
.repeat 32, Col
        sta TilemapMirror + ((20+Row) * 32 + Col) * 2 
.endrep
.endrep

        ; Clear previously active tile.
        lda #$0000
        pha
        jsr DrawActive
        pla

        ; read joypad 1
        ; check whether joypad is ready
WaitForJoypad:
        lda HVBJOY                      ; get joypad status
        and #$0001                      ; check whether joypad still reading...
        bne WaitForJoypad               ; ...if not, wait a bit more

        lda JOY1L                       ; Read controller status
        pha                             ; ... and push it onto the stack.

        lda PrevInput                   ; Load last frame's status.
        eor #$ffff
        and $01, S                      ; A = current status - last frame's status (i.e., new buttons)

        bit #RIGHT_BUTTON               ; If right is pressed, move right, etc.
        bne MoveRight

        bit #LEFT_BUTTON
        bne MoveLeft

        bit #DOWN_BUTTON
        bne MoveDown

        bit #UP_BUTTON
        bne MoveUp

        jmp MoveEnd

MoveLeft:
        pea -1
        jsr TryMove
        pla
        jmp MoveEnd

MoveRight:
        pea $0001
        jsr TryMove
        pla
        jmp MoveEnd

MoveDown:
        pea $20
        jsr TryMove
        pla
        jmp MoveEnd

MoveUp:
        pea -$20
        jsr TryMove
        pla
        jmp MoveEnd

MoveEnd:
        lda #$0001                      ; Draw the active tile in red.
        pha
        jsr DrawActive
        pla

        pla                             ; Pop controller input
        sta PrevInput                   ; Store it in PrevInput

        A8                              ; Back to 8 bit
        jmp GameLoop
.endproc
;-------------------------------------------------------------------------------

; A = 16
;
; Parameters
; - fill (word): The tile to set all the active blocks to.
.proc DrawActive
        .a16
        ldx ActiveShape         ; Load offset into ShapeMap

        ldy ShapeMap + 2, X
        phy                     ; End of shape offsets

        ldy ShapeMap, X         ; Iterator over shape data

BlockLoop:
        lda ShapeData, Y        ; Load the shape-relative offset for this block
        clc
        adc ActiveOffset        ; Add the center of the shape
        asl                     ; Multiply by 2 to get memory offset
        tax                     ; X = block location

        lda $05, S              ; Set block to the "fill" parameter.
        sta TilemapMirror, X

        iny
        iny

        tya                     ; Copy y to a to check loop termination.
        cmp $01, S              ; Check if at end
        bne BlockLoop
        
        pla                     ; Reset the stack

        rts
.endproc

; A = 16
;
; Parameters:
; - Delta (word)
;
; Trashes registers: A, X
.proc TryMove
        .a16
        phd                             ; push Direct Register to stack
        tsc                             ; transfer Stack to... (via Accumulator)
        tcd                             ; ...Direct Register.

        Delta = $05

        lda ActiveOffset                ; Compute the effective new offset
        clc
        adc Delta
        sta ActiveOffset                ; Update the offset (for now).
        asl                             ; Double it to get a memory offset.
        tax                             ; Move it into x.

        lda TilemapMirror, X            ; Get the tile where we're trying to go.
        beq SkipReset                   ; If it's free, skip over the "reset" code.

        lda ActiveOffset
        sec
        sbc Delta
        sta ActiveOffset

SkipReset:
        pld                     ; restore caller's frame pointer
        rts                     ; return to caller

.endproc


;-------------------------------------------------------------------------------
;   Will be called during V-Blank
;-------------------------------------------------------------------------------
.proc   NMIHandler
        .a8
        lda RDNMI               ; read NMI status, acknowledge NMI

        ; Update tilemap based on mirror

        ldx #VRAM_TILEMAP
        stx VMADDL              ; set the VRAM address to VRAM_TILEMAP

        lda #V_INC_1
        sta VMAINC              ; increment VRAM address by 1 when writing to VMDATAH

	lda #1
	sta DMAP0 ; transfer mode, 2 registers 1 write
	
        lda #<VMDATAL  ; $2118
	sta BBAD0 ; destination, vram data
	ldx #.loword(TilemapMirror)
	stx A1T0L ; source
	lda #^TilemapMirror
	sta A1T0B ; bank
	ldx #(TilemapMirror_End-TilemapMirror)
	stx DAS0L ; length
	lda #1
	sta MDMAEN ; $420b start dma, channel 0

        rti
.endproc
