; -----------------------------------------------------------------------------
;   File: game.s
;   Description: Main game logic.
; -----------------------------------------------------------------------------

.include "mmap.s"
.include "macros.s"
.include "out/consts.s"

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
        .a16
        wai                     ; wait for NMI / V-Blank
        ; .byte $42, $00          ; debugger breakpoint

        ; Clear previously active tile.
        lda #$0000
        pha
        jsr DrawActive
        pla

        jsr DoGravity
        jsr DoInput

        lda #$FFFF                      ; Draw the active tile in red.
        pha
        jsr DrawActive
        pla

        jmp GameLoop
.endproc
;-------------------------------------------------------------------------------

; Decrease gravity counter. If zero, reset and move block down.
.proc DoGravity
        .a16

        lda GravityCounter
        dec                     ; Decrement and update GravityCounter
        sta GravityCounter
        
        bne SkipGravity         ; If it's nonzero, skip to the end.
   
        lda #60                 ; Reset the gravity counter
        sta GravityCounter

        lda ActiveOffset                ; Move the active shape down a row.
        clc
        adc #$20
        sta ActiveOffset                ; Update the offset (for now).
     
        jsr CheckCollision              ; Check for collisions.
        beq SkipLockdown                ; If none, skip over the "lock down" code.

        lda ActiveOffset                ; If there was a collision, back out of the drop.
        sec
        sbc #$20                     
        sta ActiveOffset

        pea $FFFF                       ; Add the active shape to the background.
        jsr DrawActive
        pla

        jsr RandomizeActive
        lda #SPAWN_OFFSET               ; And reset the offset.
        sta ActiveOffset

        jsr DoLineClears

SkipLockdown:
SkipGravity:
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
        jsr TryRotate
        jmp MoveEnd

MoveEnd:
        pla                             ; Pop controller input
        sta PrevInput                   ; Store it in PrevInput
        rts

.endproc

.proc DoLineClears
        .a16
        ; .byte $42, $00          ; debugger breakpoint

        pea TilemapMirror               ; Push row pointer onto the stack.

RowLoop:
        pha                             ; Stack space for CheckRow result.
        jsr CheckRow
        pla

        beq NextRow

        ; If it made it here, the line should be cleared.
        jsr Downshift
        
NextRow:
        lda $01, S                      ; Advance row pointer
        clc
        adc #$0040
        sta $01, S

        cmp #TilemapMirror_End          ; if row < 32
        bcc RowLoop

        ; End of RowLoop

        pla                             ; Pop row pointer
        rts
.endproc

; Stack
;   $05 - Pointer to start of row.
;   $03 - (out) 1 if row should be cleared (contains no blanks and at least one destructible).
;   $01 - RTA.
.proc CheckRow
        .a16
        RowPointer = $05
        ClearableOut = $03

        lda #$0000                      ; Result = 0
        sta ClearableOut, S

        ldy #$0000                      ; Y = offset into row
TileLoop:
        lda (RowPointer, S), Y          ; Load current tile
        beq FoundBlank                  ; If it's zero (empty), end early.

        cmp #$0001                      ; If it's 1 (indestructible) 
        beq NextTile                    ; Skip to next tile.

        ; If what we found wasn't blank, and it wasn't indestructible,
        ; then we potentially have a clearable row. Speculatively set
        ; the output.
        lda #$0001
        sta ClearableOut, S

NextTile:
        iny                             ; y += 2
        iny
        cpy #$40                        ; if y < 64
        bcc TileLoop                    ; continue row loop

        rts                             ; Return when done.

FoundBlank:
        ; If we found a blank, it's not clearable no matter what.
        lda #$0000
        sta ClearableOut, S

        rts
.endproc

; Stack
;   $03 - Pointer to bottom row to shift into.
.proc Downshift
        .a16
        lda $03, S                      ; Copy the dest row into our stack frame.
        pha

        phd                             ; push Direct Register to stack
        tsc                             ; transfer Stack to... (via Accumulator)
        tcd                             ; ...Direct Register.

        DestRow = $03
RowLoop:
        lda DestRow                     ; Push DestRow
        pha 
        sec
        sbc #$40
        pha                             ; Push SrcRow = DestRow - $40

        jsr CopyRow                     ; CopyRow
        pla                             ; Pop SrcRow
        sta DestRow                     ; DestRow = SrcRow
        pla                             ; Pop old DestRow copy

        lda DestRow                     ; If DestRow is after the first row, continue.S
        cmp #TilemapMirror + $40
        bcs RowLoop

        pld                             ; Reset direct page.
        pla                             ; Pop DestRow

        rts                             ; Return
.endproc

; Stack:
;   05  Pointer to start of dest row.
;   03  Pointer to start of src row.
;   01  RTA
.proc CopyRow
        .a16

        Dest = $05
        Src = $03

        ldy #$0000              ; Tile offset into row
TileLoop:
        lda (Dest, S), Y
        cmp #$0001
        beq NextTile            ; If the dest tile is indestructible, skip it.

        lda (Src, S), Y         ; If the src tile is indestructible, write a 0 to dest.
        cmp #$0001
        beq WriteZero

        ; Otherwise, do the copy.
        sta (Dest, S), Y
        jmp NextTile

WriteZero:
        lda #$0000
        sta (Dest, S), Y
        
NextTile:
        iny
        iny
        cpy #$0040              ; If we havent finished the row
        bcc TileLoop            ; Continue.

        rts
.endproc

; A = 16
;
; Parameters
; - fillMask (word): Mask the shape's tile with this before drawing.
.proc DrawActive
        .a16
        lda ActiveShape         ; Load offset into ShapeData
        asl                     ; Shift left 4 - 16 bytes per shape.
        asl
        asl
        asl
        tax

        lda $03, S              ; Load fillMask.
        and ShapeData, x        ; And-in the shape's tile.
        pha                     ; Push it onto the stack.


.repeat 7, Block                ; For each of 7 blocks (after the tile)...
        lda ShapeData + 2 * Block + 2, X    ; Get the offset for that block.
        clc
        adc ActiveOffset        ; Add the center of the shape
        asl                     ; Multiply by 2 to get memory offset
        tay                     ; Y = block location

        lda $01, S              ; Set block to the tile on the stack.
        sta TilemapMirror, Y
.endrep

        pla                     ; Clear stack.
        rts
.endproc

; A = 16
;
; Zero flag indicates that there's no collision.
.proc CheckCollision
        .a16
        lda ActiveShape         ; Load offset into ShapeData
        asl                     ; Shift left 4 - 16 bytes per shape.
        asl
        asl
        asl
        tax

.repeat 7, Block                ; For each of 7 blocks (after the tile)...
        lda ShapeData + 2 * Block + 2, X    ; Get the offset for that block.
        clc
        adc ActiveOffset        ; Add the center of the shape
        asl                     ; Multiply by 2 to get memory offset
        tay                     ; Y = block location

        lda TilemapMirror, Y
        bne Conflict
.endrep


Conflict:
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
   
        jsr CheckCollision              ; Check for collisions.
        beq SkipReset                   ; If none, skip over the "reset" code.

        lda ActiveOffset
        sec
        sbc Delta
        sta ActiveOffset

SkipReset:
        pld                     ; restore caller's frame pointer
        rts                     ; return to caller

.endproc

.proc TryRotate
        .a16

        lda ActiveShape         ; Get active shape
        pha                     ; Save in case we revert.
        inc
        and #$3                 ; Mask off all but the last 2 bits.
        pha                     ; Push the new rotation selector onto the stack.

        lda ActiveShape         ; Reload active shape.
        and #$FFFC              ; Mask off last three bits.
        ora $01, S              ; Or with the new rotation value.

        sta ActiveShape         ; Update the active shape.
        pla

        jsr CheckCollision      ; Check for collisions.
        beq SkipReset           ; If none, skip over the "reset" code.

        lda $01, s              ; Grab the old active shape
        sta ActiveShape         ; Restore it.

SkipReset:
        pla                     ; Clean up stack

        rts                     ; return to caller

.endproc

; Ported from https://www.nesdev.org/wiki/Random_number_generator
;
; Returns a random 8-bit number in A (0-255), clobbers Y (0).
;
; Requires a 2-byte value on the zero page called "seed".
; Initialize seed to any value except 0 before the first call to prng.
; (A seed value of 0 will cause prng to always return 0.)
;
; This is a 16-bit Galois linear feedback shift register with polynomial $0039.
; The sequence of numbers it generates will repeat after 65535 calls.
;
; Execution time is an average of 125 cycles (excluding jsr and rts)
.proc GetRandom
        A8
	ldy #8                  ; iteration count (generates 8 bits)
	lda RandomSeed + 0
NextBit:
	asl                     ; shift the register
	rol RandomSeed + 1
	bcc SkipXor
	eor #$39   ; apply XOR feedback whenever a 1 bit is shifted out
SkipXor:
	dey
	bne NextBit
	sta RandomSeed + 0
	cmp #0     ; reload flags
        A16
        and #$00FF
	rts
.endproc

.proc RandomizeActive 
        .a16

Reroll:
        jsr GetRandom
        and #$001F              ; Assumes SHAPE_COUNT < 32
        cmp #SHAPE_COUNT
        bpl Reroll              ; If random index is >= shape count, get another.

        asl
        asl
        sta ActiveShape

        rts
.endproc

;-------------------------------------------------------------------------------
;   Will be called during V-Blank
;-------------------------------------------------------------------------------
.proc   NMIHandler
        lda RDNMI               ; read NMI status, acknowledge NMI
        A8

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

        A16
        rti
.endproc
