; -----------------------------------------------------------------------------
;   File: game.s
;   Description: Main game logic.
; -----------------------------------------------------------------------------

.include "mmap.s"
.include "macros.s"
.include "out/consts.s"
.include "rodata.s"

;----- Assembler Directives ----------------------------------------------------
.p816                           ; tell the assembler this is 65816 code
.a16
.i16
;-------------------------------------------------------------------------------

.include "graphics.s"
.include "input.s"
.include "init.s"
.include "header.s"


.BSS
.res 32                             ; Make TilemapMirror not be at 0 to catch bugs.
TilemapMirror:
                        .res 2 * 32 * 32
TilemapMirror_End:
NextShape:              .res 2
ActiveShape:            .res 2
ActiveOffset:           .res 2
GravityCounter:         .res 2
RandomSeed:             .res 2
Score:                  .res 2
RowSummary:             .res 2 * 32
InGameLogic:            .res 2

SPAWN_OFFSET            = $0026
PIECE_PREVIEW_PTR       = TilemapMirror + $00B0
ZERO_TILE_OFFSET        = $0007
DRAW_SCORE_PTR          = TilemapMirror + $066C

.RODATA
ScoreTable:
        .word $0000             ; Score to add based on number of lines cleared.
        .word $0001             ; = 2^N - 1 
        .word $0003             ; Stored in BCD
        .word $0007
        .word $0015
        .word $0031

.CODE

.proc InitGame
        stz Score
        stz NextShape
        stz InGameLogic
        jsr RandomizeActive
        jsr RandomizeActive
        rts
.endproc

;-------------------------------------------------------------------------------
;   After the ResetHandler will jump to here
;-------------------------------------------------------------------------------
.proc   GameLoop
        wai                     ; wait for NMI / V-Blank
        ; .byte $42, $00          ; debugger breakpoint
        lda #$0001              ; Indicate we're in the middle of game logic.
        sta InGameLogic         

        jsr ClearActive
        jsr ClearPreview

        jsr DoGravity
        jsr DoInput

        jsr DrawActive
        jsr DrawPreview

        jsr DrawScore

        lda InGameLogic         ; Check InGameLogic is still 1, or panic.
        cmp #$0001
        beq DontPanic

        brk

DontPanic:
        stz InGameLogic         ; Clear InGameLogic.

        jmp GameLoop
.endproc
;-------------------------------------------------------------------------------

; Decrease gravity counter. If zero, reset and move block down.
.proc DoGravity
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

        jsr DrawActive                  ; Add the active shape to the background.

        jsr RandomizeActive
        lda #SPAWN_OFFSET               ; And reset the offset.
        sta ActiveOffset

        jsr DoLineClears

SkipLockdown:
SkipGravity:
        rts

.endproc


; Checks for potential line clears after locking down and performs
; them, if necessary.
.proc DoLineClears
        jsr CheckRows
        jsr IncreaseScore
        jsr ComputeDests
        jsr CopyRows
        rts
.endproc

; Sets RowSummary[Row] to 1 if the row is clearable, otherwise 0.
.proc CheckRows 
        pea TilemapMirror               ; Push row pointer onto the stack.

        RowPtr    = $01
        ldy #$0000
RowLoop:
        pha                             ; Stack space for CheckRow result.
        jsr CheckRow
        pla                             ; Load result into A.
        sta RowSummary, Y               ; Store result.

        lda RowPtr, S                   ; Advance row pointer.
        clc
        adc #$0040
        sta RowPtr, S

        iny                             ; ... and y
        iny

        cmp #TilemapMirror_End          ; if row < 32
        bcc RowLoop

        pla                             ; Clear stack.

        rts
.endproc

; Stack
;   $05 - Pointer to start of row.
;   $03 - (out) 1 if row should be cleared (contains no blanks and at least one destructible).
;   $01 - RTA.
.proc CheckRow
        phy                             ; Save state of Y.

        RowPointer = $07
        ClearableOut = $05

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
        cpy #$20                        ; if y < 32
        bcc TileLoop                    ; continue row loop

        bra Return                      ; Return when done.

FoundBlank:
        ; If we found a blank, it's not clearable no matter what.
        lda #$0000
        sta ClearableOut, S

Return:
        ply                             ; Restore Y
        rts
.endproc

; Increase the current score based on row clears. Assumes RowSummary[row] 
; contains a 1 for rows that should be cleared, and 0 otherwise.
.proc IncreaseScore
        lda #$0000                      ; First, sum up the number of cleared rows.

        ldy #$0000
Loop:
        clc
        adc RowSummary, Y

        iny
        iny
        cpy #$0040
        bcc Loop

        ; Then, add the appropriate amount to the score based on the number of clears.

        asl                             ; Double the number of cleared rows to 
                                        ; get an offset into the score table.
        tay

        sep #$08                        ; Enable decimal mode.	

        lda ScoreTable, Y               ; Look up the amount to add to the score.

        clc                             ; ... and add it to the score.
        adc Score               
        sta Score

        rep #$08                        ; Disable decimal mode.	

        rts
.endproc

; For each row, computes the (relative) "destination" row that
; it should be copied into, and stores the result in RowSummary.
; So, if RowSummary[R] = 0, then row R should be left alone.
; If RowSummary[R] = 1, it should be copied one row down, etc.
;
; Assumes that RowSummary[R] begins with 1 for all clearable rows,
; and 0 otherwise.
.proc ComputeDests
        lda #$0000              ; Cumulative sum

        ldy #$003E              ; Loop from bottom to top.
Loop:
        ldx RowSummary, Y       ; Load whether the row is clearable.
        phx                     ; ... and save it on the stack.

        sta RowSummary, Y       ; RowSummary gets set to the current cumulative
                                ; sum, _not_ counting the current row.

        clc                     ; Add the current row's value to the sum, and 
        adc $01, S              ; remove it from the stack.
        plx

        dey                     ; Advance to previous row.
        dey

        bpl Loop
        rts
.endproc

; Copies rows according to offsets in RowSummaries. Assumes that ComputeDests
; was just called.
.proc CopyRows
        ldy #$003E              ; Start from last row.
Loop:
        tya                     ; A = current row offset (not doubled, like usual)
        lsr

        clc                     ; Add the offset from RowSummary
        adc RowSummary, Y

.repeat 6                       ; A << 6 to get row pointer (64 bytes per row)
        asl
.endrep

        clc                     ; Add TilemapMirror to get dest absolute pointer.
        adc #TilemapMirror      
        pha                     ; Push it onto the stack as an argument.

        tya                     ; Reload Y. The src pointer will be based on it.
.repeat 5                       ; Only shift 5 because Y is already doubled.
        asl
.endrep

        clc                     ; Add base pointer.
        adc #TilemapMirror
        pha                     ; Push src.

        jsr CopyRow

        pla                     ; Clear arguments.
        pla

        dey                     ; Next row.
        dey
        bpl Loop

        rts
.endproc

; Stack:
;   05  Pointer to start of dest row.
;   03  Pointer to start of src row.
;   01  RTA
.proc CopyRow
        phy
        Dest = $07
        Src = $05

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
        cpy #$0020              ; If we havent finished the row
        bcc TileLoop            ; Continue.

        ply
        rts
.endproc


; A = 16
;
; Zero flag indicates that there's no collision.
.proc CheckCollision
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
        lda NextShape
        sta ActiveShape

Reroll:
        jsr GetRandom
        and #$001F              ; Assumes SHAPE_COUNT < 32
        cmp #SHAPE_COUNT
        bpl Reroll              ; If random index is >= shape count, get another.

        asl
        asl
        sta NextShape

        rts
.endproc
