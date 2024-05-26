

;----- Assembler Directives ----------------------------------------------------
.p816                           ; tell the assembler this is 65816 code
.a16
.i16
;-------------------------------------------------------------------------------


.proc DrawActive
        lda ActiveShape         ; Load offset into ShapeData
        asl                     ; Shift left 4 - 16 bytes per shape.
        asl
        asl
        asl
        tax

        lda ShapeData, x        ; Get the shape's color.
        pha                     ; Push it onto the stack.

        txa
        clc
        adc #(ShapeData+2)      ; Push pointer to shape data.
        pha

        lda ActiveOffset        ; Add the center of the shape
        asl                     ; Multiply by 2 to get memory offset
        clc
        adc #TilemapMirror      ; Make it into the origin pointer.
        pha 

        jsr DrawShape

        pla                     ; Clear stack.
        pla
        pla

        rts
.endproc

.proc ClearActive
        pea $0000               ; Push 0 color.

        lda ActiveShape         ; Load offset into ShapeData
        asl                     ; Shift left 4 - 16 bytes per shape.
        asl
        asl
        asl
        clc
        adc #(ShapeData+2)      ; Add base of ShapeData (+2 to skip color)
        pha

        lda ActiveOffset        ; Add the center of the shape
        asl                     ; Multiply by 2 to get memory offset
        clc
        adc #TilemapMirror
        pha 

        jsr DrawShape

        pla                     ; Clear stack.
        pla
        pla                     

        rts
.endproc

.proc DrawPreview
        lda NextShape           ; Load offset into ShapeData
        asl                     ; Shift left 4 - 16 bytes per shape.
        asl
        asl
        asl
        tax

        lda ShapeData, x        ; Get the shape's color.
        pha                     ; Push it onto the stack.

        txa
        clc
        adc #(ShapeData+2)      ; Push pointer to shape data.
        pha

        pea PIECE_PREVIEW_PTR   ; The place to draw the preview.

        jsr DrawShape

        pla                     ; Clear stack.
        pla
        pla

        rts
.endproc

.proc ClearPreview
        pea $0000               ; Push 0 color.

        lda NextShape           ; Load offset into ShapeData
        asl                     ; Shift left 4 - 16 bytes per shape.
        asl
        asl
        asl
        clc
        adc #(ShapeData+2)      ; Add base of ShapeData (+2 to skip color)
        pha

        pea PIECE_PREVIEW_PTR   ; The place where the preview was drawn.

        jsr DrawShape

        pla                     ; Clear stack.
        pla
        pla                     

        rts
.endproc

; Stack
;  $07: Color to draw.
;  $05: Pointer to the start of the shape.
;  $03: Pointer to zero tile.
.proc DrawShape
        Color  = $07
        Shape  = $05
        Origin = $03

.repeat 7, Block                ; For each of 7 blocks
        ldy #(2 * Block)
        lda (Shape, S), Y       ; A = Block offset
        asl                     ; Double to get pointer offset.
        tay

        lda Color, S            ; Load the color
        sta (Origin, S), Y      ; Store it at the destination (Origin + 2*Offset)
.endrep

        rts
.endproc


.proc DrawScore
        lda Score

.repeat 4, Digit
        pha                                     ; Preserve the remaining score.
        and #$000F                              ; Get just the last 4 bits (LSD)
        clc
        adc #ZERO_TILE_OFFSET                   ; Add to the tile for zero.
        sta DRAW_SCORE_PTR + 2 * (3 - Digit)    ; Draw to the corresponding tile.
        pla                                     ; Restore the remaining score.

.repeat 4                                       ; Shift right 4 to get next digit.
        lsr
.endrep
.endrep
        rts
.endproc
