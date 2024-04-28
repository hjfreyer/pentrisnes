; -----------------------------------------------------------------------------
;   File: nihil.s
;   Description: Your very first SNES game!
; -----------------------------------------------------------------------------

.include "mmap.s"
.include "macros.s"

;----- Assembler Directives ----------------------------------------------------
.p816                           ; tell the assembler this is 65816 code
.smart
;-------------------------------------------------------------------------------

.segment "SPRITEDATA"
SpriteData: .incbin "out/sprites.vra"
SpriteData_End:
ColorData:  .incbin "out/colors.pal"
ColorData_End:

.BSS
TilemapMirror:
                .res 2 * 32 * 32
TilemapMirror_End:
ActiveOffset:   .res 2
PrevInput:      .res 2 

.segment "CODE"
;-------------------------------------------------------------------------------
;   This is the entry point of the demo
;-------------------------------------------------------------------------------
.proc   ResetHandler
        sei                     ; disable interrupts
        clc                     ; clear the carry flag
        xce                     ; switch the 65816 to native (16-bit mode)
        rep #$10                ; Set X, Y to 16-bit
        sep #$20                ; Set A to 8-bit
        lda #$8f                ; force v-blanking
        sta INIDISP
        stz NMITIMEN            ; disable NMI

        ; set the stack pointer to $1fff
        ldx #$1fff              ; load X with $1fff
        txs                     ; copy X to stack pointer

        jsr DMA_Tiles
        jsr DMA_Palette

        lda #12
        sta ActiveOffset

        ldx #$00
TilemapBlank:
        stz TilemapMirror, X
        inx
        cpx #(TilemapMirror_End - TilemapMirror)
        bcc TilemapBlank

        ; Set up demo Tilemap
.repeat 6, PAL
.repeat 5, SPR
        ldx #(SPR+1)
        stx TilemapMirror + 2*((PAL * 33) +SPR)
.endrep
.endrep
      
	lda #1 ; mode 1, tilesize 8x8 all
	sta BGMODE ; $2105
	
; 210b = tilesets for bg 1 and bg 2
; (210c for bg 3 and bg 4)
; steps of $1000 -321-321... bg2 bg1
	stz BG12NBA ; $210b BG 1 and 2 TILES at VRAM address $0000
	
	; 2107 map address bg 1, steps of $400... -54321yx
	; y/x = map size... 0,0 = 32x32 tiles
	; $6000 / $100 = $60
	lda #$60 ; bg1 map at VRAM address $6000
	sta BG1SC ; $2107

	lda #BG1_ON	; $01 = only bg 1 is active
	sta TM
        ; release forced blanking, set screen to full brightness
        lda #$0f
        sta INIDISP
        ; enable NMI, turn on automatic joypad polling
        lda #$81
        sta NMITIMEN

        jmp GameLoop            ; all initialization is done
.endproc

.proc DMA_Tiles
        ldx #VRAM_TILES
        stx VMADDL              ; set the VRAM address to VRAM_TILES

        lda #V_INC_1
        sta VMAINC              ; increment VRAM address by 1 when writing to VMDATAH

	lda #1
	sta DMAP0 ; transfer mode, 2 registers 1 write
	
        lda #<VMDATAL  ; $2118
	sta BBAD0 ; destination, vram data
	ldx #.loword(SpriteData)
	stx A1T0L ; source
	lda #^SpriteData
	sta A1T0B ; bank
	ldx #(SpriteData_End-SpriteData)
	stx DAS0L ; length
	lda #1
	sta MDMAEN ; $420b start dma, channel 0
        rts
.endproc

.proc DMA_Palette
	stz CGADD ; $2121 cgram address = zero
	
	stz DMAP0 ; transfer mode 0 = 1 register write once
	lda #<CGDATA  ; $2122
	sta BBAD0 ; destination, cgram data
	ldx #.loword(ColorData)
	stx A1T0L ; source
	lda #^ColorData
	sta A1T0B ; bank
	ldx #(ColorData_End - ColorData)
	stx DAS0L ; length
	lda #1
	sta MDMAEN ; $420b start dma, channel 0
        rts
.endproc

;-------------------------------------------------------------------------------
;   After the ResetHandler will jump to here
;-------------------------------------------------------------------------------
.proc   GameLoop
        wai                     ; wait for NMI / V-Blank
        ; .byte $42, $00          ; debugger breakpoint

        A16

        ; Clear previously active tile.
        lda ActiveOffset
        asl ; Multiply offset by 2
        tax 
        stz TilemapMirror, X

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
        lda ActiveOffset
        dec
        sta ActiveOffset
        jmp MoveEnd

MoveRight:
        lda ActiveOffset
        inc
        sta ActiveOffset
        jmp MoveEnd

MoveDown:
        lda ActiveOffset
        clc
        adc #$20
        sta ActiveOffset
        jmp MoveEnd

MoveUp:
        lda ActiveOffset
        sec
        sbc #$20
        sta ActiveOffset
        jmp MoveEnd

MoveEnd:
        lda ActiveOffset
        asl                             ; Multiply offset by 2
        tax 
        lda #$0002
        sta TilemapMirror, X            ; Set the active tile to be filled in.

        pla                             ; Pop controller input
        sta PrevInput                   ; Store it in PrevInput

        A8                              ; Back to 8 bit
        jmp GameLoop
.endproc
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   Will be called during V-Blank
;-------------------------------------------------------------------------------
.proc   NMIHandler
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

;-------------------------------------------------------------------------------
;   Is not used in this program
;-------------------------------------------------------------------------------
.proc   IRQHandler
        ; code
        rti
.endproc
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   Interrupt and Reset vectors for the 65816 CPU
;-------------------------------------------------------------------------------
.segment "VECTOR"
; native mode   COP,        BRK,        ABT,
.addr           $0000,      $0000,      $0000
;               NMI,        RST,        IRQ
.addr           NMIHandler, $0000,      $0000

.word           $0000, $0000    ; four unused bytes

; emulation m.  COP,        BRK,        ABT,
.addr           $0000,      $0000,      $0000
;               NMI,        RST,        IRQ
.addr           $0000,      ResetHandler, $0000
