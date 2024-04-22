; -----------------------------------------------------------------------------
;   File: nihil.s
;   Description: Your very first SNES game!
; -----------------------------------------------------------------------------

.include "mmap.s"

;----- Assembler Directives ----------------------------------------------------
.p816                           ; tell the assembler this is 65816 code
;-------------------------------------------------------------------------------


;----- Includes ----------------------------------------------------------------
.segment "SPRITEDATA"
SpriteData: .incbin "out/sprites.vra"
ColorData:  .incbin "out/colors.pal"
;-------------------------------------------------------------------------------


.macro writeObj xCoord, yCoord
        lda # xCoord       ; horizontal position of first sprite
        sta OAMDATA
        lda # yCoord       ; vertical position of first sprite
        sta OAMDATA
        lda #$00                ; name of first sprite
        sta OAMDATA
        lda #$00                ; no flip, prio 0, palette 0
        sta OAMDATA
.endmacro

.macro writeGrid gridX, gridY
        ; set up OAM data              
        stz OAMADDL             ; set the OAM address to ...
        stz OAMADDH             ; ...at $0000

        .repeat 5, row
                .repeat 12, col

                        writeObj (gridX + (col * 9)), (gridY + row * 9)

                .endrep
        .endrep

.endmacro


; .macro writeObjMirror xCoord, yCoord
;         lda # xCoord       ; horizontal position of first sprite
;         sta OAMMIRROR, X
;         inx
;         lda # yCoord       ; vertical position of first sprite
;         sta OAMMIRROR, X
;         inx
;         lda #$00                ; name of first sprite
;         sta OAMMIRROR, X
;         inx
;         lda #$00                ; no flip, prio 0, palette 0
;         sta OAMMIRROR, X
;         inx
; .endmacro

; .macro writeGridMirror gridX, gridY

;         ldx #$00

;         .repeat 5, row
;                 .repeat 12, col

;                         writeObjMirror (gridX + (col * 9)), (gridY + row * 9)

;                 .endrep
;         .endrep

; .endmacro

.segment "CODE"
;-------------------------------------------------------------------------------
;   This is the entry point of the demo
;-------------------------------------------------------------------------------
.proc   ResetHandler
        sei                     ; disable interrupts
        clc                     ; clear the carry flag
        xce                     ; switch the 65816 to native (16-bit mode)
        rep #$10 
        .i16
        lda #$8f ;#$8f                ; force v-blanking
        sta INIDISP
        stz NMITIMEN            ; disable NMI

        lda #$00
        sta BGMODE
        

        ; transfer VRAM data
        stz VMADDL              ; set the VRAM address to $0000
        stz VMADDH
        lda #$80
        sta VMAINC              ; increment VRAM address by 1 when writing to VMDATAH
        ldx #$00                ; set register X to zero, we will use X as a loop counter and offset
VRAMLoop:
        lda SpriteData, X       ; get bitplane 0/2 byte from the sprite data
        sta VMDATAL             ; write the byte in A to VRAM
        inx                     ; increment counter/offset
        lda SpriteData, X       ; get bitplane 1/3 byte from the sprite data
        sta VMDATAH             ; write the byte in A to VRAM
        inx                     ; increment counter/offset
        cpx #$20                ; check whether we have written $04 * $20 = $80 bytes to VRAM (four sprites)
        bcc VRAMLoop            ; if X is smaller than $80, continue the loop

        ; transfer CGRAM data
        lda #$80
        sta CGADD               ; set CGRAM address to $80
        ldx #$00                ; set X to zero, use it as loop counter and offset
CGRAMLoop:
        lda ColorData, X        ; get the color low byte
        sta CGDATA              ; store it in CGRAM
        inx                     ; increase counter/offset
        lda ColorData, X        ; get the color high byte
        sta CGDATA              ; store it in CGRAM
        inx                     ; increase counter/offset
        cpx #$20                ; check whether 32/$20 bytes were transfered
        bcc CGRAMLoop           ; if not, continue loop

        .byte $42, $00          ; debugger breakpoint

        ; OAM data for first sprite
        writeGrid 20, 20   
        ; ; OAM data for second sprite
        ; lda # (256/2)           ; horizontal position of second sprite
        ; sta OAMDATA
        ; lda # (224/2 - 8)       ; vertical position of second sprite
        ; sta OAMDATA
        ; lda #$01                ; name of second sprite
        ; sta OAMDATA
        ; lda #$00                ; no flip, prio 0, palette 0
        ; sta OAMDATA
        ; ; OAM data for third sprite
        ; lda # (256/2 - 8)       ; horizontal position of third sprite
        ; sta OAMDATA
        ; lda # (224/2)           ; vertical position of third sprite
        ; sta OAMDATA
        ; lda #$02                ; name of third sprite
        ; sta OAMDATA
        ; lda #$00                ; no flip, prio 0, palette 0
        ; sta OAMDATA
        ; ; OAM data for fourth sprite
        ; lda # (256/2)           ; horizontal position of fourth sprite
        ; sta OAMDATA
        ; lda # (224/2)           ; vertical position of fourth sprite
        ; sta OAMDATA
        ; lda #$03                ; name of fourth sprite
        ; sta OAMDATA
        ; lda #$00                ; no flip, prio 0, palette 0
        ; sta OAMDATA

        ; make Objects visible
        lda #$10
        sta TM
        ; release forced blanking, set screen to full brightness
        lda #$0f
        sta INIDISP
        ; enable NMI, turn on automatic joypad polling
        lda #$81
        sta NMITIMEN

        jmp GameLoop            ; all initialization is done
.endproc
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   After the ResetHandler will jump to here
;-------------------------------------------------------------------------------
; .smart ; keep track of registers widths
.proc   GameLoop
        wai                     ; wait for NMI / V-Blank

        ; here we would place all of the game logic
        ; and loop forever

        writeGrid 10, 10

        jmp GameLoop
.endproc
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   Will be called during V-Blank
;-------------------------------------------------------------------------------
.proc   NMIHandler
        lda RDNMI               ; read NMI status, acknowledge NMI

        ; writeGrid 10, 10
        ; this is where we would do graphics update
        tsx                     ; save old stack pointer 
        pea OAMMIRROR           ; push mirror address to stack 
        ; jsr UpdateOAMRAM        ; update OAMRAM 
        txs                     ; restore old stack pointer

        rti
.endproc

.proc   UpdateOAMRAM
        phx                     ; save old stack pointer
        ; create frame pointer
        phd                     ; push Direct Register to stack
        tsc                     ; transfer Stack to... (via Accumulator)
        tcd                     ; ...Direct Register.
        ; use constants to access arguments on stack with Direct Addressing
        MirrorAddr  = $07       ; address of the mirror we want to copy

        ; set up DMA channel 0 to transfer data to OAMRAM
        lda #%00000010          ; set DMA channel 0
        sta DMAP0
        lda #$04                ; set destination to OAMDATA
        sta BBAD0   
        ldx MirrorAddr          ; get address of OAMRAM mirror
        stx A1T0L               ; set low and high byte of address 
        stz A1T0B               ; set bank to zero, since the mirror is in WRAM
        ldx #$0220              ; set the number of bytes to transfer 
        stx DAS0L

        lda #$01                ; start DMA transfer 
        sta MDMAEN

        ; OAMRAM update is done, restore frame and stack pointer
        pld                     ; restore caller's frame pointer
        plx                     ; restore old stack pointer
        rts                     
.endproc
;-------------------------------------------------------------------------------

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
