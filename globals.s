
.segment "SPRITEDATA"
SpriteData: .incbin "out/sprites.vra"
SpriteData_End:
ColorData:  .incbin "out/colors.pal"
ColorData_End:
ShapeData:
    ; Domino
    .word $0000
    .word $0001
    .word $0000
    .word $0020
    .word $0000
    .word $FFFF
    .word $0000
    .word $FFE0
ShapeMap:
    .word $0000
    .word $0004
    .word $0004
    .word $0008
    .word $0008
    .word $000C
    .word $000C
    .word $0010


.BSS
TilemapMirror:
                .res 2 * 32 * 32
TilemapMirror_End:
ActiveShape:    .res 2
ActiveOffset:   .res 2
PrevInput:      .res 2 
