
.segment "SPRITEDATA"
SpriteData: .incbin "out/sprites.vra"
SpriteData_End:
ColorData:  .incbin "out/colors.pal"
ColorData_End:
ShapeData:
    .incbin "out/shapes.bin"



.BSS
TilemapMirror:
                .res 2 * 32 * 32
TilemapMirror_End:
ActiveShape:    .res 2
ActiveOffset:   .res 2
GravityCounter: .res 2
PrevInput:      .res 2
