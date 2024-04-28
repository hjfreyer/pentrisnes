
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
