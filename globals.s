
.RODATA
SpriteData: .incbin "out/sprites.vra"
SpriteData_End:
ColorData:  .incbin "out/colors.pal"
ColorData_End:
ShapeData:
    .incbin "out/shapes.bin"
PlayFieldTemplate:
    .incbin "out/playfield.bin"
PlayFieldTemplate_End:



.BSS
.res 32                             ; Make TilemapMirror not be at 0 to catch bugs.
TilemapMirror:
                .res 2 * 32 * 32
TilemapMirror_End:
NextShape:      .res 2
ActiveShape:    .res 2
ActiveOffset:   .res 2
GravityCounter: .res 2
RandomSeed:     .res 2

SPAWN_OFFSET = $0026
PIECE_PREVIEW_PTR = TilemapMirror + $00B0
ZERO_TILE_OFFSET  = $0007
DRAW_SCORE_PTR    = TilemapMirror + $066C