rowCounts .byte 24,18
ldfLUTLO .byte <leveldatafile, <leveldatafile2
ldfLUTHI .byte >leveldatafile, >leveldatafile2
sxLUTLO .byte <startX, <startX2
sxLUTHI .byte >startX, >startX2
exLUTLO .byte <endX, <endX2
exLUTHI .byte >endX, >endX2
typeLUTLO .byte <type,<type2
typeLUTHI .byte >type,>type2


leveldatafile .byte 0,8,9,7,7,7,8,7,9,7,8,7,9,9,9,9,9,10,9,9,7,8,7,9,79
startX .byte 0,12,15,16,1,6,1,7,8,15,10,3,5,4,18,8,17,3,10,5,6,4,5,12
endX .byte 30,22,25,26,11,16,11,18,18,25,21,12,15,15,26,19,27,14,20,16,17,13,16,12
type .byte 1,1,1,1,1,1,1,9,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,10

leveldatafile2 .byte 0,11,10,13,8,10,10,12,11,11,9,12,13,10,12,12,10,12,79
startX2 .byte 0,7,8,11,13,9,11,8,7,11,9,3,4,3,8,8,8,11
endX2 .byte 30,16,15,19,21,19,20,17,16,18,19,10,14,12,15,16,16,11
type2 .byte 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1