rowCounts .byte 24,18
ldfLUTLO .byte <leveldatafile, <leveldatafile2
ldfLUTHI .byte >leveldatafile, >leveldatafile2
sxLUTLO .byte <startX, <startX2
sxLUTHI .byte >startX, >startX2
exLUTLO .byte <endX, <endX2
exLUTHI .byte >endX, >endX2
typeLUTLO .byte <type,<type2
typeLUTHI .byte >type,>type2


leveldatafile .byte 00,08,09,07,07,07,08,07,09,07,08,07,09,09,09,09,09,10,09,09,07,08,07,09,79
startX .byte 		00,12,15,16,01,06,01,07,08,15,10,03,05,04,18,08,17,03,10,05,06,04,05,12
;endX .byte 30,22,25,26,11,16,11,18,18,25,21,12,15,15,26,19,27,14,20,16,17,13,16,12
endX .byte 			30,22,25,09,134,16,11,18,18,25,21,12,15,15,26,19,27,14,20,16,17,13,16,12
;type .byte 1,1,1,1,1,1,1,9,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,10
type .byte 			01,01,01,07,07,01,01,09,08,01,01,01,01,01,01,01,01,01,01,01,01,01,01,10

leveldatafile2 .byte 0,11,10,13,8,10,10,12,11,11,9,12,13,10,12,12,10,12,79
startX2 .byte 0,7,8,11,13,9,11,8,7,11,9,3,4,3,8,8,8,11
endX2 .byte 30,16,15,19,21,19,20,17,16,18,19,10,14,12,15,16,16,11
type2 .byte 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1