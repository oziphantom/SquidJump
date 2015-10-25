WARN = 0

kVectors .block
	charBase = $4000
	spr0ID = charBase+1016
	spr1ID = charBase+1017
	spr2ID = charBase+1018
	spr3ID = charBase+1019
	spr4ID = charBase+1020
	spr5ID = charBase+1021
	spr6ID = charBase+1022
	spr7ID = charBase+1023	
	mapBottom = $c000-40	
.bend

kTileXCount = 16
kTileYCount = 12

kBounds .block
	screenMinX = 24
	screenMinY = 50
	screenMaxX = 24 + ( kTileXCount * 8 )
	screenMaxY = 50 + ( kTileYCount * 8 )
.bend

kTimers .block
.bend

kPlayerParams .block
	jumpStartDelta = 255-1
	jumpDeltaAccum = 19
	jumpDeltaAccumFloat = 9
	maxFallSpeed = 2
	normalColour = 15
.bend

kScoreSprites .block
	scoreDef = 34
	hiScoreDef = 36
	baseAddr = fileSprites + (scoreDef*64)
	scoreDigit6 = 24
	scoreDigit5 = scoreDigit6+1
	scoreDigit4 = scoreDigit5+1
	scoreDigit3 = 64+scoreDigit6
	scoreDigit2 = 64+scoreDigit5
	scoreDigit1 = 64+scoreDigit4
	hiScoreDigit6 = scoreDigit6 + 128
	hiScoreDigit5 = hiScoreDigit6+1
	hiScoreDigit4 = hiScoreDigit5+1
	hiScoreDigit3 = 64+hiScoreDigit6
	hiScoreDigit2 = 64+hiScoreDigit5
	hiScoreDigit1 = 64+hiScoreDigit4
.bend

kEntSprits .block
	bounceBase = kSprBase + 24
	doubleJumpBase = kSprBase + 20
.bend

kFont .block
	digitsStart = 48
	digitsPointer = fileChars + ( digitsStart * 8 ) + 1
.bend

kSprBase = 64

kRaster .block
	bottomRaster = 241
.bend

kDeadZone .block
	top = 160
	bottom = 180
.bend

kSpriteLevelDataTypesStart  = 6
kSpriteAutoBounceID = 8
kEmptySprite = kSprBase+49

mplex .block
kMaxSpr = $1f
sort = $02
ybuf = $22		;sprite y position raster buffer
xbuf = $42		;sprite x position raster buffer
mbuf = $62		;sprite x msb raster buffer
cbuf = $82		;sprite color raster buffer
pbuf = $a2		;sprite pointer raster buffer
sptr = $c2		;sprite pointer for raster
cnt  = $c3
mnt  = $c4
lsbtod = $c5
ypos = $c000	;sprite y position frame buffer
xpos = $c020	;sprite x position frame buffer
xmsb = $c040	;sprite x msb frame buffer
sprc = $c060	;sprite color frame buffer
sprp = $c080	;sprite pointer frame buffer
.bend

kEntSpriteBuffer .block
	startOffset = statusSpritesDefs.kNum+2
	endOffset = statusSpritesDefs.kNum+1+16
	entYPtr = mplex.ypos+startOffset
	entXPtr = mplex.xpos+startOffset
	entXMSBPtr = mplex.xmsb + startOffset
	entColPtr = mplex.sprc + startOffset
	entPtrPtr = mplex.sprp + startOffset 
.bend
kCharDefines .block
	coral = 19
	ice = 128+64+20
	check = 128+64+21
	leftConvayer = 22
	rightConvayer = 128+64+23	
.bend	

* = $c6
pointer1	.word ? ;	= $c6 ;7
pointer2	.word ?	;= $c8 ;9
pointer3	.word ?	;= $ca ;b
screenPointer	.word ? ;= $cc ;d
mapCopyTemp		.word ? ;= $ce ;f
ZPTemp2			.byte ?;= $d2
ZPTemp			.byte ?;= $d3
ZPTemp3			.byte ?;= $d4
mapYDelta		.word ? ;= $d5
mapXStart		.word ? ;= $d7
mapXEnd			.word ? ;= $d9
mapType			.word ? ;= $db
currMapPtr		.word ? ;= $dd
numItemsMap		.byte ? ;= $df
mapItemIndex	.byte ? ;= $f0

* = $f1
checkSpriteToCharData .dstruct sCSTCCParams 

sGameData .struct 
lives .byte ?
score .byte ?,?,?,?,?,?
high .byte ?,?,?,?,?,?
currLevel .byte ?
.ends

sLevelData .struct
playerX .byte ?
playerY .byte ?
.ends

sTimerTickDowns .struct
playerAnim 	.byte ?
playerFlash .byte ?
playerDoubleJumpFlash .byte ?
.ends

sPlayerData .struct
dead .byte ?
onGround .byte ?
hasJumped .byte ?
deltaToAddToJumped .byte ?
isFalling .byte ?
yDeltaAccum .word ?
baseSprite .byte ?
frameOffset .byte ?
frameCount .byte ?
frameTimer .byte ?
currAnim .byte ?
jumpChargePump .word ?
fireWasDown .byte ?
lastXDelta .byte ?
onIce .byte ?
spriteFlashIndex .byte ?
spriteFlashChargePump .word ?
onMovingPlatform .byte ?
onConvayerLeft .byte ?
onConvayerRight .byte ?
spriteFlashDoubleJumpIndex .byte ?
.ends

sCSTCCParams .struct	
xDeltaCheck .byte ? ; pixels
yDeltaCheck .byte ? ; pixels
yCharCollide .byte ?
xCharCollide .byte ?
screenLoc .word ?
.ends

sMapDataTracking .struct
topCharsToNext .byte ?
topIndex .byte ?
bottomCharsToNext .byte ?
bottomIndex .byte ?
.ends

sScoreValues .struct
digit6 .byte ?
digit5 .byte ?
digit4 .byte ?
digit3 .byte ?
digit2 .byte ?
digit1 .byte ?
.ends

sMovingPlatform .struct
y	.byte ?,?,?
sprIndex .byte ?,?,?
numSprites .byte ?,?,?
direction .byte ?,?,?
mapTableY .byte ?,?,?
width	.byte ?,?,?
.ends
sOtherEnts .struct
y .byte ?,?,?
sprIndex .byte ?,?,?
mapTableY .byte ?,?,?
type .byte ?,?,?
.ends

variables = $0200
* = $0200
joyLeft	 .byte ?
joyRight .byte ?
joyUp	 .byte ?
joyDown	 .byte ?
joyFire	 .byte ?
.if WARN 
.warn "al .GameData ", * 
.endif
GameData .dstruct sGameData
.if WARN 
.warn "al .LevelData ", *
.endif
LevelData .dstruct sLevelData
.if WARN 
.warn "al .PlayerData ", *
.endif
PlayerData .dstruct sPlayerData
.if WARN 
.warn "al .MapTrackingData ", *
.endif
MapTrackingData .dstruct sMapDataTracking 
.if WARN 
.warn "al .Score ", * 
.endif
Score .dstruct sScoreValues
.if WARN 
.warn "al .HiScore ", *
.endif
HiScore .dstruct sScoreValues
; put the level values here
.if WARN 
.warn "al .MovingPlatform ", *
.endif
MovingPlatform .dstruct sMovingPlatform
OtherEnts .dstruct sOtherEnts
.if WARN 
.warn "al .TickDowns ", *
.endif
TickDowns .dstruct sTimerTickDowns
.if WARN 
.warn "al .CRAMPlotColour ", *
.endif
CRAMPlotColour .byte ?
.if WARN 
.warn "al .CRAMBorderPlotColour ", *
.endif
CRAMBorderPlotColour .byte ?
.if WARN 
.warn "al .FirstLineToPlot ", *
.endif
FirstLineToPlot .byte ?
.if WARN 
.warn "al .yScroll ", *
.endif
yScroll	.byte ?
.if WARN 
.warn "al .entitySpriteBufferHead ", *
.endif
entitySpriteBufferHead .byte ?
.if WARN 
.warn "al .entitySpriteBufferTail ", *
.endif
entitySpriteBufferTail .byte ?
.if WARN 
.warn "al .entitySpriteBufferPointer ", *
.endif
entitySpriteBufferPointer .byte ?
.if WARN 
.warn "al .ScrollDelta ", *
.endif
ScrollDelta .byte ?
.if WARN 
.warn "al .NormalTemp1 ", *
.endif
NormalTemp1 .byte ?
.if WARN 
.warn "al .NormalTemp2 ", *
.endif
NormalTemp2 .byte ?
.if WARN 
.warn "al .NormalTem3 ", *
.endif
NormalTemp3 .byte ?
.if WARN 
.warn "al .NormalTemp4 ", *
.endif
NormalTemp4 .byte ?
.if WARN 
.warn "al .NormalTemp5 ", *
.endif
NormalTemp5 .byte ?
.if WARN 
.warn "al ",*," .PlatformLeftIndex "
.endif
ConvayerLeftDelta .byte ?
.if WARN 
.warn "al ",*," .ConvayerLeftDelta "
.endif
ConvayerRightDelta .byte ?
.if WARN 
.warn "al ",*," .ConvayerRightDelta "
.endif
ConvayerLeftIndex .byte ?
.if WARN 
.warn "al ",*," .ConvayerLeftIndex "
.endif
ConvayerRightIndex .byte ?
.if WARN 
.warn "al ",*," .ConvayerRightIndex "
.endif
NumJumpless2 .byte ?	
DoChargePump .byte ?	
FallingOrOnGround .byte ?	
DoJump	.byte ?	

*= $0801 ; 00 0C 08 0A 00 9E 20 32 30 36 34 00 00
	.word (+), 2005 ;pointer, line number
	.null $9e, ^start;will be sys 4096
+	.word 0	 ;basic line end
	
*= $0810
start
		jsr setirq ; clear all interupts and put out own in
		sei
		lda $dd02
		ora #2
		sta $dd02
		lda $dd00
		and #252
		ora #2
		sta $dd00
		lda #%00000010
		sta $d018
		lda #%00001000
		sta $d016
		lda #6
		sta $d021
		lda #0
		sta $d022
		sta $d020
		lda #1
		sta $d023
		lda #9
		sta $d025
		lda #1
		sta $d026
		lda #kSprBase
		sta mplex.sprp
		lda #kSprBase+1
		sta mplex.sprp+1
		lda #1
		sta $d015
		lda #255
		sta $d01c
		lda #15
		sta mplex.sprc
		sta mPlex.xmsb+1
		lda #0
		sta mplex.sprc+1
		sta mPlex.xmsb
		;lda #%00010000
		;sta $d011
		; clear Mplex 
		ldx #$00	  ;init x with 00
-		txa			  ;transfer x to a to create index vals
		sta mplex.sort,x	;store it in the sort table
		lda #255
		sta mplex.ypos,x	; disbale all sprites
		inx			  
		cpx # mPlex.kMaxSpr+1	 ;have we reached 32 yet?
		bne -
		ldx #0
		lda #0
-		sta variables,x		; clear all the variables
		inx
		bne -
		jsr emptyCRAM
		;jsr plotTileMap
		jsr resetPlayerData 
		jsr setPlayerToSpawnPoint
		lda #0
		sta mplex.lsbtod
		lda #5
		sta GameData.lives
		lda #1
		sta ConvayerLeftDelta
		sta ConvayerRightDelta
		jsr loadMapVectors
		jsr clearLargeMapArea
		jsr convertMapDataToLargeMap
		jsr resetCurrMapPtrToFirstScreen
		jsr copyBigMapToLittleMap
		jsr initMapTracker
		jsr plotCRAMForCurrentScreen
		jsr setStatusScreenSprites
		jsr plotStatusChars
		jsr buildConvayerChars
		; set up ECBM colours
		lda #0
		sta $d021
		lda #6
		sta $d022
		lda #3
		sta $d023
		lda #1
		sta $d024
		
		
		lda #9
		sta HiScore.Digit6
		lda #8
		sta HiScore.Digit5
		lda #6
		sta HiScore.Digit4
		lda #1
		sta HiScore.Digit3
		lda #7
		sta HiScore.Digit2
		lda #4
		sta HiScore.Digit1
		jsr plotScore
		jsr plotHiScore
		lda #0
		sta $300
		sta $301
;		
;		cli
;		-		lda $300	
;		bne +	
;		lda $301		
;		beq -	
;		dec $301
;		dec $d020
;		jsr clearCRAMForCurrentScreen
;		jsr moveMapTrackeDownOneRow
;		jsr plotCRAMForCurrentScreen		
;		dec $d020		
;		jsr moveCurrMapPtrDownOneRow
;		jsr copyBigMapToLittleMap
;		inc $d020
;		inc $d020
;		jmp -	
;+
;		dec $300
;		dec $d020
;		jsr clearCRAMForCurrentScreen
;		jsr moveMapTrackeUpOneRow
;		jsr plotCRAMForCurrentScreen		
;		dec $d020		
;		jsr moveCurrMapPtrUpOneRow
;		jsr copyBigMapToLittleMap
;		inc $d020
;		inc $d020
;		jmp -

		
		
		cli
;
;		main loop
;
MainLoop
-		lda mplex.lsbtod	
		beq -	
		lda $300
		beq _skipDebug
		lda #159
		sta mplex.ypos
		lda #0
		sta $300
_skipDebug
		lda $0301
		beq _skipDebug2
		lda #181
		sta mPlex.yPos
		lda #0
		sta $0301
_skipDebug2
		lda #0
		sta mplex.lsbtod
		jsr updateTickdowns
		jsr joyToPlayerDelta
		jsr checkCollision
		lda checkSpriteToCharData.xDeltaCheck
		beq _addY
		lda mplex.xpos
		clc
		adc checkSpriteToCharData.xDeltaCheck
		sta ZPTemp
		; xdelta +ve if this is +ve but original was -ve we have gone over
		lda checkSpriteToCharData.xDeltaCheck
		bmi _subbedX
		lda mplex.xpos
		bpl _loadX 
		; so last pos in negative >80
		lda ZPTemp
		bmi _storeX
		; new pos is positive 0-80
	;	lda #0			; enable MSB
	;	sta mplex.xmsb
		jmp _storeX
_subbedX
		; xdelta -ve if this is -ve but original was +ve we have gone over
		lda mplex.xpos
		bmi _loadX
		; last post is positive >80
		lda ZPTemp
		bpl _storeX		
	;	lda #1			; clear MSB
	;	sta mplex.xmsb
		lda #255
		jmp _storeX
_loadX
		lda ZPTemp
_storeX		
	;	ldx mplex.xmsb
	;	beq _XClipInMSB
	;	cmp #24
	;	bcs _storeX2
	;	lda #24
	;	bne _storeX2
;_XClipInMSB	
;		cmp #8	
;		bcc _storeX2	
;		lda #8		
_storeX2
		sta mplex.xpos
		sta mplex.xpos+1
_addY		
+		lda mplex.ypos
		clc
		adc checkSpriteToCharData.yDeltaCheck
		sta mplex.ypos
		sta mplex.ypos+1
		jsr updatePlayerAnim
		lda #0
		sta ScrollDelta
		lda mplex.ypos
		cmp # kDeadZone.top
		bcs _noScroll
		
		;lda mPlex.yPos
		;clc
		;adc #8
		
		lda # kDeadZone.top
		sec
		sbc mplex.ypos
		sta ScrollDelta
		clc
		adc yScroll
		sta yScroll
		ldx # kDeadZone.top+1
		stx mPlex.yPos
		stx mPlex.yPos+1
		cmp #8
		bcc WriteD011
		sec
		sbc #8
		sta yScroll
		ora #%01010000
		sta $d011
;		inc $d020
		
		jsr clearCRAMForCurrentScreen		
		jsr moveMapTrackeUpOneRow
		jsr plotCRAMForCurrentScreen
		jsr moveCurrMapPtrUpOneRow
		jsr copyBigMapToLittleMap
		jmp PostLoop
;		dec $d020
_noScroll
		cmp # kDeadZone.bottom
		bcc PostLoop
		lda currMapPtr+1
		cmp # >(kVectors.mapBottom-(40*23))
		bcc Fall ; if we are above fall
		bne PostLoop ; if we are not equal thus greater then check the lo
		lda currMapPtr
		cmp # <(kVectors.mapBottom-(40*23))
		bcc Fall ; above that lo as well fall

PostLoop
		jsr updatePlayerDoubleJumpFlash
		jsr updateMovingPlatform
		inc $d020
		jsr rolConvayerLeft
		jsr rorConvayerRight
		dec $d020
		lda ScrollDelta
		beq _noYMove
		jsr updateEntsY
_noYMove
		jmp -		
				
WriteD011
		lda yScroll
		ora #%01010000
		sta $d011
		jmp PostLoop		
				
Fall
		lda # kDeadZone.bottom
		sec
		sbc mplex.ypos
		sta ScrollDelta
		
		lda mplex.ypos
		sec
		sbc # kDeadZone.bottom
		sta ZPTemp
		lda yScroll
		sec
		sbc ZPTemp
		sta yScroll
		ldx # kDeadZone.bottom-1
		stx mPlex.yPos
		stx mPlex.yPos+1
		cmp #0
		bpl WriteD011
		clc
		adc #8
		sta yScroll
		ora #%01010000
		sta $d011
		
;		inc $d020
		jsr clearCRAMForCurrentScreen
		jsr moveMapTrackeDownOneRow
		jsr plotCRAMForCurrentScreen
		jsr moveCurrMapPtrDownOneRow
		jsr copyBigMapToLittleMap
;		dec $d020
		jmp PostLoop

;
; end loop
;
checkCharsInternal		
	lda (checkSpriteToCharData.screenLoc),y
	cmp # kCharDefines.coral
	beq CCong
	cmp # kCharDefines.ice
	beq CConIce
	cmp # kCharDefines.leftConvayer
	beq CConLeftCon
	cmp # kCharDefines.rightConvayer
	beq CConRightCon	
	rts		
			
checkCollision
	lda checkSpriteToCharData.yDeltaCheck
	bmi CCexit	
	lda mplex.ypos
	cmp #222
	bcc CCcheckChars
	pha
	pha ; to fix up for the dropthrough
CCong
	pla
	pla ; pull jsr to checkCharsInternal off the stack
	jmp setPlayerOnGroundStatus
	; implied rts from above
CCexit
	jmp checkOtherEnts
CConIce
	lda #1
	sta PlayerData.onIce
	bne CCong
CConLeftCon	
	lda #1	
	sta PlayerData.onConvayerLeft
	bne CCong
CConRightCon
	lda #1
	sta PlayerData.onConvayerRight
	bne CCong
	
CCcheckChars
	lda mplex.yPos
	lda mplex.yPos
	sec
	sbc #34
	sta ZPTemp
	and #7
	cmp #4
	bcs checkMovingPlatforms
	lda ZPTemp
	lsr 
	lsr 
	lsr 
	sta checkSpriteToCharData.yCharCollide
	tax
	lda mplex.xPos
	sec
	sbc #16
	lsr 
	lsr 
	lsr
	sta checkSpriteToCharData.xCharCollide
	clc
	lda screenRowLUTLO,x
	adc checkSpriteToCharData.xCharCollide
	sta checkSpriteToCharData.screenLoc
	lda screenRowLUTHi,x
	adc #0
	sta checkSpriteToCharData.screenLoc+1
	ldy #0
	jsr checkCharsInternal
	ldy #1
	jsr checkCharsInternal
	; fall through to checkMovingPlatforms
	
checkMovingPlatforms
	lda mplex.yPos
	clc
	adc #11
	sta ZPTemp
	ldx #2
_l	lda MovingPlatform.numSprites,x
	bne _foundOne
_next	
	dex	
	bpl _l	
	lda PlayerData.onGround
	beq _exit
	jsr setPlayerFallenOffLedge
_exit	
	jmp checkOtherEnts

_foundOne
	lda MovingPlatform.sprIndex,x
	tay
	lda kEntSpriteBuffer.entYPtr,y
	sec
	sbc ZPTemp 
	bmi _next ; they are above me
	sbc #4
	bpl _next ; they are below me
	; now we check the X
	lda MovingPlatform.width,x
	sta NormalTemp1
	lda mplex.xpos
	sec
	sbc kEntSpriteBuffer.entXPtr,y
	adc #10 ; player left X collide
	bmi _next
	ldy NormalTemp1
	sbc platformWidthTable,y
	bpl _next
	stx PlayerData.onMovingPlatform
	jsr setPlayerOnGroundStatus
	jmp checkOtherEnts ; could be fall thorugh eventually
	
checkOtherEnts
	ldx #2
_l	lda OtherEnts.mapTableY,x
	bne _foundOne
_next	
	dex
	bpl _l
	rts
_foundOne
	lda OtherEnts.sprIndex,x
	tay
	lda kEntSpriteBuffer.entXPtr,y
	clc
	adc #8
	sta NormalTemp1
	lda kEntSpriteBuffer.entYPtr,y
	clc
	adc #8
	sta NormalTemp2
	
	lda mplex.xpos
	clc
	adc #8
	sec
	sbc NormalTemp1
	sta ZPTemp
	sec
	sbc #$F0 ; -16
	bvc _skipEORX
	eor #$80
_skipEORX
	bmi _next ; not in range
	lda ZPTemp
	bmi _goodX
	cmp #16
	bcs _next ; not in range
_goodX	
	lda mPlex.yPos
	clc
	adc #8
	sec
	sbc NormalTemp2
	sta ZPTemp
	sec
	sbc #256-16 ;$F0 ; -16
	bvc _skipEORY
	eor #$80
_skipEORY
	bmi _next
	lda ZPTemp
	bmi _goodY
	cmp #16
	bcs _next
	; hit
_goodY
	lda OtherEnts.MapTableY,x
	tay
	lda #$80				; make it taken
	sta (mapXEnd),y	
	lda OtherEnts.sprIndex,x
	sta entitySpriteBufferPointer
	tay
	lda # kEmptySprite
	sta kEntSpriteBuffer.entPtrPtr,y
	jsr advanceESBPointer
	ldy entitySpriteBufferPointer
	lda # kEmptySprite
	sta kEntSpriteBuffer.entPtrPtr,y	
	lda OtherEnts.type,x
	sec
	sbc # kSpriteAutoBounceID
	asl a
	tay
	lda EntFuncTable+1,y
	pha
	lda EntFuncTable,y
	pha
	rts ; call function above
EntFuncTable .rta CollFuncBouce,CollFuncDoubleJump 
 	
CollFuncBouce	
	lda #0
	sta PlayerData.yDeltaAccum
	sta PlayerData.onGround
	sta PlayerData.isFalling
	lda #$FA
	sta PlayerDAta.yDeltaAccum+1
	lda #1
	sta PlayerData.hasJumped
	rts
	
CollFuncDoubleJump
	lda #1
	sta PlayerData.deltaToAddToJumped
	rts
		
platformWidthTable	
	.byte 3*8,3*8,3*8,3*8,4*8,5*8,6*8,7*8,8*8,9*8	
	
screenRowLUTLO		
.for ue = kVectors.charBase, ue < kVectors.charBase + $400, ue = ue + 40
.byte <ue
.next
screenRowLUTHi	
.for ue = kVectors.charBase, ue < kVectors.charBase + $400, ue = ue + 40
.byte >ue
.next

joyToPlayerDelta
		jsr scanJoystick
		lda #0
		sta checkSpriteToCharData.xDeltaCheck
		sta checkSpriteToCharData.yDeltaCheck
		lda PlayerData.onIce
		beq _notIce
		lda PlayerData.lastXDelta
		sta checkSpriteToCharData.xDeltaCheck
_notIce
		lda PlayerData.onConvayerLeft
		beq _notConLeft
		lda #0
		sec
		sbc ConvayerLeftDelta
		sta checkSpriteToCharData.xDeltaCheck
_notConLeft
		lda PlayerData.onConvayerRight
		beq _notConRight
		lda ConvayerRightDelta
		sta checkSpriteToCharData.xDeltaCheck
_notConRight
		lda PlayerData.onGround
		bne _noChangeLR
; Check Left and Right
		lda joyLeft
		beq _cr
		lda #$ff
		sta checkSpriteToCharData.xDeltaCheck
		sta PlayerData.lastXDelta
		jmp _noChangeLR
_cr		lda joyRight
		beq _noChangeLR
		lda #1		
		sta checkSpriteToCharData.xDeltaCheck
		sta PlayerData.lastXDelta
; Check Up and Down		
;_cu		lda joyLeft
;		ora joyRight
;		bne _noChangeLR
		; joy l r = not movement
_noChangeLR		

; calculate nu Jumps < 2
		ldx #0
		lda PlayerData.hasJumped
		cmp #2
		bcs _skip1
		ldx #1
_skip1	stx NumJumpless2
		; calculate FallingOrOnGround
		lda PlayerData.isFalling
		ora PlayerData.onGround
		sta FallingOrOnGround
		; calculate Do Charge Pump
		and joyFire
		sta DoChargePump
		;calculate DoJump		
		lda joyFire		
		eor #1 ; not joyFire
		and PlayerData.fireWasDown		; has fire been released
		and NumJumpless2 ; and we stil have slots		
		sta DoJump		
		;calculate new falling value 
		; isFalling = jump > 0 and yDeltaAccum > 0
		ldx #0
		lda PlayerData.hasJumped
		beq _skip2
		clc
		adc PlayerData.yDeltaAccum + 1
		bmi _skip2
		ldx #1
_skip2	stx PlayerData.isFalling
		; react
		lda DoJump
		beq skipJumpSetup
		lda PlayerData.hasJumped
		adc PlayerData.deltaToAddToJumped
		sta PlayerData.hasJumped	; we are jumping
		lda PlayerData.jumpChargePump
		sta PlayerData.yDeltaAccum	; set the Y jump accleration
		lda PlayerData.jumpChargePump + 1	; set the other half of jump accleration
		sta PlayerData.yDeltaAccum + 1
		jsr ClearPlayerOnGroundStatus
		lda #0
		sta PlayerData.jumpChargePump
		sta PlayerData.jumpChargePump + 1
		lda PlayerData.yDeltaAccum + 1
		sta checkSpriteToCharData.yDeltaCheck
		jmp deltaExit
skipJumpSetup
		lda DoChargePump
		beq noChargePump
		lda PlayerData.jumpChargePump
		sec
		sbc #10
		sta PlayerData.jumpChargePump
		lda PlayerData.jumpChargePump+1
		sbc #0
		sta PlayerData.jumpChargePump+1
		lda PlayerData.fireWasDown
		bne _justUpdate
		lda #8
		sta PlayerData.spriteFlashChargePump
_justUpdate
		jsr updatePlayerFlash
		jmp endChargePump		
noChargePump
		lda # kPlayerParams.normalColour
		sta mplex.sprc
endChargePump
		
_updateJump		
		lda PlayerData.onGround		; are we on the ground	
		bne downOne
		ldx PlayerData.isFalling
_incD	jsr incPlayerYDeltaAndReturn	; no, we are in gravity so fall
		sta checkSpriteToCharData.yDeltaCheck
;		cmp #0
;		bmi _notFalling
;		lda #1
;		sta PlayerData.isFalling
;_notFalling		
deltaExit	
		lda joyFire
		sta PlayerData.fireWasDown
		rts
		
downOne
		lda #1
		sta checkSpriteToCharData.yDeltaCheck
		bne deltaExit		
			
.comment		
		lda PlayerData.isFalling
		ora PlayerData.onGround
		beq _restSpriteColour
		lda joyFire
		beq _restSpriteColour
		lda PlayerData.jumpChargePump
		sec
		sbc #10
		sta PlayerData.jumpChargePump
		lda PlayerData.jumpChargePump+1
		sbc #0
		sta PlayerData.jumpChargePump+1
		lda PlayerData.fireWasDown
		bne _justUpdate
		lda #1
		sta PlayerData.fireWasDown
		lda #8
		sta PlayerData.spriteFlashChargePump
_justUpdate
		jsr updatePlayerFlash
		jmp _handleJumpLogic
;		jmp _incD ; could be bne
_restSpriteColour
		lda # kPlayerParams.normalColour
		sta mplex.sprc
_handleJumpLogic
		lda PlayerData.onGround
		beq _updateJump ; not on ground
		lda joyFire
		bne _ong
		lda PlayerData.fireWasDown
		beq _ong
		
_checkStartJump		
		lda PlayerData.hasJumped	; have we already jumped
		cmp #2
		bcc _startJump				; no then jump
		lda PlayerData.isFalling	; have we started falling
		bne _updateJump				; yes handle the fall case then
		lda PlayerData.onGround		; or we on the ground and thus we have not let go since last jump
		bne _updateJump				; if on gound then don't move more
		; pressing up whilst we jump
_cancelJump
		ldx #0
		jsr incPlayerYDeltaAndReturn
		bne _upe					; still able to jump
		lda #1
		sta PlayerData.isFalling	; start the fall
_upe	sta checkSpriteToCharData.yDeltaCheck
		rts		
_startJump
		lda PlayerData.hasJumped
		cmp	#2
		bcs _cancelJump
		lda #0
		sta PlayerData.fireWasDown
	;	lda PlayerData.onGround		; have we trided to jump while in mid air
;		beq _updateJump						; ignore it
		lda PlayerData.hasJumped
		adc PlayerData.deltaToAddToJumped
		sta PlayerData.hasJumped	; we are jumping
		lda PlayerData.jumpChargePump
		sta PlayerData.yDeltaAccum	; set the Y jump accleration
		lda PlayerData.jumpChargePump + 1	; set the other half of jump accleration
		sta PlayerData.yDeltaAccum + 1
		jsr ClearPlayerOnGroundStatus
		lda #0
		sta PlayerData.jumpChargePump
		sta PlayerData.jumpChargePump + 1
		lda PlayerData.yDeltaAccum + 1
		jmp _upe					; start jumping
			
_noUp	lda PlayerData.fireWasDown
		bne _startJump
		lda PlayerData.onGround
		bne _ong 
		lda PlayerData.hasJumped
		bne _updateJump
		
_ong	lda #0
		sta PlayerData.hasJumped
_downOne
		lda #1
		sta checkSpriteToCharData.yDeltaCheck
		rts
		
_updateJump		
		lda PlayerData.onGround		; are we on the ground	
		bne _downOne
		ldx PlayerData.isFalling
_incD	jsr incPlayerYDeltaAndReturn	; no, we are in gravity so fall
		sta checkSpriteToCharData.yDeltaCheck
		cmp #0
		bmi _notFalling
		lda #1
		sta PlayerData.isFalling
_notFalling		
		rts
.endc		
		
ClearPlayerOnGroundStatus
		lda #$FF
		sta PlayerData.onMovingPlatform
		lda #0
		sta PlayerData.isFalling	; not falling
		sta PlayerData.onGround		; not on the ground
		sta PlayerData.onIce
		sta PlayerData.onConvayerLeft
		sta PlayerData.onConvayerRight
		rts
		
setPlayerFallenOffLedge
		lda #0
		sta PlayerData.onGround
		sta PlayerData.onIce
		sta PlayerData.onConvayerLeft
		sta PlayerData.onConvayerRight
		lda #$FF
		sta PlayerData.onMovingPlatform
		lda #1
		sta PlayerData.hasJumped
		sta PlayerData.isFalling
		rts
		
setPlayerOnGroundStatus
		lda #1
		sta PlayerData.onGround
		sta PlayerData.yDeltaAccum
		lda #0
		sta PlayerData.isFalling
		sta PlayerData.hasJumped
		sta PlayerData.yDeltaAccum + 1
		sta checkSpriteToCharData.yDeltaCheck
		rts
		
;changePlayerDir
;		sta PlayerData.facingRight
;		tax
;		lda PlayerData.movingLR
;		beq _still
;		inx	 ; THIS IS BAD AND A HACK
;		inx
;_still	jsr setPlayerAnimeTo
;		rts
	
incPlayerYDeltaAndReturn
		lda PlayerData.yDeltaAccum
		clc
		adc PlayerJumpLUT,x
		sta PlayerData.yDeltaAccum
		lda PlayerData.yDeltaAccum + 1
		adc #0
		bmi +
		cmp # kPlayerParams.maxFallSpeed
		bcc +
		lda # kPlayerParams.maxFallSpeed
+		sta PlayerData.yDeltaAccum + 1
		rts
		
PlayerJumpLUT .byte kPlayerParams.jumpDeltaAccum, kPlayerParams.jumpDeltaAccumFloat

setPlayerAnimeTo
		rts
				
updatePlayerAnim
		rts 
		
resetPlayerData
	lda #$FF
	sta PlayerData.onMovingPlatform
	lda #2
	sta PlayerData.deltaToAddToJumped
	lda #0
	sta PlayerData.dead
	sta PlayerData.onIce
	sta PlayerData.onGround
	sta PlayerData.hasJumped
	rts
		
setPlayerToSpawnPoint
	lda #128+24
	sta mplex.xpos
	sta mplex.xpos+1
	sta mplex.xmsb+1
	lda #222
	sta mplex.ypos
	sta mplex.ypos+1
	rts
			
emptyCRAM
		ldx #00
		lda #6
-		sta $d800,x
		sta $d900,x
		sta $da00,x
		sta $db00,x
	;	sta $dc00,x
		dex
		bne -
		rts
		
plotStatusChars
		lda #< (kVectors.charBase + 40 - 9)
		sta screenPointer
		lda #> (kVectors.charBase + 40 - 9)
		sta screenPointer+1		
		ldx #24
_loop2
		lda #0
		ldy #8
_loop
		sta (screenPointer),y
		dey
		bpl _loop
		lda screenPointer
		clc
		adc #40
		sta screenPointer
		lda screenPointer+1
		adc #0
		sta screenPointer+1
		dex
		bpl _loop2
		rts
		
		
clearCRAMForCurrentScreen		
		lda #6		
		sta CRAMPlotColour		
		sta CRAMBorderPlotColour	
		bne plotCRAMinternal		
		
plotCRAMForCurrentScreen
		lda #10
		sta CRAMPlotColour
		lda #0
		sta CRAMBorderPlotColour
		
plotCRAMinternal
		ldy MapTrackingData.topIndex
		iny 
		lda (mapYDelta),y
		sec
		sbc MapTrackingData.topCharsToNext
		sta ZPTemp
		tax
		dey
_loop
		lda screenROWLUTLo,x
		sta screenPointer
		lda screenROWLUTHi,x
		eor # (>kVectors.charBase) ^ $D8
		sta screenPointer+1
		lda (mapType),y
		cmp # kSpriteLevelDataTypesStart
		bcs _skip
		tax
		lda (mapXEnd),y
		sta ZPTemp2
		lda (mapXStart),y
		sta ZPTemp3
		lda CRAMBorderPlotColour
		bne _clear		
		lda charColourTBL,x
_clear
		sta CRAMPlotColour
		tya
		pha
_plot	ldy ZPTemp3 ; do leading cap
_loop2	lda CRAMPlotColour		; middle row
		sta (screenPointer),y		
		iny		
		cpy ZPTemp2
		bcc _loop2
		beq _loop2
	
_skipEnd
		pla
		tay
_skip
		lda (mapYDelta),y
		clc
		adc ZPTemp
		adc #1
		sta ZPTemp
		tax
		dey
		cpy MapTrackingData.bottomIndex
		bcc _done
		bmi _done		
		jmp _loop ; probably can be bne
_done	rts


.comment 		
plotTileMap		
		ldy #00
-		lda kVectors.charBase,y
		tax
		lda fileCharCols,x
		sta $d800,y
		lda kVectors.charBase+$100,y
		tax
		lda fileCharCols,x
		sta $d900,y
		lda kVectors.charBase+$200,y
		tax
		lda fileCharCols,x
		sta $da00,y
		lda kVectors.charBase+$300,y
		tax
		lda fileCharCols,x
		sta $db00,y
		;lda kVectors.charBase+$3FF,y
		;tax
		;lda fileCharCols,x
		;sta $dbff,y
		dey
		bne -
		rts
.ENDC		
		
resetCurrMapPtrToFirstScreen
		lda # <(kVectors.mapBottom-(40*23))
		sta currMapPtr
		lda # >(kVectors.mapBottom-(40*23))
		sta currMapPtr+1
		rts
		
initMapTracker
		ldy #0
		sty MapTrackingData.bottomCharsToNext
		sty MapTrackingData.bottomIndex
		sty ZPTemp
		clc
_findTop
		lda (mapYDelta),y
		adc ZPTemp
		sta ZPTemp
		cmp #24
		beq _deadOn
		bcs _goneOver
		iny
		bne _findTop

_goneOver		
		sec		
		sbc (mapYDelta),y	
		sta ZPTemp	
		lda #25	
		sec	
		sbc ZPTemp	
		dey
		;dey
_exit
		sta MapTrackingData.topCharsToNext	
		sty MapTrackingData.topIndex	
		rts	
_deadOn							
		lda #0						
		sta _exit						
								
moveCurrMapPtrUpOneRow
		sec
		lda currMapPtr
		sbc #40
		sta currMapPtr
		lda currMapPtr+1
		sbc #0
		sta currMapPtr+1
		rts

moveMapTrackeUpOneRow
		lda MapTrackingData.topCharsToNext
		sec
		sbc #1
		sta MapTrackingData.topCharsToNext
		bpl _notExpired
		ldy MapTrackingData.topIndex		
		iny
		lda (mapType),y
		cmp # kSpriteLevelDataTypesStart
		bcc _notSpecial		
		jsr addMovingPlatformTop		
_notSpecial		
		ldy MapTrackingData.topIndex		
		iny
		sty MapTrackingData.topIndex
		iny
		lda (mapYDelta),y
		sta MapTrackingData.topCharsToNext
_notExpired
		lda MapTrackingData.bottomCharsToNext
		sec
		sbc #1
		sta MapTrackingData.bottomCharsToNext
		bpl _notExpired2
		ldy MapTrackingData.bottomIndex
		lda (mapType),y
		cmp # kSpriteLevelDataTypesStart
		bcc _notSpecial2
		sec ; remove from bottom
		jsr removeMovingPatform
_notSpecial2
		iny
		sty MapTrackingData.bottomIndex
		;iny
		lda (mapYDelta),y
		sta MapTrackingData.bottomCharsToNext
_notExpired2		
		rts
		
moveMapTrackeDownOneRow
		ldy MapTrackingData.topIndex
		iny
		lda (mapYDelta),y
		cmp MapTrackingData.topCharsToNext
		beq _expired
		lda MapTrackingData.topCharsToNext
		clc
		adc #1
		sta MapTrackingData.topCharsToNext
		bne _notExpired ; just added one can't be FF right?
_expired		
		ldy MapTrackingData.topIndex
		lda (mapType),y
		cmp # kSpriteLevelDataTypesStart
		bcc _notSpecial2		
		clc ; remove from top	
		jsr removeMovingPatform		
_notSpecial2		
		dec MapTrackingData.topIndex	
		lda #0
		sta MapTrackingData.topCharsToNext
_notExpired
		ldy MapTrackingData.bottomIndex
		lda (mapYDelta),y
		cmp MapTrackingData.bottomCharsToNext
		beq _expired2
		lda MapTrackingData.bottomCharsToNext
		clc
		adc #1
		sta MapTrackingData.bottomCharsToNext
		rts
_expired2
		dec MapTrackingData.bottomIndex
		ldy MapTrackingData.bottomIndex
		lda (mapType),y
		cmp # kSpriteLevelDataTypesStart
		bcc _notSpecial		
		jsr addMovingPlatformBottom		
_notSpecial		
		lda #0
		sta MapTrackingData.bottomCharsToNext
_notExpired2		
		rts
		

ABI_foundOne
		lda ZPTemp2
		sta OtherEnts.type,x
		jsr calcSpriteXFromMapStart
		tya
		sta OtherEnts.mapTableY,x
		lda (mapXEnd),y
		sta NormalTemp1
		plp
		bcs _tail
		lda entitySpriteBufferHead
		sta entitySpriteBufferPointer ; cache the current head
		sta OtherEnts.sprIndex,x ; write the sprite index to use
		tax
		lda #2 ; number of sprite to alloc
		jsr allocAInESBHead				; make room for it
		jmp _allocd
_tail	lda #2 ; number of sprite to alloc
		jsr allocAInESBTail
		lda entitySpriteBufferTail
		sta entitySpriteBufferPointer ; cache the current tail		
		sta OtherEnts.sprIndex,x ; write the sprite index to use
		tax
_allocd		
		ldy ZPTemp2
		lda ZPTemp3
		sta kEntSpriteBuffer.entYPtr,x
		lda NormalTemp3
		sta kEntSpriteBuffer.entXPtr,x
		lda MapTypeToSpriteColours - kSpriteLevelDataTypesStart - 2,y
		sta kEntSpriteBuffer.entColPtr,x
		lda #0
		sta kEntSpriteBuffer.entXMSBPtr,x
		lda NormalTemp1
		bmi _blankIt
		lda MapTypeToSprite - kSpriteLevelDataTypesStart - 2,y
		bne _storeIT
_blankIt
		lda # kEmptySprite
_storeIT
		sta kEntSpriteBuffer.entPtrPtr,x
		jsr advanceESBPointer
		ldx entitySpriteBufferPointer
		lda ZPTemp3
		sta kEntSpriteBuffer.entYPtr,x
		lda NormalTemp3
		sta kEntSpriteBuffer.entXPtr,x
		sta kEntSpriteBuffer.entXMSBPtr,x
		lda #0
		sta kEntSpriteBuffer.entColPtr,x		
		lda NormalTemp1
		bmi _blankIt2
		lda MapTypeToSprite - kSpriteLevelDataTypesStart - 2,y
		clc
		adc #1
		bne _storeIT2
_blankIt2
		lda # kEmptySprite
_storeIT2	
		sta kEntSpriteBuffer.entPtrPtr,x
		rts
addBounceInteral		
		ldx #0
_l		lda OtherEnts.mapTableY,x
		bne +
		jmp ABI_foundOne
+		inx
		cpx #3
		bne _l
		inc $d020 ; error
		rts
		
addBounceTop
		sta ZPTemp2
		clc
		php
		lda #50-8
		sta ZPTemp3
		bne addBounceInteral
		
addBounceBottom
		sta ZPTemp2
		sec
		php
		lda #250
		sta ZPTemp3
		bne addBounceInteral
	
MapTypeToSprite
.byte kEntSprits.bounceBase,kEntSprits.doubleJumpBase
MapTypeToSpriteColours
.byte 2,3

calcSpriteXFromMapStart
		lda (mapXStart),y
		asl a
		asl a
		asl a
		clc
		adc #24
		sta NormalTemp3	; X start pos of Sprite		
		rts		
		
addMovingPlatformTop		
		cmp #kSpriteAutoBounceID		
		bcs addBounceTop		
		clc
		php
		lda #50-8
		sta ZPTemp3
		bne addMovingPlatformInternal
		
addMovingPlatformBottom
		cmp #kSpriteAutoBounceID		
		bcs addBounceBottom
		sec
		php
		lda #250
		sta ZPTemp3
		bne addMovingPlatformInternal
		
;call with Y being the line index
addMovingPlatformInternal
		sty ZPTemp2
		ldx #0
_l		lda MovingPlatform.numSprites,x
		beq _foundOne
		inx
		cpx #3
		bne _l
		inc $d020 ; error
		rts
_foundOne		
		lda (mapXEnd),y				; get the X end
		sec
		sbc (mapXStart),y
		sta MovingPlatform.width,x	; X width of platform
		sbc #1 						;make 0 based
		sta NormalTemp1
		sta NormalTemp5
		jsr calcSpriteXFromMapStart
		stx NormalTemp2				; moving plaform number
		ldx NormalTemp1				; X width of platform 
		lda PlatformSizeToSpritesCountLUT,x	
		ldx NormalTemp2				; moving plaform number
		sta MovingPlatform.numSprites,x
		sta NormalTemp1				; number of sprites
		plp
		bcs _tail
		lda entitySpriteBufferHead
		sta entitySpriteBufferPointer ; cache the current head
		sta MovingPlatform.sprIndex,x ; write the sprite index to use
		lda NormalTemp1	
		jsr allocAInESBHead				; make room for it
		jmp _allocd
_tail	jsr allocAInESBTail
		lda entitySpriteBufferTail
		sta entitySpriteBufferPointer ; cache the current tail		
		sta MovingPlatform.sprIndex,x ; write the sprite index to use	
_allocd		
		tya							; save the end number
		sta MovingPlatform.mapTableY,x				
		lda NormalTemp5				; number of sprites
		asl a
		asl a ; times by 4
		sta NormalTemp4		
		ldy #0	
_setSpritesLoop			
		lda ZPTemp3					; y to appear at
		ldx entitySpriteBufferPointer	; sprite number
		sta kEntSpriteBuffer.entYPtr,x	; store Y
		lda NormalTemp3					; get start X
		sta kEntSpriteBuffer.entXPtr,x	; store X
		lda #1
		sta kEntSpriteBuffer.entXMSBPtr,x ; Mono Sprite
		lda # kSprBase+30
		sta kEntSpriteBuffer.entPtrPtr,x  ; bugs
		lda #1
		sta kEntSpriteBuffer.entColPtr,x ; white
		inc NormalTemp4					; next sprite delta offset	
		ldx NormalTemp4
		lda MovingPlatformXDeltaTable,x
		clc 
		adc NormalTemp3
		sta NormalTemp3
		jsr advanceESBPointer 	; next sprite
		iny
		cpy NormalTemp1			; number of sprites
		bne _setSpritesLoop
		
		ldy ZPTemp2
		rts

removeBounce
		lda #2
		plp
		bcs _bottom
		jsr removeAInESBHead
		jmp _done
_bottom	jsr removeAInESBTail
_done	ldx #2
_l		lda OtherEnts.MapTableY,x
		cmp ZPTemp2
		beq _found
		dex
		bpl _l
		inc $d020
		rts
_found	lda #0
		sta OtherEnts.MapTableY,x
		ldy #1
		lda OtherEnts.sprIndex,x
		sta entitySpriteBufferPointer
		jmp RemoveSpritesRestoreYExit
		
		
;y contains the map index to remove
; carry set = remove from bottom
removeMovingPatform
		php			; store the Carry flag
		sty ZPTemp2
		cmp #kSpriteAutoBounceID		
		bcs removeBounce
		ldx #0
_l		lda MovingPlatform.numSprites,x
		beq _next
		lda MovingPlatform.mapTableY,x
		sta ZPTemp3
		cpy ZPTemp3
		beq _foundOne
_next	inx
		cpx # kSpriteLevelDataTypesStart
		bne _l
		dec $d020
		rts
_foundOne
		lda MovingPlatform.numSprites,x
		tay
		plp
		bcs _bottom
		jsr removeAInESBHead
		jmp _done
_bottom	jsr removeAInESBTail
_done
		lda MovingPlatform.sprIndex,x
		sta entitySpriteBufferPointer
		lda #0
		sta MovingPlatform.numSprites,x
		sta MovingPlatform.mapTableY,x
		sta MovingPlatform.sprIndex,x
		dey ; must drop below
RemoveSpritesRestoreYExit		
		lda #$ff
		ldx entitySpriteBufferPointer
		sta kEntSpriteBuffer.entYPtr,x
		jsr advanceESBPointer
		dey
		bpl RemoveSpritesRestoreYExit
		ldy ZPTemp2
		rts
		
updateMovingPlatform		
		ldx #2
		stx ZPTemp
_l		lda MovingPlatform.numSprites,x
		bne _foundOne
_next	dec ZPTemp
		ldx ZPTemp
		bpl _l
		rts
_foundOne
		sec
		sbc #1
		sta NormalTemp1
		lda MovingPlatform.sprIndex,x
		sta entitySpriteBufferPointer
		lda MovingPlatform.direction,x
		bne _back		
		lda PlayerData.onMovingPlatform
		cmp ZPTemp
		bne _notOnThisOne
		inc mplex.xpos
		inc mplex.xpos+1
_notOnThisOne		
		ldy entitySpriteBufferPointer	
		lda kEntSpriteBuffer.entXPtr,y 
		clc
		adc #1
		sta kEntSpriteBuffer.entXPtr,y
		jsr advanceESBPointer
		dec NormalTemp1
		bpl _notOnThisOne
		lda kEntSpriteBuffer.entXPtr,y 
		ldx #0 ; fix me
		cmp MovingPlatformMaxXForLenght,x
		bcs _flip
		jmp _next
_flip	ldx ZPTemp
		lda MovingPlatform.direction,x
		eor #1
		sta MovingPlatform.direction,x
		jmp _next
		
_back	lda PlayerData.onMovingPlatform
		cmp ZPTemp
		bne _notOnThisOneEither
		dec mplex.xpos
		dec mplex.xpos+1
_notOnThisOneEither
		ldy entitySpriteBufferPointer	
		lda kEntSpriteBuffer.entXPtr,y 
		cmp #25
		bcc _flip
		sec
		sbc #1
		sta kEntSpriteBuffer.entXPtr,y
		jsr advanceESBPointer
		dec NormalTemp1
		bpl _notOnThisOneEither
		bmi _next

		
updateEntsY		
		jsr resetESBPointer	
_loop	lda entitySpriteBufferPointer
		cmp entitySpriteBufferHead 
		beq _exit
		tax
		lda kEntSpriteBuffer.entYPtr,x
		clc
		adc ScrollDelta
		sta kEntSpriteBuffer.entYPtr,x
		jsr advanceESBPointer
		jmp _loop
_exit	rts 

MovingPlatformXDeltaTable		
.byte 00,00,00,1 ; 3		
.byte 00,00,00,1 ; 3		
.byte 00,08,00,2 ; 4		
.byte 00,16,00,2 ; 5		
.byte 00,24,00,2 ; 6		
.byte 00,24,08,3 ; 7		
.byte 00,24,16,3 ; 8		
.byte 00,24,24,3 ; 9		

PlatformSizeToSpritesCountLUT	
.byte 1,1,1,2,2,2,3,3,3	

MovingPlatformMaxXForLenght	
.byte 255,255,255 ; first 3
.for count = 1, count < 7, count = count + 1 
.byte 255-((count)*8)	
.next	

initEntitySpriteBuffer
	lda # kEntSpriteBuffer.startOffset
	sta entitySpriteBufferHead
	sta entitySpriteBufferTail
	sta entitySpriteBufferPointer
	rts
	
allocAInESBHead
	clc
	adc entitySpriteBufferHead
	cmp # kEntSpriteBuffer.endOffset-kEntSpriteBuffer.startOffset
	bcc _noOver
	sec
	sbc # kEntSpriteBuffer.endOffset-kEntSpriteBuffer.startOffset
_noOver
	sta entitySpriteBufferHead
	rts

allocAInESBTail
	sta NormalTemp4
	lda entitySpriteBufferTail
	sec
	sbc NormalTemp4
	bpl _noOver
	clc
	adc # kEntSpriteBuffer.endOffset-kEntSpriteBuffer.startOffset
_noOver
	sta entitySpriteBufferTail
	rts	
		
removeAInESBTail
	clc
	adc entitySpriteBufferTail
	cmp # kEntSpriteBuffer.endOffset-kEntSpriteBuffer.startOffset
	bcc _noOver
	sec
	sbc # kEntSpriteBuffer.endOffset-kEntSpriteBuffer.startOffset
_noOver
	sta entitySpriteBufferTail
	rts
	
removeAInESBHead
	sta NormalTemp1
	lda entitySpriteBufferHead
	sec
	sbc NormalTemp1
	cmp # kEntSpriteBuffer.endOffset-kEntSpriteBuffer.startOffset
	bcc _noUnder
	clc
	adc # kEntSpriteBuffer.endOffset-kEntSpriteBuffer.startOffset
_noUnder
	sta entitySpriteBufferHead
	rts
	
resetESBPointer
	lda entitySpriteBufferTail
	sta entitySpriteBufferPointer
	rts

advanceESBPointer
	lda entitySpriteBufferPointer
	clc
	adc #1
	cmp # kEntSpriteBuffer.endOffset-kEntSpriteBuffer.startOffset
	bcc _noOver
	lda #0
_noOver
	sta entitySpriteBufferPointer
	rts
	
		
moveCurrMapPtrDownOneRow
		clc
		lda currMapPtr
		adc #40
		sta currMapPtr
		lda currMapPtr+1
		adc #0
		sta currMapPtr+1
		rts		
				
copyBigMapToLittleMap
		lda currMapPtr
	;	adc #40
		sta MapCopyTemp
		lda currMapPtr+1
	;	adc #0
		sta MapCopyTemp+1
		lda MapCopyTemp
		clc
		adc #40
		sta pointer1
		lda MapCopyTemp+1
		adc #0
		sta pointer1+1
		lda pointer1
		clc
		adc #40
		sta pointer2
		lda pointer1+1
		adc #0
		sta pointer2+1
		lda pointer2
		clc
		adc #40
		sta pointer3
		lda pointer2+1
		adc #0
		sta pointer3+1
		
		
;		lda #$d8
;		sta ZPTemp
		;lda # >kVectors.charBase
		;sta screenPointer+1
		;lda #$00
		;sta ZPTemp2
		;sta screenPointer
		;lda #4
		;pha
		ldy #30
_loop	lda (MapCopyTemp),y
		sta kVectors.charBase+00,y
		lda (pointer1),y
		sta kVectors.charBase+40,y
		lda (pointer2),y
		sta kVectors.charBase+80,y
		lda (pointer3),y
		sta kVectors.charBase+120,y
	;	tax
	;	lda fileCharCols,x
	;	sta (ZPTemp2),y
		dey
		bpl _loop
		
		lda MapCopyTemp
		adc #160
		sta MapCopyTemp
		lda MapCopyTemp+1
		adc #0
		sta MapCopyTemp+1
		lda MapCopyTemp
		clc
		adc #40
		sta pointer1
		lda MapCopyTemp+1
		adc #0
		sta pointer1+1
		lda pointer1
		clc
		adc #40
		sta pointer2
		lda pointer1+1
		adc #0
		sta pointer2+1
		lda pointer2
		clc
		adc #40
		sta pointer3
		lda pointer2+1
		adc #0
		sta pointer3+1
		ldy #30
_loop2	lda (MapCopyTemp),y
		sta kVectors.charBase+160,y
		lda (pointer1),y
		sta kVectors.charBase+200,y
		lda (pointer2),y
		sta kVectors.charBase+240,y
		lda (pointer3),y
		sta kVectors.charBase+280,y
		dey
		bpl _loop2		
		lda MapCopyTemp
		adc #160
		sta MapCopyTemp
		lda MapCopyTemp+1
		adc #0
		sta MapCopyTemp+1
		lda MapCopyTemp
		adc #40
		sta pointer1
		lda MapCopyTemp+1
		adc #0
		sta pointer1+1
		lda pointer1
		clc
		adc #40
		sta pointer2
		lda pointer1+1
		adc #0
		sta pointer2+1
		lda pointer2
		clc
		adc #40
		sta pointer3
		lda pointer2+1
		adc #0
		sta pointer3+1
		ldy #30
_loop3	lda (MapCopyTemp),y
		sta kVectors.charBase+320,y
		lda (pointer1),y
		sta kVectors.charBase+360,y
		lda (pointer2),y
		sta kVectors.charBase+400,y
		lda (pointer3),y
		sta kVectors.charBase+440,y
		dey
		bpl _loop3
		
		lda MapCopyTemp
		adc #160
		sta MapCopyTemp
		lda MapCopyTemp+1
		adc #0
		sta MapCopyTemp+1
		lda MapCopyTemp
		clc
		adc #40
		sta pointer1
		lda MapCopyTemp+1
		adc #0
		sta pointer1+1
		lda pointer1
		clc
		adc #40
		sta pointer2
		lda pointer1+1
		adc #0
		sta pointer2+1
		lda pointer2
		clc
		adc #40
		sta pointer3
		lda pointer2+1
		adc #0
		sta pointer3+1
		ldy #30
_loop4	lda (MapCopyTemp),y
		sta kVectors.charBase+480,y
		lda (pointer1),y
		sta kVectors.charBase+520,y
		lda (pointer2),y
		sta kVectors.charBase+560,y
		lda (pointer3),y
		sta kVectors.charBase+600,y
		dey
		bpl _loop4
		
		lda MapCopyTemp
		adc #160
		sta MapCopyTemp
		lda MapCopyTemp+1
		adc #0
		sta MapCopyTemp+1
		lda MapCopyTemp
		clc
		adc #40
		sta pointer1
		lda MapCopyTemp+1
		adc #0
		sta pointer1+1
		lda pointer1
		clc
		adc #40
		sta pointer2
		lda pointer1+1
		adc #0
		sta pointer2+1
		lda pointer2
		clc
		adc #40
		sta pointer3
		lda pointer2+1
		adc #0
		sta pointer3+1
		ldy #30
_loop5	lda (MapCopyTemp),y
		sta kVectors.charBase+640,y
		lda (pointer1),y
		sta kVectors.charBase+680,y
		lda (pointer2),y
		sta kVectors.charBase+720,y
		lda (pointer3),y
		sta kVectors.charBase+760,y
		dey
		bpl _loop5
		
		lda MapCopyTemp
		adc #160
		sta MapCopyTemp
		lda MapCopyTemp+1
		adc #0
		sta MapCopyTemp+1
		lda MapCopyTemp
		clc
		adc #40
		sta pointer1
		lda MapCopyTemp+1
		adc #0
		sta pointer1+1
		lda pointer1
		clc
		adc #40
		sta pointer2
		lda pointer1+1
		adc #0
		sta pointer2+1
		lda pointer2
		clc
		adc #40
		sta pointer3
		lda pointer2+1
		adc #0
		sta pointer3+1
		ldy #30
_loop6	lda (MapCopyTemp),y
		sta kVectors.charBase+800,y
		lda (pointer1),y
		sta kVectors.charBase+840,y
		lda (pointer2),y
		sta kVectors.charBase+880,y
		lda (pointer3),y
		sta kVectors.charBase+920,y
		dey
		bpl _loop6
	;	tax
	;	lda fileCharCols,x
	;	sta (ZPTemp2),y
		;dey
		;beq _loop
		;pla
		;sec
		;sbc #1
		;beq _exit
		;pha
		;inc screenPointer+1
		;inc MapCopyTemp+1
	;	inc ZPTemp
		;jmp _loop
_exit	rts

loadMapVectors
		lda # <leveldatafile
		sta mapYDelta
		lda # >leveldatafile
		sta mapYDelta+1
		lda # <startX
		sta mapXStart
		lda # >startX
		sta mapXStart+1
		lda # <endX
		sta mapXEnd
		lda # >endX
		sta mapXEnd+1
		lda # <type
		sta mapType
		lda # >type
		sta mapType+1
		lda # <kVectors.mapBottom
		sta currMapPtr
		lda # >kVectors.mapBottom
		sta currMapPtr+1
		rts
		
clearLargeMapArea
		sei
		ldx #0
		lda #65
_loop	
.for ptr = $6000, ptr < $c000, ptr = ptr + $100
		sta ptr,x
.next
		inx
		beq _exit
		jmp _loop
_exit
		cli
		rts
		
		
convertMapDataToLargeMap
		lda rowCounts
		sta numItemsMap
		ldy #0
		sty mapItemIndex
		sty FirstLineToPlot
_nextItem
		ldy mapItemIndex
		lda (mapYDelta),y
		tax
		beq _sameLine
_loopYDelta
		lda currMapPtr		
		sec		
		sbc #40		
		sta currMapPtr		
		lda currMapPtr+1		
		sbc #0		
		sta currMapPtr+1		
		dex		
		bpl _loopYDelta				
		jmp _startPlot				
_sameLine
		cpy #0
		bne _startPlot
		inc FirstLineToPlot		
_startPlot
		lda (mapXEnd),y
		sta ZPTemp
		inc ZPTemp
		lda (mapXStart),y
		sta ZPTemp2
		lda (mapType),y
		cmp # kSpriteLevelDataTypesStart
		bcs _skipBottom ; not a solid or ice so skip it
		tax 
		lda TypeToCharLUT,x
		sta pointer2

		lda currMapPtr
		sec
		sbc #40
		sta	pointer1
		lda currMapPtr+1
		sbc #0
		sta pointer1+1
		

		lda #15 ; top line
		jsr _doXLoop
		
		jsr addFortyToBothZPTemp
		
		ldy ZPTemp2
		dey
		lda #16 ; left line
		sta (pointer1),y
		lda pointer2
		jsr _doXLoop
		lda #17 ; top line
		sta (pointer1),y
		lda FirstLineToPlot
		bne _skipBottom
		
		jsr addFortyToBothZPTemp
		
		lda #18 ; bottom line
		jsr _doXLoop
_skipBottom
		lda #0
		sta FirstLineToPlot
		
		inc mapItemIndex
		lda mapItemIndex
		cmp numItemsMap
		bcc _nextItem2
		rts
_nextItem2 		
		jmp _nextItem		
_doXLoop
		ldy ZPTemp2
_writeXLoop
		sta (pointer1),y
		iny
		cpy ZPTemp
		bcc _writeXLoop
		rts
		
addFortyToBothZPTemp
		lda ZPTemp		
		clc		
		adc #40		
		sta ZPTemp		
		lda ZPTemp2
		clc
		adc #40
		sta ZPTemp2
		rts
TypeToCharLUT		
		.byte 0,kCharDefines.coral,kCharDefines.ice,kCharDefines.check,kCharDefines.leftConvayer,kCharDefines.rightConvayer		
updateTickdowns
	ldx # size(TickDowns)-1
_l	lda TickDowns,x
	beq _next
	dec TickDowns,x
_next
	dex
	bpl _l
	rts
	
scanJoystick
		lda #0
		sta joyDown
		sta joyLeft
		sta joyRight
		sta joyUp
		sta joyFire
		ldx #1
		lda $DC00
		lsr 
		bcc _joyUp
		lsr
		bcc _joyDown
_checkLR 
		lsr
		bcc _joyLeft
		lsr
		bcc _joyRight
_checkFire		
		lsr 
		bcs _joyEnd
		stx joyFire
_joyEnd rts
		
_joyUp	
		lsr ; skip down bit
		stx joyUp
		jmp _checkLR
		
_joyDown 
		stx joyDown
		jmp _checkLR
		
_joyLeft 
		stx joyLeft
		lsr ; skip right bit
		jmp _checkFire

_joyRight
		stx joyRight
		jmp _checkFire		
				
setStatusScreenSprites				
	ldy # statusSpritesDefs.kNum-1		
_loop		
	lda statusSpritesDefs.x,y		
	sta mplex.xpos+2,y		
	lda statusSpritesDefs.y,y		
	sta mplex.ypos+2,y		
	lda statusSpritesDefs.def,y		
	sta mplex.sprp+2,y		
	lda # statusSpritesDefs.kColour		
	sta mplex.sprc+2,y		
	dey	
	bpl _loop	
	rts	
	
plotHiScore	
	lda #6	; start digit
	ldx #11	; always draw digit
	ldy #12	; max digit
	bne plotScoreInternal	
	
plotScore
	lda #0 ; start digit start at 6th digit in stuct
	ldx #5 ; always draw
	ldy #6 ; max digit
plotScoreInternal
	sta ZPTemp ; digit index
	stx pointer3
	sty pointer3+1
	lda # <kScoreSprites.baseAddr
	sta pointer1
	lda # >kScoreSprites.baseAddr
	sta pointer1+1
	lda # <kFont.digitsPointer
	sta pointer2
	lda # >kFont.digitsPointer
	sta pointer2+1
	
_loop
	ldx ZPTemp
	lda Score,x
	bne _needToDraw
	cpx pointer3				; always draw last digit
	beq _needToDraw
_next
	inx
	stx ZPTemp
	cpx pointer3+1
	bne _loop		
	rts		
_needToDraw				
	asl a			
	asl a			
	asl a ; times digit by 8			
	tay			
	clc
	adc #7
	sta ZPTemp2
	txa
	lda spriteOffsetIndex,x	
	tax
_loopPlt				
	lda (pointer2),y					
	sta kScoreSprites.baseAddr,x				
	inx				
	inx			
	inx			
	iny				
	cpy ZPTemp2				
	bne _loopPlt				
	ldx ZPTemp			
	jmp _next				
				
spriteOffsetIndex				
	.byte kScoreSprites.scoreDigit6,kScoreSprites.scoreDigit5,kScoreSprites.scoreDigit4,kScoreSprites.scoreDigit3,kScoreSprites.scoreDigit2,kScoreSprites.scoreDigit1				
	.byte kScoreSprites.hiScoreDigit6,kScoreSprites.hiScoreDigit5,kScoreSprites.hiScoreDigit4,kScoreSprites.hiScoreDigit3,kScoreSprites.hiScoreDigit2,kScoreSprites.hiScoreDigit1				
PlayerChargeColourTable				
	.byte 9,2,8,10,7,10,8,2
PlayerChargeColourCount = * -playerChargeColourTable 

PlayerDoubleJumpColourTable
;	.byte 14,14,3,3,14,14,0,0
	.byte 11,5,13,7,1,7,13,5,11
PlayerDoubleJumpFlashCount = * -PlayerDoubleJumpColourTable

 				
updatePlayerFlash
	lda TickDowns.playerFlash
	bne _exit
	lda PlayerData.spriteFlashChargePump+1
	sec
	sbc #40
	sta PlayerData.spriteFlashChargePump+1
	lda PlayerData.spriteFlashChargePump
	sbc #0
	sta PlayerData.spriteFlashChargePump
	bpl _skip
	lda #0
_skip
	sta TickDowns.playerFlash
	
	ldx PlayerData.spriteFlashIndex
	lda PlayerChargeColourTable,x
	sta mplex.sprc
	inx
	cpx # playerChargeColourCount
	bne _notrest
	ldx #0
_notrest	
	stx PlayerData.spriteFlashIndex
_exit	
	rts
	
updatePlayerDoubleJumpFlash
	lda PlayerData.deltaToAddToJumped
	cmp #1
	bne _exit
	lda TickDowns.playerDoubleJumpFlash
	bne _exit
	lda #8
	sta TickDowns.playerDoubleJumpFlash
	ldx PlayerData.spriteFlashDoubleJumpIndex
	lda PlayerDoubleJumpColourTable,x
	sta mplex.sprc+1
	inx
	cpx # PlayerDoubleJumpFlashCount
	bne _notReset
	ldx #0
_notReset
	stx PlayerData.spriteFlashDoubleJumpIndex
_exit
	rts
	
kConvayerLeftChar = 22	
kConvayerRightChar = 23	
kConvayerRolledChars = 87
kConvayerRolledCharsRight = kConvayerRolledChars+8

buildConvayerChars
	lda # <fileChars+(8*kConvayerRolledChars)
	sta pointer1
	lda # >fileChars+(8*kConvayerRolledChars)
	sta pointer1+1
	ldx #7
_loop
	ldy #7
_copy	
	lda fileChars+(8*kConvayerLeftChar),y	
	sta (pointer1),y	
	dey	
	bpl _copy	
.for c = 0 , c < 8 , c = c + 1
	lda fileChars+(8*kConvayerLeftChar)+c
	asl a
	rol fileChars+(8*kConvayerLeftChar)+c
+	
.next		
	lda pointer1		
	clc		
	adc #8		
	sta pointer1		
	lda pointer1+1		
	adc #0		
	sta pointer1+1		
	dex	
	bpl _loop	
	
	lda # <fileChars+(8*kConvayerRolledCharsRight)
	sta pointer1
	lda # >fileChars+(8*kConvayerRolledCharsRight)
	sta pointer1+1
	ldx #7
_loop2
	ldy #7
_copy2	
	lda fileChars+(8*kConvayerRightChar),y	
	sta (pointer1),y	
	dey	
	bpl _copy2	
.for c = 0 , c < 8 , c = c + 1
	lda fileChars+(8*kConvayerRightChar)+c
	asl a
	rol fileChars+(8*kConvayerRightChar)+c
+	
.next		
	lda pointer1		
	clc		
	adc #8		
	sta pointer1		
	lda pointer1+1		
	adc #0		
	sta pointer1+1		
	dex	
	bpl _loop2	
	
	rts	
		
rolConvayerLeft
	lda ConvayerLeftIndex
	clc
	adc ConvayerLeftDelta
	and #7
	sta ConvayerLeftIndex
	asl a ; 2
	asl a ; 4
	asl a ; 8
	clc   ; should be safe to remove
	adc #7
	tax
	ldy #7
_loop
	lda fileChars+(8*kConvayerRolledChars),x
	sta fileChars+(8*kConvayerLeftChar),y
	dex
	dey
	bpl _loop
	rts
	
rorConvayerRight
	lda ConvayerRightIndex
	sec
	sbc ConvayerRightDelta
	and #7
	sta ConvayerRightIndex
	asl a ; 2
	asl a ; 4
	asl a ; 8
	clc   ; should be safe to remove
	adc #7
	tax
	ldy #7
_loop
	lda fileChars+(8*kConvayerRolledCharsRight),x
	sta fileChars+(8*kConvayerRightChar),y
	dex
	dey
	bpl _loop
	rts

; multiplexor
setirq
	sei			 ;set interrupt disable
	lda #%01010000
	sta $d011		 ;raster irq to 1st half of screen.
	lda # kRaster.bottomRaster
	sta $d012		 ;irq to happen at line #$fb
	lda #<irq0
	sta $fffe		 ;hardware irq vector low byte
	lda #>irq0
	sta $ffff		 ;hardware irq vector high byte
	lda #$1f
	sta $dc0d		 ;turn off all types of cia irq/nmi.
	sta $dd0d
	lda #$01
	sta $d01a		 ;turn on raster irq.
	lda #$35
	sta $01		 ;no basic or kernal
	lda $dc0d		 ;acknowledge any irq that has occured during setup.
	lda $dd0d
	inc $d019
	lda # <start
	sta $fffc
	lda # >start
	sta $fffd
	lda # <justRTI
	sta $fffa
	lda # >justRTI
	sta $fffb
	cli			 ;clear interrupt disable
	rts			 ;return from subroutine

irq0
	pha		;use stack instead of zp to prevent bugs.
	txa
	pha
	tya
	pha  ;13
	inc $d019		 ;acknowledge irq 19
;	inc $d021
		ldx #0
slop  ldy mplex.sort+1,x	;main index sort algo
slep  lda mplex.ypos,y
	  ldy mplex.sort,x		;this sorter uses the previous frame as a prediction buffer.
	  cmp mplex.ypos,y		;as y position does not change much from frame to frame.
	  bcc swap				;otherwise, it is a simple swap sort.
	  inx					;our linked list (sort) is sorted in decending order, according
	  cpx #mplex.kMaxSpr-1	;to sprite y positions.
	  bne slop
	  beq end
swap 
	  lda mplex.sort+1,x
	  sta mplex.sort,x
	  sty mplex.sort+1,x
	  tay
	  dex
	  bpl slep
	  inx
	  beq slop
end

.for spr = 0, spr < mPlex.kMaxSpr, spr = spr + 1
	  ldy mplex.sort+spr		;re arrange frame buffers, into the raster buffers.
	  lda mplex.ypos,y			;this is unrolled for speed.
	  sta mplex.ybuf+spr		;this allows us to use only 1 index pointer for our sprite plotter.
	  lda mplex.xpos,y			;it is double buffered, to allow runtime code to calculate the sprite
	  sta mplex.xbuf+spr		;positions.
	  lda mplex.xmsb,y
	  sta mplex.mbuf+spr
	  lda mplex.sprc,y
	  sta mplex.cbuf+spr
	  lda mplex.sprp,y
	  sta mplex.pbuf+spr
.next

		ldx #$00	 ;find # of used sprites (you can remove sprites by
		stx mplex.sptr	 ;placing #$ff into the ypos buffer for the corresponding
maxc	lda mplex.ybuf,x   ;sprite. It will not be displayed by the raster routine.
		cmp #$ff
		beq maxs
		inx
		cpx mplex.kMaxSpr
		bne maxc
maxs	stx mplex.cnt		 ;max sprites this frame count.
		cpx #$07	 ;check if were doing more than 8
		bcc maxm	 ;if not, we want the plotter to stop after 1 irq.
		ldx #$07	 
maxm	stx mplex.mnt

		lda #$ff	;reset sprites to off screen.
		sta $d001	;prevents bugs.
		sta $d003
		sta $d005
		sta $d007
		sta $d009
		sta $d00b
		sta $d00d
		sta $d00f
		sta $d015

		inc mplex.lsbtod	 ;buffers are swapped, so we can do the next frame now.

		lda #<irq1	 ;irq chain for raster code. prolly want a routine before
		sta $fffe	 ;this one, to turn the sprites back on ;)
		lda #>irq1	 ;i.e. lda #$ff sta $d015
		sta $ffff
		lda #$28
		sta $d012
;	dec $d021
		jmp eirq
		
irq1
		pha			;save registers
		txa
		pha
		tya
		pha
		inc $d019		;acknowledge irq
		lda mplex.cnt
		bne +
		jmp done			; don't have any sprites to render exit
+		ldx mplex.sptr		;get current sprite index
hlop1	lda mplex.ybuf,x	;get sprite y position
		sta $d001		;store sprite y postion.
		lda mplex.xbuf,x	;get sprite x position.
		sta $d000		;sta sprite x position.
		lda mplex.mbuf,x	;get sprite x position msb
		bne no1		;set msb register
		lda $d01c
		ora #%00000001
		bne yes1
no1		lda $d01c
		and #%11111110
yes1	sta $d01c
		lda mplex.pbuf,x	;get sprite image pointer
		sta kVectors.spr0ID		;store it screen.
		lda mplex.cbuf,x	;get sprite color
		sta $d027		;store sprite color
		inx			;next sprite index
		cpx mplex.mnt		;lets go to next plot, if < then 8 yet.
		bcc hlop2
		cpx mplex.cnt		;no more sprites?
		bne ok1
		jmp done		;no more sprites.

ok1		stx mplex.sptr		;save sprite index
		lda $d003		;get last position of next sprite
		clc
		adc #$15		;add 21 lines
		cmp $d012		;we there yet?
		bcc hlop2		;yeah, so plot next sprite
		adc #$02		;no, so calculate next irq position (+3)
		sta $d012		;set it
		lda #<irq2	;irq for next sprite.
		sta $fffe
		lda #>irq2
		sta $ffff
		jmp eirq

irq2
		pha			;and so on
		txa
		pha
		tya
		pha
		inc $d019
		ldx mplex.sptr
hlop2	lda mplex.ybuf,x
		sta $d003
		lda mplex.xbuf,x
		sta $d002
		lda mplex.mbuf,x
		bne no2
		lda $d01c
		ora #%00000010
		bne yes2
no2		lda $d01c
		and #%11111101
yes2	sta $d01c
		lda mplex.pbuf,x
		sta kVectors.spr1ID		;store it screen.
		lda mplex.cbuf,x
		sta $d028
		inx
		cpx mplex.mnt
		bcc hlop3
		cpx mplex.cnt
		bne ok2
		jmp done

ok2		stx mplex.sptr
		lda $d005
		clc
		adc #$15
		cmp $d012
		bcc hlop3
		adc #$02
		sta $d012
		lda #<irq3
		sta $fffe
		lda #>irq3
		sta $ffff
		jmp eirq

irq3
		pha
		txa
		pha
		tya
		pha
		inc $d019
		ldx mplex.sptr
hlop3	lda mplex.ybuf,x
		sta $d005
		lda mplex.xbuf,x
		sta $d004
		lda mplex.mbuf,x
		bne no3
		lda $d01c
		ora #%00000100
		bne yes3
no3		lda $d01c
		and #%11111011
yes3	sta $d01c
		lda mplex.pbuf,x
		sta kVectors.spr2ID		;store it screen.
		lda mplex.cbuf,x
		sta $d029
		inx
		cpx mplex.mnt
		bcc hlop4
		cpx mplex.cnt
		bne ok3
		jmp done

ok3		stx mplex.sptr
		lda $d007
		clc
		adc #$15
		cmp $d012
		bcc hlop4
		adc #$02
		sta $d012
		lda #<irq4
		sta $fffe
		lda #>irq4
		sta $ffff
		jmp eirq

irq4  
		pha
		txa
		pha
		tya
		pha
		inc $d019
		ldx mplex.sptr
hlop4	lda mplex.ybuf,x
		sta $d007
		lda mplex.xbuf,x
		sta $d006
		lda mplex.mbuf,x
		bne no4
		lda $d01c
		ora #%00001000
		bne yes4
no4		lda $d01c
		and #%11110111
yes4	sta $d01c
		lda mplex.pbuf,x
		sta kVectors.spr3ID		;store it screen.
		lda mplex.cbuf,x
		sta $d02a
		inx
		cpx mplex.mnt
		bcc hlop5
		cpx mplex.cnt
		bne ok4
		jmp done

ok4		stx mplex.sptr
		lda $d009
		clc
		adc #$15
		cmp $d012
		bcc hlop5
		adc #$02
		sta $d012
		lda #<irq5
		sta $fffe
		lda #>irq5
		sta $ffff
		jmp eirq

irq5
		pha
		txa
		pha
		tya
		pha
		inc $d019
		ldx mplex.sptr
hlop5	lda mplex.ybuf,x
		sta $d009
		lda mplex.xbuf,x
		sta $d008
		lda mplex.mbuf,x
		bne no5
		lda $d01c
		ora #%00010000
		bne yes5
no5		lda $d01c
		and #%11101111
yes5	sta $d01c
		lda mplex.pbuf,x
		sta kVectors.spr4ID		;store it screen.
		lda mplex.cbuf,x
		sta $d02b
		inx
		cpx mplex.mnt
		bcc hlop6
		cpx mplex.cnt
		bne ok5
		jmp done

ok5		stx mplex.sptr
		lda $d00b
		clc
		adc #$15
		cmp $d012
		bcc hlop6
		adc #$02
		sta $d012
		lda #<irq6
		sta $fffe
		lda #>irq6
		sta $ffff
		jmp eirq

irq6	pha
		txa
		pha
		tya
		pha
		inc $d019
		ldx mplex.sptr
hlop6	lda mplex.ybuf,x
		sta $d00b
		lda mplex.xbuf,x
		sta $d00a
		lda mplex.mbuf,x
		bne no6
		lda $d01c
		ora #%00100000
		bne yes6
no6		lda $d01c
		and #%11011111
yes6	sta $d01c
		lda mplex.pbuf,x
		sta kVectors.spr5ID		;store it screen.
		lda mplex.cbuf,x
		sta $d02c
		inx
		cpx mplex.mnt
		bcc hlop7
		cpx mplex.cnt
		bne ok6
		jmp done

ok6		stx mplex.sptr
		lda $d00d
		clc
		adc #$15
		cmp $d012
		bcc hlop7
		adc #$02
		sta $d012
		lda #<irq7
		sta $fffe
		lda #>irq7
		sta $ffff
		jmp eirq

irq7	pha
		txa
		pha
		tya
		pha
		inc $d019
		ldx mplex.sptr
hlop7	lda mplex.ybuf,x
		sta $d00d
		lda mplex.xbuf,x
		sta $d00c
		lda mplex.mbuf,x
		bne no7
		lda $d01c
		ora #%01000000
		bne yes7
no7		lda $d01c
		and #%10111111
yes7	sta $d01c
		lda mplex.pbuf,x
		sta kVectors.spr6ID		;store it screen.
		lda mplex.cbuf,x
		sta $d02d
		inx
		cpx mplex.mnt
		bcc hlop8
		cpx mplex.cnt
		bne ok7
		jmp done

ok7		stx mplex.sptr
		lda $d00f
		clc
		adc #$15
		cmp $d012
		bcc hlop8
		adc #$02
		sta $d012
		lda #<irq8
		sta $fffe
		lda #>irq8
		sta $ffff
		jmp eirq

irq8	pha
		txa
		pha
		tya
		pha
		inc $d019
		ldx mplex.sptr
hlop8 	lda mplex.ybuf,x
		sta $d00f
		lda mplex.xbuf,x
		sta $d00e
		lda mplex.mbuf,x
		bne no8
		lda $d01c
		ora #%10000000
		bne yes8
no8	  	lda $d01c
		and #%01111111
yes8	sta $d01c
		lda mplex.pbuf,x
		sta kVectors.spr7ID		;store it screen.
		lda mplex.cbuf,x
		sta $d02e
		inx
		cpx mplex.mnt
		bcc hlop9
		cpx mplex.cnt
		bne ok8
		jmp done

ok8		stx mplex.sptr
		lda $d001
		clc
		adc #$15
		cmp $d012
		bcc hlop9
		adc #$02
		sta $d012
		lda #<irq1
		sta $fffe
		lda #>irq1
		sta $ffff
		jmp eirq
hlop9 	jmp hlop1

done 	lda #<irq0
		sta $fffe
		lda #>irq0
		sta $ffff
		lda # kRaster.bottomRaster
		sta $d012
eirq	pla
		tay
		pla
		tax
		pla
justRTI	rti

statusSpritesDefs .block		
	kNum = 10
	kColour = 1
	;   | high score          |  score      |    stage    | Lives | timer
x .byte 200+24+8,200+24,200+48,200+24,200+48,200+24,200+48,200+36 , 200+27, 200+51
y .byte 50,71,71, 50+54,50+54, 50+92,50+92, 50+130, 50+168,50+168
def .byte kSprBase+38,kSprBase+36,kSprBase+37, kSprBase+34,kSprBase+35, kSprBase+39,kSprBase+40, kSprBase+41, kSprBase+44,kSprBase+45 	
.bend		

; first is a dummy 
charColourTBL .byte 0,10,14,10,12,12,12

*= $4000
fileScreen .binary "map.raw"
*= $4400
fileCharCols ;		
.binary "attribs.raw"	
*= $4500
;fileTiles ;		
;.binary "tiledefs.raw"	; needs to be 80 bytes
*= $4800
fileChars ;
.binary "chars.raw"
*= $5000
fileSprites ;
.binary "sprites.bin"				

.include "level.inc"			
			