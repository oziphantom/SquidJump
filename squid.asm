WARN = 0
NTSC = 0
DISABLE_PLAYER = 0
DISABLE_RASTER_DEBUG = 1

.if NTSC = 1
	NUM_FRAMES = 60
.else
	NUM_FRAMES = 50
.endif

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
	timerPreShiftsBase = $f000
	timerZeroCache = timerPreShiftsBase + ( 15 * NUM_FRAMES )
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
	waterUpdate = 8
.bend

kPlayerParams .block
	jumpStartDelta = 255-1
	jumpDeltaAccum = 80
	jumpDeltaAccumFloat = 28
	maxFallSpeed = 4
	maxRiseSpeed = $f6
	maxRiseSpeedLo = $20
	chargeRate = 50
	normalColour = 15
	Xdelta = 30
	XRestoreDelta = 5
	maxXDelta = 1
	maxXDeltaNegative = 255 - (maxXDelta-1)
	startFlashTimer = 8
	minFlashTimer = 4
	chargeAnimFrameTimer = 4
	spriteEnableValue = 3
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

kTimerSprites .block 
	firstSpriteBase = ( kSprBase + 44 ) * 64 + 3 + $4000
	secondSpriteBase = firstSpriteBase + 64
.bend

kEntSprits .block
	bounceBase = kSprBase + 24
	doubleJumpBase = kSprBase + 20
	endOfLevel = kSprBase + 14
	movingPlatformLeft = kSprBase + 30
	movingPlatformRight = kSprBase + 32
.bend

kFont .block
	digitsStart = 48
	digitsPointer = fileChars + ( digitsStart * 8 ) + 1
.bend

kSprBase = 64

kRaster .block
.if NTSC
	bottomRaster = 238
.else
	bottomRaster = 241
.endif
.bend

kDeadZone .block
	top = 160
	bottom = 180
.bend

kSpriteLevelDataTypesStart  = 6
kSpriteAutoBounceID = 8
kEmptySprite = kSprBase+49

.enc level_end
.cdef "GG",41
.cdef "OO",42
.cdef "AA",43
.cdef "LL",44
.cdef "!!",45
.cdef "SS",46
.cdef "TT",47
.cdef "EE",58
.cdef "BB",59	
.cdef "NN",60	
.cdef "UU",61	
.cdef "II",62	
.cdef "MM",63	

sMultiDataSet .struct 
y 		.byte ?,?,?,?
x1		.byte ?,?,?,?
x2		.byte ?,?,?,?
x3		.byte ?,?,?,?
col1	.byte ?,?,?,?
col2	.byte ?,?,?,?
col3	.byte ?,?,?,?
multi	.byte ?,?,?,?
ptr1	.byte ?,?,?,?
ptr2	.byte ?,?,?,?
ptr3	.byte ?,?,?,?
enable 	.byte ?,?,?,?
.ends

kEntSpriteBuffer .block
	startOffset = 0
	endOffset = 4
.bend

kCharDefines .block
	coral = 19
	ice = 128+64+20
	check = 128+64+21
	leftConvayer = 22
	rightConvayer = 128+64+23	
	waterChar1 = 14 + 128
	waterChar2 = 24 + 128
.bend	

kNextRasterType .block
	status = 1
	ent = 2
	both = 3
.bend

kGameState .block
	game = 0
	death = 1
	EOL = 2
	title = 3
	prepareLevel = 4
.bend

* = $02
numRasters .byte ?
rasterSplitYPosEnt .byte ?,?,?,?
rasterSpriteSet .dstruct sMultiDataSet 
entitySpriteSet .dstruct sMultiDataSet 
lsbtod .byte ?
rasterEntityPointerCache .byte ?
rasterTemp1 .byte ?
rasterTemp2 .byte ?
rasterTemp3 .byte ?
rasterTemp4 .byte ?
currEntRasterIndex .byte ?
nextRasterType .byte ? 
playerXLo .byte ? 
playerX .byte ?
playerYLo .byte ?
playerY .byte ?
playerPtrMulti .byte ?
playerPtrMono .byte ?
playerColourMulti .byte ?
playerColourMono .byte ?
playerSpriteEnable .byte ?
status3rdSprCol1 .byte ?
status3rdSprCol2 .byte ?
status3rdSprCol3 .byte ?
status3rdSprCol4 .byte ?

pointer1	.word ? 	
pointer2	.word ?	
pointer3	.word ?	
screenPointer	.word ? 
mapCopyTemp		.word ? 
ZPTemp2			.byte ?
ZPTemp			.byte ?
ZPTemp3			.byte ?
mapYDelta		.word ?
mapXStart		.word ?
mapXEnd			.word ?
mapType			.word ?
currMapPtr		.word ?
numItemsMap		.byte ?
mapItemIndex	.byte ?
bottomMapPtr	.word ?
checkSpriteToCharData .dstruct sCSTCCParams 

.if * >= $100
.warn "ZP Page Overflow"
.endif

sGameData .struct 
lives .byte ?
score .byte ?,?,?,?,?,?
high .byte ?,?,?,?,?,?
currLevel .byte ?
.ends

sTimerTickDowns .struct
playerAnim 	.byte ?
playerFlash .byte ?
playerDoubleJumpFlash .byte ?
water .byte?
movingPlatformAnim .fill kNumMovingPlatforms
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
XDelta .word ?
animeFrameBit .byte ?
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
digit65 .byte ?
;digit5 .byte ?
digit43 .byte ?
;digit3 .byte ?
digit21 .byte ?
;digit1 .byte ?
.ends

kNumMovingPlatforms = 3
sMovingPlatform .struct
y	.byte ?,?,?
sprIndex .byte ?,?,?
numSprites .byte ?,?,?
direction .byte ?,?,?
mapTableY .byte ?,?,?
width	.byte ?,?,?
.ends
kNumOtherEnts = 3
sOtherEnts .struct
y .byte ?,?,?
sprIndex .byte ?,?,?
mapTableY .byte ?,?,?
type .byte ?,?,?
.ends

kMaxMapLength = 30
sMapData .struct
length  .byte ?
yDelta 	.fill kMaxMapLength
xStart 	.fill kMaxMapLength
xEnd   	.fill kMaxMapLength
type	.fill kMaxMapLength
.ends

variables = $0200
* = $0200
joyLeft	 .byte ?
joyRight .byte ?
joyUp	 .byte ?
joyDown	 .byte ?
joyFire	 .byte ?
GameData .dstruct sGameData
PlayerData .dstruct sPlayerData
MapTrackingData .dstruct sMapDataTracking 
Score .dstruct sScoreValues
HiScore .dstruct sScoreValues
; put the level values here
MovingPlatform .dstruct sMovingPlatform
OtherEnts .dstruct sOtherEnts
TickDowns .dstruct sTimerTickDowns
MapData .dstruct sMapData
CRAMPlotColour .byte ?
CRAMBorderPlotColour .byte ?
FirstLineToPlot .byte ?
yScroll	.byte ?
entitySpriteBufferHead .byte ?
entitySpriteBufferTail .byte ?
entitySpriteBufferPointer .byte ?
ScrollDelta .byte ?
NormalTemp1 .byte ?
NormalTemp2 .byte ?
NormalTemp3 .byte ?
NormalTemp4 .byte ?
NormalTemp5 .byte ?
ConvayerLeftDelta .byte ?
ConvayerRightDelta .byte ?
ConvayerLeftIndex .byte ?
ConvayerRightIndex .byte ?
NumJumpless2 .byte ?	
DoChargePump .byte ?	
FallingOrOnGround .byte ?	
DoJump	.byte ?	
YSave .byte ?
;NeedToChangeLevel .byte ?
Level .byte ?
ByteBuffer .byte ?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?
TimerSprite2SrcPtr .byte ?,?
TimerFramesCount .byte ?
TimerSecondsCount .byte ?
TimerTenSecondsCount .byte ?
TimerMinuetsCount .byte ?
currentWaterLevelPtr .word ?
currentWaterCharIndex .byte ?
WaterStartSeconds .byte ?
WaterStartTenSeconds .byte ?
WaterStartMinutes .byte ?
WaterDoWater .byte ?
CurrStatusRasterSpriteIndex .byte ? ; the sprite data index
CurrStatusRasterIndex .byte ? ; the raster index
GameState .byte ? 
MinorState .byte ?
LevelPrepareState .byte ?
LevelPrepareToState .byte ?
RANDSeed .byte ?

.if * > $0400
.warn  "variables over $400"
.endif

.enc none
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
		sta playerPtrMulti
		lda #kSprBase+1
		sta playerPtrMono
		lda #1
		sta $d015
		lda #255
		sta $d01c
		jsr setPlayerToDefaultColours
		ldx #0
		lda #0
-		sta variables,x		; clear all the variables
		sta variables+$100,x ; and all the extra ones
		inx
		bne -
		jsr emptyCRAM
		;jsr plotTileMap
		lda #0
		sta lsbtod
		lda #5
		sta GameData.lives
		lda #1
		sta ConvayerLeftDelta
		sta ConvayerRightDelta
		;sta NeedToChangeLevel		
		jsr plotStatusChars
		jsr buildConvayerChars
		jsr buildTimerFractions
		jsr cacheZeroTimerSprite
		; set up ECBM colours
		lda #0
		sta $d021
		lda #$E
		sta $d022
		lda #3
		sta $d023
		lda #1
		sta $d024
		
		lda # kGameState.EOL
		sta GameState
		jsr setNMIEndOfLevel
		
		lda #$1e
		sta RANDSeed
		
		; fake set up stuff
		lda #$98
		sta HiScore.Digit65
		lda #$61
		sta HiScore.Digit43
		lda #$74
		sta HiScore.Digit21
		jsr plotScore
		jsr plotHiScore
		lda #0
		sta $400
		sta $401
		sta $402
		
		
		cli
;
;		main loop
;
MainLoop
-		lda lsbtod	
		beq -	
		lda GameState
		cmp # kGameState.death
		bne +
		jmp DeathMainLoop
+		cmp # kGameState.EOL
		bne +
		jmp EOLMainLoop
+		cmp # kGameState.prepareLevel
		bne +
		jmp PrepareLevel
+		lda $400
		beq _skipDebug
		lda #159
		sta playerY
		lda #0
		sta $400
_skipDebug
		lda $0401
		beq _skipDebug2
		lda #181
		sta playerY
		lda #0
		sta $0401
_skipDebug2
		lda $0402
		beq _skipDebug3
		jsr DEBUG_UPDATEWATER
		lda #0
		sta $0402
_skipDebug3
		lda #0
		sta lsbtod
		
		jsr updateTickdowns
TEST
		
.if DISABLE_PLAYER = 0
		jsr joyToPlayerDelta
.else
		lda #0
		sta checkSpriteToCharData.xDeltaCheck
		sta checkSpriteToCharData.yDeltaCheck
.endif		
		jsr checkCollision
		lda checkSpriteToCharData.xDeltaCheck
		beq _addY
_addY		
+		lda checkSpriteToCharData.yDeltaCheck	
		beq +	
		lda playerYLo
		clc
		adc PlayerData.yDeltaAccum
		sta playerYLo
		lda playerY
		adc PlayerData.yDeltaAccum+1
		sta playerY
+		jsr updatePlayerAnim
		lda #0
		sta ScrollDelta
		lda playerY
		cmp # kDeadZone.top
		bcs _noScroll
		
		lda # kDeadZone.top
		sec
		sbc playerY
		sta ScrollDelta
		clc
		adc yScroll
		sta yScroll
		ldx # kDeadZone.top+1
		stx playerY
		cmp #8
		bcc WriteD011
		sec
		sbc #8
		sta yScroll
		ora #%01010000
		sta $d011
;		inc $d020
		
		lda #0
		sta pointer3 ; don't offset the water check
		jsr clearCRAMForCurrentScreen		
		jsr moveMapTrackeUpOneRow
		lda #0
		sta pointer3 ; don't offset the water check
		jsr plotCRAMForCurrentScreen
		jsr moveCurrMapPtrUpOneRow
		jsr copyBigMapToLittleMap
		jmp PostLoop
;		dec $d020
_noScroll
		cmp # kDeadZone.bottom
		bcc _updateWater
		lda currMapPtr+1
		cmp # >(kVectors.mapBottom-(40*24))
		bcc Fall ; if we are above fall
		bne _updateWater ; if we are not equal thus greater then check the lo
		lda currMapPtr
		cmp # <(kVectors.mapBottom-(40*24))
		bcc Fall ; above that lo as well fall

_makeSureYScrollIs0
		lda yScroll
		beq _updateWater
		dec yScroll
		
_updateWater
		jsr updateWater		
WriteD011
		lda yScroll
		ora #%01010000
		sta $d011	
PostLoop
		jsr updatePlayerDoubleJumpFlash
		jsr updateMovingPlatform
		jsr updateNearMovingPlatforms
.if DISABLE_RASTER_DEBUG = 0
		inc $d020
.endif
		jsr rolConvayerLeft
		jsr rorConvayerRight
.if DISABLE_RASTER_DEBUG = 0
		dec $d020
.endif
		jsr copyAndAdvanceSecondTimerSprite
		lda ScrollDelta
		beq _noYMove
		jsr updateEntsY
_noYMove
		jmp -		
				

		;jmp PostLoop		
				
Fall
		lda # kDeadZone.bottom
		sec
		sbc playerY
		sta ScrollDelta
		
		lda playerY
		sec
		sbc # kDeadZone.bottom
		sta ZPTemp
		lda yScroll
		sec
		sbc ZPTemp
		sta yScroll
		ldx # kDeadZone.bottom-1
		stx playerY
		cmp #0
		bpl WriteD011
		clc
		adc #8
		sta yScroll
		ora #%01010000
		sta $d011
		
;		inc $d020
		lda #40
		sta pointer3 
		jsr clearCRAMForCurrentScreen
		jsr moveMapTrackeDownOneRow
		lda #80
		sta pointer3
		jsr plotCRAMForCurrentScreen
		jsr moveCurrMapPtrDownOneRow
		jsr copyBigMapToLittleMap
;		dec $d020
		jmp PostLoop

;
; end main loop
;

DeathMainLoop
	lda # kSprBase+47
	sta playerPtrMulti
	lda # kSprBase+48
	sta playerPtrMono
	jsr setPlayerToDefaultColours
	lda MinorState
	beq _swtichModes
	cmp #1
	beq +
	lda #0
	sta MinorState
	lda #kGameState.game
	sta GameState
	jsr RemoveSpritesAndEnts
	jsr ResetLevel
+	jmp MainLoop
_swtichModes
	lda #kGameState.death
	sta LevelPrepareToState
	lda #kGameState.prepareLevel
	sta GameState
	lda #0
	sta LevelPrepareState
	inc MinorState
	jmp MainLoop
	
EOLMainLoop
	lda MinorState
	beq _DoPlot
	cmp #1
	beq _wait
	jsr ResetLevel
_wait
	jmp MainLoop
_DoPlot
	jsr plotEndOfLevel
	jsr RemoveSpritesAndEnts
	inc MinorState
	lda GameState
	sta LevelPrepareToState
	lda #kGameState.prepareLevel
	sta GameState
	lda #0
	sta LevelPrepareState
	jmp MainLoop
	
prepareLevel
	lda LevelPrepareState
	beq _start
	cmp #1
	beq _clear
	; misc resets and set state back
	ldx Level
	jsr loadMapVectors
	jsr convertMapDataToLargeMap
	jsr initMapTracker
	jsr resetCurrMapPtrToFirstScreen
	lda LevelPrepareToState
	sta GameState	
	jmp MainLoop
_start
	jsr startLargeMapArea
	inc LevelPrepareState
	jmp MainLoop
_clear
	jsr clearLargeMapArea		
	bcc _wait		
	inc LevelPrepareState		
_wait		
	jmp MainLoop


checkCharsInternal		
	lda ZPTemp2
	cmp #4
	bcs _justWater
	lda (checkSpriteToCharData.screenLoc),y
	cmp # kCharDefines.coral
	beq CCong
	cmp # kCharDefines.ice
	beq CConIce
	cmp # kCharDefines.leftConvayer
	beq CConLeftCon
	cmp # kCharDefines.rightConvayer
	beq CConRightCon	
_justWater	
	lda (checkSpriteToCharData.screenLoc),y
	cmp # kCharDefines.waterChar1
	beq DEATH
	rts		
		
DEATH			
	lda #1			
	sta PlayerData.dead			
	lda # kGameState.death		
	sta GameState		
	jsr setNMIDeath	
	pla			
	pla			
	rts			
											
ResetLevelNMI											
	pha										
	cld										
	lda $DD0D										
	inc MinorState										
	pla										
	rti										
												
ResetLevel
;		sei
		cld
		lda $DD0D ; ack any NMI
;		jsr clearLargeMapArea ;trahes x so needs to happen before loadvectors
		
		
		jsr copyBigMapToLittleMap
		jsr emptyCRAM
		
		jsr plotCRAMForCurrentScreen
		jsr resetPlayerData 
		jsr setPlayerToSpawnPoint		
		lda #0
		sta MinorState
		lda # kGameState.game
		sta GameState
		sta yScroll
		sta ScrollDelta
;		cli	
		rts		
		
RemoveSpritesAndEnts
		lda entitySpriteBufferHead
		sta entitySpriteBufferTail
		jsr resetESBPointer
		jmp removeMovingPlatAndOtherEnts
				
checkCollision
	lda checkSpriteToCharData.yDeltaCheck
	bmi CCexit	
	bpl CCcheckChars	
	;;lda playerY
	;;cmp #222
	;;bcc CCcheckChars
	;;pha
	;;pha ; to fix up for the dropthrough
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
	lda playerY
	sec
	sbc #34
	sbc yScroll
	sta ZPTemp
	and #7
	sta ZPTemp2
;	cmp #4
;	bcs checkMovingPlatforms
	lda ZPTemp
	lsr 
	lsr 
	lsr 
	sta checkSpriteToCharData.yCharCollide
	tax
	lda playerX
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
	lda playerY
	clc
	adc #11
	sta ZPTemp
	ldx # kNumMovingPlatforms-1
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
	lda entitySpriteSet.y,y
	sec
	sbc ZPTemp 
	bmi _next ; they are above me
	sbc #4
	bpl _next ; they are below me
	; now we check the X
	lda MovingPlatform.width,x
	sta NormalTemp1
	lda playerX
	sec
	sbc entitySpriteSet.x1,y
	adc #10 ; player left X collide
	bmi _next
	ldy NormalTemp1
	sbc platformWidthTable,y
	bpl _next
	stx PlayerData.onMovingPlatform
	jsr setPlayerOnGroundStatus
	jmp checkOtherEnts ; could be fall thorugh eventually
	
checkOtherEnts
	ldx # kNumOtherEnts-1
_l	lda OtherEnts.mapTableY,x
	bne _foundOne
_next	
	dex
	bpl _l
	rts
_foundOne
	tay
	lda MapData.xEnd,y
	bmi _next ; is map end 80+ then skip
	lda OtherEnts.sprIndex,x
	tay
	lda entitySpriteSet.x1,y
	clc
	adc #8
	sta NormalTemp1
	lda entitySpriteSet.y,y
	clc
	adc #8
	sta NormalTemp2
	
	lda playerX
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
	lda playerY
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
	sta MapData.xEnd,y	
	lda OtherEnts.sprIndex,x
	sta entitySpriteBufferPointer
	tay
	lda # kEmptySprite
	sta entitySpriteSet.ptr1,y
	sta entitySpriteSet.ptr2,y
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
EntFuncTable .rta CollFuncBouce,CollFuncDoubleJump,AdvanceLevel 
 	
CollFuncBouce	
	lda #0
	sta PlayerData.yDeltaAccum
	sta PlayerData.onGround
	sta PlayerData.isFalling
	lda # kPlayerParams.maxRiseSpeed
	sta PlayerDAta.yDeltaAccum+1
	lda #1
	sta PlayerData.hasJumped
	rts
	
CollFuncDoubleJump
	lda #1
	sta PlayerData.deltaToAddToJumped
	rts
AdvanceLevel
	lda # kGameState.EOL 
	sta GameState
	inc Level
	lda #0
	sta playerSpriteEnable
	sta MinorState
	jmp setNMIEndOfLevel
		
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
		lda PlayerData.XDelta
		sec
		sbc # kPlayerParams.Xdelta
		sta PlayerData.XDelta
		lda PlayerData.XDelta+1
		sbc #0
		sta PlayerData.XDelta+1
		bpl _updateX
		cmp # kPlayerParams.maxXDeltaNegative
		bcs _updateX
		lda # kPlayerParams.maxXDeltaNegative				
		sta PlayerData.XDelta+1				
		lda #0				
		sta PlayerData.XDelta	
		jmp _updateX
_cr		lda joyRight
		beq _noChangeLR
		lda PlayerData.XDelta
		clc
		adc # kPlayerParams.Xdelta
		sta PlayerData.XDelta
		lda PlayerData.XDelta+1
		adc #0
		sta PlayerData.XDelta+1
		bmi _updateX
		cmp # kPlayerParams.maxXDelta
		bcc _updateX
		lda # kPlayerParams.maxXDelta				
		sta PlayerData.XDelta+1				
		lda #0				
		sta PlayerData.XDelta				
		jmp _updateX
_noChangeLR		
		lda PlayerData.XDelta+1	
		bne _dampen1 
		lda PlayerData.XDelta
		bne _dampen2
		jmp _updateX
_dampen2				
		lda PlayerData.XDelta+1
_dampen1		
		bpl _subRestore
		lda PlayerData.XDelta
		clc
		adc # kPlayerParams.XRestoreDelta
		sta PlayerData.XDelta
		lda PlayerData.XDelta+1
		adc #0
		sta PlayerData.XDelta+1
		jmp _updateX
_subRestore
		lda PlayerData.XDelta
		sec
		sbc # kPlayerParams.XRestoreDelta
		sta PlayerData.XDelta
		lda PlayerData.XDelta+1
		sbc #0
		sta PlayerData.XDelta+1		
_updateX
		lda PlayerData.XDelta
		clc
		adc playerXLo
		sta playerXLo
		lda PlayerData.XDelta+1		
		sta checkSpriteToCharData.xDeltaCheck
		adc playerX
		sta playerX

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
		sbc # kPlayerParams.chargeRate
		sta PlayerData.jumpChargePump
		lda PlayerData.jumpChargePump+1
		sbc #0
		sta PlayerData.jumpChargePump+1
		cmp # kPlayerParams.maxRiseSpeed
		beq _checkLo
		bcs +
		lda # kPlayerParams.maxRiseSpeed
		sta PlayerData.jumpChargePump+1
		lda # kPlayerParams.maxRiseSpeedLo
		sta PlayerData.jumpChargePump
		jmp +
_checkLo		
		lda PlayerData.jumpChargePump
		cmp # kPlayerParams.maxRiseSpeedLo
		bcs +
		lda # kPlayerParams.maxRiseSpeedLo
		sta PlayerData.jumpChargePump
+		lda PlayerData.fireWasDown
		bne _justUpdate
		lda # kPlayerParams.startFlashTimer
		sta PlayerData.spriteFlashChargePump
_justUpdate
		jsr updatePlayerFlash
		jmp endChargePump		
noChargePump
		lda # kPlayerParams.normalColour
		sta playerColourMulti
		lda #1+kSprBase
		sta playerPtrMono
		lda #0+kSprBase
		sta playerPtrMulti
		lda #0
		sta PlayerData.jumpChargePump
		sta PlayerData.jumpChargePump + 1
		lda # kPlayerParams.startFlashTimer
		sta PlayerData.spriteFlashChargePump
		
endChargePump
		
_updateJump		
		lda PlayerData.onGround		; are we on the ground	
		bne downOne
		ldx PlayerData.isFalling
_incD	jsr incPlayerYDeltaAndReturn	; no, we are in gravity so fall
		sta checkSpriteToCharData.yDeltaCheck
deltaExit	
		lda joyFire
		sta PlayerData.fireWasDown
		rts
		
downOne
		lda #1
		sta checkSpriteToCharData.yDeltaCheck
		bne deltaExit		
		
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
		sta PlayerData.XDelta
		sta PlayerData.XDelta + 1
		rts

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
		
removeMovingPlatAndOtherEnts
	ldx # kNumMovingPlatforms-1
	lda #0
-	sta MovingPlatform.numSprites,x
	dex
	bpl -
	ldx # kNumOtherEnts-1
-	sta OtherEnts.mapTableY,x
	dex
	bpl -
	rts

resetPlayerData
	ldx # size(sPlayerData) -1
	lda #0
-	sta PlayerData,x
	dex
	bpl -
	jsr setPlayerToDefaultColours
	lda #$FF
	sta PlayerData.onMovingPlatform
	lda #2
	sta PlayerData.deltaToAddToJumped
	lda #kPlayerParams.spriteEnableValue
	sta playerSpriteEnable
	jsr restoreZeroTimerSprite
	lda # <kVectors.timerPreShiftsBase
	sta TimerSprite2SrcPtr
	lda # >kVectors.timerPreShiftsBase
	sta TimerSprite2SrcPtr+1
	lda # <kVectors.mapBottom
	sta currentWaterLevelPtr
	lda # >kVectors.mapBottom
	sta currentWaterLevelPtr+1
	lda #5
	sta WaterStartSeconds
	lda #2
	sta WaterStartTenSeconds
	lda #1
	sta WaterStartMinutes
	lda #0
	sta TimerFramesCount	
	sta TimerSecondsCount
	sta TimerTenSecondsCount
	sta TimerMinuetsCount
	sta yScroll
	sta ScrollDelta
	sta TickDowns.water
	sta WaterDoWater
	rts
		
setPlayerToSpawnPoint
	lda #128+24
	sta playerX
	lda #222
	sta playerY
	rts
	
setPlayerToDefaultColours
	lda #15
	sta playerColourMulti
	lda #0
	sta playerColourMono
	rts			
					
emptyCRAM
		ldx #00
		lda #$06
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
		lda #$06		
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
		ldx MapTrackingData.topCharsToNext
		stx ZPTemp
		dey ; as the top index is the one above
_loop
		sty ZPTemp2
		lda CRAMBorderPlotColour
		bne _clear2	
		dex
		lda screenROWLUTLo,x
		sta screenPointer
		lda screenROWLUTHi,x
		sta screenPointer+1
		ldy pointer3  
		lda (screenPointer),y
		cmp # kCharDefines.waterChar1  ; have we reached a water row
		beq _done					 ; stop plotting CRAM then
		cmp # kCharDefines.waterChar2
		beq _done 
		inx
_clear2
		lda screenROWLUTLo,x
		sta screenPointer
		lda screenROWLUTHi,x
		sta screenPointer+1		
		lda screenPointer+1
		eor # (>kVectors.charBase) ^ $D8
		sta screenPointer+1
		ldy ZPTemp2
		lda MapData.type,y
		cmp # kSpriteLevelDataTypesStart
		bcs _skip
		tax
		lda MapData.xEnd,y
		sta ZPTemp2
		lda MapData.xStart,y
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
		lda MapData.yDelta,y
		clc
		adc ZPTemp
		adc #1 
		sta ZPTemp
		tax
		cpx #25
		bcs _done
		dey
		bmi _done ; catch the case if we try to go below 0		
		jmp _loop ; probably can be bne
_done	rts

resetCurrMapPtrToFirstScreen
		lda # <(kVectors.mapBottom-(40*24))
		sta currMapPtr
		lda # >(kVectors.mapBottom-(40*24))
		sta currMapPtr+1
		lda # <kVectors.mapBottom
		sta bottomMapPtr
		lda # >kVectors.mapBottom
		sta bottomMapPtr+1
		rts
		
initMapTracker
		ldy #0
		sty MapTrackingData.bottomCharsToNext
		sty MapTrackingData.bottomIndex
		sty ZPTemp
_findTop
		lda MapData.yDelta,y
		clc
		adc ZPTemp
		adc #1 ; for the fact the each number is 0 based
		sta ZPTemp
		cmp #24
		beq _deadOn
		bcs _goneOver
		iny
		bne _findTop

_goneOver		
		sec		
		sbc MapData.yDelta,y	
		sta ZPTemp	
		lda #26	
		sec	
		sbc ZPTemp	

_exit
		sta MapTrackingData.topCharsToNext	
		sty MapTrackingData.topIndex	
		rts	
_deadOn							
		iny				
		lda #1										
		bne _exit						
								
moveCurrMapPtrUpOneRow
		sec
		lda currMapPtr
		sbc #40
		sta currMapPtr
		lda currMapPtr+1
		sbc #0
		sta currMapPtr+1
		sec
		lda bottomMapPtr
		sbc #40
		sta bottomMapPtr
		lda bottomMapPtr+1
		sbc #0
		sta bottomMapPtr+1
		rts
		
moveCurrMapPtrDownOneRow
		clc
		lda currMapPtr
		adc #40
		sta currMapPtr
		lda currMapPtr+1
		adc #0
		sta currMapPtr+1
		clc
		lda bottomMapPtr
		adc #40
		sta bottomMapPtr
		lda bottomMapPtr+1
		adc #0
		sta bottomMapPtr+1
		rts
		
moveMapTrackeUpOneRow
		ldy MapTrackingData.topIndex
		lda MapTrackingData.topCharsToNext
		clc
		adc #1
		sta MapTrackingData.topCharsToNext
		cmp MapData.yDelta,y
		beq _notExpired
		bcc _notExpired ; <= 
		lda MapData.type,y
		cmp # kSpriteLevelDataTypesStart
		bcc _notSpecial
		;cary must be set so ghost sec here
		jsr addMovingPlatformTop ; preserves Y	
_notSpecial
		iny
		sty MapTrackingData.topIndex 
		lda #0
		sta MapTrackingData.topCharsToNext
_notExpired		
		; bottom index		
		ldy MapTrackingData.bottomIndex
		lda MapTrackingData.bottomCharsToNext
		clc
		adc #1
		sta MapTrackingData.bottomCharsToNext
		cmp MapData.yDelta,y
		beq _notExpired2
		bcc _notExpired2 ; <= 
		lda MapData.type,y
		cmp # kSpriteLevelDataTypesStart
		bcc _notSpecial2
		;cary must be set so ghost sec here
		jsr removeMovingPatform	
_notSpecial2
		lda #0
		sta MapTrackingData.bottomCharsToNext		
		inc MapTrackingData.bottomIndex	
_notExpired2
		rts	
				
moveMapTrackeDownOneRow
		ldy MapTrackingData.topIndex
		lda MapTrackingData.topCharsToNext
		sec
		sbc #1
		sta MapTrackingData.topCharsToNext
		bpl _notExpired
		
		lda MapTrackingData.topIndex
		sec
		sbc #1
		sta MapTrackingData.topIndex
		tay
		lda MapData.yDelta,y
		sta MapTrackingData.topCharsToNext
		lda MapData.type,y
		cmp # kSpriteLevelDataTypesStart
		bcc _notSpecial
		clc ; remove from top ( also as per above it has to be set already)
		jsr removeMovingPatform
_notSpecial
_notExpired
		; check the bottom one
		ldy MapTrackingData.bottomIndex
		lda MapTrackingData.bottomCharsToNext
		sec
		sbc #1
		sta MapTrackingData.bottomCharsToNext
		bpl _notExpired2
		dey
		sty MapTrackingData.bottomIndex
		lda MapData.type,y
		cmp # kSpriteLevelDataTypesStart
		bcc _notSpecial2
		jsr addMovingPlatformBottom ; preserves Y
_notSpecial2
		lda MapData.yDelta,y
		sta MapTrackingData.bottomCharsToNext
_notExpired2
		rts

ABI_foundOne
		sty YSave
		lda ZPTemp2
		sta OtherEnts.type,x
		lda MapData.xStart,y
		sta NormalTemp3	; X start pos of Sprite	
		tya
		sta OtherEnts.mapTableY,x
		lda MapData.xEnd,y
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
		sta entitySpriteSet.y,x
		lda NormalTemp3
		sta entitySpriteSet.x1,x
		sta entitySpriteSet.x2,x
		lda MapTypeToSpriteColours - kSpriteLevelDataTypesStart - 2,y
		sta entitySpriteSet.col2,x
		lda #0
		sta entitySpriteSet.col1,x
		lda #%00001100
		sta entitySpriteSet.enable,x
		lda #%00001000
		sta entitySpriteSet.multi,x
		lda NormalTemp1
		bmi _blankIt
		lda MapTypeToSprite - kSpriteLevelDataTypesStart - 2,y
		bne _storeIT
_blankIt
		lda # kEmptySprite
_storeIT
		sta entitySpriteSet.ptr2,x
		clc
		adc #1
		sta entitySpriteSet.ptr1,x
		ldy YSave
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
.byte kEntSprits.bounceBase,kEntSprits.doubleJumpBase,kEntSprits.endOfLevel
MapTypeToSpriteColours
.byte 2,3,7

;calcSpriteXFromMapStart
;		lda MapData.xStart,y
calcSpriteXFromA
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
		lda MapData.xEnd,y				; get direction
		and #$80
		sta MovingPlatform.direction,x
		lda MapData.xEnd,y				; get the X end
		and #$0F
	;	sec
	;	sbc MapData.xStart,y
		sta MovingPlatform.width,x	; X width of platform
		sec
		sbc #1 						;make 0 based
		sta NormalTemp5
		lda MapData.xStart,y
		sta NormalTemp3	; X start pos of Sprite	
		stx NormalTemp2				; moving plaform number
		ldx NormalTemp5				; X width of platform 
		lda PlatformSizeToSpritesCountLUT,x	
		ldx NormalTemp2				; moving plaform number
		sta MovingPlatform.numSprites,x
		sta NormalTemp1				; number of sprites
		plp
		bcs _tail
		lda entitySpriteBufferHead
		sta entitySpriteBufferPointer ; cache the current head
		sta MovingPlatform.sprIndex,x ; write the sprite index to use
		jsr allocAInESBHead				; make room for it
		jmp _allocd
_tail	jsr allocAInESBTail
		lda entitySpriteBufferTail
		sta entitySpriteBufferPointer ; cache the current tail		
		sta MovingPlatform.sprIndex,x ; write the sprite index to use	
_allocd		
		tya							; save the end number
		sta MovingPlatform.mapTableY,x				
		lda NormalTemp5				; width
		asl a
		asl a ; times by 4
		sta NormalTemp4		

		lda ZPTemp3					; y to appear at
		ldx entitySpriteBufferPointer	; sprite number
		sta entitySpriteSet.y,x			; store Y
		lda NormalTemp3					; get start X
		sta entitySpriteSet.x1,x	; store X
		ldy NormalTemp4
		clc
		adc MovingPlatformXDeltaTable-3,y
		sta entitySpriteSet.x2,x
		iny
		clc
		adc MovingPlatformXDeltaTable-3,y
		sta entitySpriteSet.x3,x
		lda #0
		sta entitySpriteSet.multi,x ; All Mono Sprites
		ldy NormalTemp2				; moving plaform number
		lda MovingPlatform.direction,y
		bne _left
		lda # kEntSprits.movingPlatformRight
		bne +
_left
		lda # kEntSprits.movingPlatformLeft
+		sta entitySpriteSet.ptr1,x
		sta entitySpriteSet.ptr2,x
		sta entitySpriteSet.ptr3,x
		lda #1
		sta entitySpriteSet.col1,x; white
		sta entitySpriteSet.col2,x; white
		sta entitySpriteSet.col3,x; white
		ldy NormalTemp1 ; number of sprites
		lda EntEnableMaskLUT,y
		sta entitySpriteSet.enable,x	
		ldy ZPTemp2
		rts

removeBounce
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
		plp
		dec $d020
		rts
_foundOne
		stx ZPTemp
		; save the direction in the high bit of xEnd
		lda MapData.xEnd,y
		and #$7F
		ora MovingPlatform.direction,x
		sta MapData.xEnd,y
		; save the sprite position back as well
		lda MovingPlatform.sprIndex,x
		tax
		lda entitySpriteSet.x1,x
		sta MapData.xStart,y
		plp
		bcs _bottom
		jsr removeAInESBHead
		jmp _done
_bottom	jsr removeAInESBTail
_done
		ldx ZPTemp
		lda #0
		sta MovingPlatform.numSprites,x
		sta MovingPlatform.mapTableY,x
		sta MovingPlatform.sprIndex,x
RemoveSpritesRestoreYExit		
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
		tay
		lda TickDowns.movingPlatformAnim,x
		bne _noFrameChange
		lda entitySpriteSet.ptr1,y
		eor #1
		sta entitySpriteSet.ptr1,y
		sta entitySpriteSet.ptr2,y
		sta entitySpriteSet.ptr3,y
		lda #16
		sta TickDowns.movingPlatformAnim,x
_noFrameChange
		lda MovingPlatform.direction,x
		bne _back		
		lda PlayerData.onMovingPlatform
		cmp ZPTemp
		bne _notOnThisOne
		inc playerX
_notOnThisOne		
		ldy entitySpriteBufferPointer	
		lda entitySpriteSet.x1,y 
		clc
		adc #1
		sta entitySpriteSet.x1,y
		lda entitySpriteSet.x2,y 
		clc
		adc #1
		sta entitySpriteSet.x2,y
		lda entitySpriteSet.x3,y 
		clc
		adc #1
		sta entitySpriteSet.x3,y
		lda entitySpriteSet.x3,y 
		ldx #0 ; fix me
		cmp MovingPlatformMaxXForLenght,x
		bcs _flip
		jmp _next
_flip	ldx ZPTemp
		lda MovingPlatform.direction,x
		eor #128
		sta MovingPlatform.direction,x
		bne _left
		lda #kEntSprits.movingPlatformRight
		bne _setSprites
_left
		lda #kEntSprits.movingPlatformLeft
_setSprites
		sta entitySpriteSet.ptr1,y
		sta entitySpriteSet.ptr2,y
		sta entitySpriteSet.ptr3,y
		jmp _next
		
_back	lda PlayerData.onMovingPlatform
		cmp ZPTemp
		bne _notOnThisOneEither
		dec playerX
_notOnThisOneEither
		ldy entitySpriteBufferPointer	
		lda entitySpriteSet.x1,y 
		sec
		sbc #1
		sta entitySpriteSet.x1,y
		lda entitySpriteSet.x2,y 
		sec
		sbc #1
		sta entitySpriteSet.x2,y
		lda entitySpriteSet.x3,y 
		sec
		sbc #1
		sta entitySpriteSet.x3,y
		lda entitySpriteSet.x1,y 
		cmp #25
		bcc _flip
		jmp _next

updateNearMovingPlatforms
		lda #0
		sta ZPTemp
		jsr uNMPUI
		jsr uNMPDI
		lda #1
		jsr uNMPUI
		jsr uNMPDI
uNMPExit
		rts
		
uNMPUI
		lda MapTrackingData.topIndex
		clc
		adc ZPtemp
		cmp MapData.length
		bcs uNMPExit
		tax
		lda MapData.type,x
		cmp #kSpriteLevelDataTypesStart+1
		bne uNMPExit
		jmp uMPOS
		
uNMPDI
		lda MapTrackingData.bottomIndex
		sec
		sbc ZPtemp
		bmi uNMPExit
		tax
		lda MapData.type,x
		cmp #kSpriteLevelDataTypesStart+1
		bne uNMPExit
		jmp uMPOS
		
uMPOS ; update Moving Platfom Off Screen
		lda MapData.xEnd,x
		and #$80
		cmp #$80
		beq _back		
		lda MapData.xStart,x		
		clc		
		adc #1		
		sta MapData.xStart,x		
		lda MapData.xEnd,x	
		and #$0F	
		tay	
		lda MapData.xStart,x
		cmp MovingPlatformMaxXForLenght,y
		bcs _flip
		rts
		
_flip	lda MapData.xEnd,x
		eor #128
		sta MapData.xEnd,x
		rts
		
_back	lda MapData.xStart,x
		sec
		sbc #1
		sta MapData.xStart,x
		cmp #25
		bcc _flip
		rts
		
updateEntsY		
		jsr resetESBPointer	
_loop	lda entitySpriteBufferPointer
		cmp entitySpriteBufferHead 
		beq _exit
		tax
		lda entitySpriteSet.y,x
		clc
		adc ScrollDelta
		sta entitySpriteSet.y,x
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
.byte 255-8,255-8,255-8 ; first 3
.for count = 1, count < 7, count = count + 1 
.byte 255-((count)*8)-8	
.next	

initEntitySpriteBuffer
	lda # kEntSpriteBuffer.startOffset
	sta entitySpriteBufferHead
	sta entitySpriteBufferTail
	sta entitySpriteBufferPointer
	rts
	
allocAInESBHead
	lda #1
	clc
	adc entitySpriteBufferHead
	cmp # kEntSpriteBuffer.endOffset-kEntSpriteBuffer.startOffset
	bcc _noOver
	lda # kEntSpriteBuffer.startOffset
_noOver
	sta entitySpriteBufferHead
	rts

allocAInESBTail
	lda entitySpriteBufferTail
	sec
	sbc #1
	bpl _noOver
	lda # kEntSpriteBuffer.endOffset - 1
_noOver
	sta entitySpriteBufferTail
	rts	
		
removeAInESBTail
	lda #1
	clc
	adc entitySpriteBufferTail
	cmp # kEntSpriteBuffer.endOffset-kEntSpriteBuffer.startOffset
	bcc _noOver
	lda # kEntSpriteBuffer.startOffset
_noOver
	sta entitySpriteBufferTail
	rts
	
removeAInESBHead
	lda entitySpriteBufferHead
	sec
	sbc #1
	cmp # kEntSpriteBuffer.startOffset
	bpl _noUnder
	lda # kEntSpriteBuffer.endOffset-1
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
	lda # kEntSpriteBuffer.startOffset
_noOver
	sta entitySpriteBufferPointer
	rts
			
copyBigMapToLittleMap
		lda currMapPtr
		sta MapCopyTemp
		lda currMapPtr+1
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
		lda pointer3
		clc
		adc #40
		sta ZPTemp
		lda pointer3+1
		adc #00
		sta ZPTemp3	
		ldy #30
_loop	lda (MapCopyTemp),y
		sta kVectors.charBase+00,y
		lda (pointer1),y
		sta kVectors.charBase+40,y
		lda (pointer2),y
		sta kVectors.charBase+80,y
		lda (pointer3),y
		sta kVectors.charBase+120,y
		lda (ZPtemp),y
		sta kVectors.charBase+160,y
		dey
		bpl _loop
		
		lda MapCopyTemp
		adc #200
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
		lda pointer3
		clc
		adc #40
		sta ZPTemp
		lda pointer3+1
		adc #00
		sta ZPTemp3	
		ldy #30
_loop2	lda (MapCopyTemp),y
		sta kVectors.charBase+200,y
		lda (pointer1),y
		sta kVectors.charBase+240,y
		lda (pointer2),y
		sta kVectors.charBase+280,y
		lda (pointer3),y
		sta kVectors.charBase+320,y
		lda (ZPTemp),y
		sta kVectors.charBase+360,y
		dey
		bpl _loop2		
		lda MapCopyTemp
		adc #200
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
		lda pointer3
		clc
		adc #40
		sta ZPTemp
		lda pointer3+1
		adc #00
		sta ZPTemp3	
		ldy #30
_loop3	lda (MapCopyTemp),y
		sta kVectors.charBase+400,y
		lda (pointer1),y
		sta kVectors.charBase+440,y
		lda (pointer2),y
		sta kVectors.charBase+480,y
		lda (pointer3),y
		sta kVectors.charBase+520,y
		lda (ZPTemp),y
		sta kVectors.charBase+560,y
		dey
		bpl _loop3
		
		lda MapCopyTemp
		adc #200
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
		lda pointer3
		clc
		adc #40
		sta ZPTemp
		lda pointer3+1
		adc #00
		sta ZPTemp3	
		ldy #30
_loop4	lda (MapCopyTemp),y
		sta kVectors.charBase+600,y
		lda (pointer1),y
		sta kVectors.charBase+640,y
		lda (pointer2),y
		sta kVectors.charBase+680,y
		lda (pointer3),y
		sta kVectors.charBase+720,y
		lda (ZPTemp),y
		sta kVectors.charBase+760,y
		dey
		bpl _loop4
		
		lda MapCopyTemp
		adc #200
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
		lda pointer3
		clc
		adc #40
		sta ZPTemp
		lda pointer3+1
		adc #00
		sta ZPTemp3	
		ldy #30
_loop5	lda (MapCopyTemp),y
		sta kVectors.charBase+800,y
		lda (pointer1),y
		sta kVectors.charBase+840,y
		lda (pointer2),y
		sta kVectors.charBase+880,y
		lda (pointer3),y
		sta kVectors.charBase+920,y
		lda (ZPTemp),y
		sta kVectors.charBase+960,y
		dey
		bpl _loop5	
_exit	rts

loadMapVectors
		; read vectors
		lda ldfLUTLO,x
		sta mapYDelta
		lda ldfLUTHI,x
		sta mapYDelta+1
		lda sxLUTLO,x
		sta mapXStart
		lda sxLUTHI,x
		sta mapXStart+1
		lda exLUTLO,x
		sta mapXEnd
		lda exLUTHI,x
		sta mapXEnd+1
		lda typeLUTLO,x
		sta mapType
		lda typeLUTHI,x
		sta mapType+1
		; copy data to local cache
		lda rowCounts,x
		sta MapData.length
		tay
-		lda (mapYDelta),y
		sta MapData.yDelta,y
		lda (mapXEnd),y
		sta MapData.xEnd,y
		lda (mapType),y
		sta MapData.type,y
		cmp # kSpriteLevelDataTypesStart
		bcc _normal
		jsr calcSpriteXFromA
		jmp _store
_normal
		lda (mapXStart),y
_store
		sta MapData.xStart,y
		dey
		bpl -		
		; reset map pointer	
		lda # <kVectors.mapBottom
		sta currMapPtr
		lda # >kVectors.mapBottom
		sta currMapPtr+1
		rts
		

startLargeMapArea
		lda #$60
		sta pointer1+1
		lda #$00
		sta pointer1
		rts
		
clearLargeMapArea
		ldy #0
		lda RANDSeed
		sta ZPTemp
		inc RANDSeed
_loop	
		lda ZPTemp
        beq _doEor
        asl
        beq _noEor ;if the input was $80, skip the EOR
        bcc _noEor
_doEor   eor #$1d
_noEor   sta ZPTemp
		and #15
		cmp #4
		bcs _zero
		clc
		adc #$42
		bne +
_zero	lda #$41
+
;.for ptr = $6000, ptr < $c000, ptr = ptr + $100
		sta (pointer1),y
;.next
		iny
		beq _exit
		jmp _loop
_exit
		lda pointer1+1		
		clc		
		adc #1		
		sta pointer1+1	
		cmp #$C0		
		bne +		
		sec				
+		rts
		
		
convertMapDataToLargeMap
		lda rowCounts,x
		sta numItemsMap
		ldy #0
		sty mapItemIndex
		sty FirstLineToPlot
_nextItem
		ldy mapItemIndex
		lda MapData.yDelta,y
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
		lda MapData.xEnd,y
		sta ZPTemp
		inc ZPTemp
		lda MapData.xStart,y
		sta ZPTemp2
		lda MapData.type,y
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
				
.comment
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
.endc	
plotHiScore	
	lda #6	; start digit
	sta ZPtemp3
	lda #3
	ldx #7	; always draw digit
	ldy #6	; max digit
	bne plotScoreInternal	
	
plotScore
	lda #0 ; start digit start at 6th digit in stuct
	sta ZPtemp3
	ldx #5 ; always draw
	ldy #3 ; max digit
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
	ldx ZPTemp				; start Mem digit
	lda Score,x				; get upper digit
	and #$F0				; mask off upper digit
	lsr a					; convert from x16 to x8
	jsr _needToDrawPre8		; no draw it
	ldx ZPTemp				; get digit index
	lda Score,x				; re get the score value
	and #$0F				; lower digit this time
	jsr _needToDraw			; draw it
	ldx ZPTemp				; get the digit count
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
_needToDrawPre8			
	tay			
	clc
	adc #7
	sta ZPTemp2
	ldx ZPTemp3
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
	inc ZPTemp3				
	ldx ZPTemp			
	rts			
				
spriteOffsetIndex				
	.byte kScoreSprites.scoreDigit6,kScoreSprites.scoreDigit5,kScoreSprites.scoreDigit4,kScoreSprites.scoreDigit3,kScoreSprites.scoreDigit2,kScoreSprites.scoreDigit1				
	.byte kScoreSprites.hiScoreDigit6,kScoreSprites.hiScoreDigit5,kScoreSprites.hiScoreDigit4,kScoreSprites.hiScoreDigit3,kScoreSprites.hiScoreDigit2,kScoreSprites.hiScoreDigit1				
PlayerChargeColourTable				
	.byte 9,2,8,10,7,10,8,2
PlayerChargeColourCount = * -playerChargeColourTable 

PlayerDoubleJumpColourTable
	.byte 11,5,13,7,1,7,13,5,11
PlayerDoubleJumpFlashCount = * -PlayerDoubleJumpColourTable

 				
updatePlayerFlash
	lda TickDowns.playerFlash
	bne _spriteUpdate
	lda PlayerData.spriteFlashChargePump
	bpl +
	lda # kPlayerParams.minFlashTimer
	sta PlayerData.spriteFlashChargePump
+	sta TickDowns.playerFlash
	inc PlayerData.spriteFlashIndex 
	lda PlayerData.spriteFlashIndex 
	cmp # playerChargeColourCount
	bne _noColourChange
	lda PlayerData.spriteFlashChargePump
	cmp # kPlayerParams.minFlashTimer
	bcc _flashTimerMin
	sec
	sbc #1
	sta PlayerData.spriteFlashChargePump
_flashTimerMin	
	lda #0
	sta PlayerData.spriteFlashIndex
_noColourChange	
	ldx PlayerData.spriteFlashIndex
	lda PlayerChargeColourTable,x
	sta playerColourMulti
; do the sprite updates		
_spriteUpdate	
	ldx #3		
_loop
	lda PlayerData.jumpChargePump+1		
	cmp playerAnimLevelsHi,x
	bcc _done
	bne _next
	lda PlayerData.jumpChargePump
	cmp playerAnimLevelsLo,x
	bcc _done
_next
	dex
	bne _loop
_done
	txa
	asl a
	ora PlayerData.animeFrameBit
	tax
	lda PlayerAnimFrameOutlines,x
	sta playerPtrMono
	lda PlayerAnimFrameBackground,x
	sta playerPtrMulti
	lda TickDowns.playerAnim
	bne _exit
	lda # kPlayerParams.chargeAnimFrameTimer
	sta TickDowns.playerAnim
	lda PlayerData.animeFrameBit
	eor #1
	sta PlayerData.animeFrameBit
_exit	
	rts

playerAnimLevelsLo .byte $00,$80,$00,$40	
playerAnimLevelsHi .byte $FF,$FA,$F8,$F6	
	
PlayerAnimFrameOutlines
	.byte kSprBase+03,kSprBase+03,kSprBase+05,kSprBase+05,kSprBase+09,kSprBase+09,kSprBase+13,kSprBase+13
PlayerAnimFrameBackground
	.byte kSprBase+02,kSprBase+02,kSprBase+04,kSprBase+06,kSprBase+08,kSprBase+10,kSprBase+12,kSprBase+12
	
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
	sta playerColourMono
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

clearByteBuffer
	ldx # size(ByteBuffer)-1
	lda #0
-	sta ByteBuffer,x
	dex
	bpl -
	rts

cleartimerPreShiftsBase
	lda #0
	tax
-	sta kVectors.timerPreShiftsBase,x
	sta kVectors.timerPreShiftsBase+$100,x
	sta kVectors.timerPreShiftsBase+$200,x
	inx
	bne -
	rts
	
;x should be char num * 8
copyCharTOBuffer
	ldy #0
_copyLoop
	lda kFont.digitsPointer,x	
	sta ByteBuffer,y
	inx
	iny
	iny
	cpy #10
	bcc _copyLoop
	rts

shiftBuffer5RowsRight16Bit
	ldx #0
_rowLoop
	lsr ByteBuffer,x
	ror ByteBuffer+1,x
	inx 
	inx
	cpx #11
	bcc _rowLoop
	dey
	bpl shiftBuffer5RowsRight16Bit	
	rts
	
buildTimerFractions
	jsr cleartimerPreShiftsBase
	lda # <kVectors.timerPreShiftsBase
	sta pointer1
	lda # >kVectors.timerPreShiftsBase
	sta pointer1+1
	lda #0
	sta NormalTemp1
_loopMakeFirstDigit
	jsr clearByteBuffer
_loopShiftDigit	
	ldx NormalTemp1	
	jsr copyCharToBuffer
	ldy #3 ; num times to shift
	jsr shiftBuffer5RowsRight16Bit
	lda ByteBuffer+2	
	ora #%01100000 ; the :
	sta ByteBuffer+2
	lda ByteBuffer+6
	ora #%01100000
	sta ByteBuffer+6
	lda #4
	sta NormalTemp2
_copyMultipleCopies	
	ldx #0
	ldy #0
_repeatLoop
	lda ByteBuffer,x
	sta (pointer1),y
	inx
	iny
	lda ByteBuffer,x
	sta (pointer1),y
	inx
	iny
	iny
	cpx #11
	bcc _repeatLoop
	clc
	lda pointer1
	adc #15
	sta pointer1
	lda pointer1+1
	adc #0
	sta pointer1+1
	dec NormalTemp2
	bpl _copyMultipleCopies
	lda NormalTemp1
	clc
	adc #8
	cmp #10*8
	beq _secondDigit
	sta NormalTemp1
	jmp _loopMakeFirstDigit
_secondDigit
	lda # <kVectors.timerPreShiftsBase + 1
	sta pointer1
	lda # >kVectors.timerPreShiftsBase
	sta pointer1+1
	lda #0
	sta NormalTemp1
_loopMakeSecondChar
	jsr clearByteBuffer
	ldx NormalTemp1	
	jsr copyCharToBuffer
	ldy #3 ; num times to shift
	jsr shiftBuffer5RowsRight16Bit
	lda #9
	sta NormalTemp2
_copyMultipleCopies2	
	ldx #0
	ldy #0
_repeatLoop2
	lda (pointer1),y
	ora ByteBuffer,x
	sta (pointer1),y
	inx
	iny
	lda (pointer1),y
	ora ByteBuffer,x
	sta (pointer1),y
	inx
	iny
	iny
	cpx #11
	bcc _repeatLoop2
	clc
	lda pointer1
	adc #15*5
	sta pointer1
	lda pointer1+1
	adc #0
	sta pointer1+1
	dec NormalTemp2
	bpl _copyMultipleCopies2
	lda NormalTemp1
	clc
	adc #16
	cmp #20*4 ;8
	beq _exit
	sta NormalTemp1
	sec
	lda pointer1
	sbc # < (15*(10*5))-15
	sta pointer1
	lda pointer1+1
	sbc # > (15*(10*5))-15
	sta pointer1+1
	jmp _loopMakeSecondChar
_exit
	rts


copyAndAdvanceSecondTimerSprite
	lda TimerSprite2SrcPtr
	sta pointer1
	clc
	adc #15
	sta TimerSprite2SrcPtr
	lda TimerSprite2SrcPtr+1
	sta pointer1+1 
	adc #0
	sta TimerSprite2SrcPtr+1
	lda TimerFramesCount
	clc 
	adc #1
	cmp #50
	bcc _skip
	lda # <kVectors.timerPreShiftsBase
	sta TimerSprite2SrcPtr
	lda # >kVectors.timerPreShiftsBase
	sta TimerSprite2SrcPtr+1
	jsr incAndUpdateSecs
	lda #0
_skip
	sta TimerFramesCount
	ldy #14
-	lda (pointer1),y	
	sta kTimerSprites.secondSpriteBase,y
	dey
	bpl -
	rts
	
cacheZeroTimerSprite
	ldx #14
-	lda kTimerSprites.firstSpriteBase,x
	sta kVectors.timerZeroCache,x
	lda kTimerSprites.secondSpriteBase,x
	sta kVectors.timerZeroCache+15,x
	dex
	bpl -
	rts
	
restoreZeroTimerSprite
	ldx #14
-	lda kVectors.timerZeroCache,x
	sta kTimerSprites.firstSpriteBase,x
	lda kVectors.timerZeroCache+15,x
	sta kTimerSprites.secondSpriteBase,x
	dex
	bpl -
	rts
	
kTimerMasks .block
   Min    = %10000001
   TensHi = %11100000
   TensLo = %01111111
   Secs   = %11000000
.bend
incAndUpdateSecs
.if DISABLE_RASTER_DEBUG = 0
	dec $d020
.endif
	lda TimerSecondsCount
	clc
	adc #1
	sta TimerSecondsCount
	cmp #10
	bcc +
	jsr incAndUpdateTens
	ldx #0
	stx TimerSecondsCount	
	beq ++	
+	asl a
	asl a
	asl a ; x8
	tax 
+	jsr copyCharTOBuffer
	ldy #0 ; shift 1
	jsr shiftBuffer5RowsRight16Bit
	ldx #0
	ldy #0
-	lda kTimerSprites.firstSpriteBase+2,x
	and # kTimerMasks.Secs
	ora ByteBuffer,y
	sta kTimerSprites.firstSpriteBase+2,x
	inx
	inx
	inx
	iny
	iny
	cpx #15
	bcc -
.if DISABLE_RASTER_DEBUG = 0
	inc $d020
.endif
	rts
	
incAndUpdateTens	
	lda TimerTenSecondsCount
	clc
	adc #1
	sta TimerTenSecondsCount
	cmp #6
	bcc +
	jsr incAndUpdateMins
	ldx #0
	stx TimerTenSecondsCount	
	beq ++	
+	asl a
	asl a
	asl a ; x8
	tax 
+	jsr copyCharTOBuffer
	ldy #1 ; shift 2
	jsr shiftBuffer5RowsRight16Bit
	ldx #0
	ldy #0
-	lda kTimerSprites.firstSpriteBase+1,x
	and # kTimerMasks.TensHi
	ora ByteBuffer,y
	sta kTimerSprites.firstSpriteBase+1,x
	inx
	iny
	lda kTimerSprites.firstSpriteBase+1,x
	and # kTimerMasks.TensLo
	ora ByteBuffer,y
	sta kTimerSprites.firstSpriteBase+1,x
	inx
	inx
	iny
	cpx #15
	bcc -
	rts

incAndUpdateMins

	lda TimerMinuetsCount
	clc
	adc #1
	sta TimerMinuetsCount
	cmp #10
	bcc +
	lda #9
	sta TimerMinuetsCount		
+	asl a
	asl a
	asl a ; x8
	tay 
	ldx #0
-	lda kTimerSprites.firstSpriteBase,x
	and # kTimerMasks.Min
	ora kFont.digitsPointer,y
	sta kTimerSprites.firstSpriteBase,x
	inx
	inx
	inx
	iny
	cpx #15
	bcc -
	rts	
		
updateWater
	lda WaterDoWater
	bne _doWater
	ldx #0
-	lda TimerSecondsCount,x
	cmp WaterStartSeconds,x
	bcc uwExit
	inx
	cpx #3
	bne -
	stx WaterDoWater
_doWater
	lda TickDowns.water
	beq +
uwExit	rts
+	
	lda #kTimers.waterUpdate
	sta TickDowns.water 
DEBUG_UPDATEWATER
	; copy next char to the spot
	lda currentWaterCharIndex
	sec
	sbc #1
	sta ZPTemp
	bpl + ; 8 chars
	lda #7
+	sta currentWaterCharIndex
	asl a
	asl a
	asl a ; x8
	tay
	ldx #0
-	lda fileChars+(6*8),y 
	sta fileChars+(24*8),x
	iny
	inx
	cpx #8
	bcc -
	lda ZPTemp
	cmp #8
	bcc uwExit 
	; over flowed so shift the rows up
	; is it above the bottom of the screen
	lda currentWaterLevelPtr+1
	cmp bottomMapPtr+1
	beq +
	bcs _below
	bcc _doScreen
+	lda bottomMapPtr
	cmp currentWaterLevelPtr
	beq _doScreen
	bcc _below
_doScreen
	sec
	lda currentWaterLevelPtr
	sbc currMapPtr
	sta pointer2
	lda currentWaterLevelPtr+1
	sbc currMapPtr+1
	sta pointer2+1
	clc
	lda pointer2+1
	adc # >kVectors.charBase
	sta pointer2+1
	ldy #30
	lda #14+128
-	sta (pointer2),y
	dey
	bpl -
	sec
	lda pointer2
	sbc #40
	sta pointer2
	sta pointer3
	lda pointer2+1
	sbc #00
	sta pointer2+1
	eor # (>kVectors.charBase) ^ $D8
	sta pointer3+1
	ldy #30
-	lda #24+128
	sta (pointer2),y
	lda #6
	sta (pointer3),y
	dey
	bpl -
_below	
	lda currentWaterLevelPtr
	sta pointer1
	lda currentWaterLevelPtr+1
	sta pointer1+1
	lda #14+128
	ldy #30
-	sta (pointer1),y
	dey
	bpl -
	lda pointer1
	sbc #40
	sta pointer1
	lda pointer1+1
	sbc #00
	sta pointer1+1
	lda #24+128
	ldy #30
-	sta (pointer1),y
	dey
	bpl -
	lda currentWaterLevelPtr
	sec
	sbc #40
	sta currentWaterLevelPtr
	lda currentWaterLevelPtr+1
	sbc #00
	sta currentWaterLevelPtr+1
	rts
	
setNMIDeath	
	lda # <ResetLevelNMI
	sta $FFFA
	lda # >ResetLevelNMI
	sta $FFFB
	lda #$0F	
	sta $DD06
	lda #0
	sta MinorState
	jmp setNMI
	
setNMIEndOfLevel
	lda # <ResetLevelNMI
	sta $FFFA
	lda # >ResetLevelNMI
	sta $FFFB
	lda #$1F	
	sta $DD06
	
setNMI
	lda #$FF	
	sta $DD04		
	lda #$FF	
	sta $DD05		
	lda #0
	sta $DD07
	lda #$82	; make it fire an NMI on Timer B underflow
	sta $DD0D	
	lda $DD0D   ; ack any NMI
	lda #$91
	sta $DD0E	
	lda #%01011001
	sta $DD0F	
	rts
	
setirq
	sei			 ;set interrupt disable
	lda #%01010000
	sta $d011		 ;raster irq to 1st half of screen.
	lda # kRaster.bottomRaster
	sta $d012		 ;irq to happen at line #$fb
	lda #< BottomRaster  ; #<irq0
	sta $fffe		 ;hardware irq vector low byte
	lda #> BottomRaster  ; #>irq0
	sta $ffff		 ;hardware irq vector high byte
	lda #$1f
	sta $dc0d		 ;turn off all types of cia irq/nmi.
	sta $dd0d
	lda $dc0e
	and #$fe
	sta $dc0e
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
	
justRTI	rti

updateSpriteRaster
		pha			;save registers
		txa
		pha
		tya
		pha
		cld
		inc $d019		;acknowledge irq
		lda nextRasterType
		and # kNextRasterType.status
		beq +
		jsr DoStatusRaster
+		lda nextRasterType
		and # kNextRasterType.ent
		beq +
		jsr setVICFromCurrMultiSlot
+		jsr DetermineNextRasterLineAndType
eirq2	pla
		tay
		pla
		tax
		pla
		rti
		
DoStatusRaster		
;dec $D020	
		ldy CurrStatusRasterSpriteIndex
		ldx CurrStatusRasterIndex
		cpx #3
		bne +
		lda #1
		sta $D02C
		bne _doneColour
+		cpx #2
		bne _doneColour
		lda #13
		sta $D02C
_doneColour
		lda	statusSpritesDefs.numSprToDo,x
		sta rasterTemp1
		tax
-		lda statusSpritesDefs.def,y
		sta kVectors.spr5ID,x
		; do colour look  up here
		txa
		asl a
		tax
		lda statusSpritesDefs.x,y
		sta $D00A,x
		lda statusSpritesDefs.y,y
		sta $D00B,x
		inc CurrStatusRasterSpriteIndex
		ldy CurrStatusRasterSpriteIndex
		dec rasterTemp1
		ldx rasterTemp1
		bpl -
		inc CurrStatusRasterIndex
;inc $D020
		rts
		
BottomRaster
		pha			;save registers
		txa
		pha
		tya
		pha
		cld
		inc $d019		;acknowledge irq
		lda #3
		sta CurrStatusRasterSpriteIndex
		lda #1
		sta CurrStatusRasterIndex
		lda statusSpritesDefs.x
		sta $D00A
		lda statusSpritesDefs.x+1
		sta $D00C
		lda statusSpritesDefs.x+2
		sta $D00E
		lda statusSpritesDefs.y
		sta $D00B
		lda statusSpritesDefs.y+1
		sta $D00D
		lda statusSpritesDefs.y+2
		sta $D00F
		lda statusSpritesDefs.def
		sta kVectors.spr5ID
		lda statusSpritesDefs.def+1
		sta kVectors.spr6ID
		lda statusSpritesDefs.def+2
		sta kVectors.spr7ID 
		lda #%11100000
		ora playerSpriteEnable
		sta $D015
		lda #%00000010
		sta $D01C
		lda #%11100000
		sta $D010
		lda #1
		sta $D02C
		sta $D02D
		sta $D02E
		lda playerX
		sta $D000
		sta $D002
		lda playerY
		sta $D001
		sta $D003
		lda playerPtrMono
		sta kVectors.spr0ID
		lda playerPtrMulti
		sta kVectors.spr1ID
		lda playerColourMono
		sta $D027
		lda playerColourMulti
		sta $D028
		; check and build Entity multiplexor
BottomRasterBuildEntity
		lda entitySpriteBufferHead
		cmp entitySpriteBufferTail
		bne +
		jmp _noEntities
+		lda entitySpriteBufferPointer
		sta rasterEntityPointerCache
		jsr resetESBPointer
		ldx #0
-		ldy entitySpriteBufferPointer
		lda entitySpriteSet.y,y		
		sta rasterSpriteSet.y,x		
		sec	
		sbc #8	
		sta rasterSplitYPosEnt,x	
		lda entitySpriteSet.x1,y		 		
		sta rasterSpriteSet.x1,x		
		lda entitySpriteSet.x2,y		
		sta rasterSpriteSet.x2,x		
		lda entitySpriteSet.x3,y		
		sta rasterSpriteSet.x3,x		
		lda entitySpriteSet.col1,y	
		sta rasterSpriteSet.col1,x	
		lda entitySpriteSet.col2,y	
		sta rasterSpriteSet.col2,x	
		lda entitySpriteSet.col3,y	
		sta rasterSpriteSet.col3,x	
		lda entitySpriteSet.multi,y	
		sta rasterSpriteSet.multi,x	
		lda entitySpriteSet.ptr1,y	
		sta rasterSpriteSet.ptr1,x	
		lda entitySpriteSet.ptr2,y	
		sta rasterSpriteSet.ptr2,x	
		lda entitySpriteSet.ptr3,y	
		sta rasterSpriteSet.ptr3,x	
		lda entitySpriteSet.enable,y
		sta rasterSpriteSet.enable,x
		inx
		jsr advanceESBPointer
		lda entitySpriteBufferPointer
		cmp entitySpriteBufferHead
		bne -
		stx numRasters
		dex
		stx currEntRasterIndex
		lda rasterEntityPointerCache
		sta entitySpriteBufferPointer
		jsr setVICFromCurrMultiSlot
		jmp _exit
_noEntities
		lda #0
		sta numRasters		
		lda #$FF		
		sta currEntRasterIndex		 
_exit		
		lda # <updateSpriteRaster
		sta $FFFE
		lda # >updateSpriteRaster
		sta $FFFF
		jsr DetermineNextRasterLineAndType
		inc lsbtod
		jmp eirq2
		
setVICFromCurrMultiSlot
;	inc $D020
;	inc $D020
	ldx currEntRasterIndex
	lda rasterSpriteSet.y,x		
	sta $D005
	sta $D007
	sta $D009
	lda rasterSpriteSet.x1,x		
	sta $D004		
	lda rasterSpriteSet.x2,x		
	sta $D006	
	lda rasterSpriteSet.x3,x		
	sta $D008		
	lda rasterSpriteSet.col1,x	
	sta $D029
	lda rasterSpriteSet.col2,x	
	sta $D02A	
	lda rasterSpriteSet.col3,x	
	sta $D02B	
	lda $D01C	
	and #%00000010
	ora rasterSpriteSet.multi,x	
	sta $D01C
	lda rasterSpriteSet.ptr1,x	
	sta kVectors.spr2ID	
	lda rasterSpriteSet.ptr2,x	
	sta kVectors.spr3ID	
	lda rasterSpriteSet.ptr3,x	
	sta kVectors.spr4ID	
	lda $D015	
	and #%11100011	
	ora rasterSpriteSet.enable,x
	sta $D015
	dec currEntRasterIndex
;	dec $D020
;	dec $D020
	rts
	
DetermineNextRasterLineAndType
	ldx CurrStatusRasterIndex
	cpx #4
	beq _noMoreStatus
	lda statusSpritesDefs.rasterPos,x	
	bne +	
_noMoreStatus		
	lda #$FF		
+	sta rasterTemp1		
	ldx currEntRasterIndex					
	bmi _noMoreEntRasters	
	;cpx numRasters		
	;beq _noMoreEntRasters		
	lda rasterSplitYPosEnt,x		
	bne +		
_noMoreEntRasters
	lda #$FF		
+	sta rasterTemp2	
	sec 	
	sbc rasterTemp1	
	cmp #256-5	
	bcs _both	
	cmp #6	
	bcc _both	
	lda rasterTemp2	
	cmp rasterTemp1		
	;beq _both		
	bcc _ent	
	lda # kNextRasterType.status	
	bne _exit
_ent	
	lda rasterTemp2
	sta rasterTemp1
	lda # kNextRasterType.ent		
	bne _exit	
_both	
	lda rasterTemp1	
	cmp rasterTemp2	
	bcc +	
	lda rasterTemp2	
	sta rasterTemp1	
+	lda # kNextRasterType.both		
_exit		
	sta nextRasterType	
	lda rasterTemp1
	cmp #kRaster.bottomRaster - 8
	bcs doneUSSR
	sta $D012
	rts			
doneUSSR	
	lda #kRaster.bottomRaster
	sta $D012
	lda # <BottomRaster
	sta $fffe
	lda # >BottomRaster
	sta $ffff	
	rts	
	
plotEndOfLevel
	jsr plotEndBox
		
	ldx #4
	stx NormalTemp1	
-	lda StringLUT,x
	sta ZPTemp
	lda XOffsetLUT,x
	sta ZPTemp2
	lda YOffsetLUT,x
	tay 
	jsr PlotEndMsg
	dec NormalTemp1
	ldx NormalTemp1
	bpl -
			
	lda #<300
	sta pointer1
	lda #>300
	sta pointer1+1
	jsr convertP1ValueToP3String
	jsr addP3toScore
	jsr unpackP3ToP1P2
		
	ldx #21
	ldy #12
	jsr pltP1P2toScreen
	jsr plotScore
	cld
	rts
		
StringLUT 	.byte 00,01,03,02,03
XOffsetLUT	.byte 15,09,15,09,15
YOffsetLUT	.byte 10,12,12,14,14

plotEndMsg
	lda screenROWLUTLo,y
	sta screenPointer
	lda screenROWLUTHi,y
	sta screenPointer+1
	ldy ZPTemp
	lda StringTBLLo,y
	sta pointer1
	lda StringTBLHi,y
	sta pointer1+1
	lda StringLength,y
	sta ZPTemp3
-	ldy ZPTemp3
	lda (pointer1),y
	ldy ZPTemp2
	sta (screenPointer),y
	inc ZPtemp2
	lda ZPTemp3
	sec
	sbc #1
	sta ZPTemp3
	bpl -
	cld
	rts

plotEndBox
	ldy #8
	sty ZPTemp
_loopRow
	lda screenROWLUTLo,y
	sta screenPointer
	sta pointer1
	lda screenROWLUTHi,y
	sta screenPointer+1
	eor # (>kVectors.charBase) ^ $D8
	sta pointer1+1
	ldy #7
_loopColoumn
	lda #0
	sta (screenPointer),y
	lda #1
	sta (pointer1),y
	iny
	cpy #19+8
	bne _loopColoumn
	lda ZPTemp
	clc
	adc #1
	tay
	sta ZPTemp
	cmp #17
	bne _loopRow
	rts 
	
convertP1ValueToP3String
	lda #0
	sta pointer3
	sta pointer3+1
	ldx #0
-	lsr pointer1+1
	bcs +
	beq _lower
	bcc ++
+	jsr addXToP3
+	inx
	inx
	bne -
_lower
	ldx #6
-	asl pointer1
	bcs +
	beq _done
	bcc ++
+	jsr addXToP3
+	inx
	inx
	bne -
_done
	rts

addXToP3
	sed
	lda pointer3
	clc
	adc BitToStringValues,x
	sta pointer3
	lda pointer3+1
	adc BitToStringValues+1,x
	sta pointer3+1
	cld
	rts
	
unpackP3ToP1P2
	lda pointer3+1
	lsr a
	lsr a
	lsr a
	lsr a
	sta pointer1
	lda pointer3+1
	and #$F
	sta pointer1+1
	lda pointer3
	lsr a
	lsr a
	lsr a
	lsr a
	sta pointer2
	lda pointer3
	and #$F
	sta pointer2+1
	rts
	
pltP1P2toScreen
	lda screenROWLUTLo,y
	sta screenPointer
	lda screenROWLUTHi,y
	sta screenPointer+1
	txa
	tay
	lda pointer1
	clc
	adc # kFont.digitsStart
	sta (screenPointer),y
	iny
	lda pointer1+1
	clc
	adc # kFont.digitsStart
	sta (screenPointer),y
	iny
	lda pointer2
	clc
	adc # kFont.digitsStart
	sta (screenPointer),y
	iny
	lda pointer2+1
	clc
	adc # kFont.digitsStart
	sta (screenPointer),y
	rts
	
bitToStringValues 	.byte $56,$02
					.byte $12,$05
					.byte $24,$10				  	

					.byte $28,$01
					.byte $64,$00
					.byte $32,$00
					.byte $16,$00
					.byte $08,$00
					.byte $04,$00
					.byte $02,$00
					.byte $01,$00
					
addP3toScore
	sed
	lda pointer3
	clc
	adc Score.digit21
	sta Score.digit21
	lda pointer3+1
	adc Score.digit43
	sta Score.digit43
	lda #0
	adc Score.digit65
	sta Score.digit65
	cld
	rts
	
					
EntEnableMaskLUT .byte 0,%00000100, %00001100, %00011100

statusSpritesDefs .block		
	kNum = 10
	kColour = 1
	;   | high score          |  score      |    stage    | Lives | timer
x .byte 24+8,24,48,24,48,24,48,36 , 27, 51
y .byte 50,71,71, 50+54,50+54, 50+92,50+92, 50+130, 50+168,50+168
def .byte kSprBase+38,kSprBase+36,kSprBase+37, kSprBase+34,kSprBase+35, kSprBase+39,kSprBase+40, kSprBase+41, kSprBase+44,kSprBase+45 
numSprToDo .byte 2,1,2,1 ;-1 of value		
rasterPos .byte 00,100,138,210
.bend		

; first is a dummy 
charColourTBL .byte 0,10,14,10,12,12,12

StringTBLLo .byte <GOALString,<StageString,<TimeString,<BonusString
StringTBLHi .byte >GOALString,>StageString,>TimeString,>BonusString
StringLength .byte 4,4,3,4
.enc level_end
GOALString .text "!LAOG"
StageString .text "EGATS"
TimeString .text "EMIT"
BonusString .text "SUNOB"



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
			