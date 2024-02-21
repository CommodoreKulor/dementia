;DemenTIA -- Sample Edition
;Version 7.4
;Last updated December 2017
;Grab my mixtape: https://kulor.bandcamp.com/album/soundchip-salad
;
;Copyright 2017 Richard 'kulor' Armijo, all rights reserved.
;This program is free software: you can redistribute it and/or modify
;it under the terms of the GNU General Public License as published by
;the Free Software Foundation, either version 3 of the License, or
;(at your option) any later version.
;
;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.
;
;The GNU General Public License version 3 can be found here:
;https://www.gnu.org/licenses/gpl-3.0.en.html
;
;This is the SAMPLE EDITION!!
;It allows for the use of 1-bit DPCM samples.
;Samples are very large and playback takes almost all the CPU
;time, so if you aren't gonna use samples or want to make music
;for a game or demo, I highly suggest grabbing the vanilla
;edition of this music engine. It's got all the same functionality
;except for samples!
;I've tried to document this as thoroughly as I can
;This program was written in pseudocode and then hand-'compiled'
;I've left the pseudocode as comments throughout the assembly,
;which should give you an idea of where in the program logic
;you are
;Assembly comments which are pseudocode snippets are tabbed
;over at least twice, additional assembly comments which are not
;pseudocode are tabbed over once
;
;---Stats---
;Program size:
;938 bytes
;3158 bytes free for song data/other program stuff
;That's including the display code
;
;Program runtime (worst case):
;	Aggregate of every operation in program:
;1121 cycles, just under 15 scanlines
;
;RAM usage:
;24 persistent bytes
;+2 scratch bytes 
;2 bytes of stack
;
;---ROM data format---
;PatLength
;value
;
;Speed
;value
;
;c1SongData
;c2SongData
;IIIIIIII
;I: Pattern index
;I = 255: End of song data
;
;c1PatternTable
;c2PatternTable
;PPPPPPPP PPPPPPPP
;P: Pointer to start of pattern data
;
;PatternData
;Iterated through every Speed + 1 frames
;BVVFFFFF TTMMMMMM
;B: Blank
;	Ignores M this row, does not restart macro
;	(basically lets a note play out for longer than a row)
;	You can change volume, tuning and frequency on a blank row
;	which has the effect of not retriggering the note
;V: Volume shift
;	Number of times to shift macro volume right
;F: Frequency
;T: Tuning
;	Affects frequency on relative macro frames according to phase:
;	T=0: No effect
;	T=1: Frequency + 1 on phase % 4 = 3
;	T=2: Frequency + 1 on phase % 2 = 1
;	T=3; Frequency + 1 on phase % 4 = 0, 1, 2
;M: Macro index
;
;MacroTable
;PPPPPPPP PPPPPPPP
;P: Pointer to start of macro data
;
;MacroData
;Iterated through every frame, macro position reset every
;non-blank pattern row
;ELRFFFFF VVVVWWWW (PPPPPPPP)
;E: End of macro
;L: Loop macro
;	End: 1, loop: 0 - Macro position will not be incremented
;	per frame
;	(data from this macro frame will be held until next
;	note is triggered)
;	End: 1, loop: 1 - Macro position will be set to 0
;	(macro will start over immediately on next frame)
;	End: 0, loop: x - Continue macro
;R: Relative or fixed frequency value
;	R=1: Frequency - 15 is added to pattern frequency value
;		(F=16 will maintain pattern frequency,
;		F<16 will subtract, F>16 will add)
;		(not terribly sure why 16 works and not 15...)
;	R=0: Frequency passed straight to register, pattern
;		frequency value ignored this frame
;F: Frequency
;V: Volume
;W: Wave
;P: If loop flag is set, macro start + P bytes is where
;	it'll loop back to
;
;Wave = 0 is a special flag which enables sample mode for
;that frame
;When Wave = 0...
;V: Sample index
;F: 1 triggers the sample, 0 continues the sample
;R: Unused
;E, L: As before
;
;SampleTable
;SSSSSSSS SSSSSSSS
;EEEEEEEE EEEEEEEE
;S: Pointer to start of sample data
;E: Pointer to end of sample data
;
;SampleData
;DDDDDDDD
;DDDDDDDD
;[...]
;D: DPCM sample data
;	DPCM counter is 4-bit, output ~7830hz
;	DPCM sample data is read from bytes in this order:
;	87654321
;	Each sample should be followed by four bytes of 0
;	to prevent clicking when the sample ends
;
;---Pseudocode---
;
;
;	---Frame---
;
;	;On new pattern row
;	if (phase == 0) {
;		;Handle c1 patterns
;		scratch_CurrentPattern = PatternData[c1PatternTable[c1SongData[songPos]] + patPos]
;		c1freq = scratch_CurrentPattern & 00011111
;		c1vol = scratch_CurrentPattern & 01100000 >> 5
;		;if this isn't a blank row
;		if (scratch_CurrentPattern & 10000000 != 10000000) {
;			c1macPos = 0
;			scratch_CurrentPattern = PatternData[c1PatternTable[c1SongData[songPos]] + patPos + 1]
;			c1macro = scratch_CurrentPattern & 00111111
;		}
;		else {
;			scratch_CurrentPattern = PatternData[c1PatternTable[c1SongData[songPos]] + patPos + 1]
;		}
;		c1tune = scratch_CurrentPattern & 11000000
;		
;		;Handle c2 patterns (same as above just copypasted)
;		scratch_CurrentPattern = PatternData[c2PatternTable[c2SongData[songPos]] + patPos]
;		c2freq = scratch_CurrentPattern & 00011111
;		c2vol = scratch_CurrentPattern & 01100000 >> 5
;		;if this isn't a blank row
;		if (scratch_CurrentPattern & 10000000 != 10000000) {
;			c2macPos = 0
;			scratch_CurrentPattern = PatternData[c2PatternTable[c2SongData[songPos]] + patPos + 1]
;			c2macro = scratch_CurrentPattern & 00111111
;		}
;		else {
;			scratch_CurrentPattern = PatternData[c2PatternTable[c2SongData[songPos]] + patPos + 1]
;		}
;		c2tune = scratch_CurrentPattern & 11000000
;		
;		;Handle positioning
;		patPos += 2
;		;If overflow
;		if (patPos > PatLength || patPos == 0) {
;			patPos = 0
;			++songPos
;			if (c1SongData[songPos] == 255 || c2SongData[songPos] == 255) {
;				songPos = 0
;			}
;		}	
;	}
;
;	;Always
;	;Handle c1 macros
;	scratch_CurrentMacro = MacroData[MacroTable[c1macro] + (c1macPos) + 1]
;	if (scratch_CurrentMacro & 00001111 == 00000000) {
;		;Shifted by 2 to account for size of sample table entries
;		dpcmIndex = scratch_CurrentMacro & 11110000 >> 2
;		if (scratch_CurrentMacro & 00000001 == 1) {
;			;Trigger sample
;			dpcmOffset = SampleTable[dpcmIndex]
;			dpcmStopAddr = SampleTable[dpcmIndex + 2]
;			dpcmPos = 0
;			dpcmPhase = 0
;			audc0 = 0
;			dpcmPlay = 1
;		}
;		dpcmChan |= 00000001
;	}
;	else {
;		audc0 = scratch_CurrentMacro & 00001111
;		audv0 = ((scratch_CurrentMacro & 11110000) >> 4) >> c1vol
;		scratch_CurrentMacro = MacroData[MacroTable[c1macro] + (c1macPos)]
;		;Check relative flag
;		if (scratch_CurrentMacro & 00100000 == 00100000) {
;			;relative
;			;handle tuning
;			if ((c1tune == 01000000 && phase & 00000011 == 3) ||
;				(c1tune == 10000000 && phase & 00000001 == 1) ||
;				(c1tune == 11000000 && phase & 00000011 != 3))
;			{
;				audf0 = c1freq + ((scratch_CurrentMacro & 00011111) - 15) + 1
;			}
;			else {
;				audf0 = c1freq + ((scratch_CurrentMacro & 00011111) - 15)
;			}
;		}
;		else {
;			;fixed
;			audf0 = scratch_CurrentMacro & 00011111
;		}
;		dpcmChan &= 11111110
;	}
;	;Check end flag
;	if (scratch_CurrentMacro & 11000000 != 00000000) {
;		;Either loop or end was set
;		if (scratch_CurrentMacro & 01000000 == 01000000) {
;			;Handle looping
;			c1macPos = MacroData[MacroTable[c1macro] + (c1macPos) + 2]
;		}
;	}
;	else {
;		c1macPos += 2
;	}
;
;	;Handle c2 macros (same as above just copypasted)
;	scratch_CurrentMacro = MacroData[MacroTable[c2macro] + (c2macPos) + 1]
;	if (scratch_CurrentMacro & 00001111 == 00000000) {
;		;Shifted by 2 to account for size of sample table entries
;		dpcmIndex = scratch_CurrentMacro & 11110000 >> 2
;		if (scratch_CurrentMacro & 00000001 == 1) {
;			;Trigger sample
;			dpcmOffset = SampleTable[dpcmIndex]
;			dpcmStopAddr = SampleTable[dpcmIndex + 2]
;			dpcmPos = 0
;			dpcmPhase = 0
;			audc1 = 0
;			dpcmPlay = 1
;		}
;		dpcmChan |= 00000010
;	}
;	else {
;		audc1 = scratch_CurrentMacro & 00001111
;		audv1 = ((scratch_CurrentMacro & 11110000) >> 4) >> c2vol
;		scratch_CurrentMacro = MacroData[MacroTable[c2macro] + (c2macPos)]
;		;Check relative flag
;		if (scratch_CurrentMacro & 00100000 == 00100000) {
;			;relative
;			;handle tuning
;			if ((c2tune == 01000000 && phase & 00000011 == 3) ||
;				(c2tune == 10000000 && phase & 00000001 == 1) ||
;				(c2tune == 11000000 && phase & 00000011 != 3))
;			{
;				audf1 = c2freq + ((scratch_CurrentMacro & 00011111) - 15) + 1
;			}
;			else {
;				audf1 = c2freq + ((scratch_CurrentMacro & 00011111) - 15)
;			}
;		}
;		else {
;			;fixed
;			audf1 = scratch_CurrentMacro & 00011111
;		}
;	}
;	;Check end flag
;	if (scratch_CurrentMacro & 11000000 != 00000000) {
;		;Either loop or end was set
;		if (scratch_CurrentMacro & 01000000 == 01000000) {
;			;Handle looping
;			c2macPos = MacroData[MacroTable[c2macro] + (c2macPos) + 2]
;		}
;	}
;	else {
;		c2macPos += 2
;	}
;
;	++phase
;	if (phase > Speed) {
;		phase = 0
;	}

;	if (dpcmPlay == 1) {
;		dpcmOffset += dpcmPos
;		dpcmPos = 0
;		if (dpcmOffset == dpcmStopAddr) {
;			dpcmPlay = 0
;		}
;	}
;
;
;	---Scanline---
;
;	if (dpcmPlay == 1) { 
;		if (dpcmPhase == 0) {
;			dpcmBuffer = dpcmOffset[dpcmPos]
;			dpcmPos++
;		}
;		else {
;			dpcmBuffer = dpcmBuffer >> 1
;		}
;		if (dpcmBuffer & 00000001 == 1) {
;			dpcmCounter++
;			if (dpcmCounter >= 15) {
;				dpcmCounter = 15
;			}
;		}
;		else {
;			dpcmCounter--
;			if (dpcmCounter < 0) {
;				dpcmCounter = 0
;			}
;		}
;		dpcmPhase = ++dpcmPhase & 00000111
;		if (dpcmChan & 00000001 == 00000001) {
;			audv0 = dpcmCounter
;		}
;		if (dpcmChan & 00000010 == 00000010) {
;			audv1 = dpcmCounter
;		}
;	}

		processor 6502
		include "vcs.h"
		
;RAM----------------------------------------------------------------------
Color = $F0					;Visualizer stuff
ColorTemp = $F1
LoopCount = $F2
VisualBuf1 = $F3
VisualBuf2 = $F4
VisualBuf3 = $F5
phase = $80					;Important stuff
songPos = $81
patPos = $82
c1freq = $83
c2freq = $84
c1vol = $85
c2vol = $86
c1tune = $87
c2tune = $88
c1macro = $89
c2macro = $8A
c1macPos = $8B
c2macPos = $8C
scratch1 = $8D
scratch2 = $8E
dpcmCounter = $8F
dpcmPos = $90
dpcmOffset1 = $91
dpcmOffset2 = $92
dpcmPhase = $93
dpcmPlay = $94
dpcmChan = $95
dpcmBuffer = $96
dpcmStopAddr1 = $97
dpcmStopAddr2 = $98
dpcmIndex = $99

		SEG
		ORG $F000

		;brk
reset	cld
		ldx #$FF
		txs
		lda #0				;Initialize variables
		sta phase
		sta songPos
		sta patPos
		sta c1freq
		sta c2freq
		sta c1tune
		sta c2tune
		sta c1macro
		sta c2macro
		sta c1macPos
		sta c2macPos
		sta scratch1
		sta scratch2
		sta dpcmCounter
		sta dpcmPos
		sta dpcmOffset1
		sta dpcmOffset2
		sta dpcmPhase
		sta dpcmPlay
		sta dpcmChan
		sta dpcmBuffer
		sta dpcmStopAddr1
		sta dpcmStopAddr2
		sta dpcmIndex
		
		sta Color
		sta VisualBuf1
		sta VisualBuf2
		sta COLUBK
		
		;fix blue lines on real hardware
		sta GRP0
		sta GRP1
		sta ENAM0
		sta ENAM1
		sta ENABL
		
		lda #1
		sta CTRLPF

;Start of vertical blank processing---------------------------------------
frame	lda #0
		sta VBLANK
		lda #2
		sta VSYNC
		sta WSYNC			;3 scanlines
		sta WSYNC
		sta WSYNC
		lda #0
		sta VSYNC  
		
;37 scanlines of vertical blank, 76 cycles per line-----------------------

		ldy #18
		sty LoopCount
vbloop	
		jsr dpcmUpdate
		sta WSYNC
		ldy LoopCount
		dey
		sty LoopCount
		cpy #0
		bne vbloop
		
		sta WSYNC

;192 scanlines of picture, 76 cycles per line-----------------------------
		ldy #96			;scanlines / 2
		sty LoopCount
		ldx Color
		stx ColorTemp
drwloop	
		ldx ColorTemp
		inx
		stx COLUPF
		stx ColorTemp
		lda VisualBuf3
		sta PF0
		sta PF1
		sta PF2
		clc
		rol
		adc #0
		sta VisualBuf3
		jsr dpcmUpdate
		sta WSYNC
		ldy LoopCount
		dey
		sty LoopCount
		;cpy #0				implied
		bne drwloop
		dec Color
		lda #%01000010
		sta VBLANK			;end of screen - enter  blanking
		
;30 scanlines of overscan, 76 cycles per line-----------------------------
;Average 3 cycles per command

;*************************************************************************
;*																		 *
;*						 DemenTIA Engine Begin!							 *
;*																		 *
;*************************************************************************

;		;On new pattern row
;		if (phase == 0) {
		lda phase										;T=3
		cmp #0											;T=2
		beq doNotJumpIfPhaseZero						;T=4
		jmp ifPhaseNotZero								;T=3
doNotJumpIfPhaseZero
;			;Handle c1 patterns
;			scratch_CurrentPattern = PatternData[c1PatternTable[c1SongData[songPos]] + patPos]
;	Hold up! Break that down:
;	c1SongData[songPos] -> y
		ldy songPos										;T=3
		lda c1SongData,y								;T=5
		asl												;T=2
		tay												;T=2
;	c1PatternTable[y] + patPos -> scratch
		clc												;T=2
		lda c1PatternTable,y							;T=5
		adc patPos										;T=3
		sta scratch1									;T=3
		iny												;T=2
		ldx c1PatternTable,y							;T=5
		bcc skipPatternTableCarry1						;T=4
		inx												;T=2
skipPatternTableCarry1
		stx scratch2									;T=3
;	PatternData[scratch]
		ldy #0											;T=2
		lda (scratch1),y								;T=6
		tax												;T=2
;			c1freq = scratch_CurrentPattern & 00011111
		and #%00011111									;T=2
		sta c1freq										;T=3
;			c1vol = scratch_CurrentPattern & 01100000
		txa												;T=2
		lsr												;T=2
		lsr												;T=2
		lsr												;T=2
		lsr												;T=2
		lsr												;T=2
;	If the blank flag is set it might mess up volume
		and #%00000011									;T=2
		sta c1vol										;T=3
;			;if this isn't a blank row
;			if (scratch_CurrentPattern & 10000000 != 10000000) {
		txa												;T=2
		and #%10000000									;T=2
		cmp #%10000000									;T=2
		beq ifCurrentPatternBlankRowFlag1				;T=4
;				c1macPos = 0
		sty c1macPos									;T=3
;				scratch_CurrentPattern = PatternData[c1PatternTable[c1SongData[songPos]] + patPos + 1]
;	already have scratch pointing where it needs to and patPos in y
		iny												;T=2
		lda (scratch1),y								;T=6
		tax												;T=2
;				c1macro = scratch_CurrentPattern & 00111111
		and #%00111111									;T=2
		sta c1macro										;T=3
		txa												;T=2
		jmp ifCurrentPatternNotBlank1					;T=3
;			}
;			else {
ifCurrentPatternBlankRowFlag1
;				scratch_CurrentPattern = PatternData[c1PatternTable[c1SongData[songPos]] + patPos + 1]
		iny												;T=2
		lda (scratch1),y								;T=6
;			}
ifCurrentPatternNotBlank1
;			c1tune = scratch_CurrentPattern & 11000000
		and #%11000000									;T=2
		sta c1tune										;T=3



;			;Handle c2 patterns (same as above just copypasted)
;			scratch_CurrentPattern = PatternData[c2PatternTable[c2SongData[songPos]] + patPos]
;	Hold up! Break that down:
;	c2SongData[songPos] -> y
		ldy songPos										;T=3
		lda c2SongData,y								;T=5
		asl												;T=2
		tay												;T=2
;	c2PatternTable[y] + patPos -> scratch
		clc												;T=2
		lda c2PatternTable,y							;T=5
		adc patPos										;T=3
		sta scratch1									;T=3
		iny												;T=2
		ldx c2PatternTable,y							;T=5
		bcc skipPatternTableCarry2						;T=4
		inx												;T=2
skipPatternTableCarry2
		stx scratch2									;T=3
;	PatternData[scratch]
		ldy #0											;T=2
		lda (scratch1),y								;T=6
		tax												;T=2
;			c2freq = scratch_CurrentPattern & 00011111
		and #%00011111									;T=2
		sta c2freq										;T=3
;			c2vol = scratch_CurrentPattern & 01100000
		txa												;T=2
		lsr												;T=2
		lsr												;T=2
		lsr												;T=2
		lsr												;T=2
		lsr												;T=2
;	If the blank flag is set it might mess up volume
		and #%00000011									;T=2
		sta c2vol										;T=3
;			;if this isn't a blank row
;			if (scratch_CurrentPattern & 10000000 != 10000000) {
		txa												;T=2
		and #%10000000									;T=2
		cmp #%10000000									;T=2
		beq ifCurrentPatternBlankRowFlag2				;T=4
;				c2macPos = 0
		sty c2macPos									;T=3
;				scratch_CurrentPattern = PatternData[c2PatternTable[c2SongData[songPos]] + patPos + 1]
;	already have scratch pointing where it needs to and patPos in y
		iny												;T=2
		lda (scratch1),y								;T=6
		tax												;T=2
;				c1macro = scratch_CurrentPattern & 00111111
		and #%00111111									;T=2
		sta c2macro										;T=3
		txa												;T=2
		jmp ifCurrentPatternNotBlank2					;T=3
;			}
;			else {
ifCurrentPatternBlankRowFlag2
;				scratch_CurrentPattern = PatternData[c2PatternTable[c2SongData[songPos]] + patPos + 1]
		iny												;T=2
		lda (scratch1),y								;T=6
;			}
ifCurrentPatternNotBlank2
;			c2tune = scratch_CurrentPattern & 11000000
		and #%11000000									;T=2
		sta c2tune										;T=3


;			;Handle positioning
;			patPos += 2
		inc patPos										;T=5
		inc patPos										;T=5
;			if (patPos > PatLength) {
		lda patPos										;T=3
		cmp PatLength									;T=4
;	branch on <
		bcc ifPatPosLessEq								;T=4
;	branch on =, total is <=
		beq ifPatPosLessEq								;T=4
;				patPos = 0
		ldx #0											;T=2
		stx patPos										;T=3
;				++songPos
		inc songPos										;T=5
;				if (c1SongData[songPos] == 255 || c2SongData[songPos] == 255) {
;					songPos = 0
;				}
;	c1SongData[songPos] == 255
		ldy songPos										;T=3
		lda c1SongData,y								;T=5
		cmp #255										;T=2
		bne ifC1SongDataNotMax							;T=4
		stx songPos										;T=3
		jmp doneWithSongPos								;T=3
ifC1SongDataNotMax
;	c2SongData[songPos] == 255
		lda c2SongData,y								;T=5
		cmp #255										;T=2
		bne ifC2SongDataNotMax							;T=4
		stx songPos										;T=3
ifC2SongDataNotMax
;			}	
ifPatPosLessEq
;		}
ifPhaseNotZero
doneWithSongPos

;		;Always
;		;Handle c1 macros
;		scratch_CurrentMacro = MacroData[MacroTable[c1macro] + c1macPos + 1]
;	Break that down again!
;	MacroTable[c1macro] -> scratch
		lda c1macro										;T=3
		asl												;T=2
		tay												;T=2
		lda MacroTable,y								;T=5
		sta scratch1									;T=3
		iny												;T=2
		lda MacroTable,y								;T=5
		sta scratch2									;T=3
;	c1macPos + 1 -> y
		ldy c1macPos									;T=3
		iny												;T=2
;	MacroData[scratch + y]
		lda (scratch1),y								;T=6
		tax												;T=2
;		if (scratch_CurrentMacro & 00001111 == 00000000) {
		and #%00001111									;T=2
		cmp #0											;T=2
		bne nonSampleMacro1								;T=4
;			;Shifted by 2 to account for size of sample table entries
;			dpcmIndex = scratch_CurrentMacro & 11110000 >> 2
		sta AUDC0										;T=3		;oops! forgot to make sure we're on wave 0 on DPCM frames
		txa												;T=2
		and #%11110000									;T=2
		lsr												;T=2
		lsr												;T=2
		sta dpcmIndex									;T=3
;			if (scratch_CurrentMacro & 00000001 == 1) {
		dey												;T=2
		lda (scratch1),y								;T=6
		and #%00000001									;T=2
		cmp #1											;T=2
		bne noSampleRetrigger1							;T=4
;				;Trigger sample
;				dpcmOffset = SampleTable[dpcmIndex]
		ldy dpcmIndex									;T=3
		lda SampleTable,y								;T=5
		sta dpcmOffset1									;T=3
		iny												;T=2
		lda SampleTable,y								;T=5
		sta dpcmOffset2									;T=3
;				dpcmStopAddr = SampleTable[dpcmIndex + 2]
		iny												;T=2
		lda SampleTable,y								;T=5
		sta dpcmStopAddr1								;T=3
		iny												;T=2
		lda SampleTable,y								;T=5
		sta dpcmStopAddr2								;T=3
;				dpcmPos = 0
;				dpcmPhase = 0
;				audc0 = 0
		ldy #0											;T=2
		sty dpcmPos										;T=3
		sty dpcmPhase									;T=3
		sty AUDC0										;T=3
;				dpcmPlay = 1
		iny												;T=2
		sty dpcmPlay									;T=3
;			}
noSampleRetrigger1
;			dpcmChan |= 00000001
		lda dpcmChan									;T=3
		ora #%00000001									;T=2
		sta dpcmChan									;T=3
;		}
		ldy c1macPos									;T=3
		lda (scratch1),y								;T=6
		tax												;T=2
		jmp doneWithDpcmMacro1							;T=3
;		else {
nonSampleMacro1
		txa												;T=2
;			audc0 = scratch_CurrentMacro & 00001111
		and #%00001111									;T=2
		sta AUDC0										;T=3
;			audv0 = ((scratch_CurrentMacro & 11110000) >> 4) >> c1vol
		txa												;T=2
		lsr												;T=2
		lsr												;T=2
		lsr												;T=2
		lsr												;T=2
;	>> c1vol
		ldx c1vol										;T=3
loopVolShift1
		cpx #0											;T=2
		beq doneShiftingVol1							;T=4
		lsr												;T=2
		dex												;T=2
		jmp loopVolShift1								;T=3
doneShiftingVol1
		sta AUDV0										;T=3
;			scratch_CurrentMacro = MacroData[MacroTable[c1macro] + c1macPos]
;	already have scratch pointing where it needs to and c1macPos + 1 in y
		dey												;T=2
;			;Check relative flag
;			if (scratch_CurrentMacro & 00100000 == 00100000) {
		lda (scratch1),y								;T=6
		tax												;T=2
		and #%00100000									;T=2
		cmp #%00100000									;T=2
		bne relativeFlagNotSet1							;T=4
;				;relative
;				;handle tuning
;				if ((c1tune == 01000000 && phase & 00000011 == 3) ||
;					(c1tune == 10000000 && phase & 00000001 == 1) ||
;					(c1tune == 11000000 && phase & 00000011 != 3))
;				{
;					audf0 = c1freq + ((scratch_CurrentMacro & 00011111) - 15) + 1
;				}
;				else {
;					audf0 = c1freq + ((scratch_CurrentMacro & 00011111) - 15)
;				}
		txa												;T=2
		clc												;T=2
		and #%00011111									;T=2
		sbc #15											;T=2
		clc												;T=2
		adc c1freq										;T=3
		tay												;T=2
;	y is c1freq + ((scratch_CurrentMacro & 00011111) - 15), x is scratch_CurrentMacro
;	(c1tune == 01000000 && phase & 00000011 == 3)
		lda c1tune										;T=3
		cmp #%01000000									;T=2
		bne tuneNotOne1									;T=4
		lda phase										;T=3
		and #%00000011									;T=2
		cmp #3											;T=2
		bne doneTuning1									;T=4
		iny												;T=2
		jmp doneTuning1									;T=3
tuneNotOne1
;	(c1tune == 10000000 && phase & 00000001 == 1)
		cmp #%10000000									;T=2
		bne tuneNotTwo1									;T=4
		lda phase										;T=3
		and #%00000001									;T=2
		cmp #1											;T=2
		bne doneTuning1									;T=4
		iny												;T=2
		jmp doneTuning1									;T=3
tuneNotTwo1
;	(c1tune == 11000000 && phase & 00000011 != 3)
		cmp #%11000000									;T=2
		bne doneTuning1									;T=4
		lda phase										;T=3
		and #%00000011									;T=2
		cmp #3											;T=2
		beq doneTuning1									;T=4
		iny												;T=2
doneTuning1
		sty AUDF0										;T=3
		sty VisualBuf1				;For visuals		;T=3
		jmp notFixedFlag1								;T=3
;			}
relativeFlagNotSet1
;			else {
;				;fixed
;				audf0 = scratch_CurrentMacro & 00011111
;	x still has scratch_CurrentMacro
		txa												;T=2
		and #%00011111									;T=2
		sta AUDF0										;T=3
;			}
notFixedFlag1
;			dpcmChan &= 11111110
		lda dpcmChan									;T=3
		and #%11111110									;T=2
		sta dpcmChan									;T=3
;		}
doneWithDpcmMacro1
;		;Check end flag
;		if (scratch_CurrentMacro & 11000000 != 00000000) {
		txa												;T=2
		and #%11000000									;T=2
		cmp #0											;T=2
		beq loopAndEndClear1							;T=4
;			;Either loop or end was set
;			if (scratch_CurrentMacro & 01000000 == 01000000) {
		txa												;T=2
		and #%01000000									;T=2
		cmp #%01000000									;T=2
;	!= means the end flag is set, so don't increment
		bne loopAndEndCheckDone1						;T=4
;				;Handle looping
;				c1macPos = MacroData[MacroTable[c1macro] + (c1macPos) + 2]
;	scratch is still pointing where it needs to, just need to handle y
		ldy c1macPos									;T=3
		iny												;T=2
		iny												;T=2
		lda (scratch1),y								;T=6
		sta c1macPos									;T=3
		jmp loopAndEndCheckDone2						;T=3
;			}
;		}
loopAndEndClear1
;		else {
;			c1macPos += 2
		inc c1macPos									;T=5
		inc c1macPos									;T=5
;		}
loopAndEndCheckDone1

;		;Handle c2 macros (same as above just copypasted)
;		scratch_CurrentMacro = MacroData[MacroTable[c2macro] + c2macPos + 1]
;	Break that down again!
;	MacroTable[c2macro] -> scratch
		lda c2macro										;T=3
		asl												;T=2
		tay												;T=2
		lda MacroTable,y								;T=5
		sta scratch1									;T=3
		iny												;T=2
		lda MacroTable,y								;T=5
		sta scratch2									;T=3
;	c2macPos + 1 -> y
		ldy c2macPos									;T=3
		iny												;T=2
;	MacroData[scratch + y]
		lda (scratch1),y								;T=6
		tax												;T=2
;		if (scratch_CurrentMacro & 00001111 == 00000000) {
		and #%00001111									;T=2
		cmp #0											;T=2
		bne nonSampleMacro2								;T=4
;			;Shifted by 2 to account for size of sample table entries
;			dpcmIndex = scratch_CurrentMacro & 11110000 >> 2
		sta AUDC1										;T=3		;oops! forgot to make sure we're on wave 0 on DPCM frames
		txa												;T=2
		and #%11110000									;T=2
		lsr												;T=2
		lsr												;T=2
		sta dpcmIndex									;T=3
;			if (scratch_CurrentMacro & 00000001 == 1) {
		dey												;T=2
		lda (scratch1),y								;T=6
		and #%00000001									;T=2
		cmp #1											;T=2
		bne noSampleRetrigger2							;T=4
;				;Trigger sample
;				dpcmOffset = SampleTable[dpcmIndex]
		ldy dpcmIndex									;T=3
		lda SampleTable,y								;T=5
		sta dpcmOffset1									;T=3
		iny
		lda SampleTable,y								;T=5
		sta dpcmOffset2									;T=3
;				dpcmStopAddr = SampleTable[dpcmIndex + 2]
		iny												;T=2
		lda SampleTable,y								;T=5
		sta dpcmStopAddr1								;T=3
		iny												;T=2
		lda SampleTable,y								;T=5
		sta dpcmStopAddr2								;T=3
;				dpcmPos = 0
;				dpcmPhase = 0
;				audc1 = 0
		ldy #0											;T=2
		sty dpcmPos										;T=3
		sty dpcmPhase									;T=3
		sty AUDC1										;T=3
;				dpcmPlay = 1
		iny												;T=2
		sty dpcmPlay									;T=3
;			}
noSampleRetrigger2
;			dpcmChan |= 00000010
		lda dpcmChan									;T=3
		ora #%00000010									;T=2
		sta dpcmChan									;T=3
;		}
		ldy c2macPos									;T=3
		lda (scratch1),y								;T=6
		tax												;T=2
		jmp doneWithDpcmMacro2							;T=3
;		else {
nonSampleMacro2
		txa												;T=2
;			audc1 = scratch_CurrentMacro & 00001111
		and #%00001111									;T=2
		sta AUDC1										;T=3
;			audv0 = ((scratch_CurrentMacro & 11110000) >> 4) >> c2vol
		txa												;T=2
		lsr												;T=2
		lsr												;T=2
		lsr												;T=2
		lsr												;T=2
;	>> c2vol
		ldx c2vol										;T=3
loopVolShift2
		cpx #0											;T=2
		beq doneShiftingVol2							;T=4
		lsr												;T=2
		dex												;T=2
		jmp loopVolShift2								;T=3
doneShiftingVol2
		sta AUDV1										;T=3
;			scratch_CurrentMacro = MacroData[MacroTable[c2macro] + c2macPos]
;	already have scratch pointing where it needs to and c2macPos + 1 in y
		dey												;T=2
;			;Check relative flag
;			if (scratch_CurrentMacro & 00100000 == 00100000) {
		lda (scratch1),y								;T=6
		tax												;T=2
		and #%00100000									;T=2
		cmp #%00100000									;T=2
		bne relativeFlagNotSet2							;T=4
;				;relative
;				;handle tuning
;				if ((c2tune == 01000000 && phase & 00000011 == 3) ||
;					(c2tune == 10000000 && phase & 00000001 == 1) ||
;					(c2tune == 11000000 && phase & 00000011 != 3))
;				{
;					audf1 = c2freq + ((scratch_CurrentMacro & 00011111) - 15) + 1
;				}
;				else {
;					audf1 = c2freq + ((scratch_CurrentMacro & 00011111) - 15)
;				}
		txa												;T=2
		clc												;T=2
		and #%00011111									;T=2
		sbc #15											;T=2
		clc												;T=2
		adc c2freq										;T=3
		tay												;T=2
;	y is c1freq + ((scratch_CurrentMacro & 00011111) - 15), x is scratch_CurrentMacro
;	(c2tune == 01000000 && phase & 00000011 == 3)
		lda c2tune										;T=3
		cmp #%01000000									;T=2
		bne tuneNotOne2									;T=4
		lda phase										;T=3
		and #%00000011									;T=2
		cmp #3											;T=2
		bne doneTuning2									;T=4
		iny												;T=2
		jmp doneTuning2									;T=3
tuneNotOne2
;	(c2tune == 10000000 && phase & 00000001 == 1)
		cmp #%10000000									;T=2
		bne tuneNotTwo2									;T=4
		lda phase										;T=3
		and #%00000001									;T=2
		cmp #1											;T=2
		bne doneTuning2									;T=4
		iny												;T=2
		jmp doneTuning2									;T=3
tuneNotTwo2
;	(c2tune == 11000000 && phase & 00000011 != 3)
		cmp #%11000000									;T=2
		bne doneTuning2									;T=4
		lda phase										;T=3
		and #%00000011									;T=2
		cmp #3											;T=2
		beq doneTuning2									;T=4
		iny												;T=2
doneTuning2
		sty AUDF1										;T=3
		sty VisualBuf2				;For visuals		;T=3
		jmp notFixedFlag2								;T=3
;			}
relativeFlagNotSet2
;		else {
;				;fixed
;				audf1 = scratch_CurrentMacro & 00011111
;	x still has scratch_CurrentMacro
		txa												;T=2
		and #%00011111									;T=2
		sta AUDF1										;T=3
;			}
notFixedFlag2
;			dpcmChan &= 11111110
		lda dpcmChan									;T=3
		and #%11111101									;T=2
		sta dpcmChan									;T=3
;		}
doneWithDpcmMacro2
;		;Check end flag
;		if (scratch_CurrentMacro & 11000000 != 00000000) {
		txa												;T=2
		and #%11000000									;T=2
		cmp #0											;T=2
		beq loopAndEndClear2							;T=4
;			;Either loop or end was set
;			if (scratch_CurrentMacro & 01000000 == 01000000) {
		txa												;T=2
		and #%01000000									;T=2
		cmp #%01000000									;T=2
;	!= means the end flag is set, so don't increment
		bne loopAndEndCheckDone2						;T=4
;				;Handle looping
;				c2macPos = MacroData[MacroTable[c2macro] + (c2macPos) + 2]
;	scratch is still pointing where it needs to, just need to handle y
		ldy c2macPos									;T=3
		iny												;T=2
		iny												;T=2
		lda (scratch1),y								;T=6
		sta c2macPos									;T=3
		jmp loopAndEndCheckDone2						;T=3
;			}
;		}
loopAndEndClear2
;		else {
;			c1macPos += 2
		inc c2macPos									;T=5
		inc c2macPos									;T=5
;		}
loopAndEndCheckDone2

;		++phase
		inc phase										;T=5
;		if (phase > Speed) {
		lda phase										;T=3
		cmp Speed										;T=4
;	branch on <
		bcc ifPhaseNotGreaterThanSpeed					;T=4
;	branch on =, total is <=
		beq ifPhaseNotGreaterThanSpeed					;T=4
;			phase = 0
		lda #0											;T=2
		sta phase										;T=3
;		}
ifPhaseNotGreaterThanSpeed

;		if (dpcmPlay == 1) {
		lda dpcmPlay									;T=3
		cmp #1											;T=2
		bne dpcmNotPlaying2								;T=4
;			dpcmOffset += dpcmPos
		clc												;T=2
		lda dpcmOffset1									;T=3
		adc dpcmPos										;T=3
		sta dpcmOffset1									;T=3
		bcc dpcmOffsetNoCarry							;T=4
		inc dpcmOffset2									;T=6
dpcmOffsetNoCarry
;			dpcmPos = 0
		ldy #0											;T=2
		sty dpcmPos										;T=3
;			if (dpcmOffset >= dpcmStopAddr) {
;	Still have dpcmOffset1 in A
;	Checking for < on a 16-bit number
;	x1 are high bits, x2 are low bits
		clc												;T=2
		cmp dpcmStopAddr1								;T=4
		bcc keepPlayingDpcm								;T=4
		lda dpcmOffset2									;T=3
		cmp dpcmStopAddr2								;T=4
		bcc keepPlayingDpcm								;T=4	
;				dpcmPlay = 0
		sty dpcmPlay									;T=3
;			}
keepPlayingDpcm
;		}
dpcmNotPlaying2
		
		
;*************************************************************************
;*																		 *
;*						   DemenTIA Engine End!							 *
;*																		 *
;*************************************************************************


		ldy #7
		sty LoopCount
osloop	
		jsr dpcmUpdate
		sta WSYNC
		ldy LoopCount
		dey
		sty LoopCount
		cpy #0
		bne osloop
		
		lda #0
		sta PF0
		sta PF1
		sta PF2
		lda #%00001111
		tay
		and VisualBuf2
		sta VisualBuf2
		tya
		and VisualBuf1
		asl
		asl
		asl
		asl
		ora VisualBuf2
		sta VisualBuf3
		
		jmp frame

		
;*************************************************************************
;*																		 *
;*						DemenTIA Sampler Begin!							 *
;*																		 *
;*************************************************************************
		
		
		;126 cycles
dpcmUpdate
;		if (dpcmPlay == 1) { 
		lda dpcmPlay									;T=3
		cmp #1											;T=2
		beq dpcmPlaying									;T=4
		jmp dpcmNotPlaying
dpcmPlaying
;			if (dpcmPhase == 0) {
		lda dpcmPhase									;T=3
;	for incrementing way later, so DON'T USE X!!
		tax												;T=2
		cmp #0											;T=2
		bne dpcmPhaseNotZero							;T=4
;				dpcmBuffer = dpcmOffset[dpcmPos]
		ldy dpcmPos										;T=3
		lda (dpcmOffset1),y								;T=6
		sta dpcmBuffer									;T=3
;				dpcmPos++
		iny												;T=2
		sty dpcmPos										;T=3
;			}
		jmp dpcmPhaseZero								;T=3
;			else {
dpcmPhaseNotZero
;				dpcmBuffer = dpcmBuffer >> 1
		lda dpcmBuffer									;T=3
		lsr												;T=2
		sta dpcmBuffer									;T=3
;			}
dpcmPhaseZero
;			if (dpcmBuffer & 00000001 == 1) {
;	A should still have dpcmBuffer
		and #%00000001									;T=2
		cmp #1											;T=2
		bne dpcmLow										;T=4
;				dpcmCounter++
		ldy dpcmCounter									;T=3
		iny												;T=2
;				if (dpcmCounter >= 15) {
		cpy #15											;T=2
		bcc dpcmCounterNotMax							;T=4
;					dpcmCounter = 15
		ldy #15											;T=2
;				}
dpcmCounterNotMax
		jmp dpcmDone									;T=3
;			}
;			else {
dpcmLow
;				dpcmCounter--
;				if (dpcmCounter < 0) {
;					dpcmCounter = 0
;				}
		ldy dpcmCounter									;T=3
		cpy #0											;T=2
		beq dpcmDone									;T=4
		dey												;T=2
;			}
dpcmDone
		sty dpcmCounter									;T=3
;			dpcmPhase = ++dpcmPhase & 00000111
;	Here's the way later we're talking about
		inx												;T=2
		txa												;T=2
		and #%00000111									;T=2
		sta dpcmPhase									;T=3
;			if (dpcmChan & 00000001 == 00000001) {
		lda dpcmChan									;T=3
		tax												;T=2
		and #%00000001									;T=2
		cmp #%00000001									;T=2
		bne chan1NotPlaying								;T=4
;				audv0 = dpcmCounter
		sty AUDV0										;T=3
;			}
chan1NotPlaying
;			if (dpcmChan & 00000010 == 00000010) {
		txa												;T=2
		and #%00000010									;T=2
		cmp #%00000010									;T=2
		bne chan2NotPlaying								;T=4
;				audv1 = dpcmCounter
		sty AUDV1										;T=3
;			}
chan2NotPlaying
;		}
		jmp dpcmReturn									;T=3
dpcmNotPlaying
;	If DPCM isn't playing, we didn't fill a scanline
		sta WSYNC										;T=3
dpcmReturn
		rts												;T=6
		
		
;*************************************************************************
;*																		 *
;*						 DemenTIA Sampler End!							 *
;*																		 *
;*************************************************************************
		
		
;*************************************************************************
;*																		 *
;*						 DemenTIA Data Begin!							 *
;*																		 *
;*************************************************************************

Speed		
		.byte #8
PatLength
		.byte #31
		
c1SongData
		.byte #5
		.byte #5
		.byte #5
		.byte #6
		.byte #5
		.byte #5
		.byte #5
		.byte #7

		.byte #1
		.byte #2
		.byte #1
		.byte #2
		.byte #1
		.byte #2
		.byte #1
		.byte #7

		.byte #8
		.byte #9
		.byte #8
		.byte #9
		.byte #3
		.byte #4
		.byte #3
		.byte #4

		.byte #1
		.byte #2
		.byte #1
		.byte #2
		.byte #1
		.byte #2
		.byte #1
		.byte #7
		
		.byte #255
		
c2SongData
		.byte #7
		.byte #7
		.byte #8
		.byte #9
		.byte #10
		.byte #10
		.byte #11
		.byte #12

		.byte #1
		.byte #2
		.byte #1
		.byte #2
		.byte #3
		.byte #4
		.byte #3
		.byte #15

		.byte #5
		.byte #6
		.byte #5
		.byte #6
		.byte #5
		.byte #6
		.byte #5
		.byte #6

		.byte #13
		.byte #14
		.byte #13
		.byte #14
		.byte #13
		.byte #14
		.byte #13
		.byte #16
		
		.byte #255



		
c1PatternTable
		.word Blank					;0
		.word ChorusBassPerc1		;1
		.word ChorusBassPerc2		;2
		.word ChorusBassPerc3		;3
		.word ChorusBassPerc4		;4
		.word IntroPercussion1		;5
		.word IntroPercussion2		;6
		.word IntroPercussion3		;7
		.word ChorusBassPercNoDrum1	;8
		.word ChorusBassPercNoDrum2	;9
c2PatternTable
		.word Blank					;0
		.word ChorusBloops1			;1
		.word ChorusBloops2			;2
		.word ChorusBloops3			;3
		.word ChorusBloops4			;4
		.word Pads1					;5
		.word Pads2					;6
		.word Gabber1				;7
		.word Gabber2				;8
		.word Gabber3				;9
		.word Gabber4				;10
		.word Gabber5				;11
		.word Gabber6				;12
		.word ChorusBloopsPads1		;13
		.word ChorusBloopsPads2		;14
		.word ChorusBloops4Alt		;15
		.word ChorusBloopsPads2Alt	;16


		
		
PatternData
;				BVVFFFFF	TTMMMMMM
Blank	
		.byte #%00011111, #%00000000
		.byte #%10011111, #%00000000
		.byte #%10011111, #%00000000
		.byte #%10011111, #%00000000
		.byte #%10011111, #%00000000
		.byte #%10011111, #%00000000
		.byte #%10011111, #%00000000
		.byte #%10011111, #%00000000
		.byte #%10011111, #%00000000
		.byte #%10011111, #%00000000
		.byte #%10011111, #%00000000
		.byte #%10011111, #%00000000
		.byte #%10011111, #%00000000
		.byte #%10011111, #%00000000
		.byte #%10011111, #%00000000
		.byte #%10011111, #%00000000
;				BVVFFFFF	TTMMMMMM
ChorusBassPerc1
		.byte #%00010101, #%00000001
		.byte #%10010101, #%00000001
		.byte #%00010101, #%00000010
		.byte #%00010101, #%00000001
		.byte #%00000000, #%00000011
		.byte #%10000000, #%00000011
		.byte #%00010101, #%00000001
		.byte #%10010101, #%00000001
		.byte #%00010101, #%00000010
		.byte #%10010101, #%00000010
		.byte #%00010101, #%00000001
		.byte #%10010101, #%00000001
		.byte #%00000000, #%00000011
		.byte #%00010111, #%11000001
		.byte #%00010111, #%11000010
		.byte #%00010111, #%11000001
;				BVVFFFFF	TTMMMMMM
ChorusBassPerc2
		.byte #%00010101, #%00000001
		.byte #%10010101, #%00000001
		.byte #%00010101, #%00000010
		.byte #%00010101, #%00000001
		.byte #%00000000, #%00000011
		.byte #%10000000, #%00000011
		.byte #%00010101, #%00000001
		.byte #%10010101, #%00000001
		.byte #%00010101, #%00000010
		.byte #%10010101, #%00000010
		.byte #%00010101, #%00000001
		.byte #%10010101, #%00000001
		.byte #%00000000, #%00000011
		.byte #%00001101, #%10000001
		.byte #%00001101, #%10000001
		.byte #%00001101, #%10000010
;				BVVFFFFF	TTMMMMMM
ChorusBassPerc3
		.byte #%00010101, #%00000001
		.byte #%10010101, #%00000001
		.byte #%00010101, #%00000010
		.byte #%00010101, #%00000001
		.byte #%00000000, #%00000011
		.byte #%10000000, #%00000011
		.byte #%00010010, #%10000001
		.byte #%10010010, #%10000001
		.byte #%00010010, #%10000010
		.byte #%10010010, #%10000010
		.byte #%00010010, #%10000001
		.byte #%10010010, #%10000001
		.byte #%00000000, #%00000011
		.byte #%00010010, #%10000001
		.byte #%00010010, #%10000010
		.byte #%00010010, #%10000001
;				BVVFFFFF	TTMMMMMM
ChorusBassPerc4
		.byte #%00011010, #%11000001
		.byte #%10011010, #%11000001
		.byte #%00011010, #%11000010
		.byte #%00011010, #%11000001
		.byte #%00000000, #%00000011
		.byte #%10000000, #%00000011
		.byte #%00011100, #%00000001
		.byte #%10011100, #%00000001
		.byte #%00011100, #%00000010
		.byte #%10011100, #%00000010
		.byte #%00011100, #%00000001
		.byte #%10011100, #%00000001
		.byte #%00000000, #%00000011
		.byte #%00011100, #%00000001
		.byte #%00011100, #%00000001
		.byte #%00011100, #%00000010
;				BVVFFFFF	TTMMMMMM
ChorusBassPercNoDrum1
		.byte #%00010101, #%00000001
		.byte #%10010101, #%00000001
		.byte #%00010101, #%00000010
		.byte #%00010101, #%00000001
		.byte #%00000000, #%00001000
		.byte #%10000000, #%00001000
		.byte #%00010010, #%10000001
		.byte #%10010010, #%10000001
		.byte #%00010010, #%10000010
		.byte #%10010010, #%10000010
		.byte #%00010010, #%10000001
		.byte #%10010010, #%10000001
		.byte #%00000000, #%00001000
		.byte #%00010010, #%10000001
		.byte #%00010010, #%10000010
		.byte #%00010010, #%10000001
;				BVVFFFFF	TTMMMMMM
ChorusBassPercNoDrum2
		.byte #%00011010, #%11000001
		.byte #%10011010, #%11000001
		.byte #%00011010, #%11000010
		.byte #%00011010, #%11000001
		.byte #%00000000, #%00001000
		.byte #%10000000, #%00001000
		.byte #%00011100, #%00000001
		.byte #%10011100, #%00000001
		.byte #%00011100, #%00000010
		.byte #%10011100, #%00000010
		.byte #%00011100, #%00000001
		.byte #%10011100, #%00000001
		.byte #%00000000, #%00001000
		.byte #%00011100, #%00000001
		.byte #%00011100, #%00000001
		.byte #%00011100, #%00000010
;				BVVFFFFF	TTMMMMMM
IntroPercussion1
		.byte #%00000000, #%00000111
		.byte #%10000000, #%00000111
		.byte #%00000000, #%00001000
		.byte #%00000000, #%00000111
		.byte #%00000000, #%00001000
		.byte #%10000000, #%00001000
		.byte #%00000000, #%00000111
		.byte #%10000000, #%00000111
		.byte #%00000000, #%00001000
		.byte #%10000000, #%00001000
		.byte #%00000000, #%00000111
		.byte #%10000000, #%00000111
		.byte #%00000000, #%00000011
		.byte #%10000000, #%00000011
		.byte #%00000000, #%00000111
		.byte #%10000000, #%00000111
;				BVVFFFFF	TTMMMMMM
IntroPercussion2
		.byte #%00000000, #%00000111
		.byte #%10000000, #%00000111
		.byte #%00000000, #%00001000
		.byte #%00000000, #%00000111
		.byte #%00000000, #%00001000
		.byte #%10000000, #%00001000
		.byte #%00000000, #%00000111
		.byte #%10000000, #%00000111
		.byte #%00000000, #%00001000
		.byte #%10000000, #%00001000
		.byte #%00000000, #%00000111
		.byte #%10000000, #%00000111
		.byte #%00000000, #%00000011
		.byte #%00000000, #%00000111
		.byte #%00000000, #%00000011
		.byte #%00100000, #%00000011
;				BVVFFFFF	TTMMMMMM
IntroPercussion3
		.byte #%00010101, #%00000001
		.byte #%10010101, #%00000001
		.byte #%00010101, #%00000010
		.byte #%00010101, #%00000001
		.byte #%00010101, #%00000001
		.byte #%10010101, #%00000001
		.byte #%00010101, #%00000010
		.byte #%00010101, #%00000001
		.byte #%00010101, #%00000001
		.byte #%10010101, #%00000001
		.byte #%00010101, #%00000010
		.byte #%00010101, #%00000001
		.byte #%00000000, #%00000011
		.byte #%00000000, #%00000111
		.byte #%00000000, #%00000011
		.byte #%00100000, #%00000011
;				BVVFFFFF	TTMMMMMM
ChorusBloops1
		.byte #%00000111, #%00001001
		.byte #%00000100, #%00001001
		.byte #%00000101, #%00001001
		.byte #%00100111, #%00001001
		.byte #%00011111, #%00010001
		.byte #%00011111, #%00010010
		.byte #%00111111, #%00001010
		.byte #%10111111, #%00001010
		.byte #%00011111, #%00001010
		.byte #%10111111, #%00001010
		.byte #%00011111, #%00001010
		.byte #%00001011, #%00001101
		.byte #%00011111, #%00001010
		.byte #%10011111, #%00001010
		.byte #%00011111, #%00001100
		.byte #%10011111, #%00001100
;				BVVFFFFF	TTMMMMMM
ChorusBloops2
		.byte #%00000111, #%00001001
		.byte #%00000100, #%00001001
		.byte #%00000101, #%00001001
		.byte #%00100111, #%00001001
		.byte #%00011111, #%00010001
		.byte #%00011111, #%00010010
		.byte #%00111111, #%00001010
		.byte #%10111111, #%00001010
		.byte #%00011111, #%00001010
		.byte #%10111111, #%00001010
		.byte #%00011111, #%00001010
		.byte #%00001011, #%00001101
		.byte #%00011111, #%00001010
		.byte #%00000111, #%00001101
		.byte #%00011111, #%00001011
		.byte #%10011111, #%00001011
;				BVVFFFFF	TTMMMMMM
ChorusBloops3
		.byte #%00000011, #%00001001
		.byte #%00000001, #%00001001
		.byte #%00000010, #%00001001
		.byte #%00100011, #%00001001
		.byte #%00011111, #%00010001
		.byte #%00011111, #%00010010
		.byte #%00111111, #%00001010
		.byte #%10111111, #%00001010
		.byte #%00011111, #%00001010
		.byte #%10111111, #%00001010
		.byte #%00011111, #%00001010
		.byte #%00001011, #%00001101
		.byte #%00011111, #%00001010
		.byte #%10011111, #%00001010
		.byte #%00011111, #%00001100
		.byte #%10011111, #%00001100
;				BVVFFFFF	TTMMMMMM
ChorusBloops4
		.byte #%00000011, #%00001001
		.byte #%00000001, #%00001001
		.byte #%00000010, #%00001001
		.byte #%00100011, #%00001001
		.byte #%00011111, #%00010001
		.byte #%00011111, #%00010010
		.byte #%00111111, #%00001010
		.byte #%10111111, #%00001010
		.byte #%00011111, #%00001010
		.byte #%10111111, #%00001010
		.byte #%00011111, #%00001010
		.byte #%00001011, #%00001101
		.byte #%00011111, #%00001010
		.byte #%00000111, #%00001101
		.byte #%00011111, #%00001011
		.byte #%10011111, #%00001011
;				BVVFFFFF	TTMMMMMM
ChorusBloops4Alt
		.byte #%00000011, #%00001001
		.byte #%00000001, #%00001001
		.byte #%00000010, #%00001001
		.byte #%00100011, #%00001001
		.byte #%00011111, #%00010001
		.byte #%00011111, #%00010010
		.byte #%00111111, #%00001010
		.byte #%10111111, #%00001010
		.byte #%00011111, #%00001010
		.byte #%10111111, #%00001010
		.byte #%00011111, #%00001010
		.byte #%00001011, #%00001101
		.byte #%00000000, #%00010100
		.byte #%00000001, #%00010100
		.byte #%00000010, #%00010100
		.byte #%00000011, #%00010100
;				BVVFFFFF	TTMMMMMM
Pads1
		.byte #%00011111, #%00000100
		.byte #%00011111, #%00000100
		.byte #%00011111, #%00000100
		.byte #%00011111, #%00000100
		.byte #%00011111, #%00000100
		.byte #%00011111, #%00000100
		.byte #%00011111, #%00000101
		.byte #%00011111, #%00000101
		.byte #%00011111, #%00000101
		.byte #%00011111, #%00000101
		.byte #%00011111, #%00000101
		.byte #%00011111, #%00000101
		.byte #%00011111, #%00000101
		.byte #%00011111, #%00000101
		.byte #%00011111, #%00000101
		.byte #%00011111, #%00000101
;				BVVFFFFF	TTMMMMMM
Pads2
		.byte #%00011111, #%00000110
		.byte #%00011111, #%00000110
		.byte #%00011111, #%00000110
		.byte #%00011111, #%00000110
		.byte #%00011111, #%00000110
		.byte #%00011111, #%00000110
		.byte #%00011111, #%00000101
		.byte #%00011111, #%00000101
		.byte #%00011111, #%00000101
		.byte #%00011111, #%00000101
		.byte #%00011111, #%00000101
		.byte #%00011111, #%00000101
		.byte #%00011111, #%00000101
		.byte #%00011111, #%00000101
		.byte #%00011111, #%00000101
		.byte #%00011111, #%00000101
;				BVVFFFFF	TTMMMMMM
Gabber1
		.byte #%00011111, #%00000000
		.byte #%10011111, #%00000000
		.byte #%00011111, #%00001111
		.byte #%00011111, #%00000000
		.byte #%10011111, #%00000000
		.byte #%10011111, #%00000000
		.byte #%10011111, #%00000000
		.byte #%10011111, #%00000000
		.byte #%10011111, #%00000000
		.byte #%10011111, #%00000000
		.byte #%00011111, #%00001111
		.byte #%00011111, #%00000000
		.byte #%10011111, #%00000000
		.byte #%10011111, #%00000000
		.byte #%00011111, #%00001110
		.byte #%00011111, #%00000000
;				BVVFFFFF	TTMMMMMM
Gabber2
		.byte #%00011111, #%00010000
		.byte #%10011111, #%00000000
		.byte #%00011111, #%00001111
		.byte #%00011111, #%00000000
		.byte #%10011111, #%00000000
		.byte #%10011111, #%00000000
		.byte #%10011111, #%00000000
		.byte #%10011111, #%00000000
		.byte #%10011111, #%00000000
		.byte #%10011111, #%00000000
		.byte #%00011111, #%00001111
		.byte #%00011111, #%00000000
		.byte #%10011111, #%00000000
		.byte #%10011111, #%00000000
		.byte #%00011111, #%00001110
		.byte #%00011111, #%00000000
;				BVVFFFFF	TTMMMMMM
Gabber3
		.byte #%00011111, #%00010000
		.byte #%10011111, #%00010000
		.byte #%00011111, #%00001111
		.byte #%10011111, #%00001111
		.byte #%00011111, #%00000000
		.byte #%10011111, #%00000000
		.byte #%10011111, #%00000000
		.byte #%10011111, #%00000000
		.byte #%00011111, #%00010000
		.byte #%10011111, #%00010000
		.byte #%00011111, #%00001111
		.byte #%10011111, #%00001111
		.byte #%00011111, #%00000101
		.byte #%10011111, #%00000101
		.byte #%00100000, #%00010100
		.byte #%00100001, #%00010100
;				BVVFFFFF	TTMMMMMM
Gabber4
		.byte #%00011111, #%00010000
		.byte #%10011111, #%00010000
		.byte #%00011111, #%00001111
		.byte #%10011111, #%00001111
		.byte #%00011111, #%00010001
		.byte #%10011111, #%00010001
		.byte #%00011111, #%00010010
		.byte #%10011111, #%00010010
		.byte #%00011111, #%00010000
		.byte #%10011111, #%00010000
		.byte #%00011111, #%00001111
		.byte #%10011111, #%00001111
		.byte #%00011111, #%00010010
		.byte #%10011111, #%00010010
		.byte #%10011111, #%00010010
		.byte #%10011111, #%00010010
;				BVVFFFFF	TTMMMMMM
Gabber5
		.byte #%00011111, #%00010000
		.byte #%10011111, #%00010000
		.byte #%00011111, #%00001111
		.byte #%10011111, #%00001111
		.byte #%00011111, #%00010010
		.byte #%10011111, #%00010010
		.byte #%00011111, #%00010001
		.byte #%10011111, #%00010001
		.byte #%00011111, #%00010001
		.byte #%00011111, #%00010001
		.byte #%00011111, #%00010000
		.byte #%10011111, #%00010000
		.byte #%00011111, #%00010010
		.byte #%00011111, #%00010010
		.byte #%00011111, #%00010000
		.byte #%10011111, #%00010000
;				BVVFFFFF	TTMMMMMM
Gabber6
		.byte #%00011111, #%00000000
		.byte #%10011111, #%00000000
		.byte #%00011111, #%00001110
		.byte #%10011111, #%00001110
		.byte #%00011111, #%00000000
		.byte #%10011111, #%00000000
		.byte #%00011111, #%00001110
		.byte #%10011111, #%00001110
		.byte #%00011111, #%00000000
		.byte #%10011111, #%00000000
		.byte #%00011111, #%00001110
		.byte #%10011111, #%00001110
		.byte #%00011111, #%00000000
		.byte #%10011111, #%00000000
		.byte #%10011111, #%00000000
		.byte #%10011111, #%00000000
;				BVVFFFFF	TTMMMMMM
ChorusBloopsPads1
		.byte #%00011111, #%00000100
		.byte #%00011111, #%00000000
		.byte #%00011111, #%00000100
		.byte #%00011111, #%00000000
		.byte #%00011111, #%00001110
		.byte #%10011111, #%00001110
		.byte #%00111111, #%00001010
		.byte #%10111111, #%00001010
		.byte #%00011111, #%00001010
		.byte #%10111111, #%00001010
		.byte #%00011111, #%00001010
		.byte #%00001011, #%00001101
		.byte #%00011111, #%00001010
		.byte #%10011111, #%00001010
		.byte #%00011111, #%00001100
		.byte #%10011111, #%00001100
;				BVVFFFFF	TTMMMMMM
ChorusBloopsPads2
		.byte #%00011111, #%00000100
		.byte #%00011111, #%00000000
		.byte #%00011111, #%00000100
		.byte #%00011111, #%00000000
		.byte #%00011111, #%00001110
		.byte #%10011111, #%00001110
		.byte #%00111111, #%00001010
		.byte #%10111111, #%00001010
		.byte #%00011111, #%00001010
		.byte #%10111111, #%00001010
		.byte #%00011111, #%00001010
		.byte #%00001011, #%00001101
		.byte #%00011111, #%00001010
		.byte #%00000111, #%00001101
		.byte #%00011111, #%00000101
		.byte #%00011111, #%00000101
;				BVVFFFFF	TTMMMMMM
ChorusBloopsPads2Alt
		.byte #%00011111, #%00000100
		.byte #%00011111, #%00000000
		.byte #%00011111, #%00000100
		.byte #%00011111, #%00000000
		.byte #%00011111, #%00001110
		.byte #%10011111, #%00001110
		.byte #%00111111, #%00001010
		.byte #%10111111, #%00001010
		.byte #%00011111, #%00001010
		.byte #%10111111, #%00001010
		.byte #%00011111, #%00001010
		.byte #%00001011, #%00001101
		.byte #%00000000, #%00010100
		.byte #%00000001, #%00010100
		.byte #%00000010, #%00010100
		.byte #%00000011, #%00010100
;				BVVFFFFF	TTMMMMMM




MacroTable
		.word MAC0
		.word MAC1	;Kick + Bass
		.word MAC2	;Hat + Bass Continue
		.word MAC3	;Snare
		.word MAC4	;Chord 1 Sample
		.word MAC5	;Chord 2 Sample
		.word MAC6	;Chord 3 Sample
		.word MAC7	;Kick
		.word MAC8	;Hat
		.word MAC9	;Flangy Hit
		.word MAC10	;Square B Hit
		.word MAC11	;Square C# Hit
		.word MAC12	;Square F# Hit
		.word MAC13	;Square Pure
		.word MAC14	;Get Sample
		.word MAC15	;Hah Sample
		.word MAC16	;Say Sample
		.word MAC17	;Get Up 1 Sample
		.word MAC18	;Get Up 2 Sample
		.word MAC20	;TEMP
		.word MAC20	;Engine Pure

		
		
		
MacroData
;				ELRFFFFF	VVVVWWWW
MAC0	.byte #%10111111, #%00000001
		
;Kick + Bass
;				ELRFFFFF	VVVVWWWW
MAC1	.byte #%00000000, #%01111000
		.byte #%00010000, #%11111111
		.byte #%00010101, #%11111111
		.byte #%00110000, #%11110110
		.byte #%00110000, #%11010110
		.byte #%00110000, #%10110110
		.byte #%00110000, #%10100110
		.byte #%00110000, #%10010110
		.byte #%00110000, #%10010110
		.byte #%10110000, #%10000110

;Hat + Bass Continue
;				ELRFFFFF	VVVVWWWW
MAC2	.byte #%00000000, #%01111000
		.byte #%10110000, #%01100110

;Snare
;				ELRFFFFF	VVVVWWWW
MAC3	.byte #%00000101, #%11111111
		.byte #%00000111, #%11111111
		.byte #%00000001, #%11011000
		.byte #%00000011, #%11011000
		.byte #%10111111, #%00000001

;Chord 1 Sample
;				ELRFFFFF	VVVVWWWW
MAC4	.byte #%00000001, #%00010000
		.byte #%10000000, #%00010000

;Chord 2 Sample
;				ELRFFFFF	VVVVWWWW
MAC5	.byte #%00000001, #%00100000
		.byte #%10000000, #%00100000

;Chord 3 Sample
;				ELRFFFFF	VVVVWWWW
MAC6	.byte #%00000001, #%00110000
		.byte #%10000000, #%00110000

;Kick
;				ELRFFFFF	VVVVWWWW
MAC7	.byte #%00000000, #%01111000
		.byte #%00010000, #%11111111
		.byte #%00010101, #%11111111
		.byte #%10111111, #%00000001

;Hat
;				ELRFFFFF	VVVVWWWW
MAC8	.byte #%00000000, #%01111000
		.byte #%10111111, #%00000001

;Flangy Hit
;				ELRFFFFF	VVVVWWWW
MAC9	.byte #%00110000, #%10000011
		.byte #%00110000, #%01100011
		.byte #%10110000, #%01000011

;Square B Hit
;				ELRFFFFF	VVVVWWWW
MAC10	.byte #%00011011, #%10000100
		.byte #%00011101, #%01100100
		.byte #%00011111, #%01000100
		.byte #%00001111, #%01000100	;Loop point
		.byte #%00011111, #%01000100
		.byte #%00011111, #%01000100
		.byte #%11011111, #%01000100, #8

;Square C# Hit
;				ELRFFFFF	VVVVWWWW
MAC11	.byte #%00010111, #%10000100
		.byte #%00011001, #%01100100
		.byte #%00011011, #%01000100
		.byte #%00001101, #%01000100	;Loop point
		.byte #%00011011, #%01000100
		.byte #%00011011, #%01000100
		.byte #%11011011, #%01000100, #8

;Square F# Hit
;				ELRFFFFF	VVVVWWWW
MAC12	.byte #%00001011, #%10001100
		.byte #%00001100, #%01101100
		.byte #%00001101, #%01001100
		.byte #%00000110, #%01001100	;Loop point
		.byte #%00001101, #%01001100
		.byte #%00001101, #%01001100
		.byte #%11001101, #%01001100, #8

;Square Pure
MAC13	.byte #%10110000, #%01001100

;Get Sample
;				ELRFFFFF	VVVVWWWW
MAC14	.byte #%00000001, #%01000000
		.byte #%10000000, #%01000000

;Hah Sample
;				ELRFFFFF	VVVVWWWW
MAC15	.byte #%00000001, #%01010000
		.byte #%10000000, #%01010000

;Say Sample
;				ELRFFFFF	VVVVWWWW
MAC16	.byte #%00000001, #%01100000
		.byte #%10000000, #%01100000

;Get Up 1 Sample
;				ELRFFFFF	VVVVWWWW
MAC17	.byte #%00000001, #%01110000
		.byte #%10000000, #%01110000

;Get Up 2 Sample
;				ELRFFFFF	VVVVWWWW
MAC18	.byte #%00000001, #%10000000
		.byte #%10000000, #%10000000

;Engine Pure
MAC20	.byte #%10110000, #%01000010

		
		
		
SampleTable
		.word SAMP0S, SAMP0E
		.word SAMP1S, SAMP1E
		.word SAMP2S, SAMP2E
		.word SAMP3S, SAMP3E
		.word SAMP4S, SAMP4E
		.word SAMP5S, SAMP5E
		.word SAMP6S, SAMP6E
		.word SAMP7S, SAMP7E
		.word SAMP8S, SAMP8E
		
		

		
SampleData
SAMP0S	.byte $00
SAMP0E
		.byte $00, $00, $00, $00

		;Chord 1
SAMP1S	.byte $18, $40, $30, $00, $60, $FF, $00, $00, $B0, $3F, $00, $82, $01, $00, $1F, $0E
		.byte $80, $30, $20, $FC, $00, $00, $FB, $00, $00, $F8, $07, $00, $80, $70, $1C, $0F
		.byte $06, $00, $F0, $71, $C8, $00, $00, $F3, $07, $00, $78, $0E, $00, $EC, $03, $08
		.byte $06, $C6, $01, $E3, $70, $80, $00, $80, $EF, $01, $00, $A0, $7F, $00, $E0, $00
		.byte $00, $7E, $07, $00, $40, $30, $8F, $03, $00, $C7, $03, $04, $F4, $1D, $00, $E0
		.byte $1C, $30, $1C, $0F, $00, $D8, $3B, $00, $03, $00, $FA, $07, $00, $40, $3D, $30
		.byte $F0, $03, $00, $90, $77, $00, $80, $3D, $00, $40, $C1, $8E, $07, $00, $C0, $7F
		.byte $00, $E0, $00, $80, $F9, $07, $00, $00, $BD, $03, $0E, $00, $0C, $07, $03, $8A
		.byte $73, $00, $C0, $0F, $00, $E0, $07, $00, $50, $7F, $00, $00, $00, $7C, $1E, $00
SAMP1E
		.byte $00, $00, $00, $00

		;Chord 2
SAMP2S	.byte $00, $00, $F4, $00, $0F, $0C, $78, $00, $38, $0F, $06, $E8, $00, $00, $FE, $00
		.byte $00, $F8, $03, $0C, $00, $C0, $0F, $18, $18, $00, $F0, $07, $00, $F8, $01, $02
		.byte $B9, $03, $00, $F0, $C0, $07, $10, $00, $76, $78, $0E, $00, $F0, $0F, $00, $B0
		.byte $0E, $C6, $71, $00, $2C, $E0, $81, $81, $60, $18, $14, $E0, $0F, $00, $E0, $1F
		.byte $00, $61, $00, $FC, $E0, $00, $00, $E0, $67, $00, $80, $1F, $0C, $C0, $63, $00
		.byte $C0, $07, $D0, $C1, $00, $38, $FC, $80, $01, $80, $FF, $00, $80, $81, $39, $F0
		.byte $03, $00, $C3, $0F, $20, $D0, $03, $7A, $00, $FC, $01, $00, $5F, $07, $80, $03
		.byte $80, $9F, $03, $00, $80, $1F, $1C, $00, $D0, $73, $00, $38, $0C, $00, $3F, $00
		.byte $B0, $07, $00, $83, $1F, $20, $00, $EE, $0F, $00, $60, $F0, $01, $3E, $00, $40
SAMP2E
		.byte $00, $00, $00, $00

		;Chord 3
SAMP3S	.byte $03, $08, $00, $82, $0F, $00, $78, $01, $D8, $83, $01, $38, $30, $04, $1F, $00
		.byte $E0, $1F, $00, $80, $07, $73, $60, $10, $06, $38, $C0, $03, $00, $F0, $0C, $D6
		.byte $01, $80, $7F, $00, $80, $1F, $06, $E0, $19, $00, $F8, $C0, $30, $10, $00, $1F
		.byte $1C, $00, $00, $1F, $07, $00, $F3, $0E, $00, $7E, $00, $E0, $CB, $00, $30, $00
		.byte $D6, $1D, $00, $30, $3F, $00, $00, $07, $78, $00, $9C, $03, $C0, $F0, $03, $00
		.byte $82, $0F, $F0, $00, $00, $FE, $00, $80, $0F, $08, $04, $1F, $00, $C0, $F1, $1C
		.byte $00, $80, $1F, $30, $08, $00, $FC, $03, $00, $7C, $18, $00, $7F, $00, $C0, $77
		.byte $00, $82, $00, $F7, $60, $00, $18, $7C, $60, $00, $08, $BC, $00, $DE, $01, $00
		.byte $FE, $01, $80, $05, $0F, $F8, $00, $00, $FD, $00, $E0, $38, $00, $07, $3E, $30
SAMP3E
		.byte $00, $00, $00, $00

		;Get
SAMP4S	.byte $00, $00, $F0, $13, $00, $00, $D4, $5A, $0C, $41, $DA, $29, $03, $DC, $13, $C0
		.byte $77, $00, $7E, $03, $E0, $3F, $00, $FF, $01, $FC, $07, $F0, $2F, $C0, $3F, $80
		.byte $7F, $80, $BF, $80, $7F, $A0, $0F, $93, $C0, $37, $F0, $0B, $7E, $70, $83, $BE
		.byte $64, $C3, $05, $FB, $28, $07, $26, $78, $98, $0C, $4E, $3A, $9C, $F2, $E0, $E0
		.byte $C1, $83, $C3, $E5, $E2, $83, $07, $0F, $1E, $3C, $78, $78, $F0, $78, $F0, $F0
		.byte $E0, $E1, $83, $07, $0F, $06, $0F, $1E, $0E, $3E, $3C, $78, $F8, $F0, $F0, $E0
		.byte $E1, $C3, $C3, $83, $07, $0F, $0E, $1E, $1C, $3C, $78, $3C, $78, $F0, $E0, $E0
		.byte $C1, $C3, $83, $87, $07, $0F, $0F, $1E, $3E, $78, $78, $F1, $91, $C9, $85, $07
		.byte $0F, $1E, $1C, $9E, $1C, $3C, $78, $F8, $C0, $F8, $81, $83, $47, $8C, $C6, $98
		.byte $33, $26, $8A, $19, $06, $E3, $0C, $4A, $07
SAMP4E
		.byte $00, $00, $00, $00

		;Hah
SAMP5S	.byte $00, $0C, $10, $C7, $70, $82, $98, $63, $C6, $1C, $93, $23, $3D, $64, $00, $8E
		.byte $B0, $11, $4D, $63, $86, $1C, $71, $12, $C6, $E1, $70, $8C, $71, $30, $83, $03
		.byte $1A, $38, $31, $C7, $1C, $84, $0E, $79, $E0, $28, $61, $85, $63, $18, $86, $33
		.byte $9C, $01, $0F, $80, $03, $63, $86, $13, $C6, $C1, $1E, $C6, $C1, $1C, $C3, $D8
		.byte $18, $07, $8C, $31, $0F, $33, $C4, $31, $3C, $AC, $F0, $30, $B7, $01, $83, $F9
		.byte $0E, $72, $E0, $7F, $B0, $C1, $F9, $30, $E0, $78, $38, $E2, $3C, $CC, $78, $87
		.byte $03, $EF, $71, $C0, $79, $1A, $F0, $8E, $07, $8C, $47, $93, $78, $C3, $80, $71
		.byte $3C, $38, $4E, $3C, $09, $1E, $0E, $0D, $8F, $07, $87, $1D, $47, $86, $1B, $87
		.byte $C7, $83, $C7, $C1, $71, $1C, $19, $6E, $1C, $1E, $8E, $38, $0E, $1E, $87, $63
		.byte $E3, $99, $C7, $81, $C3, $28, $23, $1C, $8E, $C3, $C3, $E1, $3C, $1C, $1C, $8B
		.byte $C7, $61, $8C, $C3, $71, $18, $86, $71, $1C, $0E, $87, $C3, $61, $86, $83, $E3
		.byte $38, $1C, $0E, $C7, $E1, $70, $34, $76, $9E, $C3, $E1, $70, $1C, $1C, $87, $61
		.byte $F8, $38, $07, $C7, $71, $1C, $C6, $71, $38, $A6, $1C, $31, $46, $C6, $71, $1C
		.byte $C3, $18, $03
SAMP5E
		.byte $00, $00, $00, $00

		;Say
SAMP6S	.byte $00, $55, $01, $80, $3F, $00, $80, $7F, $00, $60, $02, $FC, $01, $F0, $01, $FC
		.byte $03, $F8, $01, $FC, $03, $F8, $01, $FC, $03, $FA, $01, $FC, $01, $FE, $00, $FE
		.byte $01, $7E, $00, $FF, $01, $7E, $00, $FF, $01, $3E, $00, $FF, $01, $3E, $00, $FF
		.byte $01, $7E, $00, $FF, $03, $7C, $00, $FE, $03, $7C, $00, $BD, $17, $F8, $00, $DC
		.byte $27, $F0, $01, $9C, $4F, $E0, $03, $70, $1F, $E0, $07, $70, $3E, $C1, $0F, $E0
		.byte $79, $80, $1F, $C0, $F5, $02, $3F, $80, $EB, $05, $3E, $00, $CF, $0B, $FC, $00
		.byte $9E, $17, $F8, $01, $7C, $0F, $F0, $03, $F0, $1F, $C0, $0F, $E0, $3D, $80, $1F
		.byte $80, $7F, $02, $3E, $00, $FE, $05, $FC, $00, $FC, $13, $F0, $03, $D0, $2F, $C0
		.byte $1F, $00, $FF, $00, $FE, $00, $F8, $07, $E0, $0F, $C0, $3F, $00, $FD, $00, $FC
		.byte $07, $40, $0F, $70, $2F, $00, $90, $F0, $4B, $00, $34, $A0, $3F, $00, $40, $C1
		.byte $7B, $01, $00, $50, $F7, $02, $00, $00, $5E, $2B, $00, $00, $54, $3F, $01, $00
		.byte $00, $E8, $27, $00, $00, $00, $BF, $02
SAMP6E
		.byte $00, $00, $00, $00

		;Get Up 1
SAMP7S	.byte $80, $AA, $AA, $04, $00, $00, $50, $AB, $AA, $14, $00, $01, $68, $AB, $34, $0C
		.byte $00, $00, $7B, $46, $04, $00, $50, $FE, $04, $00, $80, $BC, $0D, $21, $21, $90
		.byte $B6, $25, $22, $05, $DB, $1A, $24, $36, $B0, $BF, $00, $6A, $01, $79, $03, $E4
		.byte $04, $D8, $1B, $E0, $0F, $C0, $9E, $90, $1F, $00, $7B, $42, $2F, $00, $EC, $08
		.byte $3F, $80, $9C, $13, $DE, $80, $3B, $46, $3C, $21, $67, $CE, $70, $03, $1E, $33
		.byte $C7, $18, $11, $73, $82, $13, $F0, $9C, $31, $C6, $8C, $30, $27, $BC, $01, $D8
		.byte $01, $7F, $80, $37, $E6, $1C, $70, $43, $CE, $30, $23, $60, $27, $F0, $03, $FC
		.byte $23, $F2, $08, $18, $66, $86, $19, $01, $CC, $19, $B0, $03, $C0, $9C, $11, $27
		.byte $00, $F2, $0D, $88, $CC, $24, $02, $D4, $4C, $53, $02, $00, $40, $AD, $4A, $15
		.byte $00, $00
SAMP7E
		.byte $00, $00, $00, $00

		;Get Up 2
SAMP8S	.byte $80, $8C, $32, $01, $89, $4C, $66, $93, $01, $88, $82, $3B, $07, $00, $80, $4B
		.byte $72, $B1, $03, $00, $E0, $0D, $CE, $01, $00, $71, $02, $1F, $83, $00, $E0, $01
		.byte $3C, $A4, $03, $E0, $43, $7C, $30, $1E, $1C, $0E, $86, $87, $23, $20, $1F, $7C
		.byte $00, $07, $80, $07, $BE, $81, $07, $F0, $81, $0F, $E0, $01, $3F, $F8, $07, $0C
		.byte $7C, $F0, $19, $78, $80, $0F, $F8, $01, $03, $1F, $BC, $07, $0C, $7C, $E0, $19
		.byte $F8, $C0, $07, $FE, $C0, $C1, $07, $3F, $01, $83, $0F, $7C, $00, $0F, $7C, $E0
		.byte $07, $38, $F8, $C1, $1F, $60, $F0, $81, $0F, $C0, $E1, $07, $1F, $C0, $07, $3E
		.byte $F0, $03, $0E, $7C, $F0, $11, $18, $F8, $C0, $07, $F0, $81, $0F, $FC, $80, $07
		.byte $7E, $E0, $07, $0C, $7E, $F0, $01, $10, $FC, $C0, $07, $F8, $C0, $0F, $FC, $80
		.byte $83, $0F, $FC, $00, $80, $1F, $F0, $07, $C0, $1F, $E0, $03, $00, $3C, $92, $16
		.byte $00
SAMP8E
		.byte $00, $00, $00, $00


		

z_EndOfData
		
		
		
		;*************************************************************************
		;*																		 *
		;*							 DemenTIA Data End!							 *
		;*																		 *
		;*************************************************************************
		
		
			
		ORG $FFFA
		.word reset			;NMI
		.word reset			;RESET
		.word reset			;IRQ

		END
