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
		.byte #7
PatLength
		.byte #31
c1SongData
		.byte #0
		.byte #0
		.byte #0
		.byte #0

		.byte #5
		.byte #6
		.byte #5
		.byte #6
		.byte #3
		.byte #4
		.byte #3
		.byte #4

		.byte #1
		.byte #2
		.byte #1
		.byte #2
		.byte #10
		.byte #11
		.byte #10
		.byte #11

		.byte #7
		.byte #8
		.byte #9
		.byte #8
		.byte #3
		.byte #4
		.byte #3
		.byte #4

		.byte #1
		.byte #2
		.byte #1
		.byte #2
		.byte #10
		.byte #11
		.byte #10
		.byte #11

		.byte #12
		.byte #13
		.byte #12
		.byte #13
		.byte #12
		.byte #13
		.byte #12
		.byte #13

		.byte #255
c2SongData
		.byte #10
		.byte #11
		.byte #10
		.byte #12

		.byte #5
		.byte #6
		.byte #5
		.byte #6
		.byte #7
		.byte #6
		.byte #5
		.byte #8

		.byte #1
		.byte #2
		.byte #1
		.byte #2
		.byte #1
		.byte #2
		.byte #3
		.byte #4

		.byte #9
		.byte #6
		.byte #9
		.byte #6
		.byte #7
		.byte #6
		.byte #5
		.byte #8

		.byte #1
		.byte #2
		.byte #1
		.byte #2
		.byte #1
		.byte #2
		.byte #3
		.byte #4

		.byte #1
		.byte #2
		.byte #1
		.byte #2
		.byte #1
		.byte #2
		.byte #13
		.byte #4

		.byte #255

c1PatternTable
		.word BLANK
		.word C1P1
		.word C1P2
		.word C1P3
		.word C1P4
		.word C1P5
		.word C1P6
		.word C1P7
		.word C1P8
		.word C1P9
		.word C1P10
		.word C1P11
		.word C1P12
		.word C1P13

c2PatternTable
		.word BLANK
		.word C2P1
		.word C2P2
		.word C2P3
		.word C2P4
		.word C2P5
		.word C2P6
		.word C2P7
		.word C2P8
		.word C2P9
		.word C2P10
		.word C2P11
		.word C2P12
		.word C2P13

c1PatternData

;Once the song is done, delete this but keep the label
BLANK	.byte #%00000000, #%00000000
		.byte #%10000000, #%00000000
		.byte #%10000000, #%00000000
		.byte #%10000000, #%00000000

		.byte #%10000000, #%00000000
		.byte #%10000000, #%00000000
		.byte #%10000000, #%00000000
		.byte #%10000000, #%00000000

		.byte #%10000000, #%00000000
		.byte #%10000000, #%00000000
		.byte #%10000000, #%00000000
		.byte #%10000000, #%00000000

		.byte #%10000000, #%00000000
		.byte #%10000000, #%00000000
		.byte #%10000000, #%00000000
		.byte #%10000000, #%00000000


;		.byte #%00010111, #%00001001	A, C#
;		.byte #%00010100, #%00001010	B, D#
;		.byte #%00010010, #%00001011	C#, E

;				BVVFFFFF	TTMMMMMM
C1P1	.byte #%00010111, #%00001001
		.byte #%10010111, #%00001001
		.byte #%10010111, #%00001001
		.byte #%00110111, #%00001001

		.byte #%10110111, #%00001001
		.byte #%01010111, #%00001001
		.byte #%00110111, #%00001001
		.byte #%00010111, #%00001001

		.byte #%10010111, #%00001001
		.byte #%01010111, #%00001001
		.byte #%00010111, #%00001001
		.byte #%10010111, #%00001001

		.byte #%01010111, #%00001001
		.byte #%00010111, #%00001001
		.byte #%01110111, #%00001001
		.byte #%01010111, #%00001001
;				BVVFFFFF	TTMMMMMM
C1P2	.byte #%00010100, #%00001010
		.byte #%10010100, #%00001010
		.byte #%10010100, #%00001010
		.byte #%00110100, #%00001010

		.byte #%10110100, #%00001010
		.byte #%01010111, #%00001001
		.byte #%00110100, #%00001010
		.byte #%00010100, #%00001010

		.byte #%10010100, #%00001010
		.byte #%01010100, #%00001010
		.byte #%00010010, #%00001011
		.byte #%10010010, #%00001011

		.byte #%00010010, #%00001011
		.byte #%00000000, #%00000000
		.byte #%00010100, #%00001010
		.byte #%01010010, #%00001011
;				BVVFFFFF	TTMMMMMM



;				BVVFFFFF	TTMMMMMM
C1P3	.byte #%00010010, #%00001011
		.byte #%10010010, #%00001011
		.byte #%10010010, #%00001011
		.byte #%00110010, #%00001011

		.byte #%10110010, #%00001011
		.byte #%01010010, #%00001011
		.byte #%00110010, #%00001011
		.byte #%00010100, #%00001001

		.byte #%10010100, #%00001001
		.byte #%01010010, #%00001011
		.byte #%00010100, #%00001001
		.byte #%10010100, #%00001001

		.byte #%01010100, #%00001001
		.byte #%00010100, #%00001001
		.byte #%01110010, #%00001011
		.byte #%01010100, #%00001001
;				BVVFFFFF	TTMMMMMM
C1P4	.byte #%00010111, #%00001001
		.byte #%10010111, #%00001001
		.byte #%10010111, #%00001001
		.byte #%00110111, #%00001001

		.byte #%10110111, #%00001001
		.byte #%01010111, #%00001001
		.byte #%00110111, #%00001001
		.byte #%00010111, #%00001001

		.byte #%10010111, #%00001001
		.byte #%01010111, #%00001001
		.byte #%00010111, #%00001001
		.byte #%10010111, #%00001001

		.byte #%00010100, #%00001010
		.byte #%00000000, #%00000000
		.byte #%00010111, #%00001001
		.byte #%01010100, #%00001010
;				BVVFFFFF	TTMMMMMM


;				BVVFFFFF	TTMMMMMM
C1P5	.byte #%00010011, #%00001000	;4
		.byte #%00000000, #%00000000
		.byte #%00001110, #%00001000	;	4
		.byte #%00110011, #%00001000	;3

		.byte #%00001001, #%00001000	;		4
		.byte #%00101110, #%00001000	;	3
		.byte #%01010011, #%00001000	;2
		.byte #%00101001, #%00001000	;		3

		.byte #%01001110, #%00001000	;	2
		.byte #%01110011, #%00001000	;1
		.byte #%01001001, #%00001000	;		2
		.byte #%01101110, #%00001000	;	1

		.byte #%00000000, #%00000000
		.byte #%01101001, #%00001000	;		1
		.byte #%00000000, #%00000000
		.byte #%10000000, #%00000000
;				BVVFFFFF	TTMMMMMM
C1P6	.byte #%00010011, #%00001000	;4
		.byte #%00001110, #%00001000	;	4
		.byte #%00000000, #%00000000
		.byte #%00001001, #%00001000	;		4

		.byte #%00101110, #%00001000	;	3
		.byte #%00000000, #%00000000
		.byte #%00101001, #%00001000	;		3
		.byte #%01001110, #%00001000	;	2

		.byte #%00000000, #%00000000
		.byte #%01001001, #%00001000	;		2
		.byte #%01101110, #%00001000	;	1
		.byte #%00000000, #%00000000

		.byte #%01101001, #%00001000	;		1
		.byte #%00000000, #%00000000
		.byte #%10000000, #%00000000
		.byte #%00100000, #%00000100
;				BVVFFFFF	TTMMMMMM


;				BVVFFFFF	TTMMMMMM
C1P7	.byte #%00010010, #%00001110
		.byte #%10010010, #%00001110
		.byte #%00001111, #%00001110
		.byte #%01010010, #%00001110

		.byte #%00001111, #%00001110
		.byte #%01001111, #%00001110
		.byte #%00010010, #%00001110
		.byte #%00010111, #%00001110

		.byte #%00000000, #%00000000
		.byte #%01010010, #%00001110
		.byte #%01010111, #%00001110
		.byte #%00000000, #%00000000

		.byte #%01110010, #%00001110
		.byte #%01110111, #%00001110
		.byte #%00000000, #%00000000
		.byte #%10000000, #%00000000
;				BVVFFFFF	TTMMMMMM
C1P8	.byte #%00010111, #%00001110
		.byte #%10010111, #%00001110
		.byte #%00010010, #%00001110
		.byte #%01010111, #%00001110

		.byte #%00010010, #%00001110
		.byte #%01010010, #%00001110
		.byte #%00010111, #%00001110
		.byte #%00011011, #%00001110

		.byte #%10011011, #%00001110
		.byte #%00011000, #%00001101
		.byte #%00011111, #%00001110
		.byte #%00000000, #%00000000

		.byte #%01011000, #%00001101
		.byte #%01011111, #%00001110
		.byte #%00000000, #%00000000
		.byte #%01111000, #%00001101
;				BVVFFFFF	TTMMMMMM
C1P9	.byte #%00010010, #%00001110	;Alternate intro
		.byte #%10010010, #%00001110
		.byte #%00001111, #%00001110
		.byte #%01010010, #%00001110

		.byte #%00001111, #%00001110
		.byte #%00001101, #%00001110
		.byte #%01001111, #%00001110
		.byte #%00010010, #%00001110

		.byte #%01001101, #%00001110
		.byte #%01101111, #%00001110
		.byte #%01010010, #%00001110
		.byte #%01101101, #%00001110

		.byte #%00000000, #%00000000
		.byte #%01110010, #%00001110
		.byte #%00000000, #%00000000
		.byte #%10000000, #%00000000
;				BVVFFFFF	TTMMMMMM



;				BVVFFFFF	TTMMMMMM
C1P10	.byte #%00000000, #%00001111
		.byte #%10000000, #%00001111
		.byte #%01000000, #%00001111
		.byte #%11000000, #%00001111

		.byte #%00010111, #%00001001
		.byte #%01010111, #%00001001
		.byte #%00110111, #%00001001
		.byte #%00010111, #%00001001

		.byte #%10010111, #%00001001
		.byte #%01010111, #%00001001
		.byte #%00010111, #%00001001
		.byte #%10010111, #%00001001

		.byte #%01010111, #%00001001
		.byte #%00010111, #%00001001
		.byte #%01110111, #%00001001
		.byte #%01010111, #%00001001
;				BVVFFFFF	TTMMMMMM
C1P11	.byte #%00000000, #%00010000
		.byte #%10000000, #%00010000
		.byte #%01000000, #%00010000
		.byte #%11000000, #%00010000

		.byte #%00010100, #%00001010
		.byte #%01010111, #%00001001
		.byte #%00110100, #%00001010
		.byte #%00010100, #%00001010

		.byte #%10010100, #%00001010
		.byte #%01010100, #%00001010
		.byte #%00010010, #%00001011
		.byte #%10010010, #%00001011

		.byte #%00010010, #%00001011
		.byte #%00000000, #%00000000
		.byte #%00010100, #%00001010
		.byte #%01010010, #%00001011
;				BVVFFFFF	TTMMMMMM


;				BVVFFFFF	TTMMMMMM
C1P12	.byte #%00000000, #%00000000
		.byte #%10000000, #%00000000
		.byte #%10000000, #%00000000
		.byte #%10000000, #%00000000

		.byte #%00110001, #%01011000
		.byte #%10110001, #%01011000
		.byte #%01110001, #%01000001
		.byte #%11110001, #%01000001

		.byte #%00000000, #%00000000
		.byte #%10000000, #%00000000
		.byte #%10000000, #%00000000
		.byte #%10000000, #%00000000

		.byte #%00110101, #%00011000
		.byte #%10110101, #%00011000
		.byte #%01110101, #%00011000
		.byte #%11110101, #%00011000
;				BVVFFFFF	TTMMMMMM
C1P13	.byte #%00000000, #%00000000
		.byte #%10000000, #%00000000
		.byte #%10000000, #%00000000
		.byte #%10000000, #%00000000

		.byte #%00110010, #%01011000
		.byte #%10110010, #%01011000
		.byte #%01110010, #%01011000
		.byte #%11110010, #%01011000

		.byte #%00000000, #%00000000
		.byte #%10000000, #%00000000
		.byte #%10000000, #%00000000
		.byte #%10000000, #%00000000

		.byte #%00101101, #%10011000
		.byte #%10101101, #%10011000
		.byte #%01101101, #%10011000
		.byte #%11101101, #%10011000
;				BVVFFFFF	TTMMMMMM



c2PatternData

;A, F#, G#, C#

;				BVVFFFFF	TTMMMMMM
C2P1	.byte #%00010001, #%01000001	;B	K		A
		.byte #%10010001, #%01000001
		.byte #%00010001, #%01000111	;	H
		.byte #%00000000, #%00000011	;	K

		.byte #%00000000, #%00000100	;	S
		.byte #%10000000, #%00000100
		.byte #%00010001, #%01000110	;B	H
		.byte #%00010101, #%00000001	;B	K		F#

		.byte #%00010101, #%00000111	;	H
		.byte #%00010101, #%00000110	;B	H
		.byte #%00001010, #%00000001	;B	K
		.byte #%00010101, #%00000010	;B

		.byte #%00000000, #%00000100	;	S
		.byte #%10000000, #%00000100
		.byte #%00010101, #%00000110	;B	H
		.byte #%10010101, #%00000110
;				BVVFFFFF	TTMMMMMM
C2P2	.byte #%00010010, #%01000001	;B	K		G#
		.byte #%10010010, #%01000001
		.byte #%00010010, #%01000111	;	H
		.byte #%00000000, #%00000011	;	K

		.byte #%00000000, #%00000100	;	S
		.byte #%10000000, #%00000100
		.byte #%00010010, #%01000110	;B	H		C#
		.byte #%00000000, #%00000011	;	K

		.byte #%00001101, #%10000110	;B	H
		.byte #%00001101, #%10000111	;	H
		.byte #%00001101, #%10000001	;B	K
		.byte #%00001101, #%10000111	;	H

		.byte #%00000000, #%00000100	;	S
		.byte #%00001101, #%10000010	;B
		.byte #%00001101, #%10000001	;B	K
		.byte #%10001101, #%10000001
;				BVVFFFFF	TTMMMMMM
C2P3	.byte #%00000000, #%00000011	;	K		A	Scratch intro
		.byte #%00101001, #%00010010
		.byte #%00101000, #%00010011	;
		.byte #%00100111, #%00010100	;

		.byte #%00000000, #%00000100	;	S
		.byte #%10000000, #%00000100
		.byte #%00010001, #%01000110	;B	H
		.byte #%00010101, #%00000001	;B	K		F#

		.byte #%00010101, #%00000111	;	H
		.byte #%00010101, #%00000110	;B	H
		.byte #%00001010, #%00000001	;B	K
		.byte #%00010101, #%00000010	;B

		.byte #%00000000, #%00000100	;	S
		.byte #%10000000, #%00000100
		.byte #%00010101, #%00000110	;B	H
		.byte #%10010101, #%00000110
;				BVVFFFFF	TTMMMMMM
C2P4	.byte #%00010010, #%01000001	;B	K		G#	Snare end
		.byte #%10010010, #%01000001
		.byte #%00010010, #%01000111	;	H
		.byte #%00000000, #%00000011	;	K

		.byte #%00000000, #%00000100	;	S
		.byte #%10000000, #%00000100
		.byte #%00010010, #%01000110	;B	H		C#
		.byte #%00000000, #%00000011	;	K

		.byte #%00001101, #%10000110	;B	H
		.byte #%00001101, #%10000111	;	H
		.byte #%00001101, #%10000001	;B	K
		.byte #%00001101, #%10000111	;	H

		.byte #%00000000, #%00000100	;	S
		.byte #%00001101, #%10000010	;B
		.byte #%00000000, #%00000100	;	S
		.byte #%00100000, #%00000100	;	S
;				BVVFFFFF	TTMMMMMM


;C#, E, A

;				BVVFFFFF	TTMMMMMM
C2P5	.byte #%00001101, #%10000001	;B	K		C#
		.byte #%10001101, #%10000001
		.byte #%00001101, #%10000111	;	H
		.byte #%00000000, #%00000011	;	K

		.byte #%00000000, #%00000100	;	S
		.byte #%10000000, #%00000100
		.byte #%00011001, #%00000110	;B	H		D#
		.byte #%00011000, #%00000001	;B	K		E

		.byte #%00011000, #%00000111	;	H
		.byte #%00011000, #%00000110	;B	H
		.byte #%00000000, #%00000011	;	K
		.byte #%00011000, #%00000010	;B

		.byte #%00000000, #%00000100	;	S
		.byte #%01101111, #%00001100
		.byte #%00011000, #%00000110	;B	H
		.byte #%01100111, #%00001100
;				BVVFFFFF	TTMMMMMM
C2P6	.byte #%00010001, #%01000001	;B	K		G#
		.byte #%10010001, #%01000001
		.byte #%00010001, #%01000111	;	H
		.byte #%00000000, #%00000011	;	K

		.byte #%00000000, #%00000100	;	S
		.byte #%10000000, #%00000100
		.byte #%00010001, #%01000110	;B	H		C#
		.byte #%00000000, #%00000011	;	K

		.byte #%00010001, #%01000110	;B	H
		.byte #%00010001, #%01000111	;	H
		.byte #%00010001, #%01000001	;B	K
		.byte #%00010001, #%01000111	;	H

		.byte #%00000000, #%00000100	;	S
		.byte #%00010001, #%01000010	;B
		.byte #%00010001, #%01000001	;B	K
		.byte #%10010001, #%01000001
;				BVVFFFFF	TTMMMMMM
C2P7	.byte #%00001101, #%10000001	;B	K		C#		Alternate fill
		.byte #%10001101, #%10000001
		.byte #%00001101, #%10000111	;	H
		.byte #%00000000, #%00000011	;	K

		.byte #%01111111, #%00010001	;	S
		.byte #%01010111, #%00010001
		.byte #%00110000, #%00010001
		.byte #%00011000, #%00000001	;B	K		E

		.byte #%00011000, #%00000111	;	H
		.byte #%00011000, #%00000110	;B	H
		.byte #%00000000, #%00000011	;	K
		.byte #%00011000, #%00000010	;B

		.byte #%00000000, #%00000100	;	S
		.byte #%01101111, #%00001100
		.byte #%00011000, #%00000110	;B	H
		.byte #%01100111, #%00001100
;				BVVFFFFF	TTMMMMMM
C2P8	.byte #%00010001, #%01000001	;B	K		G#		Alternate end
		.byte #%10010001, #%01000001
		.byte #%00010001, #%01000111	;	H
		.byte #%00000000, #%00000011	;	K

		.byte #%00000000, #%00000100	;	S
		.byte #%10000000, #%00000100
		.byte #%00010001, #%01000110	;B	H		C#
		.byte #%00000000, #%00000011	;	K

		.byte #%00010001, #%01000110	;B	H
		.byte #%00010001, #%01000111	;	H
		.byte #%00010001, #%01000001	;B	K
		.byte #%00010001, #%01000111	;	H

		.byte #%00000000, #%00000100	;	S
		.byte #%01000000, #%00000100	;	S
		.byte #%00100000, #%00000100	;	S
		.byte #%00000000, #%00000100	;	S
;				BVVFFFFF	TTMMMMMM
C2P9	.byte #%00001101, #%10000001	;B	K		C#		No beeps
		.byte #%10001101, #%10000001
		.byte #%00001101, #%10000111	;	H
		.byte #%00000000, #%00000011	;	K

		.byte #%00000000, #%00000100	;	S
		.byte #%10000000, #%00000100
		.byte #%00011001, #%00000110	;B	H		D#
		.byte #%00011000, #%00000001	;B	K		E

		.byte #%00011000, #%00000111	;	H
		.byte #%00011000, #%00000110	;B	H
		.byte #%00000000, #%00000011	;	K
		.byte #%00011000, #%00000010	;B

		.byte #%00000000, #%00000100	;	S
		.byte #%10000000, #%00000100
		.byte #%00011000, #%00000110	;B	H
		.byte #%10011000, #%00000110
;				BVVFFFFF	TTMMMMMM



;				BVVFFFFF	TTMMMMMM
C2P10	.byte #%00000000, #%00000011
		.byte #%10000000, #%00000011
		.byte #%00000000, #%00010110
		.byte #%00000000, #%00000011

		.byte #%00000000, #%00000100
		.byte #%10000000, #%00000100
		.byte #%00000000, #%00010110
		.byte #%00000000, #%00000011

		.byte #%00000000, #%00010110
		.byte #%00000000, #%00010110
		.byte #%00000000, #%00000011
		.byte #%00000000, #%00010110

		.byte #%00000000, #%00000100
		.byte #%10000000, #%00000100
		.byte #%00000000, #%00010110
		.byte #%10000000, #%00010110
;				BVVFFFFF	TTMMMMMM
C2P11	.byte #%00000000, #%00000011
		.byte #%10000000, #%00000011
		.byte #%00000000, #%00010110
		.byte #%00000000, #%00000011

		.byte #%00000000, #%00000100
		.byte #%10000000, #%00000100
		.byte #%00000000, #%00010110
		.byte #%00000000, #%00000011

		.byte #%00000000, #%00010110
		.byte #%00000000, #%00010110
		.byte #%00000000, #%00000011
		.byte #%00000000, #%00010110

		.byte #%00000000, #%00000100
		.byte #%10000000, #%00000100
		.byte #%00000000, #%00000011
		.byte #%01000000, #%00000100
;				BVVFFFFF	TTMMMMMM
C2P12	.byte #%00000000, #%00000011
		.byte #%10000000, #%00000011
		.byte #%10000000, #%00000011
		.byte #%00100000, #%00000011

		.byte #%10100000, #%00000011
		.byte #%10100000, #%00000011
		.byte #%01000000, #%00000011
		.byte #%11000000, #%00000011

		.byte #%11000000, #%00000011
		.byte #%01100000, #%00000011
		.byte #%00000000, #%00010101
		.byte #%10000000, #%00010101

		.byte #%10000000, #%00010101
		.byte #%10000000, #%00010101
		.byte #%10000000, #%00010101
		.byte #%10000000, #%00010101
;				BVVFFFFF	TTMMMMMM


C2P13	.byte #%00010001, #%01000001	;	K		A	Noise intro
		.byte #%10010001, #%01000001
		.byte #%00101010, #%00010111	;
		.byte #%10101010, #%00010111

		.byte #%00000000, #%00000100	;	S
		.byte #%10000000, #%00000100
		.byte #%00010001, #%01000110	;B	H
		.byte #%00010101, #%00000001	;B	K		F#

		.byte #%00010101, #%00000111	;	H
		.byte #%00010101, #%00000110	;B	H
		.byte #%00001010, #%00000001	;B	K
		.byte #%00010101, #%00000010	;B

		.byte #%00000000, #%00000100	;	S
		.byte #%10000000, #%00000100
		.byte #%00010101, #%00000110	;B	H
		.byte #%10010101, #%00000110


MacroTable
		.word MAC0
		.word MAC1
		.word MAC2
		.word MAC3
		.word MAC4
		.word MAC5
		.word MAC6
		.word MAC7
		.word MAC8
		.word MAC9
		.word MAC10
		.word MAC11
		.word MAC12
		.word MAC13
		.word MAC14
		.word MAC15
		.word MAC16
		.word MAC17
		.word MAC18
		.word MAC19
		.word MAC20
		.word MAC21
		.word MAC22
		.word MAC23
		.word MAC24

MacroData
MAC0	.byte #%10111111, #%00000000

;Kick + Bass
;				ExRFFFFF	VVVVWWWW
MAC1	.byte #%00010100, #%11111111
		.byte #%00011001, #%11111111
		.byte #%00011111, #%11111111
		.byte #%00110000, #%11110110
		.byte #%00110000, #%11010110
		.byte #%00110000, #%10110110
		.byte #%00110000, #%10100110
		.byte #%00110000, #%10010110
		.byte #%00110000, #%10010110
		.byte #%00110000, #%10000110
		.byte #%00110000, #%10000110
		.byte #%00110000, #%01110110
		.byte #%00110000, #%01110110
		.byte #%00110000, #%01110110
		.byte #%10110000, #%01100110

;Bass
;				ExRFFFFF	VVVVWWWW
MAC2	.byte #%00110000, #%11110110
		.byte #%00110000, #%11010110
		.byte #%00110000, #%10110110
		.byte #%00110000, #%10100110
		.byte #%00110000, #%10010110
		.byte #%00110000, #%10010110
		.byte #%00110000, #%10000110
		.byte #%00110000, #%10000110
		.byte #%00110000, #%01110110
		.byte #%00110000, #%01110110
		.byte #%00110000, #%01110110
		.byte #%10110000, #%01100110

;Kick
;				ExRFFFFF	VVVVWWWW
MAC3	.byte #%00010100, #%11111111
		.byte #%00011001, #%11111111
		.byte #%00011111, #%11111111
		.byte #%10111111, #%00000000

;Snare
;				ExRFFFFF	VVVVWWWW
MAC4	.byte #%00000101, #%11111111
		.byte #%00000101, #%11111111
		.byte #%00000111, #%11111111
		.byte #%00000111, #%11111111
		.byte #%00000100, #%11011000
		.byte #%00000110, #%11011000
		.byte #%10111111, #%00000000

;Low Square Ding
;				ExRFFFFF	VVVVWWWW
MAC5	.byte #%00110000, #%11111100
		.byte #%00110000, #%10101100
		.byte #%10110000, #%01111100

;Bass + Hat
;				ExRFFFFF	VVVVWWWW
MAC6	.byte #%00000010, #%01111000
		.byte #%00110000, #%11110110
		.byte #%00110000, #%11010110
		.byte #%00110000, #%10110110
		.byte #%00110000, #%10100110
		.byte #%00110000, #%10010110
		.byte #%00110000, #%10010110
		.byte #%00110000, #%10000110
		.byte #%00110000, #%10000110
		.byte #%00110000, #%01110110
		.byte #%00110000, #%01110110
		.byte #%00110000, #%01110110
		.byte #%10110000, #%01100110

;End Bass + Hat
;				ExRFFFFF	VVVVWWWW
MAC7	.byte #%00000010, #%01111000
		.byte #%10110000, #%01100110

;Buzz Ding
;				ExRFFFFF	VVVVWWWW
MAC8	.byte #%00110000, #%11110001
		.byte #%00110000, #%10110001
		.byte #%00110000, #%10000001
		.byte #%00110000, #%01100001
		.byte #%00110000, #%01000001
		.byte #%10110000, #%00100001


;Sq arp 05
;				ExRFFFFF	VVVVWWWW
MAC9	.byte #%00110000, #%10101100
		.byte #%00110000, #%10101100
		.byte #%00110000, #%10011100
		.byte #%00101011, #%10011100
		.byte #%00101011, #%10001100
		.byte #%00101011, #%10001100
		.byte #%00110000, #%01111100
		.byte #%00110000, #%01111100
		.byte #%00110000, #%01101100
		.byte #%00101011, #%01101100
		.byte #%00101011, #%01011100
		.byte #%00101011, #%01011100
		.byte #%00110000, #%01001100
		.byte #%00110000, #%01001100
		.byte #%00110000, #%00111100
		.byte #%00101011, #%00111100
		.byte #%10111111, #%00000000

;Sq arp 04
;				ExRFFFFF	VVVVWWWW
MAC10	.byte #%00110000, #%10101100
		.byte #%00110000, #%10101100
		.byte #%00110000, #%10011100
		.byte #%00101100, #%10011100
		.byte #%00101100, #%10001100
		.byte #%00101100, #%10001100
		.byte #%00110000, #%01111100
		.byte #%00110000, #%01111100
		.byte #%00110000, #%01101100
		.byte #%00101100, #%01101100
		.byte #%00101100, #%01011100
		.byte #%00101100, #%01011100
		.byte #%00110000, #%01001100
		.byte #%00110000, #%01001100
		.byte #%00110000, #%00111100
		.byte #%00101100, #%00111100
		.byte #%10111111, #%00000000

;Sq arp 03
;				ExRFFFFF	VVVVWWWW
MAC11	.byte #%00110000, #%10101100
		.byte #%00110000, #%10101100
		.byte #%00110000, #%10011100
		.byte #%00101101, #%10011100
		.byte #%00101101, #%10001100
		.byte #%00101101, #%10001100
		.byte #%00110000, #%01111100
		.byte #%00110000, #%01111100
		.byte #%00110000, #%01101100
		.byte #%00101101, #%01101100
		.byte #%00101101, #%01011100
		.byte #%00101101, #%01011100
		.byte #%00110000, #%01001100
		.byte #%00110000, #%01001100
		.byte #%00110000, #%00111100
		.byte #%00101101, #%00111100
		.byte #%10111111, #%00000000

;Hisq blop
;				ExRFFFFF	VVVVWWWW
MAC12	.byte #%00110000, #%11110100
		.byte #%00110000, #%11110100
		.byte #%10111111, #%00000000

;Hisq lead trill
;				ExRFFFFF	VVVVWWWW
MAC13	.byte #%00110000, #%01000100
		.byte #%00110000, #%01000100
		.byte #%00110000, #%01000100
		.byte #%00110000, #%01000100
		.byte #%10110011, #%01000100

;Hisq lead
;				ExRFFFFF	VVVVWWWW
MAC14	.byte #%00101111, #%11110100
		.byte #%00110000, #%10110100
		.byte #%00110000, #%10000100
		.byte #%00110000, #%01100100
		.byte #%10110000, #%01000100

;Hisq arp C# G# E C#
;				ExRFFFFF	VVVVWWWW
MAC15	.byte #%00001101, #%11110100
		.byte #%00010111, #%11110100
		.byte #%00010010, #%11100100
		.byte #%00011011, #%11010100
		.byte #%00001101, #%11010100
		.byte #%00010111, #%11000100
		.byte #%00010010, #%10110100
		.byte #%00011011, #%10110100
		.byte #%00001101, #%10100100
		.byte #%00010111, #%10010100
		.byte #%00010010, #%10010100
		.byte #%00011011, #%10000100
		.byte #%00001101, #%01110100
		.byte #%00010111, #%01110100
		.byte #%10111111, #%00000000

;Hisq arp B F# D# B
;				ExRFFFFF	VVVVWWWW
MAC16	.byte #%00001111, #%11110100
		.byte #%00011000, #%11110100
		.byte #%00010100, #%11100100
		.byte #%00011111, #%11010100
		.byte #%00001111, #%11010100
		.byte #%00011000, #%11000100
		.byte #%00010100, #%10110100
		.byte #%00011111, #%10110100
		.byte #%00001111, #%10100100
		.byte #%00011000, #%10010100
		.byte #%00010100, #%10010100
		.byte #%00011111, #%10000100
		.byte #%00001111, #%01110100
		.byte #%00011000, #%01110100
		.byte #%10111111, #%00000000

;Reedy 7 frame up
;				ExRFFFFF	VVVVWWWW
MAC17	.byte #%00110000, #%11110111
		.byte #%00101111, #%11110111
		.byte #%00101110, #%11110111
		.byte #%00101101, #%11110111
		.byte #%00101100, #%11110111
		.byte #%00101011, #%11110111
		.byte #%10101010, #%11110111

;5 frames of flangy
;				ExRFFFFF	VVVVWWWW
MAC18	.byte #%00110000, #%11110011
		.byte #%00110000, #%11110011
		.byte #%00110000, #%11110011
		.byte #%00110000, #%11110011
		.byte #%00110000, #%11110011
		.byte #%10111111, #%00000000

;5 frames of flangy pitch up
;				ExRFFFFF	VVVVWWWW
MAC19	.byte #%00110000, #%11110011
		.byte #%00101111, #%11110011
		.byte #%00101110, #%11110011
		.byte #%00101101, #%11110011
		.byte #%00101100, #%11110011
		.byte #%10111111, #%00000000

;5 frames of flangy pitch down
;				ExRFFFFF	VVVVWWWW
MAC20	.byte #%00110000, #%11110011
		.byte #%00110001, #%11110011
		.byte #%00110010, #%11110011
		.byte #%00110011, #%11110011
		.byte #%00110100, #%11110011
		.byte #%10111111, #%00000000

;DK Dankey Kang
;				ExRFFFFF	VVVVWWWW
MAC21	.byte #%00001010, #%01001100
		.byte #%00001010, #%01001100
		.byte #%00001010, #%01001100
		.byte #%00001010, #%01001100
		.byte #%00001000, #%01001100
		.byte #%00001000, #%01001100
		.byte #%00001000, #%01001100
		.byte #%00001000, #%01001100
		.byte #%00000111, #%01001100
		.byte #%00000111, #%01001100
		.byte #%00000111, #%01001100
		.byte #%00000111, #%01001100
		.byte #%00000110, #%01001100
		.byte #%00000110, #%01001100
		.byte #%00000110, #%01001100
		.byte #%00000110, #%01001100
		.byte #%00000110, #%01001100
		.byte #%00000110, #%01001100
		.byte #%00000110, #%01001100
		.byte #%00000110, #%01001100
		.byte #%00000110, #%01001100
		.byte #%00000110, #%01001100
		.byte #%00000110, #%01001100
		.byte #%00001010, #%01001100
		.byte #%00001010, #%01001100
		.byte #%00001010, #%01001100
		.byte #%00001010, #%01001100
		.byte #%00001010, #%01001100
		.byte #%00001010, #%01001100
		.byte #%00001010, #%01001100
		.byte #%10111111, #%00000000

;Hat Only
;				ExRFFFFF	VVVVWWWW
MAC22	.byte #%00000010, #%01111000
		.byte #%10111111, #%00000000

;Squeal plain
;				ExRFFFFF	VVVVWWWW
MAC23	.byte #%10110000, #%11001111

;Rumble plain
;				ExRFFFFF	VVVVWWWW
MAC24	.byte #%10110000, #%11000111

SampleTable
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
