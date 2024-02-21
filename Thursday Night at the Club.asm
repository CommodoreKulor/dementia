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
		.byte #1
		.byte #2
		.byte #3
		.byte #2
		.byte #1
		.byte #2
		.byte #3
		.byte #4
		
		.byte #5
		.byte #6
		.byte #7
		.byte #6
		.byte #5
		.byte #6
		.byte #7
		.byte #4
		
		.byte #8
		.byte #8
		.byte #8
		.byte #8
		.byte #9
		.byte #9
		.byte #10
		.byte #11
		
		.byte #12
		.byte #13
		.byte #12
		.byte #13
		.byte #12
		.byte #13
		.byte #12
		.byte #14
		
		.byte #255
		
		.byte #0
		.byte #15
		.byte #16
		.byte #3
		.byte #2
		
		.byte #255
		
		
		
c2SongData
		.byte #1
		.byte #1
		.byte #1
		.byte #2
		.byte #1
		.byte #1
		.byte #1
		.byte #3
		
		.byte #1
		.byte #1
		.byte #1
		.byte #2
		.byte #1
		.byte #1
		.byte #1
		.byte #3
		
		.byte #4
		.byte #5
		.byte #4
		.byte #5
		.byte #4
		.byte #5
		.byte #4
		.byte #6
		
		.byte #7
		.byte #10
		.byte #9
		.byte #8
		.byte #7
		.byte #10
		.byte #9
		.byte #11
		
		.byte #255
		
		.byte #0
		.byte #0
		.byte #0
		.byte #0
		.byte #12
		
		.byte #255
		
c1PatternTable
		.word Blank				;0
		.word BlinksPt1			;1
		.word BlinksPt2			;2
		.word BlinksPt3			;3
		.word Chords			;4
		.word HiBlinksPt1		;5
		.word HiBlinksPt2		;6
		.word HiBlinksPt3		;7
		.word FillPt1			;8
		.word FillPt2			;9
		.word FillPt3			;10
		.word FillPt4			;11
		.word BlopChordsPt1		;12
		.word BlopChordsPt2		;13
		.word BlopChordsPt3		;14
		.word IntroBlinksPt1	;15
		.word IntroBlinksPt2	;16
c2PatternTable
		.word Blank				;0
		.word BassPt1			;1
		.word BassPt2			;2
		.word BassPt3			;3
		.word FillBassPt1		;4
		.word FillBassPt2		;5
		.word FillBassPt3		;6
		.word BassBlopsPt1		;7
		.word BassBlopsPt2		;8
		.word BassBlopsPt3		;9
		.word BassBlopsPt4		;10
		.word BassBlopsPt5		;11
		.word IntroBass			;12
		
		
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
BlinksPt1
		.byte #%00010100, #%00001001
		.byte #%10010100, #%00001001
		.byte #%00010100, #%00001010
		.byte #%00001100, #%00001001
		.byte #%00001100, #%00001011
		.byte #%00001100, #%00000010
		.byte #%00001101, #%00001001
		.byte #%10001101, #%00001001
		.byte #%00001101, #%00001011
		.byte #%10001101, #%00001011
		.byte #%00001101, #%00001010
		.byte #%10001101, #%00001010
		.byte #%00001101, #%00001001
		.byte #%10001101, #%00001001
		.byte #%00001101, #%00001001
		.byte #%00001100, #%00000011
;				BVVFFFFF	TTMMMMMM
BlinksPt2
		.byte #%00010100, #%00001001
		.byte #%10010100, #%00001001
		.byte #%00010100, #%00001010
		.byte #%00001100, #%00000001
		.byte #%00001100, #%00001011
		.byte #%00001100, #%00000010
		.byte #%00001101, #%00001001
		.byte #%10001101, #%00001001
		.byte #%00001101, #%00001011
		.byte #%10001101, #%00001011
		.byte #%00001101, #%00001010
		.byte #%10001101, #%00001010
		.byte #%00001101, #%00001001
		.byte #%00110100, #%00000001
		.byte #%01001101, #%00001001
		.byte #%01110100, #%00000001
;				BVVFFFFF	TTMMMMMM
BlinksPt3
		.byte #%00010100, #%00001100
		.byte #%10010100, #%00001100
		.byte #%00010100, #%00001001
		.byte #%00001100, #%00001001
		.byte #%00001100, #%00001011
		.byte #%00001100, #%00000010
		.byte #%00001101, #%00001001
		.byte #%10001101, #%00000001
		.byte #%00001101, #%00001011
		.byte #%10001101, #%00001011
		.byte #%00001101, #%00001010
		.byte #%10001101, #%00001010
		.byte #%00001101, #%00001001
		.byte #%10001101, #%00001001
		.byte #%00001101, #%00001001
		.byte #%00001100, #%00000011
;				BVVFFFFF	TTMMMMMM
BassPt1	
		.byte #%00001111, #%01000101
		.byte #%00001010, #%00001000
		.byte #%00001111, #%01000111
		.byte #%00001111, #%01000101
		.byte #%00000000, #%00000100
		.byte #%00010100, #%00000001
		.byte #%00001111, #%01000101
		.byte #%00001111, #%01000101
		.byte #%00001010, #%00001000
		.byte #%00001111, #%01000111
		.byte #%00001111, #%01000101
		.byte #%10001111, #%01000101
		.byte #%00000000, #%00000100
		.byte #%00010001, #%00000101
		.byte #%10010001, #%00000101
		.byte #%00010001, #%00000101
;				BVVFFFFF	TTMMMMMM
BassPt2	
		.byte #%00010011, #%10000101
		.byte #%10010011, #%10000101
		.byte #%00010011, #%10000101
		.byte #%00010100, #%01000101
		.byte #%00000000, #%00000100
		.byte #%00010100, #%01000101
		.byte #%00010111, #%00000101
		.byte #%10010111, #%00000101
		.byte #%00010111, #%00000101
		.byte #%10010111, #%00000101
		.byte #%00010111, #%00000101
		.byte #%10010111, #%00000101
		.byte #%00000000, #%00000100
		.byte #%00001011, #%00000101
		.byte #%00000000, #%00000100
		.byte #%00000000, #%00000100
;				BVVFFFFF	TTMMMMMM
BassPt3
		.byte #%00010011, #%10000101
		.byte #%10010011, #%10000101
		.byte #%00010011, #%10000101
		.byte #%00010100, #%01000101
		.byte #%10010100, #%01000101
		.byte #%00010100, #%01000101
		.byte #%00010111, #%00000101
		.byte #%10010111, #%00000101
		.byte #%00010111, #%00000101
		.byte #%10010111, #%00000101
		.byte #%00010111, #%00000101
		.byte #%10010111, #%00000101
		.byte #%00000000, #%00000100
		.byte #%00010111, #%00000101
		.byte #%00000000, #%00000100
		.byte #%00000000, #%00000100
;				BVVFFFFF	TTMMMMMM
Chords	
		.byte #%00000000, #%00001101
		.byte #%10000000, #%00001101
		.byte #%00000000, #%00001101
		.byte #%00000000, #%00001110
		.byte #%00000000, #%00010000
		.byte #%00000000, #%00010001
		.byte #%00000000, #%00001111
		.byte #%10000000, #%00001111
		.byte #%00000000, #%00001111
		.byte #%10000000, #%00001111
		.byte #%00000000, #%00001111
		.byte #%10000000, #%00001111
		.byte #%00000000, #%00000000
		.byte #%00000000, #%00000000
		.byte #%00000000, #%00000000
		.byte #%00000000, #%00000000
;				BVVFFFFF	TTMMMMMM
HiBlinksPt1
		.byte #%00011111, #%00010101
		.byte #%10011111, #%00010101
		.byte #%00011111, #%00010110
		.byte #%00010011, #%00010101
		.byte #%00010011, #%00010111
		.byte #%00010011, #%00010011
		.byte #%00010100, #%00010101
		.byte #%10010100, #%00010101
		.byte #%00010100, #%00010111
		.byte #%10010100, #%00010111
		.byte #%00010100, #%00010110
		.byte #%10010100, #%00010110
		.byte #%00010100, #%00010101
		.byte #%10010100, #%00010101
		.byte #%00010100, #%00010101
		.byte #%00010011, #%00010100
;				BVVFFFFF	TTMMMMMM
HiBlinksPt2
		.byte #%00011111, #%00010101
		.byte #%10011111, #%00010101
		.byte #%00011111, #%00010110
		.byte #%00010011, #%00010010
		.byte #%00010011, #%00010111
		.byte #%00010011, #%00010011
		.byte #%00010100, #%00010101
		.byte #%10010100, #%00010101
		.byte #%00010100, #%00010111
		.byte #%10010100, #%00010111
		.byte #%00010100, #%00010110
		.byte #%10010100, #%00010110
		.byte #%00010100, #%00010101
		.byte #%00111111, #%00010010
		.byte #%01010100, #%00010101
		.byte #%01111111, #%00010010
;				BVVFFFFF	TTMMMMMM
HiBlinksPt3
		.byte #%00011111, #%00011000
		.byte #%10011111, #%00011000
		.byte #%00011111, #%00010101
		.byte #%00010011, #%00010101
		.byte #%00010011, #%00010111
		.byte #%00010011, #%00010011
		.byte #%00010100, #%00010101
		.byte #%10010100, #%00010010
		.byte #%00010100, #%00010111
		.byte #%10010100, #%00010111
		.byte #%00010100, #%00010110
		.byte #%10010100, #%00010110
		.byte #%00010100, #%00010101
		.byte #%10010100, #%00010101
		.byte #%00010100, #%00010101
		.byte #%00010011, #%00010100
;				BVVFFFFF	TTMMMMMM
FillPt1
		.byte #%00000000, #%00011010
		.byte #%10000000, #%00011010
		.byte #%00000000, #%00011011
		.byte #%00000000, #%00000100
		.byte #%00000000, #%00011011
		.byte #%10000000, #%00011011
		.byte #%00000000, #%00011100
		.byte #%10000000, #%00011100
		.byte #%00000000, #%00011010
		.byte #%10000000, #%00011010
		.byte #%00000000, #%00011010
		.byte #%10000000, #%00011010
		.byte #%00000000, #%00011010
		.byte #%10000000, #%00011010
		.byte #%00000000, #%00011100
		.byte #%10000000, #%00011100
;				BVVFFFFF	TTMMMMMM
FillPt2
		.byte #%00000000, #%00011010
		.byte #%10000000, #%00011010
		.byte #%00000000, #%00011100
		.byte #%10000000, #%00011100
		.byte #%00000000, #%00011010
		.byte #%10000000, #%00011010
		.byte #%00000000, #%00011100
		.byte #%10000000, #%00011100
		.byte #%00000000, #%00011010
		.byte #%10000000, #%00011010
		.byte #%00000000, #%00011100
		.byte #%10000000, #%00011100
		.byte #%00000000, #%00011010
		.byte #%10000000, #%00011010
		.byte #%00000000, #%00011100
		.byte #%10000000, #%00011100
;				BVVFFFFF	TTMMMMMM
FillPt3
		.byte #%00000000, #%00011010
		.byte #%00000000, #%00011001
		.byte #%00000000, #%00011010
		.byte #%00000000, #%00011001
		.byte #%00000000, #%00011010
		.byte #%00000000, #%00011001
		.byte #%00000000, #%00011010
		.byte #%00000000, #%00011001
		.byte #%00000000, #%00011010
		.byte #%00000000, #%00011001
		.byte #%00000000, #%00011010
		.byte #%00000000, #%00011001
		.byte #%00000000, #%00011010
		.byte #%00000000, #%00011001
		.byte #%00000000, #%00011010
		.byte #%00000000, #%00011001
;				BVVFFFFF	TTMMMMMM
FillPt4
		.byte #%00000000, #%00011010
		.byte #%00000000, #%00000100
		.byte #%00000000, #%00011010
		.byte #%00000000, #%00000100
		.byte #%00000000, #%00011010
		.byte #%00000000, #%00000100
		.byte #%00000000, #%00011010
		.byte #%00000000, #%00000100
		.byte #%00000000, #%00011101
		.byte #%00000000, #%00011101
		.byte #%00000000, #%00011101
		.byte #%00000000, #%00011101
		.byte #%00000000, #%00100101
		.byte #%00000001, #%00100101
		.byte #%00000010, #%00100101
		.byte #%00000011, #%00100101
;				BVVFFFFF	TTMMMMMM
FillBassPt1
		.byte #%00001111, #%01000101
		.byte #%00011101, #%00011110
		.byte #%00010100, #%00011110
		.byte #%00001111, #%01000101
		.byte #%00011101, #%00011110
		.byte #%00010100, #%00011110
		.byte #%00001111, #%01000101
		.byte #%00011101, #%00011110
		.byte #%00010100, #%00011110
		.byte #%00001111, #%01000101
		.byte #%00011101, #%00011110
		.byte #%00010100, #%00011110
		.byte #%00001111, #%01000101
		.byte #%00011101, #%00011110
		.byte #%00010001, #%01000101
		.byte #%00011101, #%00011110
;				BVVFFFFF	TTMMMMMM
FillBassPt2
		.byte #%00001111, #%01000101
		.byte #%00001010, #%00001000
		.byte #%00001111, #%01000111
		.byte #%00001111, #%01000101
		.byte #%00011101, #%00011110
		.byte #%00010100, #%00011110
		.byte #%00001111, #%01000101
		.byte #%00011101, #%00011110
		.byte #%00010100, #%00011110
		.byte #%00001111, #%01000101
		.byte #%00011101, #%00011110
		.byte #%00010100, #%00011110
		.byte #%00001111, #%01000101
		.byte #%00011101, #%00011110
		.byte #%00010001, #%01000101
		.byte #%00011101, #%00011110
;				BVVFFFFF	TTMMMMMM
FillBassPt3
		.byte #%00001111, #%01000101
		.byte #%00001010, #%00001000
		.byte #%00001111, #%01000111
		.byte #%00001111, #%01000101
		.byte #%00011101, #%00011110
		.byte #%00010100, #%00011110
		.byte #%00001111, #%01000101
		.byte #%00011101, #%00011110
		.byte #%00010100, #%00011110
		.byte #%00001111, #%01000101
		.byte #%00011101, #%00011110
		.byte #%00010100, #%00011110
		.byte #%00000000, #%00100101
		.byte #%00000001, #%00100101
		.byte #%00000010, #%00100101
		.byte #%00000011, #%00100101
;				BVVFFFFF	TTMMMMMM
BlopChordsPt1
		.byte #%00000000, #%00011111
		.byte #%00000000, #%00000000
		.byte #%00111111, #%00100110
		.byte #%00000000, #%00100001
		.byte #%00000000, #%00000100
		.byte #%00110011, #%00100111
		.byte #%00000000, #%00100000
		.byte #%00000000, #%00000000
		.byte #%00110100, #%00100111
		.byte #%10110100, #%00100111
		.byte #%00000000, #%00100000
		.byte #%00000000, #%00000000
		.byte #%00000000, #%00000100
		.byte #%00110100, #%00100111
		.byte #%00000000, #%00100000
		.byte #%00000000, #%00100001
;				BVVFFFFF	TTMMMMMM
BlopChordsPt2
		.byte #%00000000, #%00011111
		.byte #%00000000, #%00000000
		.byte #%00111111, #%00100110
		.byte #%00000000, #%00100001
		.byte #%00000000, #%00000100
		.byte #%00110011, #%00100111
		.byte #%00000000, #%00100000
		.byte #%00000000, #%00000000
		.byte #%00110100, #%00100111
		.byte #%10110100, #%00100111
		.byte #%00000000, #%00100000
		.byte #%00000000, #%00000000
		.byte #%00000000, #%00000100
		.byte #%00000000, #%00011111
		.byte #%00000000, #%00100000
		.byte #%00000000, #%00011111
;				BVVFFFFF	TTMMMMMM
BlopChordsPt3	
		.byte #%00000000, #%00001101
		.byte #%10000000, #%00001101
		.byte #%00000000, #%00001101
		.byte #%00000000, #%00001110
		.byte #%00000000, #%00010000
		.byte #%00000000, #%00010001
		.byte #%00000000, #%00001111
		.byte #%10000000, #%00001111
		.byte #%00000000, #%00011010
		.byte #%00000000, #%00000100
		.byte #%00000000, #%00011010
		.byte #%00000000, #%00000100
		.byte #%00000000, #%00011101
		.byte #%00000000, #%00011101
		.byte #%00000000, #%00011101
		.byte #%00000000, #%00011101
;				BVVFFFFF	TTMMMMMM
BassBlopsPt1
		.byte #%00001111, #%01100010
		.byte #%10001111, #%01100010
		.byte #%00001111, #%01100011
		.byte #%00001111, #%01100010
		.byte #%00001111, #%01100011
		.byte #%10001111, #%01100011
		.byte #%00001111, #%01100010
		.byte #%10001111, #%01100010
		.byte #%00001111, #%01100011
		.byte #%10001111, #%01100011
		.byte #%00001111, #%01100010
		.byte #%10001111, #%01100010
		.byte #%00001111, #%01100011
		.byte #%00010001, #%00000101
		.byte #%00010001, #%00100011
		.byte #%10010001, #%00100011
;				BVVFFFFF	TTMMMMMM
BassBlopsPt2
		.byte #%00010011, #%10100010
		.byte #%10010011, #%10100010
		.byte #%00010011, #%10100011
		.byte #%00010100, #%01000101
		.byte #%00010100, #%01100011
		.byte #%10010100, #%01100011
		.byte #%00010111, #%00100010
		.byte #%10010111, #%00100010
		.byte #%00010111, #%00100011
		.byte #%10010111, #%00100011
		.byte #%00010111, #%00100010
		.byte #%10010111, #%00100010
		.byte #%00001011, #%00100011
		.byte #%00001011, #%00000101
		.byte #%00010111, #%00000110
		.byte #%10010111, #%00000110
;				BVVFFFFF	TTMMMMMM
BassBlopsPt3
		.byte #%00001111, #%01100100
		.byte #%10001111, #%01100100
		.byte #%00001111, #%01100011
		.byte #%00001111, #%01100010
		.byte #%00001111, #%01100011
		.byte #%10001111, #%01100011
		.byte #%00001111, #%01100010
		.byte #%10001111, #%01100010
		.byte #%00001111, #%01100011
		.byte #%10001111, #%01100011
		.byte #%00001111, #%01100010
		.byte #%10001111, #%01100010
		.byte #%00001111, #%01100011
		.byte #%00010001, #%00000101
		.byte #%00010001, #%00100011
		.byte #%10010001, #%00100011
;				BVVFFFFF	TTMMMMMM
BassBlopsPt4
		.byte #%00001111, #%01100010
		.byte #%10001111, #%01100010
		.byte #%00001111, #%01100011
		.byte #%00001111, #%01000101
		.byte #%00001111, #%01100011
		.byte #%10001111, #%01100011
		.byte #%00001111, #%01100010
		.byte #%10001111, #%01100010
		.byte #%00001111, #%01100011
		.byte #%10001111, #%01100011
		.byte #%00001111, #%01100010
		.byte #%10001111, #%01100010
		.byte #%00001111, #%01100011
		.byte #%00010001, #%00000101
		.byte #%10010001, #%00000101
		.byte #%10010001, #%00000101
;				BVVFFFFF	TTMMMMMM
BassBlopsPt5
		.byte #%00010011, #%10000101
		.byte #%10010011, #%10000101
		.byte #%00010011, #%10000101
		.byte #%00010100, #%01000101
		.byte #%10010100, #%01000101
		.byte #%00010100, #%01000101
		.byte #%00010111, #%00000101
		.byte #%10010111, #%00000101
		.byte #%00001011, #%00000101
		.byte #%00010111, #%00000110
		.byte #%00001011, #%00000101
		.byte #%00010111, #%00000110
		.byte #%00001011, #%00000101
		.byte #%00010111, #%00000110
		.byte #%00001011, #%00000101
		.byte #%00010111, #%00000110
;				BVVFFFFF	TTMMMMMM
IntroBlinksPt1
		.byte #%01110100, #%00001001
		.byte #%11110100, #%00001001
		.byte #%01110100, #%00001010
		.byte #%01101100, #%00001001
		.byte #%01101100, #%00001011
		.byte #%01101100, #%00000010
		.byte #%01101101, #%00001001
		.byte #%11101101, #%00001001
		.byte #%01001101, #%00001011
		.byte #%11001101, #%00001011
		.byte #%01001101, #%00001010
		.byte #%11001101, #%00001010
		.byte #%01001101, #%00001001
		.byte #%11001101, #%00001001
		.byte #%01001101, #%00001001
		.byte #%01001100, #%00000011
;				BVVFFFFF	TTMMMMMM
IntroBlinksPt2
		.byte #%00110100, #%00001001
		.byte #%10110100, #%00001001
		.byte #%00110100, #%00001010
		.byte #%00101100, #%00000001
		.byte #%00101100, #%00001011
		.byte #%00101100, #%00000010
		.byte #%00101101, #%00001001
		.byte #%10101101, #%00001001
		.byte #%00001101, #%00001011
		.byte #%10001101, #%00001011
		.byte #%00001101, #%00001010
		.byte #%10001101, #%00001010
		.byte #%00001101, #%00001001
		.byte #%00110100, #%00000001
		.byte #%01001101, #%00001001
		.byte #%01110100, #%00000001
;				BVVFFFFF	TTMMMMMM
IntroBass	
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
		.byte #%00000000, #%00000100
		.byte #%00001010, #%00001000
		.byte #%00001111, #%01000101
		.byte #%10001111, #%01000101
;				BVVFFFFF	TTMMMMMM



MacroTable
		.word MAC0
		.word MAC1	;Sq donk
		.word MAC2	;Sq donk lower volume
		.word MAC3	;Sq donk very low volume
		.word MAC4	;Snare sample
		.word MAC5	;Kick + soft bass
		.word MAC6	;Plain soft bass
		.word MAC7	;Plain reedy bass
		.word MAC8	;Plain Tunnel Runner
		.word MAC9	;Sq donk + hat
		.word MAC10	;Sq donk lower volume + hat
		.word MAC11	;Sq donk very low volume + hat
		.word MAC12	;Sq donk + rapid hats
		.word MAC13	;Dmaj Saw sample + hat
		.word MAC14	;Esus4 Saw sample + hat
		.word MAC15	;Gmaj Saw sample + hat
		.word MAC16	;Esus4 Saw sample + hat continue
		.word MAC17	;Esus4 Saw sample
		.word MAC18	;Hi donk
		.word MAC19	;Hi donk lower volume
		.word MAC20	;Hi donk very low volume
		.word MAC21	;Hi donk + hat
		.word MAC22	;Hi donk lower volume + hat
		.word MAC23	;Hi donk very low volume + hat
		.word MAC24	;Hi donk + 32nd hat
		.word MAC25	;Sonic Yeah sample
		.word MAC26	;Snare sample + hat
		.word MAC27	;Snare sample continue + hat
		.word MAC28	;Sonic Yeah sample + hat
		.word MAC29	;Snare double sample + hat
		.word MAC30	;Siren
		.word MAC31	;Blop B sample
		.word MAC32	;Blop F# sample
		.word MAC33	;Blop G sample
		.word MAC34	;Kick + soft bass + hat
		.word MAC35	;Soft bass + hat
		.word MAC36	;Kick + soft bass + 32nd hat
		.word MAC37	;Engine thing
		.word MAC38	;Bmin arp
		.word MAC39	;Hi donk no detune
		
		
		
MacroData
;				ELRFFFFF	VVVVWWWW
MAC0	.byte #%10111111, #%00000001
		
;				ELRFFFFF	VVVVWWWW	
MAC1	.byte #%00101111, #%10001100		;Sq donk
		.byte #%00110000, #%10001100
		.byte #%00110000, #%01101100
		.byte #%00110000, #%00111100
		.byte #%00110000, #%00101100
		.byte #%10110000, #%00011100

;				ELRFFFFF	VVVVWWWW			
MAC2	.byte #%00101111, #%01101100		;Sq donk lower volume
		.byte #%00110000, #%01101100
		.byte #%00110000, #%00111100
		.byte #%00110000, #%00101100
		.byte #%10110000, #%00011100

;				ELRFFFFF	VVVVWWWW			
MAC3	.byte #%00101111, #%00111100		;Sq donk very low volume
		.byte #%00110000, #%00111100
		.byte #%00110000, #%00101100
		.byte #%10110000, #%00011100
		
;				ELRFFFFF	VVVVWWWW
MAC4	.byte #%00000001, #%00010000		;Snare sample
		.byte #%10000000, #%00010000
		
;				ELRFFFFF	VVVVWWWW
MAC5	.byte #%00010100, #%10011111		;Kick + soft bass
		.byte #%00011001, #%10011111
		.byte #%00011111, #%10011111
		.byte #%00110000, #%00010110
		.byte #%00110000, #%00100110
		.byte #%00110000, #%00110110
		.byte #%00110000, #%01000110
MAC6	.byte #%10110000, #%01010110		;Plain soft bass
		
;				ELRFFFFF	VVVVWWWW
MAC7	.byte #%10110000, #%01000111		;Plain reedy bass

;				ELRFFFFF	VVVVWWWW
MAC8	.byte #%10110000, #%01001111		;Plain Tunnel Runner
		
;				ELRFFFFF	VVVVWWWW	
MAC9	.byte #%00000000, #%01000011		;Sq donk + hat
		.byte #%00101111, #%10001100
		.byte #%00110000, #%10001100		
		.byte #%00110000, #%01101100
		.byte #%00110000, #%00111100
		.byte #%00110000, #%00101100
		.byte #%10110000, #%00011100
		
;				ELRFFFFF	VVVVWWWW			
MAC10	.byte #%00000000, #%01000011		;Sq donk lower volume + hat
		.byte #%00101111, #%01101100
		.byte #%00110000, #%01101100
		.byte #%00110000, #%00111100
		.byte #%00110000, #%00101100
		.byte #%10110000, #%00011100
		
;				ELRFFFFF	VVVVWWWW	
MAC11	.byte #%00000000, #%01000011		;Sq donk very low volume + hat
		.byte #%00101110, #%00111100
		.byte #%00110000, #%00111100
		.byte #%00110000, #%00101100
		.byte #%10110000, #%00011100
		
;				ELRFFFFF	VVVVWWWW	
MAC12	.byte #%00000000, #%01000011		;Sq donk + 32nd hat
		.byte #%00101111, #%10001100
		.byte #%00110000, #%10001100
		.byte #%00110000, #%01101100
		.byte #%00110000, #%00111100
		.byte #%00110000, #%00101100
		.byte #%00000000, #%01000011	;Loop point
		.byte #%00110000, #%00011100
		.byte #%00110000, #%00011100
		.byte #%00110000, #%00011100
		.byte #%00110000, #%00011100
		.byte #%11110000, #%00011100, #12
		
;				ELRFFFFF	VVVVWWWW
MAC13	.byte #%00000000, #%01000011		;Dmaj Saw sample + hat
		.byte #%00000001, #%00100000
		.byte #%10000000, #%00100000
		
;				ELRFFFFF	VVVVWWWW
MAC14	.byte #%00000000, #%01000011		;Esus4 Saw sample + hat
		.byte #%00000001, #%00110000
		.byte #%10000000, #%00110000
		
;				ELRFFFFF	VVVVWWWW
MAC15	.byte #%00000000, #%01000011		;Gmaj Saw sample + hat
		.byte #%00000001, #%01000000
		.byte #%10000000, #%01000000
		
;				ELRFFFFF	VVVVWWWW
MAC16	.byte #%00000000, #%01000011		;Esus4 Saw sample + hat continue
		.byte #%10000000, #%00110000
		
;				ELRFFFFF	VVVVWWWW
MAC17	.byte #%00000001, #%00110000		;Esus4 Saw sample
		.byte #%10000000, #%00110000
		
;				ELRFFFFF	VVVVWWWW	
MAC18	.byte #%00101111, #%10000100		;Hi donk
		.byte #%00110000, #%10000100
		.byte #%00110000, #%01100100
		.byte #%00110000, #%00110100
		.byte #%00110000, #%00100100
		.byte #%10110000, #%00010100

;				ELRFFFFF	VVVVWWWW			
MAC19	.byte #%00101111, #%01100100		;Hi donk lower volume
		.byte #%00110000, #%01100100
		.byte #%00110000, #%00110100
		.byte #%00110000, #%00100100
		.byte #%10110000, #%00010100

;				ELRFFFFF	VVVVWWWW			
MAC20	.byte #%00101111, #%00110100		;Hi donk very low volume
		.byte #%00110000, #%00110100
		.byte #%00110000, #%00100100
		.byte #%10110000, #%00010100
		
;				ELRFFFFF	VVVVWWWW	
MAC21	.byte #%00000000, #%01000011		;Hi donk + hat
		.byte #%00101111, #%10000100
		.byte #%00110000, #%10000100
		.byte #%00110000, #%01100100
		.byte #%00110000, #%00110100
		.byte #%00110000, #%00100100
		.byte #%10110000, #%00010100
		
;				ELRFFFFF	VVVVWWWW			
MAC22	.byte #%00000000, #%01000011		;Hi donk lower volume + hat
		.byte #%00101111, #%01100100
		.byte #%00110000, #%01100100
		.byte #%00110000, #%00110100
		.byte #%00110000, #%00100100
		.byte #%10110000, #%00010100
		
;				ELRFFFFF	VVVVWWWW	
MAC23	.byte #%00000000, #%01000011		;Hi donk very low volume + hat
		.byte #%00101110, #%00110100
		.byte #%00110000, #%00110100
		.byte #%00110000, #%00100100
		.byte #%10110000, #%00010100
		
;				ELRFFFFF	VVVVWWWW	
MAC24	.byte #%00000000, #%01000011		;Hi donk + 32nd hat
		.byte #%00101111, #%10000100
		.byte #%00110000, #%10000100
		.byte #%00110000, #%01100100
		.byte #%00110000, #%00110100
		.byte #%00110000, #%00100100
		.byte #%00000000, #%01000011	;Loop point
		.byte #%00110000, #%00010100
		.byte #%00110000, #%00010100
		.byte #%00110000, #%00010100
		.byte #%00110000, #%00010100
		.byte #%11110000, #%00010100, #12
		
;				ELRFFFFF	VVVVWWWW
MAC25	.byte #%00000001, #%01010000		;Sonic Yeah sample
		.byte #%10000000, #%01010000
		
;				ELRFFFFF	VVVVWWWW
MAC26	.byte #%00000000, #%01000011		;Snare sample + hat
		.byte #%00000001, #%00010000
		.byte #%10000000, #%00010000
		
;				ELRFFFFF	VVVVWWWW
MAC27	.byte #%00000000, #%01000011		;Snare sample continue + hat
		.byte #%10000000, #%00010000

;				ELRFFFFF	VVVVWWWW
MAC28	.byte #%00000000, #%01000011		;Sonic Yeah sample + hat
		.byte #%00000001, #%01010000
		.byte #%10000000, #%01010000
		
;				ELRFFFFF	VVVVWWWW
MAC29	.byte #%00000000, #%01000011		;Snare double sample + hat
		.byte #%00000001, #%00010000 
		.byte #%00000000, #%00010000
		.byte #%00000000, #%00010000
		.byte #%00000001, #%00010000 
		.byte #%10000000, #%00010000
		
;				ELRFFFFF	VVVVWWWW
MAC30	.byte #%00110000, #%00010100		;Siren
		.byte #%00101110, #%00010100
		.byte #%00101101, #%00010100
		.byte #%00101100, #%00010100
		.byte #%00101011, #%00010100
		.byte #%00101010, #%00010100
		.byte #%00101001, #%00010100
		.byte #%00101000, #%00010100
		.byte #%10100111, #%00010100
		
;				ELRFFFFF	VVVVWWWW
MAC31	.byte #%10000001, #%01100000		;Blop B sample
		
;				ELRFFFFF	VVVVWWWW
MAC32	.byte #%10000001, #%01110000		;Blop G sample
		
;				ELRFFFFF	VVVVWWWW
MAC33	.byte #%10000001, #%10000000		;Blop F# sample

;				ELRFFFFF	VVVVWWWW
MAC34	.byte #%00000000, #%01000011		;Kick + soft bass + hat
		.byte #%00010100, #%10011111
		.byte #%00011001, #%10011111
		.byte #%00011111, #%10011111
		.byte #%00110000, #%00010110
		.byte #%00110000, #%00100110
		.byte #%00110000, #%00110110
		.byte #%00110000, #%01000110
		.byte #%10110000, #%01010110
		
;				ELRFFFFF	VVVVWWWW
MAC35	.byte #%00000000, #%01000011		;Soft bass + hat
		.byte #%10110000, #%01010110
		
;				ELRFFFFF	VVVVWWWW	
MAC36	.byte #%00000000, #%01000011		;Kick + soft bass + 32nd hat
		.byte #%00010100, #%10011111
		.byte #%00011001, #%10011111
		.byte #%00011111, #%10011111
		.byte #%00110000, #%00010110
		.byte #%00110000, #%00100110
		.byte #%00000000, #%01000011	;Loop point
		.byte #%00110000, #%01000110
		.byte #%00110000, #%01010110
		.byte #%00110000, #%01010110
		.byte #%00110000, #%01010110
		.byte #%11110000, #%01000110, #12

;				ELRFFFFF	VVVVWWWW	
MAC37	.byte #%10110000, #%00110010		;Engine thing

;				ELRFFFFF	VVVVWWWW	
MAC38	.byte #%00011111, #%01000100		;Bmin arp
		.byte #%00011111, #%01000100
		.byte #%00011010, #%01000100
		.byte #%00011010, #%01000100
		.byte #%00010100, #%01000100
		.byte #%11010100, #%01000100, #0
		
;				ELRFFFFF	VVVVWWWW	
MAC39	.byte #%00110000, #%10000100		;Hi donk no detune
		.byte #%00110000, #%01100100
		.byte #%00110000, #%00110100
		.byte #%00110000, #%00100100
		.byte #%10110000, #%00010100
		
		
		
		
SampleTable
		.word SAMP0S, SAMP0E
		.word SAMP1S, SAMP1E		;1: Snare
		.word SAMP2S, SAMP2E		;2: Dmaj Saw
		.word SAMP3S, SAMP3E		;3: Esus4 Saw
		.word SAMP4S, SAMP4E		;4: Gmaj Saw
		.word SAMP5S, SAMP5E		;5: Sonic Yeah
		.word SAMP6S, SAMP6E		;6: Blop B
		.word SAMP7S, SAMP7E		;7: Blop F#
		.word SAMP8S, SAMP8E		;8: Blop G
		
		
		
SampleData
SAMP0S	.byte $00
SAMP0E
		.byte $00, $00, $00, $00
		
		;Snare
SAMP1S	.byte $00, $FE, $0F, $00, $DA, $06, $00, $FC, $F6, $04, $00, $C0, $FF, $1F, $00, $00
		.byte $ED, $7F, $06, $00, $90, $FF, $6F, $00, $80, $BC, $FF, $00, $00, $80, $FC, $0F
		.byte $00, $80, $FA, $FF, $04, $00, $D0, $FF, $23, $00, $40, $6F, $4F, $12, $00, $F0
		.byte $FE, $03, $00, $60, $FD, $4F, $04, $00, $7C, $BF, $00, $00, $B8, $7B, $0A, $80
		.byte $60, $6F, $4B, $40, $D2, $39, $3B, $10, $C0, $D6, $19, $80, $10, $E6, $9B, $08
		.byte $CA, $56, $34, $9C, $23, $96, $02, $83, $4C, $83, $39, $20, $65, $52, $75, $32
		.byte $C5, $C0, $20, $18, $26, $83, $68, $62, $22, $96, $15, $AA, $25, $29, $4D, $33
		.byte $A9, $54, $4C, $12, $55, $A4, $41, $20, $00, $A4, $A4, $08, $00, $52, $55, $01
		.byte $00, $80, $20, $93, $22, $24, $B5, $54, $D5, $AA, $52, $95, $88, $00, $00
SAMP1E
		.byte $00, $00, $00, $00

		;Dmaj Saw
SAMP2S	.byte $00, $00, $80, $BA, $68, $41, $00, $42, $F7, $82, $00, $00, $E0, $1F, $90, $42
		.byte $20, $7E, $00, $F8, $70, $20, $00, $C0, $79, $F2, $01, $00, $E0, $9F, $07, $00
		.byte $10, $9F, $0F, $44, $00, $30, $FF, $41, $60, $02, $E7, $83, $E0, $0F, $07, $03
		.byte $04, $9E, $8F, $07, $00, $00, $FF, $49, $00, $00, $F2, $F9, $00, $03, $00, $F1
		.byte $0F, $0C, $04, $68, $1F, $18, $9E, $1C, $78, $40, $E0, $3C, $3C, $00, $00, $F1
		.byte $1F, $06, $00, $30, $CF, $1F, $10, $00, $38, $7F, $60, $60, $80, $F3, $C0, $41
		.byte $CE, $C0, $03, $0C, $6D, $CE, $50, $00, $10, $FF, $30, $00, $00, $F3, $FC, $00
		.byte $01, $80, $F3, $07, $00, $00, $38, $0F, $1C, $E0, $00, $3C, $E0, $60, $F1, $84
		.byte $01, $00, $F3, $8B, $03, $00, $30, $E7, $07, $04, $00, $38, $7F, $00, $00, $80
		.byte $F3, $C0, $03, $0F, $C0, $07, $1E, $86, $0F, $0C, $00, $B0, $EE, $1C, $00, $80
		.byte $73, $CE, $01, $00, $C0, $F3, $03, $00, $00, $3C, $87, $15, $68, $00, $7A, $F0
		.byte $01, $F4, $E0, $04, $80, $99, $CF, $00, $00, $B8, $32, $1F, $00, $00, $3C, $3F
		.byte $00, $00, $E0, $73, $48, $80, $01, $E0, $23, $1F, $C0, $01, $4F, $00, $1C, $FC
		.byte $02, $40, $C0, $B9, $F8, $00, $04, $E0, $F3, $10, $00, $00, $3E, $C3, $11, $04
		.byte $00, $3F, $F0, $01, $0A, $F0, $04, $A4, $E3, $0F, $40, $00, $9E, $C3, $0F, $40
		.byte $00, $3E, $3B, $00, $00, $F0, $33, $84, $01, $00, $F0, $43, $06, $20, $80, $2F
		.byte $40, $0E, $3F, $00, $04, $E1, $09, $7F, $00, $02, $F0, $8B, $4B, $00, $80, $3F
		.byte $03, $59, $00, $80, $3F, $14, $01, $80, $F8, $12, $26, $F1, $41, $40, $12, $3F
		.byte $F0, $01, $60, $00, $3F, $3E, $00, $00, $F8, $13, $98, $02, $00, $FC, $43, $14
		.byte $00, $C4, $0F, $61, $90, $87, $04, $26, $F1, $C1, $0F, $10, $07, $F8, $E1, $0B
		;.byte $00, $C0, $BF, $80, $09, $40, $C2, $3F, $E0, $01, $40, $FE, $10, $82, $18, $4C
		;.byte $72, $92, $97, $38, $06, $71, $C0, $87, $3F, $00, $00, $FC, $83, $18, $00, $24
		;.byte $FE, $03, $1E, $00, $E6, $0F, $03, $48, $60, $26, $8F, $C9, $C9, $F0, $90, $07
		;.byte $9E, $F9, $00, $00, $E2, $1F, $C8, $01, $40, $F2, $3F, $C0, $01, $20, $FF, $14
		;.byte $80, $00, $67, $F2, $18, $9C, $00, $87, $73, $60, $CE, $23, $04, $20, $FE, $81
		;.byte $04, $00, $26, $FF, $C0, $02, $00, $F2, $1F, $01, $04, $70, $66, $0E, $C1, $05
		;.byte $38, $B4, $03, $F2, $0C, $43, $00, $E1, $B3, $18, $00, $60, $F2, $07, $0E, $00
		;.byte $30, $FF, $02, $00, $80, $4F, $E6, $08, $1E, $80, $93, $33, $00, $2F, $B8, $04
		;.byte $00, $9F, $0B, $00, $00, $66, $9F, $64, $00, $80, $F3, $1F, $40, $00, $F8, $32
		;.byte $0F, $F0, $00, $3C, $79, $03, $78, $C0, $07, $80, $78, $BE, $00, $02, $78, $F2
		;.byte $2C, $04, $00, $58, $FF, $00, $00, $C0, $2F, $E5, $00, $09, $E0, $99, $33, $80
		;.byte $03, $7D, $84, $C8, $F3, $03, $10, $80, $27, $7B, $02, $00, $A0, $FC, $17, $00
		;.byte $00, $7E, $E6, $04, $00, $00, $CE, $79, $01, $24, $D0, $0F, $00, $CC, $1F, $20
		;.byte $01, $7C, $C2, $C7, $10, $00, $CC, $BF, $01, $00, $E0, $67, $0E, $00, $00, $F0
		;.byte $3C, $27, $00, $00, $7F, $08, $20, $3C, $91, $0A, $C0, $67, $38, $82, $00, $60
		;.byte $7E, $2A, $00, $00, $6E, $25, $12, $0A
SAMP2E
		.byte $00, $00, $00, $00

		;Esus4 Saw
SAMP3S	.byte $00, $00, $50, $87, $06, $40, $10, $87, $0F, $7C, $00, $10, $3F, $00, $C0, $1F
		.byte $E9, $00, $C0, $E7, $C3, $03, $10, $00, $FC, $0F, $00, $00, $1F, $3E, $00, $FE
		.byte $11, $E0, $00, $C0, $A1, $FF, $01, $00, $80, $9F, $0F, $A0, $60, $1E, $78, $C0
		.byte $79, $38, $FC, $00, $00, $F8, $E7, $03, $80, $98, $0F, $1F, $78, $10, $30, $7F
		.byte $40, $E0, $3F, $F0, $00, $40, $F7, $E1, $07, $80, $00, $F4, $0F, $08, $00, $3F
		.byte $3C, $00, $FC, $71, $C0, $01, $80, $C1, $FF, $02, $00, $00, $9F, $0F, $C0, $20
		.byte $1E, $68, $80, $74, $1C, $FE, $00, $00, $F8, $CF, $01, $00, $00, $87, $0F, $3C
		.byte $04, $20, $3F, $00, $C0, $3F, $61, $00, $00, $E7, $C5, $07, $00, $01, $F8, $0F
		.byte $00, $00, $7E, $18, $00, $F8, $39, $C0, $04, $00, $80, $FF, $01, $00, $00, $9F
		.byte $07, $80, $4C, $2A, $20, $01, $98, $39, $F3, $00, $00, $F8, $E7, $01, $00, $12
		.byte $73, $4E, $38, $01, $60, $3E, $00, $C0, $3F, $21, $00, $00, $E7, $9C, $03, $00
		.byte $00, $F0, $0F, $80, $00, $7C, $08, $01, $F8, $39, $84, $04, $00, $88, $FF, $05
		.byte $00, $00, $DD, $43, $80, $79, $44, $20, $01, $90, $27, $CF, $00, $00, $F0, $E7
		.byte $00, $00, $06, $71, $CE, $70, $01, $A0, $3D, $00, $80, $7F, $18, $00, $00, $73
		.byte $3E, $83, $00, $00, $E0, $0F, $00, $81, $7C, $00, $00, $F0, $15, $0E, $00, $00
		.byte $10, $FF, $03, $00, $00, $9E, $C3, $80, $39, $C4, $01, $10, $10, $4F, $3E, $00
		.byte $00, $F0, $67, $02, $00, $04, $E0, $C9, $22, $83, $C1, $39, $00, $00, $7F, $18
		.byte $08, $00, $52, $3F, $03, $03, $00, $E0, $0F, $00, $43, $73, $21, $00, $F0, $1C
		.byte $1E, $08, $00, $20, $FE, $03, $00, $00, $CE, $C9, $00, $33, $C7, $21, $62, $00
		.byte $9F, $FC, $00, $00, $E0, $E7, $10, $00, $08, $E0, $C9, $1C, $82, $87, $73, $00
		;.byte $00, $FF, $B0, $00, $00, $22, $FE, $10, $06, $00, $C0, $1F, $00, $02, $4F, $02
		;.byte $02, $E0, $9C, $BC, $40, $00, $E0, $FC, $03, $00, $00, $9C, $8B, $14, $27, $87
		;.byte $03, $40, $00, $BE, $FC, $01, $00, $E0, $E7, $22, $00, $08, $A0, $C7, $1C, $02
		;.byte $0F, $73, $00, $00, $FE, $91, $00, $00, $62, $FE, $21, $04, $00, $C0, $1F, $20
		;.byte $06, $3F, $04, $00, $E0, $9C, $79, $00, $00, $A0, $FC, $07, $00, $00, $58, $D9
		;.byte $21, $0E, $27, $47, $40, $00, $3E, $F9, $01, $00, $C0, $73, $66, $00, $08, $C0
		;.byte $87, $39, $02, $1F, $66, $00, $20, $FE, $90, $09, $00, $40, $FC, $61, $08, $00
		;.byte $C4, $1F, $E0, $0C, $3E, $04, $00, $C0, $9C, $77, $02, $00, $C0, $F9, $47, $00
		;.byte $02, $88, $B3, $C8, $1C, $66, $0E, $40, $00, $7C, $F6, $01, $00, $C0, $F3, $CC
		;.byte $00, $06, $80, $0F, $B9, $04, $5E, $4A, $00, $00, $FC, $86, $16, $00, $40, $FC
		;.byte $0B, $20, $00, $80, $1F, $C0, $0C, $3E, $8C, $00, $40, $9C, $EF, $10, $00, $C0
		;.byte $F9, $07, $20, $06, $88, $73, $89, $1D, $CE, $19, $40, $00, $7C, $EE, $01, $00
		;.byte $80, $F3, $5C, $02, $04, $80, $4F, $98, $29, $9E, $19, $00, $00, $FC, $09, $27
		;.byte $00, $00, $F4, $17, $42, $00, $80, $5F, $80, $59, $3E, $19, $00, $40, $3C, $BF
		;.byte $04, $00, $80, $F9, $17, $40, $02, $08, $E7, $C8, $39, $5C, $1B, $02, $00, $FC
		;.byte $1C, $4E, $00, $00, $DD, $34, $01
SAMP3E
		.byte $00, $00, $00, $00

		;Gmaj Saw
SAMP4S	.byte $00, $00, $6D, $31, $20, $00, $86, $1F, $E1, $00, $20, $95, $38, $FC, $60, $40
		.byte $00, $F9, $7C, $00, $00, $E0, $37, $0F, $00, $80, $1F, $93, $14, $E0, $14, $38
		.byte $FC, $41, $20, $00, $E7, $7D, $00, $00, $C4, $8F, $17, $00, $90, $4F, $19, $0E
		.byte $C0, $61, $30, $FC, $83, $00, $00, $DE, $79, $00, $00, $A4, $1F, $67, $00, $20
		.byte $E7, $31, $5C, $80, $C3, $90, $F9, $0F, $00, $00, $3E, $37, $01, $00, $80, $1F
		.byte $EE, $00, $60, $E6, $23, $7C, $00, $C9, $00, $F3, $1F, $00, $00, $FC, $26, $07
		.byte $00, $90, $0F, $E7, $00, $C0, $E4, $03, $F8, $00, $91, $00, $F2, $3F, $00, $00
		.byte $FC, $0D, $0F, $00, $92, $CF, $26, $03, $C0, $E0, $1B, $F8, $01, $78, $00, $CE
		.byte $7F, $00, $00, $F8, $1B, $1E, $00, $26, $E7, $26, $07, $C0, $E1, $0B, $F9, $03
		.byte $F0, $00, $CE, $7F, $00, $00, $F8, $0F, $1E, $00, $1C, $E6, $83, $1F, $80, $C1
		.byte $63, $F2, $07, $E0, $01, $BC, $BE, $01, $00, $E8, $1F, $1C, $00, $3C, $F2, $03
		.byte $3F, $80, $C0, $E1, $E1, $0F, $E0, $00, $7C, $BE, $01, $00, $F0, $0F, $1E, $00
		.byte $78, $E0, $07, $3F, $00, $98, $E3, $C3, $1F, $E0, $00, $7C, $DE, $03, $00, $F1
		.byte $4F, $88, $01, $F8, $C1, $07, $7F, $00, $98, $E1, $87, $1F, $40, $10, $F8, $CD
		.byte $07, $00, $F2, $E7, $08, $01, $F8, $C1, $83, $7F, $00, $68, $C0, $1F, $1F, $40
		.byte $20, $F8, $C9, $17, $00, $E2, $F3, $01, $03, $F0, $C1, $21, $FF, $01, $70, $80
		.byte $1F, $3E, $00, $20, $F0, $C3, $1F, $00, $C8, $F3, $03, $06, $E0, $84, $21, $FE
		.byte $01, $60, $00, $3F, $5C, $00, $40, $E0, $C7, $0D, $00, $88, $F9, $03, $0E, $C0
		.byte $94, $C0, $FC, $03, $00, $00, $3F, $DC, $00, $40, $E0, $87, $07, $00, $30, $EB
		.byte $03, $0E, $40, $0E, $80, $FC, $07, $00, $00, $7D, $C9, $01, $80, $C4, $97, $1A
		;.byte $00, $70, $F8, $04, $1E, $40, $1A, $80, $F3, $07, $00, $00, $7E, $87, $01, $80
		;.byte $C8, $33, $4D, $00, $E0, $78, $06, $1E, $00, $1A, $80, $E7, $0F, $00, $00, $FE
		;.byte $C4, $01, $80, $99, $7B, $28, $00, $F0, $70, $C6, $1C, $01, $1C, $80, $E7, $07
		;.byte $00, $00, $FE, $44, $01, $00, $39, $F9, $48, $00, $60, $90, $C3, $19, $02, $04
		;.byte $00, $DF, $07, $00, $00, $FC, $91, $00, $00, $71, $38, $65, $02, $60, $02, $93
		;.byte $9B, $04, $00, $00, $9F, $0F, $00, $00, $FC, $23, $00, $00, $F1, $B0, $C3, $04
		;.byte $00, $06, $99, $27, $0D, $00, $00, $7E, $07, $00, $80, $FC, $23, $00, $00, $F2
		;.byte $90, $E3, $0C, $00, $02, $31, $2F, $03, $40, $00, $FE, $42, $00, $80, $FC, $A1
		;.byte $00, $00, $66, $82, $A3, $0D, $00, $00, $28, $1F, $03, $00, $20, $FE, $12, $00
		;.byte $80, $F3, $41, $05, $00, $4E, $86, $99, $1D, $08, $20, $30, $7F, $04, $00, $40
		;.byte $FC, $13, $00, $00, $E7, $86, $07, $00, $0E, $81, $39, $3B, $10, $40, $70, $7E
		;.byte $04, $00, $80, $FC, $03, $02, $00, $EF, $84, $07, $00, $1C, $00, $79, $3A, $00
		;.byte $20, $60, $FE, $00, $00, $80, $FD, $01, $06, $00, $5F, $C0, $03, $01, $1C, $10
		;.byte $78, $72, $00, $00, $D0, $FE, $00, $00, $80, $FB, $00, $05, $00, $1F, $C8, $0B
		;.byte $13, $34, $20, $F8, $71, $00, $00, $E2, $FD, $01, $00, $80, $F7, $10, $06, $00
		;.byte $BE, $18, $19, $10, $AA, $15, $00
SAMP4E
		.byte $00, $00, $00, $00
		
		;Sonic Yeah
SAMP5S	.byte $F0, $00, $00, $F0, $0F, $00, $C0, $1F, $00, $E0, $1F, $00, $E0, $1F, $00, $E0
		.byte $1F, $00, $E0, $0F, $00, $F0, $0F, $A0, $FE, $03, $E8, $3E, $00, $C0, $1F, $00
		.byte $F0, $03, $60, $FF, $02, $40, $97, $00, $F8, $37, $00, $FE, $05, $A0, $F7, $01
		.byte $E8, $1F, $00, $EF, $05, $80, $FE, $00, $F0, $1F, $00, $BC, $01, $C0, $17, $00
		.byte $FD, $00, $E4, $4B, $90, $3E, $01, $DA, $87, $04, $78, $02, $04, $2E, $41, $E2
		.byte $02, $24, $2E, $C1, $A2, $07, $68, $5C, $80, $C5, $0D, $5A, $F8, $A0, $85, $0B
		.byte $7C, $B0, $41, $0B, $1F, $FC, $70, $C1, $07, $3E, $78, $E1, $83, $0F, $3C, $F8
		.byte $C0, $05, $0F, $7C, $E0, $81, $0B, $3E, $78, $E0, $03, $17, $7C, $E0, $C0, $0F
		.byte $1E, $F8, $C0, $83, $1F, $3C, $F8, $83, $07, $3E, $F0, $C0, $07, $1E, $7C, $C0
		.byte $87, $0F, $78, $F0, $E0, $03, $0E, $7C, $F8, $C0, $03, $7E, $F0, $C0, $01, $83
		.byte $05, $07, $10, $54, $2F, $1D, $38, $C0, $E0, $E0, $03, $06, $18, $78, $78, $C0
		.byte $03, $07, $3E, $78, $E0, $81, $03, $0F, $2C, $30, $C1, $00, $68, $4E, $34, $38
		.byte $60, $80, $07, $26, $1D, $3C, $70, $C0, $83, $07, $0F, $1C, $30, $00, $5A, $0E
		.byte $34, $30
SAMP5E
		.byte $00, $00, $00, $00
		
		;Blop B
SAMP6S	.byte $00, $1F, $80, $3F, $80, $1F, $C0, $1F, $C0, $0F, $E0, $0F, $E0, $07, $F0, $07
		.byte $F0, $03, $F8, $03, $F8, $01, $FC, $01, $1C
SAMP6E
		.byte $00, $00, $00, $00

		;Blop F#
SAMP7S	.byte $80, $03, $1E, $F8, $C0, $03, $1F, $F8, $C0, $03, $1F, $F8, $E0, $03, $1F, $78
		.byte $E0, $03, $1F, $78, $E0, $03, $1F, $7C, $E0, $01
SAMP7E
		.byte $00, $00, $00, $00

		;Blop G
SAMP8S	.byte $C0, $80, $07, $3E, $F8, $E0, $83, $0F, $3E, $F8, $E0, $83, $0F, $3E, $F8, $E0
		.byte $83, $0F, $3E, $F8, $E0, $83, $0F, $3C, $70, $00
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
