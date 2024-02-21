/*
	Example DPCM Sample Converter
	For Unity, should work in any version.
	Just attach it to a gameobject, drag your wave files into the Wave
	array, then hit run and it'll spit out the (already formatted!)
	DPCM bytes into the console. Just put in linebreaks where you feel
	like, add your own .byte before each line, and you're away!
	Remember to add four 0s after each sample so it doesn't overrun:
	.byte $00, $00, $00, $00
	I'll leave comments for those not at all familiar with Unity trying
	to adapt this for something else.

	This script is public domain, use it however you want.
	Grab my mixtape: https://kulor.bandcamp.com/album/soundchip-salad
*/
using UnityEngine;				//Need this for AudioClip
using System.Collections;

public class DPCMConverter : MonoBehaviour {
	
	public AudioClip[] Wave;
	
	void Awake() {				//This is our version of main()
		foreach (var w in Wave) {
			byte[] DPCMStreamByteArray = convertWaveToByteArray(w);
			string s = "";
			int i = 0;
			foreach (byte b in DPCMStreamByteArray) {
				if (i % 16 == 0) {
					s += "\t\t.byte ";
				}
				s += $"${b.ToString("X2")}";
				if (i % 16 != 15) {
					s += ", ";
				}
				else {
					s += "\n";
				}

				i++;
			}
			Debug.Log(s);		//This will go to the console in Unity, s is your converted output
		}
	}
	
	private byte[] convertWaveToByteArray(AudioClip wave) {
		if (wave.channels > 1) {		//AudioClip.channels is 1 for mono waves
			Debug.LogError("DPCM conversion requires mono wave!");
			return null;
		}

		byte[] ret = new byte[wave.samples / 8 + (wave.samples % 8 == 0 ? 0 : 1)];
		float[] waveData = new float[wave.samples];		//AudioClip.samples is the length of the sound
		wave.GetData(waveData, 0);	//AudioClip.GetData(out float[] container, int offset)
									//This will populate waveData with samples ranging from -1f to 1f
		int deltaCounter = 8;
		int bitMaskPosition = 0;
		int bytePos = 0;
		int maxAmp = 0;
		int minAmp = 128;
		for (int i = 0; i < waveData.Length; ++i) {
			if (deltaCounter < (waveData[i] * 16.0f)) {
				//set a 1
				switch (bitMaskPosition) {
				case 0:
					ret[bytePos] = (byte)(ret[bytePos] | 1);
					++bitMaskPosition;
					break;
				case 1:
					ret[bytePos] = (byte)(ret[bytePos] | 2);
					++bitMaskPosition;
					break;
				case 2:
					ret[bytePos] = (byte)(ret[bytePos] | 4);
					++bitMaskPosition;
					break;
				case 3:
					ret[bytePos] = (byte)(ret[bytePos] | 8);
					++bitMaskPosition;
					break;
				case 4:
					ret[bytePos] = (byte)(ret[bytePos] | 16);
					++bitMaskPosition;
					break;
				case 5:
					ret[bytePos] = (byte)(ret[bytePos] | 32);
					++bitMaskPosition;
					break;
				case 6:
					ret[bytePos] = (byte)(ret[bytePos] | 64);
					++bitMaskPosition;
					break;
				case 7:
					ret[bytePos] = (byte)(ret[bytePos] | 128);
					bitMaskPosition = 0;
					++bytePos;
					break;
				}
				++deltaCounter;
				if (deltaCounter > 15) deltaCounter = 15;
				if (deltaCounter > maxAmp) maxAmp = deltaCounter;
			}
			else {
				//0 by default
				switch (bitMaskPosition) {
				case 0:
				case 1:
				case 2:
				case 3:
				case 4:
				case 5:
				case 6:
					++bitMaskPosition;
					break;
				case 7:
					bitMaskPosition = 0;
					++bytePos;
					break;
				}
				--deltaCounter;
				if (deltaCounter < 0) deltaCounter = 0;
				if (deltaCounter < minAmp) minAmp = deltaCounter;
			}
		}

		Debug.Log (string.Format("DPCM Wave Converter: Max amplitude is {0}, min amplitude is {1}", maxAmp, minAmp));

		return ret;
	}
}
