package com.github.windymelt.zmm
package domain.repository

import cats.effect.IO
import com.github.windymelt.zmm.domain.model.speech.SpeechParameters

trait VoiceVoxComponent {
  type AudioQuery
  type SpeakerInfo
  def voiceVox: VoiceVox

  trait VoiceVox {
    // API
    def speakers(): IO[SpeakerInfo]
    def audioQuery(text: String, speaker: String): IO[SpeechParameters]
    def synthesis(aq: SpeechParameters, speaker: String): IO[fs2.Stream[IO, Byte]]
    // misc.
    def controlSpeed(aq: SpeechParameters, speed: String): IO[SpeechParameters]
    def registerDict(word: String, pronounce: String, lowerPoint: Int): IO[Unit]
    def getVowels(aq: SpeechParameters): IO[domain.model.VowelSeqWithDuration]
  }
}
