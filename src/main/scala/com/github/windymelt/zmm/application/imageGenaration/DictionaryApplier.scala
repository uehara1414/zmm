package com.github.windymelt.zmm.application.imageGenaration

import cats.effect.IO
import com.github.windymelt.zmm.{domain, infrastructure, util}

class DictionaryApplier
    extends domain.repository.VoiceVoxComponent
    with infrastructure.VoiceVoxComponent
    with util.UtilComponent {

  val voiceVoxUri =
    sys.env.get("VOICEVOX_URI") getOrElse config.getString("voicevox.apiUri")

  def voiceVox: VoiceVox = new ConcreteVoiceVox(voiceVoxUri)

  /** 辞書要素を反映させる。
   *
   * 今のところVOICEVOX用の発音辞書に登録を行うだけだが、今後の開発によってはその他の音声合成ソフトウェアの辞書登録に使ってよい。
   *
   * @param ctx
   * 辞書を取り出す元となるコンテキスト
   * @return
   * 有用な情報は返されない
   */
  def execute(dict: Seq[(String, String, Int)]): IO[Unit] = {
    val registerList = dict.map { d =>
      voiceVox.registerDict(d._1, d._2, d._3)
    }
    registerList.reduceLeft[IO[Unit]] { case (acc, i) => i >> acc }
  }
}
