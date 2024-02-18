package com.github.windymelt.zmm.domain.model.speech

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.circe.{Decoder, Encoder, HCursor, Json}
import io.circe.parser._
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}

class MoraSpec extends AnyFlatSpec with Matchers {
  it should "JSONをパースして生成できる" in {
    val rawJson =
      "{\n  \"accent_phrases\" : [\n    {\n      \"moras\" : [\n        {\n          \"text\" : \"コ\",\n          \"consonant\" : \"k\",\n          \"consonant_length\" : 0.07424897700548172,\n          \"vowel\" : \"o\",\n          \"vowel_length\" : 0.05687778443098068,\n          \"pitch\" : 5.5947442054748535\n        },\n        {\n          \"text\" : \"ノ\",\n          \"consonant\" : \"n\",\n          \"consonant_length\" : 0.044946178793907166,\n          \"vowel\" : \"o\",\n          \"vowel_length\" : 0.08408229053020477,\n          \"pitch\" : 5.717047691345215\n        }\n      ],\n      \"accent\" : 2,\n      \"pause_mora\" : null,\n      \"is_interrogative\" : false\n    },\n    {\n      \"moras\" : [\n        {\n          \"text\" : \"ゲ\",\n          \"consonant\" : \"g\",\n          \"consonant_length\" : 0.057817280292510986,\n          \"vowel\" : \"e\",\n          \"vowel_length\" : 0.11145380139350891,\n          \"pitch\" : 5.866921901702881\n        },\n        {\n          \"text\" : \"ン\",\n          \"consonant\" : null,\n          \"consonant_length\" : null,\n          \"vowel\" : \"N\",\n          \"vowel_length\" : 0.07309161871671677,\n          \"pitch\" : 5.967981338500977\n        },\n        {\n          \"text\" : \"コ\",\n          \"consonant\" : \"k\",\n          \"consonant_length\" : 0.06376253813505173,\n          \"vowel\" : \"o\",\n          \"vowel_length\" : 0.07324860990047455,\n          \"pitch\" : 6.053856372833252\n        },\n        {\n          \"text\" : \"オ\",\n          \"consonant\" : null,\n          \"consonant_length\" : null,\n          \"vowel\" : \"o\",\n          \"vowel_length\" : 0.08164631575345993,\n          \"pitch\" : 6.044763088226318\n        },\n        {\n          \"text\" : \"ファ\",\n          \"consonant\" : \"f\",\n          \"consonant_length\" : 0.07041019946336746,\n          \"vowel\" : \"a\",\n          \"vowel_length\" : 0.09843575954437256,\n          \"pitch\" : 6.0873870849609375\n        },\n        {\n          \"text\" : \"イ\",\n          \"consonant\" : null,\n          \"consonant_length\" : null,\n          \"vowel\" : \"i\",\n          \"vowel_length\" : 0.06148599088191986,\n          \"pitch\" : 6.064660549163818\n        },\n        {\n          \"text\" : \"ル\",\n          \"consonant\" : \"r\",\n          \"consonant_length\" : 0.03721270710229874,\n          \"vowel\" : \"u\",\n          \"vowel_length\" : 0.06348922848701477,\n          \"pitch\" : 5.947973251342773\n        },\n        {\n          \"text\" : \"ワ\",\n          \"consonant\" : \"w\",\n          \"consonant_length\" : 0.04837597906589508,\n          \"vowel\" : \"a\",\n          \"vowel_length\" : 0.08619136363267899,\n          \"pitch\" : 5.654484748840332\n        }\n      ],\n      \"accent\" : 5,\n      \"pause_mora\" : null,\n      \"is_interrogative\" : false\n    },\n    {\n      \"moras\" : [\n        {\n          \"text\" : \"ゼ\",\n          \"consonant\" : \"z\",\n          \"consonant_length\" : 0.07171138375997543,\n          \"vowel\" : \"e\",\n          \"vowel_length\" : 0.11746758222579956,\n          \"pitch\" : 5.609307765960693\n        },\n        {\n          \"text\" : \"ッ\",\n          \"consonant\" : null,\n          \"consonant_length\" : null,\n          \"vowel\" : \"cl\",\n          \"vowel_length\" : 0.0601685494184494,\n          \"pitch\" : 0.0\n        },\n        {\n          \"text\" : \"ト\",\n          \"consonant\" : \"t\",\n          \"consonant_length\" : 0.053402792662382126,\n          \"vowel\" : \"o\",\n          \"vowel_length\" : 0.09768085926771164,\n          \"pitch\" : 5.996347904205322\n        },\n        {\n          \"text\" : \"エ\",\n          \"consonant\" : null,\n          \"consonant_length\" : null,\n          \"vowel\" : \"e\",\n          \"vowel_length\" : 0.08249028772115707,\n          \"pitch\" : 6.040262699127197\n        },\n        {\n          \"text\" : \"ム\",\n          \"consonant\" : \"m\",\n          \"consonant_length\" : 0.061322301626205444,\n          \"vowel\" : \"u\",\n          \"vowel_length\" : 0.08412571996450424,\n          \"pitch\" : 6.066816806793213\n        },\n        {\n          \"text\" : \"エ\",\n          \"consonant\" : null,\n          \"consonant_length\" : null,\n          \"vowel\" : \"e\",\n          \"vowel_length\" : 0.09247830510139465,\n          \"pitch\" : 6.094496726989746\n        },\n        {\n          \"text\" : \"ム\",\n          \"consonant\" : \"m\",\n          \"consonant_length\" : 0.054945819079875946,\n          \"vowel\" : \"u\",\n          \"vowel_length\" : 0.061236925423145294,\n          \"pitch\" : 6.050816535949707\n        },\n        {\n          \"text\" : \"ノ\",\n          \"consonant\" : \"n\",\n          \"consonant_length\" : 0.04614829644560814,\n          \"vowel\" : \"o\",\n          \"vowel_length\" : 0.0776258036494255,\n          \"pitch\" : 5.770895957946777\n        }\n      ],\n      \"accent\" : 6,\n      \"pause_mora\" : null,\n      \"is_interrogative\" : false\n    },\n    {\n      \"moras\" : [\n        {\n          \"text\" : \"サ\",\n          \"consonant\" : \"s\",\n          \"consonant_length\" : 0.08979356288909912,\n          \"vowel\" : \"a\",\n          \"vowel_length\" : 0.10687843710184097,\n          \"pitch\" : 5.795814037322998\n        },\n        {\n          \"text\" : \"ン\",\n          \"consonant\" : null,\n          \"consonant_length\" : null,\n          \"vowel\" : \"N\",\n          \"vowel_length\" : 0.06516235321760178,\n          \"pitch\" : 5.860093116760254\n        },\n        {\n          \"text\" : \"プ\",\n          \"consonant\" : \"p\",\n          \"consonant_length\" : 0.049476973712444305,\n          \"vowel\" : \"u\",\n          \"vowel_length\" : 0.04908725619316101,\n          \"pitch\" : 5.981357574462891\n        },\n        {\n          \"text\" : \"ル\",\n          \"consonant\" : \"r\",\n          \"consonant_length\" : 0.03534481301903725,\n          \"vowel\" : \"u\",\n          \"vowel_length\" : 0.07522799074649811,\n          \"pitch\" : 6.00290584564209\n        },\n        {\n          \"text\" : \"ゲ\",\n          \"consonant\" : \"g\",\n          \"consonant_length\" : 0.05782042443752289,\n          \"vowel\" : \"e\",\n          \"vowel_length\" : 0.11836332082748413,\n          \"pitch\" : 6.0505475997924805\n        },\n        {\n          \"text\" : \"ン\",\n          \"consonant\" : null,\n          \"consonant_length\" : null,\n          \"vowel\" : \"N\",\n          \"vowel_length\" : 0.07114559412002563,\n          \"pitch\" : 6.0701680183410645\n        },\n        {\n          \"text\" : \"コ\",\n          \"consonant\" : \"k\",\n          \"consonant_length\" : 0.05933015048503876,\n          \"vowel\" : \"o\",\n          \"vowel_length\" : 0.0743345320224762,\n          \"pitch\" : 5.9115071296691895\n        },\n        {\n          \"text\" : \"オ\",\n          \"consonant\" : null,\n          \"consonant_length\" : null,\n          \"vowel\" : \"o\",\n          \"vowel_length\" : 0.06167090684175491,\n          \"pitch\" : 5.702077865600586\n        },\n        {\n          \"text\" : \"デ\",\n          \"consonant\" : \"d\",\n          \"consonant_length\" : 0.049269743263721466,\n          \"vowel\" : \"e\",\n          \"vowel_length\" : 0.0849362164735794,\n          \"pitch\" : 5.50343132019043\n        },\n        {\n          \"text\" : \"ス\",\n          \"consonant\" : \"s\",\n          \"consonant_length\" : 0.057654738426208496,\n          \"vowel\" : \"U\",\n          \"vowel_length\" : 0.09936844557523727,\n          \"pitch\" : 0.0\n        }\n      ],\n      \"accent\" : 5,\n      \"pause_mora\" : null,\n      \"is_interrogative\" : false\n    }\n  ],\n  \"speedScale\" : 1.0,\n  \"pitchScale\" : 0.0,\n  \"intonationScale\" : 1.0,\n  \"volumeScale\" : 1.0,\n  \"prePhonemeLength\" : 0.1,\n  \"postPhonemeLength\" : 0.1,\n  \"outputSamplingRate\" : 24000,\n  \"outputStereo\" : false,\n  \"kana\" : \"コノ'/ゲンコオファ'イルワ/ゼットエムエ'ムノ/サンプルゲ'ンコオデ_ス\"\n}"

    val result = AudioQueryParser.parseJson(rawJson)
    result shouldBe a[Some[SpeechParameters]]
  }

  it should "JSONに変換できる" in {
    import io.circe.generic.auto._
    import io.circe.syntax._

    val rawJson =
      "{\n  \"accent_phrases\" : [\n    {\n      \"moras\" : [\n        {\n          \"text\" : \"コ\",\n          \"consonant\" : \"k\",\n          \"consonant_length\" : 0.07424897700548172,\n          \"vowel\" : \"o\",\n          \"vowel_length\" : 0.05687778443098068,\n          \"pitch\" : 5.5947442054748535\n        },\n        {\n          \"text\" : \"ノ\",\n          \"consonant\" : \"n\",\n          \"consonant_length\" : 0.044946178793907166,\n          \"vowel\" : \"o\",\n          \"vowel_length\" : 0.08408229053020477,\n          \"pitch\" : 5.717047691345215\n        }\n      ],\n      \"accent\" : 2,\n      \"pause_mora\" : null,\n      \"is_interrogative\" : false\n    },\n    {\n      \"moras\" : [\n        {\n          \"text\" : \"ゲ\",\n          \"consonant\" : \"g\",\n          \"consonant_length\" : 0.057817280292510986,\n          \"vowel\" : \"e\",\n          \"vowel_length\" : 0.11145380139350891,\n          \"pitch\" : 5.866921901702881\n        },\n        {\n          \"text\" : \"ン\",\n          \"consonant\" : null,\n          \"consonant_length\" : null,\n          \"vowel\" : \"N\",\n          \"vowel_length\" : 0.07309161871671677,\n          \"pitch\" : 5.967981338500977\n        },\n        {\n          \"text\" : \"コ\",\n          \"consonant\" : \"k\",\n          \"consonant_length\" : 0.06376253813505173,\n          \"vowel\" : \"o\",\n          \"vowel_length\" : 0.07324860990047455,\n          \"pitch\" : 6.053856372833252\n        },\n        {\n          \"text\" : \"オ\",\n          \"consonant\" : null,\n          \"consonant_length\" : null,\n          \"vowel\" : \"o\",\n          \"vowel_length\" : 0.08164631575345993,\n          \"pitch\" : 6.044763088226318\n        },\n        {\n          \"text\" : \"ファ\",\n          \"consonant\" : \"f\",\n          \"consonant_length\" : 0.07041019946336746,\n          \"vowel\" : \"a\",\n          \"vowel_length\" : 0.09843575954437256,\n          \"pitch\" : 6.0873870849609375\n        },\n        {\n          \"text\" : \"イ\",\n          \"consonant\" : null,\n          \"consonant_length\" : null,\n          \"vowel\" : \"i\",\n          \"vowel_length\" : 0.06148599088191986,\n          \"pitch\" : 6.064660549163818\n        },\n        {\n          \"text\" : \"ル\",\n          \"consonant\" : \"r\",\n          \"consonant_length\" : 0.03721270710229874,\n          \"vowel\" : \"u\",\n          \"vowel_length\" : 0.06348922848701477,\n          \"pitch\" : 5.947973251342773\n        },\n        {\n          \"text\" : \"ワ\",\n          \"consonant\" : \"w\",\n          \"consonant_length\" : 0.04837597906589508,\n          \"vowel\" : \"a\",\n          \"vowel_length\" : 0.08619136363267899,\n          \"pitch\" : 5.654484748840332\n        }\n      ],\n      \"accent\" : 5,\n      \"pause_mora\" : null,\n      \"is_interrogative\" : false\n    },\n    {\n      \"moras\" : [\n        {\n          \"text\" : \"ゼ\",\n          \"consonant\" : \"z\",\n          \"consonant_length\" : 0.07171138375997543,\n          \"vowel\" : \"e\",\n          \"vowel_length\" : 0.11746758222579956,\n          \"pitch\" : 5.609307765960693\n        },\n        {\n          \"text\" : \"ッ\",\n          \"consonant\" : null,\n          \"consonant_length\" : null,\n          \"vowel\" : \"cl\",\n          \"vowel_length\" : 0.0601685494184494,\n          \"pitch\" : 0.0\n        },\n        {\n          \"text\" : \"ト\",\n          \"consonant\" : \"t\",\n          \"consonant_length\" : 0.053402792662382126,\n          \"vowel\" : \"o\",\n          \"vowel_length\" : 0.09768085926771164,\n          \"pitch\" : 5.996347904205322\n        },\n        {\n          \"text\" : \"エ\",\n          \"consonant\" : null,\n          \"consonant_length\" : null,\n          \"vowel\" : \"e\",\n          \"vowel_length\" : 0.08249028772115707,\n          \"pitch\" : 6.040262699127197\n        },\n        {\n          \"text\" : \"ム\",\n          \"consonant\" : \"m\",\n          \"consonant_length\" : 0.061322301626205444,\n          \"vowel\" : \"u\",\n          \"vowel_length\" : 0.08412571996450424,\n          \"pitch\" : 6.066816806793213\n        },\n        {\n          \"text\" : \"エ\",\n          \"consonant\" : null,\n          \"consonant_length\" : null,\n          \"vowel\" : \"e\",\n          \"vowel_length\" : 0.09247830510139465,\n          \"pitch\" : 6.094496726989746\n        },\n        {\n          \"text\" : \"ム\",\n          \"consonant\" : \"m\",\n          \"consonant_length\" : 0.054945819079875946,\n          \"vowel\" : \"u\",\n          \"vowel_length\" : 0.061236925423145294,\n          \"pitch\" : 6.050816535949707\n        },\n        {\n          \"text\" : \"ノ\",\n          \"consonant\" : \"n\",\n          \"consonant_length\" : 0.04614829644560814,\n          \"vowel\" : \"o\",\n          \"vowel_length\" : 0.0776258036494255,\n          \"pitch\" : 5.770895957946777\n        }\n      ],\n      \"accent\" : 6,\n      \"pause_mora\" : null,\n      \"is_interrogative\" : false\n    },\n    {\n      \"moras\" : [\n        {\n          \"text\" : \"サ\",\n          \"consonant\" : \"s\",\n          \"consonant_length\" : 0.08979356288909912,\n          \"vowel\" : \"a\",\n          \"vowel_length\" : 0.10687843710184097,\n          \"pitch\" : 5.795814037322998\n        },\n        {\n          \"text\" : \"ン\",\n          \"consonant\" : null,\n          \"consonant_length\" : null,\n          \"vowel\" : \"N\",\n          \"vowel_length\" : 0.06516235321760178,\n          \"pitch\" : 5.860093116760254\n        },\n        {\n          \"text\" : \"プ\",\n          \"consonant\" : \"p\",\n          \"consonant_length\" : 0.049476973712444305,\n          \"vowel\" : \"u\",\n          \"vowel_length\" : 0.04908725619316101,\n          \"pitch\" : 5.981357574462891\n        },\n        {\n          \"text\" : \"ル\",\n          \"consonant\" : \"r\",\n          \"consonant_length\" : 0.03534481301903725,\n          \"vowel\" : \"u\",\n          \"vowel_length\" : 0.07522799074649811,\n          \"pitch\" : 6.00290584564209\n        },\n        {\n          \"text\" : \"ゲ\",\n          \"consonant\" : \"g\",\n          \"consonant_length\" : 0.05782042443752289,\n          \"vowel\" : \"e\",\n          \"vowel_length\" : 0.11836332082748413,\n          \"pitch\" : 6.0505475997924805\n        },\n        {\n          \"text\" : \"ン\",\n          \"consonant\" : null,\n          \"consonant_length\" : null,\n          \"vowel\" : \"N\",\n          \"vowel_length\" : 0.07114559412002563,\n          \"pitch\" : 6.0701680183410645\n        },\n        {\n          \"text\" : \"コ\",\n          \"consonant\" : \"k\",\n          \"consonant_length\" : 0.05933015048503876,\n          \"vowel\" : \"o\",\n          \"vowel_length\" : 0.0743345320224762,\n          \"pitch\" : 5.9115071296691895\n        },\n        {\n          \"text\" : \"オ\",\n          \"consonant\" : null,\n          \"consonant_length\" : null,\n          \"vowel\" : \"o\",\n          \"vowel_length\" : 0.06167090684175491,\n          \"pitch\" : 5.702077865600586\n        },\n        {\n          \"text\" : \"デ\",\n          \"consonant\" : \"d\",\n          \"consonant_length\" : 0.049269743263721466,\n          \"vowel\" : \"e\",\n          \"vowel_length\" : 0.0849362164735794,\n          \"pitch\" : 5.50343132019043\n        },\n        {\n          \"text\" : \"ス\",\n          \"consonant\" : \"s\",\n          \"consonant_length\" : 0.057654738426208496,\n          \"vowel\" : \"U\",\n          \"vowel_length\" : 0.09936844557523727,\n          \"pitch\" : 0.0\n        }\n      ],\n      \"accent\" : 5,\n      \"pause_mora\" : null,\n      \"is_interrogative\" : false\n    }\n  ],\n  \"speedScale\" : 1.0,\n  \"pitchScale\" : 0.0,\n  \"intonationScale\" : 1.0,\n  \"volumeScale\" : 1.0,\n  \"prePhonemeLength\" : 0.1,\n  \"postPhonemeLength\" : 0.1,\n  \"outputSamplingRate\" : 24000,\n  \"outputStereo\" : false,\n  \"kana\" : \"コノ'/ゲンコオファ'イルワ/ゼットエムエ'ムノ/サンプルゲ'ンコオデ_ス\"\n}"

    val result = AudioQueryParser.parseJson(rawJson)
    result shouldBe a[Some[SpeechParameters]]

    result.get.asJson
  }
}
