package com.hyenawarrior.OldNorseGrammar.grammar.nouns

import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.StemTransform.{FixJAugmentation, FixVAugmentation}
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.NounStemClass

/**
  * Created by HyenaWarrior on 2018.01.31..
  */
case class NounStem(rootStr: String, stemClass: NounStemClass) {

}

object NounStem {

  def fromStrRepr(stemStr: String, stemClass: NounStemClass): NounStem = {

    val thematicVowel = stemClass.thematicVowel.getOrElse("")

    val rootStr = normalizeStem(stemStr stripSuffix thematicVowel)

    NounStem(rootStr, stemClass)
  }

  private def normalizeStem(stemStr: String): String = stemStr match {

    case FixVAugmentation(fixedStemStr) => fixedStemStr
    case FixJAugmentation(fixedStemStr) => fixedStemStr
    case _ => stemStr
  }
}