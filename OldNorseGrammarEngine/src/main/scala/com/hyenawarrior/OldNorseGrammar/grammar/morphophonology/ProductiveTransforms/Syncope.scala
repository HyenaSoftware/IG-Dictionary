package com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.ProductiveTransforms

import com.hyenawarrior.OldNorseGrammar.grammar.Syllables

/**
  * Created by HyenaWarrior on 2018.03.11..
  */
object Syncope {

  def apply(word: String): String = word match {

    case Syllables(syllables) if syllables.length > 2 =>

      val SYNCOPATED_SYLLABLE = syllables.length - 2

      syllables.zipWithIndex.flatMap {

        case (sy, SYNCOPATED_SYLLABLE) => sy.onset + sy.coda
        case (sy, _) => sy.letters

      }.mkString

    case _ => word
  }
}
