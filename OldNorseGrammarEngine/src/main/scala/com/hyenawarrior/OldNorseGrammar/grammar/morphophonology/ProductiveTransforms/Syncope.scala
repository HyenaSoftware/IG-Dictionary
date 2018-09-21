package com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.ProductiveTransforms

import com.hyenawarrior.OldNorseGrammar.grammar.phonology.Vowel.isVowel
import com.hyenawarrior.OldNorseGrammar.grammar.{Syllable, Syllables}

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

  def transform(word: String, suffix: String): String = {

    val sys = Syllables.syllablesOf(word)
    val inflectionCanTriggerSyncope = suffix.headOption.exists(isVowel)

    if(inflectionCanTriggerSyncope) {

      sys.reverse match {

        case sy1 :: sy2 :: sy3 :: other =>
          val sys2 = sy1 :: Syllable(sy2.onset, "", sy2.coda, sy2.isStressed, sy2.length) :: sy3 :: other

          sys2.reverse.map(_.letters).mkString

        case _ => word
      }

    } else { word }
  }

  def adjustSyncopatedInflection(str: String): String = {

    val i = str.lastIndexWhere(isVowel)

    // is there any inflectional ending with 3 vowels ???

    if(i != -1 ) {

      val j = str.lastIndexWhere(isVowel, i - 1)

      if(j != -1) {

        val (p1, p2) = str.splitAt(j)

        p1 + p2.substring(1)

      } else str

    } else str
  }
}
