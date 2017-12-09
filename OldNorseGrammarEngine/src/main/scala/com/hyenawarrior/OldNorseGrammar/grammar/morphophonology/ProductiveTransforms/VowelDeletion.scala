package com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.ProductiveTransforms

import com.hyenawarrior.OldNorseGrammar.grammar.phonology.Vowel
import com.hyenawarrior.OldNorseGrammar.grammar.phonology.Vowel.{LONG, SHORT, isBackVowel, isFrontVowel, isLabialVowel, isLong, isLow}

/**
  * Created by HyenaWarrior on 2017.10.23..
  */
object VowelDeletion {

  def apply(str: String): String = {

    val stream = (' ' +: str) zip str

    val filteredStream = stream.filterNot {

      case (p, 'i') => isLong(p) && isFrontVowel(p)
      case (p, 'a') => isBackVowel(p) && isLow(p)
      case (p, 'u') => isBackVowel(p) && isLabialVowel(p)
      case (Vowel(prev, LONG), Vowel(curr, SHORT)) => curr==prev
      case _ => false
    }

    filteredStream
      .map(_._2)
      .mkString
  }
}
