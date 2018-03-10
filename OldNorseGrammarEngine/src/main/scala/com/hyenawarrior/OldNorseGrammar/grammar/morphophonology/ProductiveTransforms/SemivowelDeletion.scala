package com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.ProductiveTransforms

import com.hyenawarrior.OldNorseGrammar.grammar.phonology.Vowel._

/**
  * Created by HyenaWarrior on 2017.10.20..
  */
object SemivowelDeletion {

  def apply(str: String): String = {

    val stream = str zip (str.tail + ' ')

    val filteredStream = stream.filter {

      case ('j' | 'v', ' ') => false	// end of the word
      case ('j', next) => isBackVowel(next)
      case ('v', next) if next != 'á' && next != 'ǫ' => isVowel(next) && !isLabialVowel(next)
      case _ => true
    }

    filteredStream
      .map(_._1)
      .mkString
  }
}
