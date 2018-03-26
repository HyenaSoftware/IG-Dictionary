package com.hyenawarrior.OldNorseGrammar.grammar

/**
  * Created by HyenaWarrior on 2018.03.25..
  */
package object morphophonology {

  /*
    tré + i > tr[éi] > tré
    á + ar > [áa]r > ár
    á + um > [áu]m > ám
    trú + um > tr[úu]m > trúm
   */

  def endsWith(word: String, suffix: String): Boolean = word endsWith adjustedSuffixFrom(word, suffix)

  def stripSuffix(word: String, suffix: String): String = word stripSuffix adjustedSuffixFrom(word, suffix)

  private def adjustedSuffixFrom(word: String, suffix: String): String = {

    if(suffix.isEmpty) return ""

    val mergedVowelIndex = word.length - suffix.length
    val mergedVowel = word charAt mergedVowelIndex

    (mergedVowel, suffix.headOption) match {

      case ('é', Some('i'))
         | ('á', Some('a' | 'u'))
         | ('ú', Some('u')) => suffix.tail
      case _ => suffix
    }
  }
}
