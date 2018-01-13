package com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.ProductiveTransforms

import com.hyenawarrior.OldNorseGrammar.grammar.phonology.Vowel
import com.hyenawarrior.OldNorseGrammar.grammar.phonology.Vowel.isLong

/**
  * Created by HyenaWarrior on 2017.12.27..
  */
object StressShift {

  def apply(word: String, suffixLength: Int): (String, String) = {

    val stemLength = word.length - suffixLength
    val (stemStr, suffix) = word splitAt stemLength

    (stemStr.last, suffix.headOption) match {

      case ('é', Some(v)) if Vowel.isBackVowel(v) =>
        val newStemStr = stemStr.stripSuffix("é").concat("j")
        val newSuffix = (Vowel lengthen v) + suffix.tail
        (newStemStr + newSuffix) -> newSuffix

      case _ => word -> suffix
    }
  }

  def unapply(stemAndSuffixLength: (String, Int)): Option[(String, String)] = {

    val (stem, suffixLength) = stemAndSuffixLength

    val stemLength = stem.length - suffixLength
    val (modifiedStem, suffix) = stem splitAt stemLength

    (modifiedStem.last, suffix.headOption) match {

      case ('j', Some(v)) if isLong(v) =>
        val prevStem = modifiedStem.stripSuffix("j").concat("é")
        val prevSuffix = (Vowel shorten v) + suffix.tail
        Some((prevStem concat prevSuffix) -> prevSuffix)

      case _ => None
    }
  }
}
