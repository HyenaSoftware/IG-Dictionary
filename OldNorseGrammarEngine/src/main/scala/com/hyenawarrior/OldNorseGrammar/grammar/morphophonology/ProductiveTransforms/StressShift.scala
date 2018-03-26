package com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.ProductiveTransforms

import java.lang.Math.max

import com.hyenawarrior.OldNorseGrammar.grammar.phonology.Vowel
import com.hyenawarrior.OldNorseGrammar.grammar.phonology.Vowel.{isBackVowel, isLong}

/**
  * Created by HyenaWarrior on 2017.12.27..
  */
object StressShift {

  def apply(word: String, suffixLength: Int): (String, String) = {

    val stemLength = word.length - suffixLength
    val (stemStr, suffix) = word splitAt stemLength

    (stemStr.last, suffix.headOption) match {

      case ('é', Some(v)) if isBackVowel(v) =>
        val newStemStr = stemStr.stripSuffix("é").concat("j")
        val newSuffix = (Vowel lengthen v) + suffix.tail
        (newStemStr + newSuffix) -> newSuffix

      case _ => word -> suffix
    }
  }

  /**
    * éV > jáV
    *
    * @param word
    * @return
    */
  def apply(word: String): Option[String] = {

    val indexOfE = max(0, word.indexWhere(c => c == 'e' || c == 'é'))

    if(indexOfE < word.length - 1) {

      val c1 = word charAt indexOfE
      val c2 = word charAt (indexOfE + 1)

      (c1, c2) match {

        case (('e' | 'é'), w) if isBackVowel(w) =>
          Some(word.substring(0, max(0, indexOfE)) + "já" + word.substring(indexOfE + 1))

        case _ => None
      }

    } else None
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

  /**
    * jV > éV
    *
    * Be aware apply and unnapply are not simmetric!
    *
    * apply:   knéum > knjáum (> knjám; it's done by VD)  éu > jáu (instead of éu > jú)
    * unapply: knjám > knéam                              já > éa
    *
    * @param word
    * @return
    */
  def unapply(word: String): Option[String] = word.indexOf('j') match {

    case -1 => None
    case indexOfJ if word.length < indexOfJ + 2 => None

    case indexOfJ =>
      val nextChar = word.charAt(indexOfJ + 1)

      if(isLong(nextChar)) {

        val prefix = word.substring(0, indexOfJ)
        val shortenedVowel = Vowel shorten nextChar
        val suffix = word.substring(indexOfJ + 2) // ignore the 'j' and the vowel after that
        Some(prefix + "é" + shortenedVowel + suffix)

      } else None
  }
}
