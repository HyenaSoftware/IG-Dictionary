package com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.ProductiveTransforms

import com.hyenawarrior.OldNorseGrammar.grammar.Syllables
import com.hyenawarrior.OldNorseGrammar.grammar.phonology.Vowel

/**
  * Created by HyenaWarrior on 2017.11.05..
  */
object VowelLengthening {

  /*
      6.4 Lengthening and Shortening
      A stressed vowel in final position is long, e.g. þú. This occurs even when loss of a final consonant leaves
       a stressed vowel in final position, e.g. *vag > vá 'slew', past tense of vega 'to slay'.
     */

  def apply(word: String): String = word match {

    case Syllables(sy :: tail) if sy.isStressed && sy.coda.isEmpty =>

      val firstSyl = sy.onset + sy.nucleus.map(Vowel.lengthen) + sy.coda

      firstSyl + tail.flatMap(_.letters).mkString

    case _ => word
  }

  def unapply(word: String): Option[String] = word match {

    case Syllables(sy :: tail) if sy.isStressed && sy.coda.isEmpty =>

      val firstSyl = sy.onset + sy.nucleus.map(Vowel.shorten) + sy.coda

      Some(firstSyl + tail.flatMap(_.letters).mkString)

    case _ => Some(word)
  }
}
