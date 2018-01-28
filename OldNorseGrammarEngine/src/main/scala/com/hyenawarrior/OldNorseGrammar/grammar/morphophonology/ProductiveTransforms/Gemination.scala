package com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.ProductiveTransforms

import com.hyenawarrior.OldNorseGrammar.grammar.phonology.{Consonant, Vowel}

/**
  * Created by HyenaWarrior on 2017.12.26..
  */
object Gemination {

  def apply(stemStr: String, suffix: String): (String, String) = suffix.toList match {

    case c :: d :: _ if Consonant.isConsonant(d) => stemStr -> suffix
    case (c @ ('t' | 'r')) :: _ if Vowel.isLong(stemStr.last) => stemStr -> (c + suffix)
    case _ => stemStr -> suffix
  }
}
