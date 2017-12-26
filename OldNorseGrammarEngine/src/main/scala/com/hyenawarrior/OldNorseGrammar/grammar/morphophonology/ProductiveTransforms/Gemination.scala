package com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.ProductiveTransforms

import com.hyenawarrior.OldNorseGrammar.grammar.phonology.Vowel

/**
  * Created by HyenaWarrior on 2017.12.26..
  */
object Gemination {

  def apply(stemStr: String, suffix: String): String = suffix.headOption match {

    case Some(c @ ('t' | 'r')) if Vowel.isLong(stemStr.last) => stemStr + c + suffix
    case _ => stemStr + suffix
  }
}
