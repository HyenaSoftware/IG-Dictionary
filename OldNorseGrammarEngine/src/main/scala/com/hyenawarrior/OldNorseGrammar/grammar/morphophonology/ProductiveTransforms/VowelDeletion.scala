package com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.ProductiveTransforms

import com.hyenawarrior.OldNorseGrammar.grammar.phonology.Vowel
import com.hyenawarrior.OldNorseGrammar.grammar.phonology.Vowel.{LONG, SHORT}

/**
  * Created by HyenaWarrior on 2017.10.23..
  */
object VowelDeletion {

  def apply(str: String): String = {

    val stream = str zip (' ' + str)

    val filteredStream = stream.filter {

      case (Vowel(prop, SHORT), Vowel(qroq, LONG)) => prop!=qroq
      case _ => true
    }

    filteredStream
      .map(_._1)
      .mkString
  }
}
