package com.hyenawarrior.OldNorseGrammar.grammar

import com.hyenawarrior.OldNorseGrammar.grammar.enums.{Case, GNumber}
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.U_Umlaut
import com.hyenawarrior.OldNorseGrammar.grammar.phonology.Vowel._

/**
  * Created by HyenaWarrior on 2018.02.02..
  */
package object nouns {

  type NounType = (GNumber, Case)
  type NounFormType = (NounType, Boolean)

  def theseCanCauseUUmlaut(str: String): Boolean = {

    val i = str.indexWhere(isVowel) match {

      case -1 => 0
      case i => i
    }

    val str2 = str substring i

    U_Umlaut.triggers.exists { str2.contains(_) }
  }
}
