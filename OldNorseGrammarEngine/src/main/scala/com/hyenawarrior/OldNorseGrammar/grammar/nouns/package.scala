package com.hyenawarrior.OldNorseGrammar.grammar

import com.hyenawarrior.OldNorseGrammar.grammar.enums.{Case, GNumber}
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.{U_Umlaut, Umlaut, V_Umlaut}
import com.hyenawarrior.OldNorseGrammar.grammar.phonology.Vowel._

/**
  * Created by HyenaWarrior on 2018.02.02..
  */
package object nouns {

  type NounType = (GNumber, Case)
  type NounFormType = (NounType, Boolean)

  def theseCanCauseUUmlaut(str: String): Option[Umlaut] = {

    val i = str.indexWhere(isVowel) match {

      case -1 => 0
      case i => i
    }

    (str substring i).find(c => "uv".contains(c)).map {

      case 'u' => U_Umlaut
      case 'v' => V_Umlaut
    }
  }
}
