package com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses

import com.hyenawarrior.OldNorseGrammar.grammar.enums.Case._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.GNumber.SINGULAR
import com.hyenawarrior.OldNorseGrammar.grammar.enums.{Case, GNumber}
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.Explicit_I_Umlaut
import com.hyenawarrior.OldNorseGrammar.grammar.nouns._

/**
  * Created by HyenaWarrior on 2017.07.28..
  *
  * The a1-class has a long root syllable with i-umlaut.
  */
object StrongStemClassFeminineA1 extends StrongStemClassFeminineA
{
  override def toString = "Strong feminine A1-class"

  override def transformationFor(decl: NounType) = Some(Explicit_I_Umlaut)

  override def inflection(decl: (GNumber, Case)) = decl match
  {
    case (SINGULAR, NOMINATIVE) => "r"
    case (SINGULAR, ACCUSATIVE | DATIVE)	=> "i"
    case _ => super.inflection(decl)
  }
}
