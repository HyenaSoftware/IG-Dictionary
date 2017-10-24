package com.hyenawarrior.OldNorseGrammar.grammar

import com.hyenawarrior.OldNorseGrammar.grammar.Case.NOMINATIVE
import com.hyenawarrior.OldNorseGrammar.grammar.GNumber.{PLURAL, SINGULAR}
import com.hyenawarrior.auxiliary.EnumLike

/**
	* Created by HyenaWarrior on 2017.03.01..
	*/
case class Pronoun(number: GNumber, person: Int)
{
	Pronoun.add((number, person) -> this)

  override def toString: String = s"$ordinal person $number"

  private def ordinal = person match {

    case 1 => "1st"
    case 2 => "2nd"
    case 3 => "3rd"
  }
}

object Pronoun extends EnumLike[(GNumber, Int), Pronoun]
{
	val SG_1 = Pronoun(SINGULAR, 1)
	val SG_2 = Pronoun(SINGULAR, 2)
	val SG_3 = Pronoun(SINGULAR, 3)

	val PL_1 = Pronoun(PLURAL, 1)
	val PL_2 = Pronoun(PLURAL, 2)
	val PL_3 = Pronoun(PLURAL, 3)
}