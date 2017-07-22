package com.hyenawarrior.OldNorseGrammar.grammar

import com.hyenawarrior.OldNorseGrammar.grammar.Case.NOMINATIVE
import com.hyenawarrior.OldNorseGrammar.grammar.GNumber.{PLURAL, SINGULAR}
import com.hyenawarrior.auxiliary.EnumLike

/**
	* Created by HyenaWarrior on 2017.03.01..
	*/
case class Pronoun(str: String, cs: Case, number: GNumber, person: String)
{
	Pronoun.add(str -> this)
}

object Pronoun extends EnumLike[String, Pronoun]
{
	val SG_1 			= Pronoun("Ek", NOMINATIVE, SINGULAR, "1st")
	val SG_2			= Pronoun("Þú", NOMINATIVE, SINGULAR, "2nd")

	val SG_3_MASC = Pronoun("Hann", NOMINATIVE, SINGULAR, "3rd")
	val SG_3_FEMN = Pronoun("Hon", 	NOMINATIVE, SINGULAR, "3rd")
	val SG_3_NEUT = Pronoun("Þat", 	NOMINATIVE, SINGULAR, "3rd")

	val DL_1 = Pronoun("Vit", NOMINATIVE, PLURAL, "1st")
	val DL_2 = Pronoun("Þit", NOMINATIVE, PLURAL, "2nd")

	val PL_1 = Pronoun("Vér", NOMINATIVE, PLURAL, "1st")
	val PL_2 = Pronoun("Þér", NOMINATIVE, PLURAL, "2nd")

	val PL_3_MASC = Pronoun("Þeir", NOMINATIVE, PLURAL, "3rd")
	val PL_3_FEMN = Pronoun("Þaer", NOMINATIVE, PLURAL, "3rd")
	val PL_3_NEUT = Pronoun("Þau", 	NOMINATIVE, PLURAL, "3rd")
}