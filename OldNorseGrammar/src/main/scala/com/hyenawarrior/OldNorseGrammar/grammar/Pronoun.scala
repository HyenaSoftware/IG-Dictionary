package com.hyenawarrior.OldNorseGrammar.grammar

import com.hyenawarrior.OldNorseGrammar.grammar.Case.NOMINATIVE
import com.hyenawarrior.OldNorseGrammar.grammar.GNumber.{PLURAL, SINGULAR}
import com.hyenawarrior.auxiliary.EnumLike

/**
	* Created by HyenaWarrior on 2017.03.01..
	*/
case class Pronoun(str: String, cs: Case, number: GNumber)
{
	Pronoun.add(str -> this)
}

object Pronoun extends EnumLike[Pronoun]
{
	val SG_1 			= Pronoun("Ek", NOMINATIVE, SINGULAR)
	val SG_2			= Pronoun("Þú", NOMINATIVE, SINGULAR)

	val SG_3_MASC = Pronoun("Hann", NOMINATIVE, SINGULAR)
	val SG_3_FEMN = Pronoun("Hon", NOMINATIVE, SINGULAR)
	val SG_3_NEUT = Pronoun("Þat", NOMINATIVE, SINGULAR)

	val DL_1 = Pronoun("Vit", NOMINATIVE, PLURAL)
	val DL_2 = Pronoun("Þit", NOMINATIVE, PLURAL)

	val PL_1 = Pronoun("Vér", NOMINATIVE, PLURAL)
	val PL_2 = Pronoun("Þér", NOMINATIVE, PLURAL)

	val PL_3_MASC = Pronoun("Þeir", NOMINATIVE, PLURAL)
	val PL_3_FEMN = Pronoun("Þaer", NOMINATIVE, PLURAL)
	val PL_3_NEUT = Pronoun("Þau", NOMINATIVE, PLURAL)
}