package com.hyenawarrior.OldNorseGrammar.grammar

/**
	* Created by HyenaWarrior on 2017.03.01..
	*/
case class Pronouns(str: String, cs: Case)
{

}

object Pronouns
{
	val SG_1 = Pronouns("Ek", Case.NOMINATIVE)
	val SG_2 = Pronouns("Þú", Case.NOMINATIVE)
	val SG_3_MASC = Pronouns("Hann", Case.NOMINATIVE)
	val SG_3_FEMN = Pronouns("Hon", Case.NOMINATIVE)
	val SG_3_NEUT = Pronouns("Þat", Case.NOMINATIVE)

	val DL_1 = Pronouns("Vit", Case.NOMINATIVE)
	val DL_2 = Pronouns("Þit", Case.NOMINATIVE)

	val PL_1 = Pronouns("Vér", Case.NOMINATIVE)
	val PL_2 = Pronouns("Þér", Case.NOMINATIVE)
	val PL_3_MASC = Pronouns("Þeir", Case.NOMINATIVE)
	val PL_3_FEMN = Pronouns("Þaer", Case.NOMINATIVE)
	val PL_3_NEUT = Pronouns("Þau", Case.NOMINATIVE)
}