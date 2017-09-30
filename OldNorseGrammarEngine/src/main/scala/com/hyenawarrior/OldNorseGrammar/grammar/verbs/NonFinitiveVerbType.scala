package com.hyenawarrior.OldNorseGrammar.grammar.verbs

import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.{VerbStem, VerbStemEnum}
import com.hyenawarrior.auxiliary.EnumLike

/**
	* Created by HyenaWarrior on 2017.04.19..
	*/
case class NonFinitiveVerbType(name: String, verbStemBase: VerbStemEnum, mood: NonFinitiveMood)
{
	NonFinitiveVerbType.add(name -> this)
}

object NonFinitiveVerbType extends EnumLike[String, NonFinitiveVerbType] {

	val INFINITIVE					= NonFinitiveVerbType("Infinitive",					VerbStemEnum.PRESENT_STEM, VerbModeEnum.INFINITIVE)
	val PRESENT_PARTICIPLE	= NonFinitiveVerbType("Present Participle", VerbStemEnum.PRESENT_STEM, VerbModeEnum.PARTICIPLE)
	val PAST_PARTICIPLE			= NonFinitiveVerbType("Past Participle",		VerbStemEnum.PERFECT_STEM, VerbModeEnum.PARTICIPLE)
}