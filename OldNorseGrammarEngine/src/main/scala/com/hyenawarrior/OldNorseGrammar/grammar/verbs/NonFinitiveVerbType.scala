package com.hyenawarrior.OldNorseGrammar.grammar.verbs

import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.EnumVerbStem
import com.hyenawarrior.auxiliary.EnumLike

/**
	* Created by HyenaWarrior on 2017.04.19..
	*/
case class NonFinitiveVerbType(name: String, verbStemBase: EnumVerbStem, mood: NonFinitiveMood)
{
	NonFinitiveVerbType.add(name -> this)
}

object NonFinitiveVerbType extends EnumLike[String, NonFinitiveVerbType] {

	val INFINITIVE					= NonFinitiveVerbType("Infinitive",					EnumVerbStem.PRESENT_STEM, VerbModeEnum.INFINITIVE)
	val PRESENT_PARTICIPLE	= NonFinitiveVerbType("Present Participle", EnumVerbStem.PRESENT_STEM, VerbModeEnum.PARTICIPLE)
	val PAST_PARTICIPLE			= NonFinitiveVerbType("Past Participle",		EnumVerbStem.PERFECT_STEM, VerbModeEnum.PARTICIPLE)
}