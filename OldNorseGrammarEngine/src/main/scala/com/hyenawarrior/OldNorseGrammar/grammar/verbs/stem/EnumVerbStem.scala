package com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem

import com.hyenawarrior.auxiliary.EnumLike


/**
	* Created by HyenaWarrior on 2017.04.19..
	*/
case class EnumVerbStem(name: String)
{
	EnumVerbStem.add(name -> this)
}

/**
	* Created by HyenaWarrior on 2017.04.19..
	*/
object EnumVerbStem extends EnumLike[String, EnumVerbStem]
{
	val PRESENT_STEM							= EnumVerbStem("Present")
	val PRETERITE_SINGULAR_STEM		= EnumVerbStem("Preterite Singular")
	val PRETERITE_PLURAL_STEM			= EnumVerbStem("Preterite Plural")
	val PERFECT_STEM							= EnumVerbStem("Perfect")
}
