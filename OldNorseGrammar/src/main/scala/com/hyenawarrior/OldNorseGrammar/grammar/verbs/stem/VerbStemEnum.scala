package com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem

import com.hyenawarrior.auxiliary.EnumLike


/**
	* Created by HyenaWarrior on 2017.04.19..
	*/
case class VerbStemEnum(name: String)
{
	VerbStemEnum.add(name -> this)
}

/**
	* Created by HyenaWarrior on 2017.04.19..
	*/
object VerbStemEnum extends EnumLike[VerbStemEnum]
{
	val PRESENT_STEM							= VerbStemEnum("Present")
	val PRETERITE_SINGULAR_STEM		= VerbStemEnum("Preterite Singular")
	val PRETERITE_PLURAL_STEM			= VerbStemEnum("Preterite Plural")
	val PERFECT_STEM							= VerbStemEnum("Perfect")
}
