package com.hyenawarrior.OldNorseGrammar.grammar.verbs.stemclasses

import com.hyenawarrior.auxiliary.EnumLike

/**
	* Created by HyenaWarrior on 2017.04.17..
	*/
case class VerbStemClassEnum(name: String, stemClass: Any)
{
	VerbStemClassEnum.add(name -> this)

	override def toString = name
}

object VerbStemClassEnum extends EnumLike[VerbStemClassEnum]
{
	val STRONG_1ST_CLASS = VerbStemClassEnum("Strong 1st class", null)
	val STRONG_2ND_CLASS = VerbStemClassEnum("Strong 2nd class", null)
	val STRONG_3RD_CLASS = VerbStemClassEnum("Strong 3rd class", null)
	val STRONG_4TH_CLASS = VerbStemClassEnum("Strong 4th class", null)
	val STRONG_5TH_CLASS = VerbStemClassEnum("Strong 5th class", null)
	val STRONG_6TH_CLASS = VerbStemClassEnum("Strong 6th class", null)
	val STRONG_7TH_CLASS = VerbStemClassEnum("Strong 7th class", null)

	val WEAK_A_STEM = VerbStemClassEnum("Weak A-stem", null)
	val WEAK_I_STEM = VerbStemClassEnum("Weak I-stem", null)
	val WEAK_J_STEM = VerbStemClassEnum("Weak J-stem", null)

	val IRREGULAR = VerbStemClassEnum("Irregular", null)
}