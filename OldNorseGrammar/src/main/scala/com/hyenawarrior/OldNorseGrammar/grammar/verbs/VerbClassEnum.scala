package com.hyenawarrior.OldNorseGrammar.grammar.verbs

import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stemclasses.VerbStemClass
import com.hyenawarrior.auxiliary.EnumLike

/**
	* Created by HyenaWarrior on 2017.04.17..
	*/
case class VerbClassEnum(name: String, a: VerbStemClass)
{
	VerbClassEnum.add(name -> this)

	override def toString = name
}

object VerbClassEnum extends EnumLike[VerbClassEnum]
{
	val STRONG_1ST_CLASS = VerbClassEnum("Strong 1st class", null)
	val STRONG_2ND_CLASS = VerbClassEnum("Strong 2nd class", null)
	val STRONG_3RD_CLASS = VerbClassEnum("Strong 3rd class", null)
	val STRONG_4TH_CLASS = VerbClassEnum("Strong 4th class", null)
	val STRONG_5TH_CLASS = VerbClassEnum("Strong 5th class", null)
	val STRONG_6TH_CLASS = VerbClassEnum("Strong 6th class", null)
	val STRONG_7TH_CLASS = VerbClassEnum("Strong 7th class", null)

	val WEAK_A_STEM = VerbClassEnum("Weak A-stem", null)
	val WEAK_I_STEM = VerbClassEnum("Weak I-stem", null)
	val WEAK_J_STEM = VerbClassEnum("Weak J-stem", null)

	val IRREGULAR = VerbClassEnum("Irregular", null)
}