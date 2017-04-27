package com.hyenawarrior.OldNorseGrammar.grammar.verbs

import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.Ablaut
import com.hyenawarrior.auxiliary.EnumLike

/**
	* Created by HyenaWarrior on 2017.04.17..
	*/
case class VerbClassEnum(name: String, ablaut: Ablaut)
{
	VerbClassEnum.add(name -> this)

	override def toString = name
}

object VerbClassEnum extends EnumLike[VerbClassEnum]
{
	val STRONG_1ST_CLASS = VerbClassEnum("Strong 1st class", Ablaut("í",	"ei", "i", "i"))
	val STRONG_2ND_CLASS = VerbClassEnum("Strong 2nd class", Ablaut("jú", "au", "u", "o"))
	val STRONG_3RD_CLASS = VerbClassEnum("Strong 3rd class", Ablaut("e", 	"a", 	"u", "o"))
	val STRONG_4TH_CLASS = VerbClassEnum("Strong 4th class", Ablaut("e",	"a", 	"á", "o"))
	val STRONG_5TH_CLASS = VerbClassEnum("Strong 5th class", Ablaut("e", 	"a", 	"á", "e"))
	val STRONG_6TH_CLASS = VerbClassEnum("Strong 6th class", Ablaut("a", 	"ó", 	"ó", "a"))
	val STRONG_7TH_CLASS = VerbClassEnum("Strong 7th class", null)

	val WEAK_A_STEM = VerbClassEnum("Weak A-stem", null)
	val WEAK_I_STEM = VerbClassEnum("Weak I-stem", null)
	val WEAK_J_STEM = VerbClassEnum("Weak J-stem", null)

	val IRREGULAR = VerbClassEnum("Irregular", null)
}