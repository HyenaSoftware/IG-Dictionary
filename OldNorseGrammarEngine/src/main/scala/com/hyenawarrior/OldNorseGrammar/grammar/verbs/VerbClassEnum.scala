package com.hyenawarrior.OldNorseGrammar.grammar.verbs

import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.{CalculatedAblaut, StaticAblaut}
import com.hyenawarrior.auxiliary.EnumLike

/**
	* Created by HyenaWarrior on 2017.04.17..
	*/
class VerbClassEnum(name: String)
{
	VerbClassEnum.add(name -> this)

	override def toString = name
}

case class StrongVerbClassEnum(name: String) extends VerbClassEnum(name)

	//CalculatedAblaut.extractAblautFrom(verb).map(ablaut => StrongVerbClassDesc(this, ablaut))

case class WeakVerbClassEnum(name: String) extends VerbClassEnum(name)

object VerbClassEnum extends EnumLike[String, VerbClassEnum]
{
	val STRONG_1ST_CLASS = StrongVerbClassEnum("Strong 1st class")
	val STRONG_2ND_CLASS = StrongVerbClassEnum("Strong 2nd class")
	val STRONG_3RD_CLASS = StrongVerbClassEnum("Strong 3rd class")
	val STRONG_4TH_CLASS = StrongVerbClassEnum("Strong 4th class")
	val STRONG_5TH_CLASS = StrongVerbClassEnum("Strong 5th class")
	val STRONG_6TH_CLASS = StrongVerbClassEnum("Strong 6th class")
	val STRONG_7TH_CLASS = StrongVerbClassEnum("Strong 7th class")

	val WEAK_A_STEM = WeakVerbClassEnum("Weak A-stem")
	val WEAK_I_STEM = WeakVerbClassEnum("Weak I-stem")
	val WEAK_J_STEM = WeakVerbClassEnum("Weak J-stem")

	val IRREGULAR = WeakVerbClassEnum("Irregular")
}

// descriptors

class VerbClassDesc(val vClass: VerbClassEnum)
{
	def className = vClass.toString
}

case class StrongVerbClassDesc(override val vClass: StrongVerbClassEnum, ablaut: StaticAblaut) extends VerbClassDesc(vClass)