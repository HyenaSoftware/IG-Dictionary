package com.hyenawarrior.OldNorseGrammar.grammar.verbs

import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.StaticAblaut
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.enum.EnumVerbStem
import com.hyenawarrior.auxiliary.enum.{EnumConst, EnumLike}

/**
  * Created by HyenaWarrior on 2017.04.19..
  */
object enums {

  //
  case class NonFinitiveVerbType(name: String, verbStemBase: EnumVerbStem, mood: NonFinitiveMood) extends EnumConst[NonFinitiveVerbType]

  implicit object NonFinitiveVerbType extends EnumLike[NonFinitiveVerbType] {

    val INFINITIVE					= NonFinitiveVerbType("Infinitive",					EnumVerbStem.PRESENT_STEM, VerbModeEnum.INFINITIVE)
    val PRESENT_PARTICIPLE	= NonFinitiveVerbType("Present Participle", EnumVerbStem.PRESENT_STEM, VerbModeEnum.PARTICIPLE)
    val PAST_PARTICIPLE			= NonFinitiveVerbType("Past Participle",		EnumVerbStem.PERFECT_STEM, VerbModeEnum.PARTICIPLE)
  }

  //
  case class VerbTenseEnum(name: String) extends EnumConst[VerbTenseEnum]

  implicit object VerbTenseEnum extends EnumLike[VerbTenseEnum] {

    val PRESENT = VerbTenseEnum("Present")
    val PAST = VerbTenseEnum("Past")
  }

  //
  implicit object VerbModeEnum extends EnumLike[VerbModeEnum] {

    val INDICATIVE	=	FinitiveMood("Indicative")
    val SUBJUNCTIVE	= FinitiveMood("Subjunctive")
    val IMPERATIVE	= FinitiveMood("Imperative")
    val INFINITIVE 	= NonFinitiveMood("Infinitive")
    val PARTICIPLE 	= NonFinitiveMood("Participle")
  }

  class VerbModeEnum(val name: String) extends EnumConst[VerbModeEnum]

  case class FinitiveMood(override val name: String) 		extends VerbModeEnum(name)
  case class NonFinitiveMood(override val name: String) extends VerbModeEnum(name)

  //
  case class VerbVoice(name: String) extends EnumConst[VerbVoice]

  implicit object VerbVoice extends EnumLike[VerbVoice] {

    val ACTIVE = VerbVoice("Active")
    val MEDIO_PASSIVE = VerbVoice("Mediopassive")
  }

  //
  class VerbClassEnum(name: String) extends EnumConst[VerbClassEnum] {

    override def toString = name
  }

  case class StrongVerbClassEnum(name: String) extends VerbClassEnum(name)

  case class WeakVerbClassEnum(name: String) extends VerbClassEnum(name)

  implicit object VerbClassEnum extends EnumLike[VerbClassEnum]	{

    val STRONG_1ST_CLASS = StrongVerbClassEnum("Strong 1st class")
    val STRONG_2ND_CLASS = StrongVerbClassEnum("Strong 2nd class")
    val STRONG_3RD_CLASS = StrongVerbClassEnum("Strong 3rd class")
    val STRONG_4TH_CLASS = StrongVerbClassEnum("Strong 4th class")
    val STRONG_5TH_CLASS = StrongVerbClassEnum("Strong 5th class")
    val STRONG_6TH_CLASS = StrongVerbClassEnum("Strong 6th class")

    val STRONG_7_1_CLASS = StrongVerbClassEnum("Strong 7.1 class")
    val STRONG_7_2A_CLASS = StrongVerbClassEnum("Strong 7.2a class")
    val STRONG_7_2B_CLASS = StrongVerbClassEnum("Strong 7.2b class")
    val STRONG_7_3_CLASS = StrongVerbClassEnum("Strong 7.3 class")
    val STRONG_7_4_CLASS = StrongVerbClassEnum("Strong 7.4 class")
    val STRONG_7_5_CLASS = StrongVerbClassEnum("Strong 7.5 class")

    val WEAK_A_STEM = WeakVerbClassEnum("Weak A-stem")
    val WEAK_I_STEM = WeakVerbClassEnum("Weak I-stem")
    val WEAK_J_STEM = WeakVerbClassEnum("Weak J-stem")

    val IRREGULAR = WeakVerbClassEnum("Irregular")
  }

  class VerbClassDesc(val vClass: VerbClassEnum) {

    def className = vClass.toString
  }

  case class StrongVerbClassDesc(override val vClass: StrongVerbClassEnum, ablaut: StaticAblaut) extends VerbClassDesc(vClass)
}
