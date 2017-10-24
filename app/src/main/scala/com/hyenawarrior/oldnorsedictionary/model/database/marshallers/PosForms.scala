package com.hyenawarrior.oldnorsedictionary.model.database.marshallers

import com.hyenawarrior.OldNorseGrammar.grammar.GNumber.{PLURAL, SINGULAR}
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.NounStemClassEnum
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.{VerbClassEnum, VerbModeEnum, VerbTenseEnum}
import com.hyenawarrior.OldNorseGrammar.grammar.{Case, GNumber, Pronoun}
import com.hyenawarrior.auxiliary.EnumLike


/**
  * Created by HyenaWarrior on 2017.05.27..
  */
trait PosType
{
	val id: Int

	PosType.add(id -> this)
}

trait PosForm
{
	val id: Int
}

case class NounForm(id: Int, number: GNumber, caze: Case) extends PosForm { NounForm.add(id -> this) }
case class VerbForm(id: Int, vtype: (VerbModeEnum, Option[VerbTenseEnum], Option[Pronoun])) extends PosForm {

  VerbForm.add(id -> this)
}

case class VerbType(id: Int, verbClass: VerbClassEnum) extends PosType {

	override def toString: String = verbClass.toString
}

case class NounType(id: Int, stemClass: NounStemClassEnum) extends PosType {

	override def toString: String = stemClass.name
}

case class AdjectiveType(id: Int, text: String) extends PosType

// subview of PosType enum
object VerbType
{
	lazy val verbs = PosType.values
		.flatMap
		{
			case e: VerbType => Some(e.asInstanceOf[VerbType])
			case _ => None
		}
		.map(e => e.verbClass -> e).toMap

	def findByVerbClass(verbClassEnum: VerbClassEnum) = verbs(verbClassEnum)
}

object PosType extends EnumLike[Int, PosType]
{
	// Verbs
	// - strong verbs
	val VERB_STRONG_1ST = VerbType(0, VerbClassEnum.STRONG_1ST_CLASS)
	val VERB_STRONG_2ND = VerbType(1, VerbClassEnum.STRONG_2ND_CLASS)
	val VERB_STRONG_3RD = VerbType(2, VerbClassEnum.STRONG_3RD_CLASS)
	val VERB_STRONG_4TH = VerbType(3, VerbClassEnum.STRONG_4TH_CLASS)
	val VERB_STRONG_5TH = VerbType(4, VerbClassEnum.STRONG_5TH_CLASS)
	val VERB_STRONG_6TH = VerbType(5, VerbClassEnum.STRONG_6TH_CLASS)
	val VERB_STRONG_7TH = VerbType(6, VerbClassEnum.STRONG_7TH_CLASS)

	// - weak verbs

	// Nouns
	// - strong nouns
	val NOUN_STRONG_FEM_A2 = NounType(10, NounStemClassEnum.STRONG_FEMININE_A1)
	val NOUN_STRONG_FEM_A1 = NounType(23, NounStemClassEnum.STRONG_FEMININE_A2)
	val NOUN_STRONG_FEM_I = NounType(11, NounStemClassEnum.STRONG_FEMININE_I)
	val NOUN_STRONG_FEM_R = NounType(12, NounStemClassEnum.STRONG_FEMININE_R)

	val NOUN_STRONG_MASC_A = NounType(13, NounStemClassEnum.STRONG_MASCULINE_A)
	val NOUN_STRONG_MASC_I = NounType(14, NounStemClassEnum.STRONG_MASCULINE_I)
	val NOUN_STRONG_MASC_U = NounType(15, NounStemClassEnum.STRONG_MASCULINE_U)
	val NOUN_STRONG_MASC_R = NounType(16, NounStemClassEnum.STRONG_MASCULINE_R)

	val NOUN_STRONG_NEUT = NounType(17, NounStemClassEnum.STRONG_NEUTER)

	// - weak nouns
	val NOUN_WEAK_FEM_I = NounType(18, NounStemClassEnum.WEAK_FEMININE_I)
	val NOUN_WEAK_FEM_U = NounType(19, NounStemClassEnum.WEAK_FEMININE_U)

	val NOUN_WEAK_MASC_A = NounType(20, NounStemClassEnum.WEAK_MASCULINE_A)
	val NOUN_WEAK_MASC_R = NounType(21, NounStemClassEnum.WEAK_MASCULINE_R)

	val NOUN_WEAK_NEUT_U = NounType(22, NounStemClassEnum.WEAK_NEUTER_U)

	// Adjectives
	val ADJECTIVE = AdjectiveType(30, "Adjective")
}

object NounType
{
	lazy val nouns = PosType.values
		.collect
		{
			case e: NounType => e.asInstanceOf[NounType]
		}
		.map(e => e.stemClass -> e).toMap

	def findByVerbClass(nounStemClassEnum: NounStemClassEnum) = nouns(nounStemClassEnum)
}



object VerbForm extends EnumLike[Int, VerbForm]
{
	// Verbs, it could be split by verb-tense
	// - PRESENT
	val VERB_INDICATIVE_PRESENT_1ST_SG = VerbForm(0, (VerbModeEnum.INDICATIVE, Some(VerbTenseEnum.PRESENT), Some(Pronoun.SG_1)))
	val VERB_INDICATIVE_PRESENT_2ND_SG = VerbForm(1, (VerbModeEnum.INDICATIVE, Some(VerbTenseEnum.PRESENT), Some(Pronoun.SG_2)))
	val VERB_INDICATIVE_PRESENT_3RD_SG = VerbForm(2, (VerbModeEnum.INDICATIVE, Some(VerbTenseEnum.PRESENT), Some(Pronoun.SG_3)))

	val VERB_INDICATIVE_PRESENT_1ST_PL = VerbForm(3, (VerbModeEnum.INDICATIVE, Some(VerbTenseEnum.PRESENT), Some(Pronoun.PL_1)))
	val VERB_INDICATIVE_PRESENT_2ND_PL = VerbForm(4, (VerbModeEnum.INDICATIVE, Some(VerbTenseEnum.PRESENT), Some(Pronoun.PL_2)))
	val VERB_INDICATIVE_PRESENT_3RD_PL = VerbForm(5, (VerbModeEnum.INDICATIVE, Some(VerbTenseEnum.PRESENT), Some(Pronoun.PL_3)))

	val VERB_SUBJUNCTIVE_PRESENT_1ST_SG = VerbForm(6, (VerbModeEnum.SUBJUNCTIVE, Some(VerbTenseEnum.PRESENT), Some(Pronoun.SG_1)))
	val VERB_SUBJUNCTIVE_PRESENT_2ND_SG = VerbForm(7, (VerbModeEnum.SUBJUNCTIVE, Some(VerbTenseEnum.PRESENT), Some(Pronoun.SG_2)))
	val VERB_SUBJUNCTIVE_PRESENT_3RD_SG = VerbForm(8, (VerbModeEnum.SUBJUNCTIVE, Some(VerbTenseEnum.PRESENT), Some(Pronoun.SG_3)))

	val VERB_SUBJUNCTIVE_PRESENT_1ST_PL = VerbForm(9, (VerbModeEnum.SUBJUNCTIVE, Some(VerbTenseEnum.PRESENT), Some(Pronoun.PL_1)))
	val VERB_SUBJUNCTIVE_PRESENT_2ND_PL = VerbForm(10, (VerbModeEnum.SUBJUNCTIVE, Some(VerbTenseEnum.PRESENT), Some(Pronoun.PL_2)))
	val VERB_SUBJUNCTIVE_PRESENT_3RD_PL = VerbForm(11, (VerbModeEnum.SUBJUNCTIVE, Some(VerbTenseEnum.PRESENT), Some(Pronoun.PL_3)))

	// - PAST
	val VERB_INDICATIVE_PAST_1ST_SG = VerbForm(12, (VerbModeEnum.INDICATIVE, Some(VerbTenseEnum.PAST), Some(Pronoun.SG_1)))
	val VERB_INDICATIVE_PAST_2ND_SG = VerbForm(13, (VerbModeEnum.INDICATIVE, Some(VerbTenseEnum.PAST), Some(Pronoun.SG_2)))
	val VERB_INDICATIVE_PAST_3RD_SG = VerbForm(14, (VerbModeEnum.INDICATIVE, Some(VerbTenseEnum.PAST), Some(Pronoun.SG_3)))

	val VERB_INDICATIVE_PAST_1ST_PL = VerbForm(15, (VerbModeEnum.INDICATIVE, Some(VerbTenseEnum.PAST), Some(Pronoun.PL_1)))
	val VERB_INDICATIVE_PAST_2ND_PL = VerbForm(16, (VerbModeEnum.INDICATIVE, Some(VerbTenseEnum.PAST), Some(Pronoun.PL_2)))
	val VERB_INDICATIVE_PAST_3RD_PL = VerbForm(17, (VerbModeEnum.INDICATIVE, Some(VerbTenseEnum.PAST), Some(Pronoun.PL_3)))

	val VERB_SUBJUNCTIVE_PAST_1ST_SG = VerbForm(18, (VerbModeEnum.SUBJUNCTIVE, Some(VerbTenseEnum.PAST), Some(Pronoun.SG_1)))
	val VERB_SUBJUNCTIVE_PAST_2ND_SG = VerbForm(19, (VerbModeEnum.SUBJUNCTIVE, Some(VerbTenseEnum.PAST), Some(Pronoun.SG_2)))
	val VERB_SUBJUNCTIVE_PAST_3RD_SG = VerbForm(20, (VerbModeEnum.SUBJUNCTIVE, Some(VerbTenseEnum.PAST), Some(Pronoun.SG_3)))

	val VERB_SUBJUNCTIVE_PAST_1ST_PL = VerbForm(21, (VerbModeEnum.SUBJUNCTIVE, Some(VerbTenseEnum.PAST), Some(Pronoun.PL_1)))
	val VERB_SUBJUNCTIVE_PAST_2ND_PL = VerbForm(22, (VerbModeEnum.SUBJUNCTIVE, Some(VerbTenseEnum.PAST), Some(Pronoun.PL_2)))
	val VERB_SUBJUNCTIVE_PAST_3RD_PL = VerbForm(23, (VerbModeEnum.SUBJUNCTIVE, Some(VerbTenseEnum.PAST), Some(Pronoun.PL_3)))

	val VERB_INFINITIVE							= VerbForm(24, (VerbModeEnum.INFINITIVE, None, 												None))
	val VERB_PRESENT_PARTICIPLE			= VerbForm(25, (VerbModeEnum.PARTICIPLE, Some(VerbTenseEnum.PRESENT),	None))
	val VERB_PAST_PARTICIPLE				= VerbForm(26, (VerbModeEnum.PARTICIPLE, Some(VerbTenseEnum.PAST), 		None))

	val VERB_IMPERATIVE_SG_2ND			= VerbForm(27, (VerbModeEnum.IMPERATIVE, Some(VerbTenseEnum.PRESENT),	Some(Pronoun.SG_2)))
	val VERB_IMPERATIVE_PL_1ST			= VerbForm(28, (VerbModeEnum.IMPERATIVE, Some(VerbTenseEnum.PRESENT),	Some(Pronoun.PL_1)))
	val VERB_IMPERATIVE_PL_2ND			= VerbForm(29, (VerbModeEnum.IMPERATIVE, Some(VerbTenseEnum.PRESENT),	Some(Pronoun.PL_2)))

	// TODO: reflexive verb forms
}


object NounForm extends EnumLike[Int, NounForm]
{
	// Nouns it can be divided by number
	// - number x case x gender (x definiteness) = 2*4*3 (*2) = 24(*2)
	val NOUN_NOM_SG		= NounForm(1000, SINGULAR, Case.NOMINATIVE)
	val NOUN_ACC_SG		= NounForm(1001, SINGULAR, Case.ACCUSATIVE)
	val NOUN_DAT_SG		= NounForm(1002, SINGULAR, Case.DATIVE)
	val NOUN_GEN_SG		= NounForm(1003, SINGULAR, Case.GENITIVE)

	val NOUN_NOM_PL		= NounForm(1004, PLURAL, Case.NOMINATIVE)
	val NOUN_ACC_PL		= NounForm(1005, PLURAL, Case.ACCUSATIVE)
	val NOUN_DAT_PL		= NounForm(1006, PLURAL, Case.DATIVE)
	val NOUN_GEN_PL		= NounForm(1007, PLURAL, Case.GENITIVE)

	// adjectival
	// - number x case x gender x strength x type = 2*4*3*2=48
}