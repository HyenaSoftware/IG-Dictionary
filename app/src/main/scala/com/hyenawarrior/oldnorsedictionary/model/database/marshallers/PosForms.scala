package com.hyenawarrior.oldnorsedictionary.model.database.marshallers

import com.hyenawarrior.OldNorseGrammar.grammar.GNumber.{PLURAL, SINGULAR}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbVoice.ACTIVE
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.{VerbModeEnum, VerbTenseEnum, VerbType}
import com.hyenawarrior.OldNorseGrammar.grammar.{Case, GNumber, Pronoun}
import com.hyenawarrior.auxiliary.EnumLike


/**
  * Created by HyenaWarrior on 2017.05.27..
  */
trait PosForm
{
	val id: Int
}

case class NounForm(id: Int, number: GNumber, caze: Case) extends PosForm { NounForm.add(id -> this) }
case class VerbForm(id: Int, vtype: VerbType) extends PosForm {

  VerbForm.add(id -> this)
}

object VerbForm extends EnumLike[Int, VerbForm]
{
	// Verbs, it could be split by verb-tense
	// - PRESENT
	val VERB_INDICATIVE_PRESENT_1ST_SG = VerbForm(0, (VerbModeEnum.INDICATIVE, ACTIVE, Some(VerbTenseEnum.PRESENT), Some(Pronoun.SG_1)))
	val VERB_INDICATIVE_PRESENT_2ND_SG = VerbForm(1, (VerbModeEnum.INDICATIVE, ACTIVE, Some(VerbTenseEnum.PRESENT), Some(Pronoun.SG_2)))
	val VERB_INDICATIVE_PRESENT_3RD_SG = VerbForm(2, (VerbModeEnum.INDICATIVE, ACTIVE, Some(VerbTenseEnum.PRESENT), Some(Pronoun.SG_3)))

	val VERB_INDICATIVE_PRESENT_1ST_PL = VerbForm(3, (VerbModeEnum.INDICATIVE, ACTIVE, Some(VerbTenseEnum.PRESENT), Some(Pronoun.PL_1)))
	val VERB_INDICATIVE_PRESENT_2ND_PL = VerbForm(4, (VerbModeEnum.INDICATIVE, ACTIVE, Some(VerbTenseEnum.PRESENT), Some(Pronoun.PL_2)))
	val VERB_INDICATIVE_PRESENT_3RD_PL = VerbForm(5, (VerbModeEnum.INDICATIVE, ACTIVE, Some(VerbTenseEnum.PRESENT), Some(Pronoun.PL_3)))

	val VERB_SUBJUNCTIVE_PRESENT_1ST_SG = VerbForm(6, (VerbModeEnum.SUBJUNCTIVE, ACTIVE, Some(VerbTenseEnum.PRESENT), Some(Pronoun.SG_1)))
	val VERB_SUBJUNCTIVE_PRESENT_2ND_SG = VerbForm(7, (VerbModeEnum.SUBJUNCTIVE, ACTIVE, Some(VerbTenseEnum.PRESENT), Some(Pronoun.SG_2)))
	val VERB_SUBJUNCTIVE_PRESENT_3RD_SG = VerbForm(8, (VerbModeEnum.SUBJUNCTIVE, ACTIVE, Some(VerbTenseEnum.PRESENT), Some(Pronoun.SG_3)))

	val VERB_SUBJUNCTIVE_PRESENT_1ST_PL = VerbForm(9, (VerbModeEnum.SUBJUNCTIVE, ACTIVE, Some(VerbTenseEnum.PRESENT), Some(Pronoun.PL_1)))
	val VERB_SUBJUNCTIVE_PRESENT_2ND_PL = VerbForm(10, (VerbModeEnum.SUBJUNCTIVE, ACTIVE, Some(VerbTenseEnum.PRESENT), Some(Pronoun.PL_2)))
	val VERB_SUBJUNCTIVE_PRESENT_3RD_PL = VerbForm(11, (VerbModeEnum.SUBJUNCTIVE, ACTIVE, Some(VerbTenseEnum.PRESENT), Some(Pronoun.PL_3)))

	// - PAST
	val VERB_INDICATIVE_PAST_1ST_SG = VerbForm(12, (VerbModeEnum.INDICATIVE, ACTIVE, Some(VerbTenseEnum.PAST), Some(Pronoun.SG_1)))
	val VERB_INDICATIVE_PAST_2ND_SG = VerbForm(13, (VerbModeEnum.INDICATIVE, ACTIVE, Some(VerbTenseEnum.PAST), Some(Pronoun.SG_2)))
	val VERB_INDICATIVE_PAST_3RD_SG = VerbForm(14, (VerbModeEnum.INDICATIVE, ACTIVE, Some(VerbTenseEnum.PAST), Some(Pronoun.SG_3)))

	val VERB_INDICATIVE_PAST_1ST_PL = VerbForm(15, (VerbModeEnum.INDICATIVE, ACTIVE, Some(VerbTenseEnum.PAST), Some(Pronoun.PL_1)))
	val VERB_INDICATIVE_PAST_2ND_PL = VerbForm(16, (VerbModeEnum.INDICATIVE, ACTIVE, Some(VerbTenseEnum.PAST), Some(Pronoun.PL_2)))
	val VERB_INDICATIVE_PAST_3RD_PL = VerbForm(17, (VerbModeEnum.INDICATIVE, ACTIVE, Some(VerbTenseEnum.PAST), Some(Pronoun.PL_3)))

	val VERB_SUBJUNCTIVE_PAST_1ST_SG = VerbForm(18, (VerbModeEnum.SUBJUNCTIVE, ACTIVE, Some(VerbTenseEnum.PAST), Some(Pronoun.SG_1)))
	val VERB_SUBJUNCTIVE_PAST_2ND_SG = VerbForm(19, (VerbModeEnum.SUBJUNCTIVE, ACTIVE, Some(VerbTenseEnum.PAST), Some(Pronoun.SG_2)))
	val VERB_SUBJUNCTIVE_PAST_3RD_SG = VerbForm(20, (VerbModeEnum.SUBJUNCTIVE, ACTIVE, Some(VerbTenseEnum.PAST), Some(Pronoun.SG_3)))

	val VERB_SUBJUNCTIVE_PAST_1ST_PL = VerbForm(21, (VerbModeEnum.SUBJUNCTIVE, ACTIVE, Some(VerbTenseEnum.PAST), Some(Pronoun.PL_1)))
	val VERB_SUBJUNCTIVE_PAST_2ND_PL = VerbForm(22, (VerbModeEnum.SUBJUNCTIVE, ACTIVE, Some(VerbTenseEnum.PAST), Some(Pronoun.PL_2)))
	val VERB_SUBJUNCTIVE_PAST_3RD_PL = VerbForm(23, (VerbModeEnum.SUBJUNCTIVE, ACTIVE, Some(VerbTenseEnum.PAST), Some(Pronoun.PL_3)))

	val VERB_INFINITIVE							= VerbForm(24, (VerbModeEnum.INFINITIVE, ACTIVE, None, 												None))
	val VERB_PRESENT_PARTICIPLE			= VerbForm(25, (VerbModeEnum.PARTICIPLE, ACTIVE, Some(VerbTenseEnum.PRESENT),	None))
	val VERB_PAST_PARTICIPLE				= VerbForm(26, (VerbModeEnum.PARTICIPLE, ACTIVE, Some(VerbTenseEnum.PAST), 		None))

	val VERB_IMPERATIVE_SG_2ND			= VerbForm(27, (VerbModeEnum.IMPERATIVE, ACTIVE, Some(VerbTenseEnum.PRESENT),	Some(Pronoun.SG_2)))
	val VERB_IMPERATIVE_PL_1ST			= VerbForm(28, (VerbModeEnum.IMPERATIVE, ACTIVE, Some(VerbTenseEnum.PRESENT),	Some(Pronoun.PL_1)))
	val VERB_IMPERATIVE_PL_2ND			= VerbForm(29, (VerbModeEnum.IMPERATIVE, ACTIVE, Some(VerbTenseEnum.PRESENT),	Some(Pronoun.PL_2)))

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