package com.hyenawarrior

import com.hyenawarrior.OldNorseGrammar.grammar.verbs.StrongVerb._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbClassEnum.STRONG_1ST_CLASS
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbModeEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbTenseEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.{EnumVerbStem, StrongVerbStem}
import com.hyenawarrior.OldNorseGrammar.grammar.{Pronoun, Root}
import org.junit.Assert._
import org.junit.{Assert, Test}

import scala.collection.immutable.Map

/**
	* Created by HyenaWarrior on 2017.06.26..
	*/
class VerbTest
{
	@Test
	def testStrong6VerbInflection()
	{
		val svStemPR = StrongVerbStem(Root("tak"), VerbClassEnum.STRONG_6TH_CLASS, EnumVerbStem.PRESENT_STEM)
		val svStemPS = StrongVerbStem(Root("tak"), VerbClassEnum.STRONG_6TH_CLASS, EnumVerbStem.PRETERITE_SINGULAR_STEM)
		val svStemPP = StrongVerbStem(Root("tak"), VerbClassEnum.STRONG_6TH_CLASS, EnumVerbStem.PRETERITE_PLURAL_STEM)
		val svStemPF = StrongVerbStem(Root("tak"), VerbClassEnum.STRONG_6TH_CLASS, EnumVerbStem.PERFECT_STEM)

		assertEquals("taka",		verbFrom(svStemPR, None, INFINITIVE).strForm)

		assertEquals("takinn", 	verbFrom(svStemPF, Some(PAST), 		PARTICIPLE).strForm)
		assertEquals("takandi", verbFrom(svStemPR, Some(PRESENT), PARTICIPLE).strForm)

		assertEquals("tek", 		verbFrom(svStemPR, Pronoun.SG_1, 			PRESENT,	INDICATIVE).strForm)
		assertEquals("tekr", 		verbFrom(svStemPR, Pronoun.SG_2,			PRESENT,	INDICATIVE).strForm)
		assertEquals("tekr", 		verbFrom(svStemPR, Pronoun.SG_3_MASC, PRESENT,	INDICATIVE).strForm)

		assertEquals("tökum", 	verbFrom(svStemPR, Pronoun.PL_1, 			PRESENT,	INDICATIVE).strForm)
		assertEquals("takið", 	verbFrom(svStemPR, Pronoun.PL_2, 			PRESENT,	INDICATIVE).strForm)
		assertEquals("taka",		verbFrom(svStemPR, Pronoun.PL_3_MASC,	PRESENT,	INDICATIVE).strForm)

		assertEquals("tók", 		verbFrom(svStemPS, Pronoun.SG_1, 			PAST, 		INDICATIVE).strForm)
		assertEquals("tókt" /*"tókst"*/, verbFrom(svStemPS, Pronoun.SG_2, PAST,	INDICATIVE).strForm)
		assertEquals("tók", 		verbFrom(svStemPS, Pronoun.SG_3_MASC, PAST,			INDICATIVE).strForm)

		assertEquals("tókum",		verbFrom(svStemPP, Pronoun.PL_1, 			PAST,			INDICATIVE).strForm)
		assertEquals("tókuð",		verbFrom(svStemPP, Pronoun.PL_2,			PAST,			INDICATIVE).strForm)
		assertEquals("tóku",		verbFrom(svStemPP, Pronoun.PL_3_MASC, PAST,			INDICATIVE).strForm)
	}

  @Test
  def testVerbGenerator(): Unit = try {

    val vfSg1 = (VerbModeEnum.INDICATIVE, Some(VerbTenseEnum.PAST), Some(Pronoun.SG_1))
    val vfSg2 = (VerbModeEnum.INDICATIVE, Some(VerbTenseEnum.PAST), Some(Pronoun.SG_2))

    val verb = StrongVerbContext(STRONG_1ST_CLASS, Map(vfSg2 -> "beit"))

    val verbForm = verb.verbForms(vfSg1)

    assertEquals(verbForm.strForm, "bei")

  } catch {

    case e: RuntimeException => throw e
  }

	/*
		Class 2nd
		PRESENT		PAST-SG		PAST-PL		PERFECT
		jú/jó/ú		au				u					o

		*/
	@Test
	def class2ndUseJuForPresent(): Unit = {

		val stem = StrongVerbStem.fromStrRepr("kraup", VerbClassEnum.STRONG_2ND_CLASS, EnumVerbStem.PRETERITE_SINGULAR_STEM)

		val verbform = StrongVerb.verbFrom(stem, None, VerbModeEnum.INFINITIVE)

		assertEquals("krjúpa", verbform.strForm)
	}

	@Test
	def class2ndChangeToJoForPresent(): Unit = {

		val stem = StrongVerbStem.fromStrRepr("skaut", VerbClassEnum.STRONG_2ND_CLASS, EnumVerbStem.PRETERITE_SINGULAR_STEM)

		val verbform = StrongVerb.verbFrom(stem, None, VerbModeEnum.INFINITIVE)

		assertEquals("skjóta", verbform.strForm)
	}

	/*
		Class 3th
		PRESENT		PAST-SG		PAST-PL		PERFECT
		e/ja			a					u					o(/u)

		*/

	@Test
	def class3rdRaising(): Unit = {

		val stem = StrongVerbStem.fromStrRepr("spenn", VerbClassEnum.STRONG_3RD_CLASS, EnumVerbStem.PRESENT_STEM)

		val verbform = StrongVerb.verbFrom(stem, None, VerbModeEnum.INFINITIVE)

		assertEquals("spinna", verbform.strForm)
	}

	@Test
	def class3rdNasalAssimilation(): Unit = {

		val stem = StrongVerbStem.fromStrRepr("band", VerbClassEnum.STRONG_3RD_CLASS, EnumVerbStem.PRETERITE_SINGULAR_STEM)

		val verbform = StrongVerb.verbFrom(stem, Pronoun.SG_1, VerbTenseEnum.PAST, VerbModeEnum.INDICATIVE)

		assertEquals("batt", verbform.strForm)
	}

	@Test
	def class3rdChangeToJa(): Unit = {

		val stem = StrongVerbStem.fromStrRepr("help", VerbClassEnum.STRONG_3RD_CLASS, EnumVerbStem.PRESENT_STEM)

		assertEquals("hjalpa", 	StrongVerb.verbFrom(stem, None, VerbModeEnum.INFINITIVE).strForm)
		assertEquals("help", 		StrongVerb.verbFrom(stem, Pronoun.SG_1, VerbTenseEnum.PRESENT, VerbModeEnum.INDICATIVE).strForm)
		assertEquals("hjölpum",	StrongVerb.verbFrom(stem, Pronoun.PL_1, VerbTenseEnum.PRESENT, VerbModeEnum.INDICATIVE).strForm)
	}

	/*
		Class 4th
		PRESENT		PAST-SG		PAST-PL		PERFECT
		e/o				a					á					o
	 */


	@Test
	def sanityCheck() {

		val svStemPR = StrongVerbStem(Root("tak"), VerbClassEnum.STRONG_6TH_CLASS, EnumVerbStem.PRESENT_STEM)

		try {

			verbFrom(svStemPR, Some(PAST), 		PARTICIPLE)

			Assert.fail("Past participle should not be sonstructible from present stem")
		}
		catch {
			case e: RuntimeException => ()
		}
	}
}
