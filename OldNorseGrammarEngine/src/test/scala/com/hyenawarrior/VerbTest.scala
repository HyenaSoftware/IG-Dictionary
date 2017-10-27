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
	/*
			Class 1st
			PRESENT		PAST-SG		PAST-PL		PERFECT
			i					ei				i					i
	 */

  @Test
  def testClass1stConvertFromPlToSg(): Unit = try {

    val srcForm = (VerbModeEnum.INDICATIVE, Some(VerbTenseEnum.PAST), Some(Pronoun.PL_3))
		val trgForm = (VerbModeEnum.INDICATIVE, Some(VerbTenseEnum.PAST), Some(Pronoun.SG_1))

    val verb = StrongVerbContext(STRONG_1ST_CLASS, Map(srcForm -> "bitu"))

    val verbForm = verb.verbForms(trgForm)

    assertEquals(verbForm.strForm, "beit")

  } catch {

    case e: RuntimeException => fail(e.getMessage)
  }

	@Test
	def testClass1stConvertFromSgToPl(): Unit = try {

		val srcForm = (VerbModeEnum.INDICATIVE, Some(VerbTenseEnum.PAST), Some(Pronoun.SG_1))
		val trgForm = (VerbModeEnum.INDICATIVE, Some(VerbTenseEnum.PAST), Some(Pronoun.PL_3))

		val verb = StrongVerbContext(STRONG_1ST_CLASS, Map(srcForm -> "beit"))

		val verbForm = verb.verbForms(trgForm)

		assertEquals(verbForm.strForm, "bitu")

	} catch {

		case e: RuntimeException => fail(e.getMessage)
	}

	/*
		Class 2nd
		PRESENT		PAST-SG		PAST-PL		PERFECT
		jú/jó/ú		au				u					o

		*/
	@Test
	def testClass2ndUseJuForPresent(): Unit = {

    val srcType: VerbType = (VerbModeEnum.INDICATIVE, Some(VerbTenseEnum.PAST), Some(Pronoun.SG_3))

    val verb = StrongVerbContext(VerbClassEnum.STRONG_2ND_CLASS, Map(srcType -> "kraup"))

		val verbform = verb.verbForms((VerbModeEnum.INFINITIVE, None, None))

		assertEquals("krjúpa", verbform.strForm)
	}

	@Test
	def testClass2ndChangeToJoForPresent(): Unit = {

    val srcType: VerbType = (VerbModeEnum.INDICATIVE, Some(VerbTenseEnum.PAST), Some(Pronoun.SG_3))

    val verb = StrongVerbContext(VerbClassEnum.STRONG_2ND_CLASS, Map(srcType -> "skaut"))

		val verbform = verb.verbForms((VerbModeEnum.INFINITIVE, None, None))

		assertEquals("skjóta", verbform.strForm)
	}

	/*
		Class 3th
		PRESENT		PAST-SG		PAST-PL		PERFECT
		e/ja/i		a					u					o(/u)

		*/

	@Test
	def testClass3rdRaising(): Unit = {

		val stem = StrongVerbStem.fromStrRepr("spenn", VerbClassEnum.STRONG_3RD_CLASS, EnumVerbStem.PRESENT_STEM)

		val verbform = StrongVerb.verbFrom(stem, None, VerbModeEnum.INFINITIVE)

		assertEquals("spinna", verbform.strForm)
	}

	@Test
	def testClass3rdNasalAssimilation(): Unit = {

		val stem = StrongVerbStem.fromStrRepr("band", VerbClassEnum.STRONG_3RD_CLASS, EnumVerbStem.PRETERITE_SINGULAR_STEM)

		val verbform = StrongVerb.verbFrom(stem, Pronoun.SG_1, VerbTenseEnum.PAST, VerbModeEnum.INDICATIVE)

		assertEquals("batt", verbform.strForm)
	}

  @Test
  def testClass3rdFromJaStem(): Unit = {

    val stem = StrongVerbStem.fromStrRepr("help", VerbClassEnum.STRONG_3RD_CLASS, EnumVerbStem.PRESENT_STEM)

		assertEquals("hjalpa", 	StrongVerb.verbFrom(stem, None, VerbModeEnum.INFINITIVE).strForm)
		assertEquals("help", 		StrongVerb.verbFrom(stem, Pronoun.SG_1, VerbTenseEnum.PRESENT, VerbModeEnum.INDICATIVE).strForm)
		assertEquals("hjǫlpum",	StrongVerb.verbFrom(stem, Pronoun.PL_1, VerbTenseEnum.PRESENT, VerbModeEnum.INDICATIVE).strForm)
	}

  @Test
  def testClass3rdFromJaStemFromPastTense(): Unit = {

    val srcType: VerbType = (VerbModeEnum.INDICATIVE, Some(VerbTenseEnum.PAST), Some(Pronoun.SG_3))

    val verb = StrongVerbContext(VerbClassEnum.STRONG_3RD_CLASS, Map(srcType -> "halp"))

    assertEquals("hjalpa", 	verb.verbForms(VerbModeEnum.INFINITIVE, None, None).strForm)
    assertEquals("help", 		verb.verbForms(VerbModeEnum.INDICATIVE, Some(VerbTenseEnum.PRESENT), Some(Pronoun.SG_1)).strForm)
    assertEquals("hjǫlpum",	verb.verbForms(VerbModeEnum.INDICATIVE, Some(VerbTenseEnum.PRESENT), Some(Pronoun.PL_1)).strForm)
  }

  @Test
  def testClass3rdChangeFromJa(): Unit = {

    val srcForm = (INDICATIVE, Some(PRESENT), Some(Pronoun.PL_3))

    val verb = StrongVerbContext(VerbClassEnum.STRONG_3RD_CLASS, Map(srcForm -> "hjalpa"))

    val verbFormP3 = verb.verbForms(INDICATIVE, Some(PRESENT), Some(Pronoun.SG_3))

    assertEquals("helpr", verbFormP3.strForm)
  }


  @Test
  def testClass3rdChangeToJa(): Unit = {

    val srcForm = (INDICATIVE, Some(PRESENT), Some(Pronoun.SG_3))

    val verb = StrongVerbContext(VerbClassEnum.STRONG_3RD_CLASS, Map(srcForm -> "helpr"))

    val verbFormP3 = verb.verbForms(INDICATIVE, Some(PRESENT), Some(Pronoun.PL_3))

    assertEquals("hjalpa", verbFormP3.strForm)
  }

	@Test
	def testClass3rdUUmlaut(): Unit = {

		val presStem = StrongVerbStem.fromStrRepr("sekkv", VerbClassEnum.STRONG_3RD_CLASS, EnumVerbStem.PRESENT_STEM)

		assertEquals("søkkr", 	StrongVerb.verbFrom(presStem, Pronoun.SG_3, VerbTenseEnum.PRESENT, VerbModeEnum.INDICATIVE).strForm)
		assertEquals("søkkva", 	StrongVerb.verbFrom(presStem, Pronoun.PL_3, VerbTenseEnum.PRESENT, VerbModeEnum.INDICATIVE).strForm)

		val pastSgStem = StrongVerbStem.fromStrRepr("sakkv", VerbClassEnum.STRONG_3RD_CLASS, EnumVerbStem.PRETERITE_SINGULAR_STEM)
		assertEquals("sǫkk", 	StrongVerb.verbFrom(pastSgStem, Pronoun.SG_3, VerbTenseEnum.PRESENT, VerbModeEnum.INDICATIVE).strForm)

		val pastPlStem = StrongVerbStem.fromStrRepr("sukk", VerbClassEnum.STRONG_3RD_CLASS, EnumVerbStem.PRETERITE_PLURAL_STEM)
		assertEquals("sukku", StrongVerb.verbFrom(pastPlStem, Pronoun.PL_3, VerbTenseEnum.PRESENT, VerbModeEnum.INDICATIVE).strForm)
	}

  @Test
  def testClass3rdSongva(): Unit = {

    val srcForm = (INDICATIVE, Some(PRESENT), Some(Pronoun.PL_3))

    val verb = StrongVerbContext(VerbClassEnum.STRONG_3RD_CLASS, Map(srcForm -> "syngva"))

    assertEquals("syngr", 	verb.verbForms(VerbModeEnum.INDICATIVE, Some(PRESENT), Some(Pronoun.SG_3)))
    assertEquals("sǫng",    verb.verbForms(VerbModeEnum.INDICATIVE, Some(PAST),    Some(Pronoun.SG_3)))
  }

	/*
		Class 4th
		PRESENT		PAST-SG		PAST-PL		PERFECT
		e/o				a					á					o
	 */

  @Test
  def testClass6VerbInflection() {

    val svStemPR = StrongVerbStem(Root("tak"), VerbClassEnum.STRONG_6TH_CLASS, EnumVerbStem.PRESENT_STEM)
    val svStemPS = StrongVerbStem(Root("tak"), VerbClassEnum.STRONG_6TH_CLASS, EnumVerbStem.PRETERITE_SINGULAR_STEM)
    val svStemPP = StrongVerbStem(Root("tak"), VerbClassEnum.STRONG_6TH_CLASS, EnumVerbStem.PRETERITE_PLURAL_STEM)
    val svStemPF = StrongVerbStem(Root("tak"), VerbClassEnum.STRONG_6TH_CLASS, EnumVerbStem.PERFECT_STEM)

    assertEquals("taka",		verbFrom(svStemPR, None, INFINITIVE).strForm)

    assertEquals("takinn", 	verbFrom(svStemPF, Some(PAST), 		PARTICIPLE).strForm)
    assertEquals("takandi", verbFrom(svStemPR, Some(PRESENT), PARTICIPLE).strForm)

    assertEquals("tek", 		verbFrom(svStemPR, Pronoun.SG_1, PRESENT,	INDICATIVE).strForm)
    assertEquals("tekr", 		verbFrom(svStemPR, Pronoun.SG_2, PRESENT,	INDICATIVE).strForm)
    assertEquals("tekr", 		verbFrom(svStemPR, Pronoun.SG_3, PRESENT,	INDICATIVE).strForm)

    assertEquals("tǫkum", 	verbFrom(svStemPR, Pronoun.PL_1, PRESENT,	INDICATIVE).strForm)
    assertEquals("takið", 	verbFrom(svStemPR, Pronoun.PL_2, PRESENT,	INDICATIVE).strForm)
    assertEquals("taka",		verbFrom(svStemPR, Pronoun.PL_3, PRESENT,	INDICATIVE).strForm)

    assertEquals("tók", 		verbFrom(svStemPS, Pronoun.SG_1, PAST, 		INDICATIVE).strForm)
    assertEquals("tókt" /*"tókst"*/, verbFrom(svStemPS, Pronoun.SG_2, PAST,	INDICATIVE).strForm)
    assertEquals("tók", 		verbFrom(svStemPS, Pronoun.SG_3, PAST,    INDICATIVE).strForm)

    assertEquals("tókum",		verbFrom(svStemPP, Pronoun.PL_1, PAST, INDICATIVE).strForm)
    assertEquals("tókuð",		verbFrom(svStemPP, Pronoun.PL_2, PAST, INDICATIVE).strForm)
    assertEquals("tóku",		verbFrom(svStemPP, Pronoun.PL_3, PAST, INDICATIVE).strForm)
  }

  @Test
  def testClass7thVowelDeletion(): Unit = {

    val stem = StrongVerbStem.fromStrRepr("fá", VerbClassEnum.STRONG_7TH_CLASS, EnumVerbStem.PRESENT_STEM)

    assertEquals("fá", verbFrom(stem, None, INFINITIVE).strForm)
  }

	@Test
	def sanityCheck() {

		val svStemPR = StrongVerbStem(Root("tak"), VerbClassEnum.STRONG_6TH_CLASS, EnumVerbStem.PRESENT_STEM)

		try {

			verbFrom(svStemPR, Some(PAST), 		PARTICIPLE)

			Assert.fail("Past participle should not be constructible from present stem")
		}
		catch {
			case e: RuntimeException => ()
		}
	}
}
