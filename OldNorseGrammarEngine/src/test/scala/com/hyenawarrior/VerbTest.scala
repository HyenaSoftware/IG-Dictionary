package com.hyenawarrior

import com.hyenawarrior.OldNorseGrammar.grammar.Pronoun
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.StemTransform.Raising
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.StrongVerb._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.TransformationMode.EnabledFor
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbClassEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbModeEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbTenseEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.EnumVerbStem._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.StrongVerbStem
import org.junit.Assert._
import org.junit.Test

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

    val srcForm = (INDICATIVE, Some(PAST), Some(Pronoun.PL_3))
		val trgForm = (INDICATIVE, Some(PAST), Some(Pronoun.SG_1))

    val verb = StrongVerbContext(STRONG_1ST_CLASS, Map(srcForm -> "bitu"))

    val verbForm = verb.verbForms(trgForm)

    assertEquals(verbForm.strForm, "beit")

  } catch {

    case e: RuntimeException => fail(e.getMessage)
  }

	@Test
	def testClass1stConvertFromSgToPl(): Unit = try {

		val srcForm = (INDICATIVE, Some(PAST), Some(Pronoun.SG_1))
		val trgForm = (INDICATIVE, Some(PAST), Some(Pronoun.PL_3))

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

    val srcType: VerbType = (INDICATIVE, Some(PAST), Some(Pronoun.SG_3))

    val verb = StrongVerbContext(VerbClassEnum.STRONG_2ND_CLASS, Map(srcType -> "kraup"))

		val verbform = verb.verbForms((INFINITIVE, None, None))

		assertEquals("krjúpa", verbform.strForm)
	}

	@Test
	def testClass2ndChangeToJoForPresent(): Unit = {

    val srcType: VerbType = (INDICATIVE, Some(PAST), Some(Pronoun.SG_3))

    val verb = StrongVerbContext(VerbClassEnum.STRONG_2ND_CLASS, Map(srcType -> "skaut"))

		val verbform = verb.verbForms((INFINITIVE, None, None))

		assertEquals("skjóta", verbform.strForm)
	}

	/**
			stem		Past S2
		ON
			skaut-	skautt
			braut-	brauzt
		PGmc
			skaut-	skaust
			braut-	braust

			<!> skautt is considered irregular, it need to be overridden manually
	 */
	@Test
	def testClass2ndPastSg2SuffixForTStemEnd(): Unit = {

		val stem = StrongVerbStem.fromStrRepr("braut", VerbClassEnum.STRONG_2ND_CLASS, PRETERITE_SINGULAR_STEM)

		val verbform = StrongVerb.verbFrom(stem, Pronoun.SG_2, PAST, INDICATIVE)

		assertEquals("brauzt", verbform.strForm)
	}


	/*
		Class 3th
		PRESENT		PAST-SG		PAST-PL		PERFECT
		e/ja/i		a					u					o(/u)

		*/

  @Test
  def testClass3rdRegular(): Unit = {

    val stem = StrongVerbStem.fromStrRepr("bregð", STRONG_3RD_CLASS, PRESENT_STEM)

    assertEquals("bregða", StrongVerb.verbFrom(stem, None, INFINITIVE).strForm)
  }

	@Test
	def testClass3rdRaising(): Unit = {

		val stem = StrongVerbStem.fromStrRepr("spinn", STRONG_3RD_CLASS, PRESENT_STEM)

    val StrongVerbStem(normalizedStemStr, _, _, EnabledFor(Raising)) = stem
    assertEquals("spenn", normalizedStemStr)

		val verbform = StrongVerb.verbFrom(stem, None, INFINITIVE)
		assertEquals("spinna", verbform.strForm)
	}

	@Test
	def testClass3rdNasalAssimilation(): Unit = {

		val srcType: VerbType = (INDICATIVE, Some(PAST), Some(Pronoun.SG_3))

    val verb = StrongVerbContext(STRONG_3RD_CLASS, Map(srcType -> "batt"))

    assertEquals("binda", verb.verbForms(INFINITIVE, None, None).strForm)
    assertEquals("batt",  verb.verbForms(INDICATIVE, Some(PAST), Some(Pronoun.SG_1)).strForm)
    assertEquals("batzt", verb.verbForms(INDICATIVE, Some(PAST), Some(Pronoun.SG_2)).strForm)
    assertEquals("batt",  verb.verbForms(INDICATIVE, Some(PAST), Some(Pronoun.SG_3)).strForm)
	}

  /*
   The following two test cases are focusing on to use the same properties on the output forms during transformation
   'e' sometimes changes to 'i' in the 3rd class of verbs, like in the case of "binda", but there are exceptions
   like "brenna"
  */

  @Test
  def testCoTransformation1(): Unit = {

    val srcType: VerbType = (INDICATIVE, Some(PRESENT), Some(Pronoun.SG_1))

    val verb = StrongVerbContext(STRONG_3RD_CLASS, Map(srcType -> "brenn"))

    assertEquals("brennr",  verb.verbForms(INDICATIVE, Some(PRESENT), Some(Pronoun.SG_3)).strForm)
  }

  @Test
  def testCoTransformation2(): Unit = {

    val srcType: VerbType = (INDICATIVE, Some(PRESENT), Some(Pronoun.SG_1))

    val verb = StrongVerbContext(STRONG_3RD_CLASS, Map(srcType -> "bind"))

    assertEquals("bindr",  verb.verbForms(INDICATIVE, Some(PRESENT), Some(Pronoun.SG_3)).strForm)
  }

  @Test
  def testClass3rdFromJaStem(): Unit = {

    val stem = StrongVerbStem.fromStrRepr("hjalp", STRONG_3RD_CLASS, PRESENT_STEM)

		assertEquals("hjalpa", 	StrongVerb.verbFrom(stem, None, INFINITIVE).strForm)
		assertEquals("help", 		StrongVerb.verbFrom(stem, Pronoun.SG_1, PRESENT, INDICATIVE).strForm)
		assertEquals("hjǫlpum",	StrongVerb.verbFrom(stem, Pronoun.PL_1, PRESENT, INDICATIVE).strForm)
	}

  @Test
  def testClass3rdStemFinalDevoicingAfterL(): Unit = {

    val stem = StrongVerbStem.fromStrRepr("gald", STRONG_3RD_CLASS, PRETERITE_SINGULAR_STEM)

    assertEquals("galzt", StrongVerb.verbFrom(stem, Pronoun.SG_2, PAST, INDICATIVE).strForm)
    assertEquals("galt",  StrongVerb.verbFrom(stem, Pronoun.SG_3, PAST, INDICATIVE).strForm)
  }

  /** Fracture does not occur at all if *e is preceded by v, l, or r, e.g. verða, leðr. */
  @Test
  def testClass3rdFromDoNotChangeStemToJa(): Unit = {

    val srcType: VerbType = (INDICATIVE, Some(PRESENT), Some(Pronoun.SG_3))

    val verb = StrongVerbContext(STRONG_3RD_CLASS, Map(srcType -> "verðr"))

    assertEquals("verða", verb.verbForms(INFINITIVE, None, None).strForm)
		assertEquals("verð", 	verb.verbForms(INDICATIVE, Some(PRESENT), Some(Pronoun.SG_1)).strForm)
    assertEquals("varð", 	verb.verbForms(INDICATIVE, Some(PAST), Some(Pronoun.SG_3)).strForm)
    assertEquals("urðu",	verb.verbForms(INDICATIVE, Some(PAST), Some(Pronoun.PL_3)).strForm)
  }

  @Test
  def testClass3rdFromJaStemFromPastTense(): Unit = {

    val srcType: VerbType = (INDICATIVE, Some(PAST), Some(Pronoun.SG_3))

    val verb = StrongVerbContext(STRONG_3RD_CLASS, Map(srcType -> "halp"))

    assertEquals("hjalpa", 	verb.verbForms(INFINITIVE, None, None).strForm)
    assertEquals("help", 		verb.verbForms(INDICATIVE, Some(PRESENT), Some(Pronoun.SG_1)).strForm)
    assertEquals("hjǫlpum",	verb.verbForms(INDICATIVE, Some(PRESENT), Some(Pronoun.PL_1)).strForm)
    assertEquals("halpt", 	verb.verbForms(INDICATIVE, Some(PAST),    Some(Pronoun.SG_2)).strForm)
  }

  @Test
  def testClass3rdChangeFromJa(): Unit = {

    val srcForm = (INDICATIVE, Some(PRESENT), Some(Pronoun.PL_3))

    val verb = StrongVerbContext(STRONG_3RD_CLASS, Map(srcForm -> "hjalpa"))

    val verbFormP3 = verb.verbForms(INDICATIVE, Some(PRESENT), Some(Pronoun.SG_3))

    assertEquals("helpr", verbFormP3.strForm)
  }


  @Test
  def testClass3rdChangeToJa(): Unit = {

    val srcForm = (INDICATIVE, Some(PRESENT), Some(Pronoun.SG_3))

    val verb = StrongVerbContext(STRONG_3RD_CLASS, Map(srcForm -> "helpr"))

    val verbFormP3 = verb.verbForms(INDICATIVE, Some(PRESENT), Some(Pronoun.PL_3))

    assertEquals("hjalpa", verbFormP3.strForm)
  }

	@Test
	def testClass3rdUUmlaut(): Unit = {

		val presStem = StrongVerbStem.fromStrRepr("sekkv", STRONG_3RD_CLASS, PRESENT_STEM)

		assertEquals("søkkr", 	StrongVerb.verbFrom(presStem, Pronoun.SG_3, PRESENT, INDICATIVE).strForm)
		assertEquals("søkkva", 	StrongVerb.verbFrom(presStem, Pronoun.PL_3, PRESENT, INDICATIVE).strForm)

		val pastSgStem = StrongVerbStem.fromStrRepr("sakkv", STRONG_3RD_CLASS, PRETERITE_SINGULAR_STEM)
		assertEquals("sǫkk", 	StrongVerb.verbFrom(pastSgStem, Pronoun.SG_3, PAST, INDICATIVE).strForm)

		val pastPlStem = StrongVerbStem.fromStrRepr("sukk", STRONG_3RD_CLASS, PRETERITE_PLURAL_STEM)
		assertEquals("sukku", StrongVerb.verbFrom(pastPlStem, Pronoun.PL_3, PAST, INDICATIVE).strForm)
	}

  @Test
  def testClass3rdUUmlaut2(): Unit = {

    val srcForm = (INDICATIVE, Some(PRESENT), Some(Pronoun.PL_3))

    val verb = StrongVerbContext(STRONG_3RD_CLASS, Map(srcForm -> "søkkva"))

    assertEquals("søkkr", 	verb.verbForms(INDICATIVE,  Some(PRESENT), Some(Pronoun.SG_3)).strForm)
    assertEquals("søkkva", 	verb.verbForms(INDICATIVE,  Some(PRESENT), Some(Pronoun.PL_3)).strForm)

    assertEquals("sǫkk", 	  verb.verbForms(INDICATIVE,  Some(PAST), Some(Pronoun.SG_3)).strForm)
    assertEquals("sukku",   verb.verbForms(INDICATIVE,  Some(PAST), Some(Pronoun.PL_3)).strForm)
  }

  @Test
  def testClass3rdSongva(): Unit = {

    val srcForm = (INDICATIVE, Some(PRESENT), Some(Pronoun.PL_3))

    val verb = StrongVerbContext(STRONG_3RD_CLASS, Map(srcForm -> "syngva"))

		assertEquals("syngva", 	verb.verbForms(INFINITIVE, None, None).strForm)
		assertEquals("syngr", 	verb.verbForms(INDICATIVE, Some(PRESENT), Some(Pronoun.SG_3)).strForm)
    assertEquals("sǫng",    verb.verbForms(INDICATIVE, Some(PAST),    Some(Pronoun.SG_3)).strForm)
  }

	@Test
	def testClass3rdUmlautedAblautExtraction(): Unit = {

		val verb = StrongVerb.fromStringRepr("syngva", STRONG_3RD_CLASS, (INDICATIVE, Some(PRESENT), Some(Pronoun.PL_3)))

		val StrongVerb((_, verbStem)) = verb

		assertEquals("singv", verbStem.stringForm())
	}

  @Test
  def testClass3rdInflectionCornerCases(): Unit = {

    val verb = StrongVerb.fromStringRepr("batzt", STRONG_3RD_CLASS, (INDICATIVE, Some(PAST), Some(Pronoun.SG_2)))

    val StrongVerb((_, verbStem)) = verb

    assertEquals("batt", verbStem.stringForm())
  }

	/*
		Class 4th
		PRESENT		PAST-SG		PAST-PL		PERFECT
		e/o				a					á					o
	 */

  /*
		Class 5th
		PRESENT		PAST-SG		PAST-PL		PERFECT
		e/i				a					á					e
   */
  @Test
  def testClass5thVerbLiggja(): Unit = {

    val srcForm = (INDICATIVE, Some(PRESENT), Some(Pronoun.SG_2))

    val verb = StrongVerbContext(STRONG_5TH_CLASS, Map(srcForm -> "liggr"))

    assertEquals("liggr",   verb.verbForms(INDICATIVE, Some(PRESENT), Some(Pronoun.SG_3)).strForm)
    assertEquals("liggjum", verb.verbForms(INDICATIVE, Some(PRESENT), Some(Pronoun.PL_1)).strForm)

    assertEquals("lá",      verb.verbForms(INDICATIVE, Some(PAST),    Some(Pronoun.SG_1)).strForm)
    assertEquals("lát",     verb.verbForms(INDICATIVE, Some(PAST),    Some(Pronoun.SG_2)).strForm)
    assertEquals("lágum",   verb.verbForms(INDICATIVE, Some(PAST),    Some(Pronoun.PL_1)).strForm)
    assertEquals("leginn",  verb.verbForms(PARTICIPLE, Some(PAST),    None).strForm)
  }

  @Test
  def testClass5thVerbVega(): Unit = {

    val srcForm = (INFINITIVE, None, None)

    val verb = StrongVerbContext(STRONG_5TH_CLASS, Map(srcForm -> "vega"))

    assertEquals("vegr", verb.verbForms(INDICATIVE, Some(PRESENT), Some(Pronoun.SG_3)).strForm)
    assertEquals("vegum",verb.verbForms(INDICATIVE, Some(PRESENT), Some(Pronoun.PL_1)).strForm)
    assertEquals("vá",   verb.verbForms(INDICATIVE, Some(PAST),    Some(Pronoun.SG_3)).strForm)
    assertEquals("vágum",verb.verbForms(INDICATIVE, Some(PAST),    Some(Pronoun.PL_1)).strForm)
  }

  @Test
  def testClass6VerbInflection() {

    val svStemPR = StrongVerbStem("tak", STRONG_6TH_CLASS, PRESENT_STEM)
    val svStemPS = StrongVerbStem("tak", STRONG_6TH_CLASS, PRETERITE_SINGULAR_STEM)
    val svStemPP = StrongVerbStem("tak", STRONG_6TH_CLASS, PRETERITE_PLURAL_STEM)
    val svStemPF = StrongVerbStem("tak", STRONG_6TH_CLASS, PERFECT_STEM)

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

    val stem = StrongVerbStem.fromStrRepr("fá", STRONG_7TH_CLASS, PRESENT_STEM)

    assertEquals("fá", verbFrom(stem, None, INFINITIVE).strForm)
  }

	@Test
	def sanityCheck() {

		val svStemPR = StrongVerbStem("tak", STRONG_6TH_CLASS, PRESENT_STEM)

		try {

			verbFrom(svStemPR, Some(PAST), 		PARTICIPLE)

			fail("Past participle should not be constructible from present stem")
		}
		catch {
			case e: RuntimeException => ()
		}
	}
}
