package com.hyenawarrior

import com.hyenawarrior.OldNorseGrammar.grammar.Pronoun
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.StemTransform.Raising
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.StrongVerbForm._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.TransformationMode.EnabledFor
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbClassEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbModeEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbTenseEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbVoice.{apply => _, unapply => _, _}
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
  @Test
  def testFormGeneration(): Unit = {

    val srcForm =  (INFINITIVE, ACTIVE, None, None)

    val verb = StrongVerb(STRONG_3RD_CLASS, Map(srcForm -> "binda"))

    // 6 x {PST, PRS} x {IND, SUBJ} = 24
    // + 1 PRESP
    // + 1 PP
    // + 1 INF
    // = 27
    // 27 x {ACT, REFL} = 54

    assertEquals(54, verb.verbForms.size)
  }

	/*
			Class 1st
			PRESENT		PAST-SG		PAST-PL		PERFECT
			i					ei				i					i
	 */

  @Test
  def testClass1stConvertFromPlToSg(): Unit = try {

    val srcForm = (INDICATIVE, ACTIVE, Some(PAST), Some(Pronoun.PL_3))
		val trgForm = (INDICATIVE, ACTIVE, Some(PAST), Some(Pronoun.SG_1))

    val verb = StrongVerb(STRONG_1ST_CLASS, Map(srcForm -> "bitu"))

    val verbForm = verb.verbForms(trgForm)

    assertEquals(verbForm.strForm, "beit")

  } catch {

    case e: RuntimeException => fail(e.getMessage)
  }

	@Test
	def testClass1stConvertFromSgToPl(): Unit = try {

		val srcForm = (INDICATIVE, ACTIVE, Some(PAST), Some(Pronoun.SG_1))
		val trgForm = (INDICATIVE, ACTIVE, Some(PAST), Some(Pronoun.PL_3))

		val verb = StrongVerb(STRONG_1ST_CLASS, Map(srcForm -> "beit"))

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

    val srcType: VerbType = (INDICATIVE, ACTIVE, Some(PAST), Some(Pronoun.SG_3))

    val verb = StrongVerb(VerbClassEnum.STRONG_2ND_CLASS, Map(srcType -> "kraup"))

		val verbform = verb.verbForms( (INFINITIVE, ACTIVE, None, None))

		assertEquals("krjúpa", verbform.strForm)
	}

	@Test
	def testClass2ndChangeToJoForPresent(): Unit = {

    val srcType: VerbType = (INDICATIVE, ACTIVE, Some(PAST), Some(Pronoun.SG_3))

    val verb = StrongVerb(VerbClassEnum.STRONG_2ND_CLASS, Map(srcType -> "skaut"))

		assertEquals("skjóta", verb.verbForms (INFINITIVE, ACTIVE, None, None).strForm)

    assertEquals("skýt",    verb.verbForms(INDICATIVE, ACTIVE, Some(PRESENT), Some(Pronoun.SG_1)).strForm)
    assertEquals("skýtr",   verb.verbForms(INDICATIVE, ACTIVE, Some(PRESENT), Some(Pronoun.SG_2)).strForm)
    assertEquals("skýtr",   verb.verbForms(INDICATIVE, ACTIVE, Some(PRESENT), Some(Pronoun.SG_3)).strForm)

    assertEquals("skjótum", verb.verbForms(INDICATIVE, ACTIVE, Some(PRESENT), Some(Pronoun.PL_1)).strForm)
    assertEquals("skjótið", verb.verbForms(INDICATIVE, ACTIVE, Some(PRESENT), Some(Pronoun.PL_2)).strForm)
    assertEquals("skjóta",  verb.verbForms(INDICATIVE, ACTIVE, Some(PRESENT), Some(Pronoun.PL_3)).strForm)

    assertEquals("skaut",   verb.verbForms(INDICATIVE, ACTIVE, Some(PAST), Some(Pronoun.SG_1)).strForm)
    assertEquals("skauzt",  verb.verbForms(INDICATIVE, ACTIVE, Some(PAST), Some(Pronoun.SG_2)).strForm)
    assertEquals("skaut",   verb.verbForms(INDICATIVE, ACTIVE, Some(PAST), Some(Pronoun.SG_3)).strForm)

    assertEquals("skutum",  verb.verbForms(INDICATIVE, ACTIVE, Some(PAST), Some(Pronoun.PL_1)).strForm)
    assertEquals("skutuð",  verb.verbForms(INDICATIVE, ACTIVE, Some(PAST), Some(Pronoun.PL_2)).strForm)
    assertEquals("skutu",   verb.verbForms(INDICATIVE, ACTIVE, Some(PAST), Some(Pronoun.PL_3)).strForm)

  }

  @Test
  def testClass2ndChangeToJoForPresentInverse(): Unit = {

    val srcType: VerbType = (INDICATIVE, ACTIVE, Some(PRESENT), Some(Pronoun.SG_3))

    val verb = StrongVerb(VerbClassEnum.STRONG_2ND_CLASS, Map(srcType -> "skýtr"))

    assertEquals("skjóta", verb.verbForms(INDICATIVE, ACTIVE, Some(PRESENT), Some(Pronoun.PL_3)).strForm)

    assertEquals("skýt",   verb.verbForms(INDICATIVE, ACTIVE, Some(PRESENT), Some(Pronoun.SG_1)).strForm)
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

		val verbform = StrongVerbForm.verbFrom(stem, Pronoun.SG_2, PAST, INDICATIVE, ACTIVE)

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

    assertEquals("bregða", StrongVerbForm.verbFrom(stem, None, INFINITIVE, ACTIVE).strForm)
  }

	@Test
	def testClass3rdRaising(): Unit = {

		val stem = StrongVerbStem.fromStrRepr("spinn", STRONG_3RD_CLASS, PRESENT_STEM)

    val StrongVerbStem(normalizedStemStr, _, _, EnabledFor(Raising)) = stem
    assertEquals("spenn", normalizedStemStr)

		val verbform = StrongVerbForm.verbFrom(stem, None, INFINITIVE, ACTIVE)
		assertEquals("spinna", verbform.strForm)
	}

	@Test
	def testClass3rdNasalAssimilation(): Unit = {

		val srcType: VerbType = (INDICATIVE, ACTIVE, Some(PAST), Some(Pronoun.SG_3))

    val verb = StrongVerb(STRONG_3RD_CLASS, Map(srcType -> "batt"))

    assertEquals("binda", verb.verbForms (INFINITIVE, ACTIVE, None, None).strForm)
    assertEquals("batt",  verb.verbForms(INDICATIVE, ACTIVE, Some(PAST), Some(Pronoun.SG_1)).strForm)
    assertEquals("batzt", verb.verbForms(INDICATIVE, ACTIVE, Some(PAST), Some(Pronoun.SG_2)).strForm)
    assertEquals("batt",  verb.verbForms(INDICATIVE, ACTIVE, Some(PAST), Some(Pronoun.SG_3)).strForm)
	}

  /*
   The following two test cases are focusing on to use the same properties on the output forms during transformation
   'e' sometimes changes to 'i' in the 3rd class of verbs, like in the case of "binda", but there are exceptions
   like "brenna"
  */

  @Test
  def testCoTransformation1(): Unit = {

    val srcType: VerbType = (INDICATIVE, ACTIVE, Some(PRESENT), Some(Pronoun.SG_1))

    val verb = StrongVerb(STRONG_3RD_CLASS, Map(srcType -> "brenn"))

    assertEquals("brennr",  verb.verbForms(INDICATIVE, ACTIVE, Some(PRESENT), Some(Pronoun.SG_3)).strForm)
  }

  @Test
  def testCoTransformation2(): Unit = {

    val srcType: VerbType = (INDICATIVE, ACTIVE, Some(PRESENT), Some(Pronoun.SG_1))

    val verb = StrongVerb(STRONG_3RD_CLASS, Map(srcType -> "bind"))

    assertEquals("bindr",  verb.verbForms(INDICATIVE, ACTIVE, Some(PRESENT), Some(Pronoun.SG_3)).strForm)
  }

  @Test
  def testClass3rdStems(): Unit = {

    val srcForm =  (INFINITIVE, ACTIVE, None, None)
    val verb = StrongVerb(STRONG_3RD_CLASS, Map(srcForm -> "spinna"))

    assertEquals("spann",		verb.verbForms(INDICATIVE, ACTIVE, Some(PAST), Some(Pronoun.SG_3)).strForm)
    assertEquals("spunnu",	verb.verbForms(INDICATIVE, ACTIVE, Some(PAST), Some(Pronoun.PL_3)).strForm)

    assertEquals("spunninn",	verb.verbForms( PARTICIPLE, ACTIVE, Some(PAST), None).strForm)
  }

  @Test
  def testClass3rdFromJaStem(): Unit = {

    val stem = StrongVerbStem.fromStrRepr("hjalp", STRONG_3RD_CLASS, PRESENT_STEM)

		assertEquals("hjalpa", 	StrongVerbForm.verbFrom(stem, None, INFINITIVE, ACTIVE).strForm)
		assertEquals("help", 		StrongVerbForm.verbFrom(stem, Pronoun.SG_1, PRESENT, INDICATIVE, ACTIVE).strForm)
		assertEquals("hjǫlpum",	StrongVerbForm.verbFrom(stem, Pronoun.PL_1, PRESENT, INDICATIVE, ACTIVE).strForm)
	}

  @Test
  def testClass3rdStemFinalDevoicingAfterL(): Unit = {

    val stem = StrongVerbStem.fromStrRepr("gald", STRONG_3RD_CLASS, PRETERITE_SINGULAR_STEM)

    assertEquals("galzt", StrongVerbForm.verbFrom(stem, Pronoun.SG_2, PAST, INDICATIVE, ACTIVE).strForm)
    assertEquals("galt",  StrongVerbForm.verbFrom(stem, Pronoun.SG_3, PAST, INDICATIVE, ACTIVE).strForm)
  }

  /** Fracture does not occur at all if *e is preceded by v, l, or r, e.g. verða, leðr. */
  @Test
  def testClass3rdFromDoNotChangeStemToJa(): Unit = {

    val srcType: VerbType = (INDICATIVE, ACTIVE, Some(PRESENT), Some(Pronoun.SG_3))

    val verb = StrongVerb(STRONG_3RD_CLASS, Map(srcType -> "verðr"))

    assertEquals("verða", verb.verbForms (INFINITIVE, ACTIVE, None, None).strForm)
		assertEquals("verð", 	verb.verbForms(INDICATIVE, ACTIVE, Some(PRESENT), Some(Pronoun.SG_1)).strForm)
    assertEquals("varð", 	verb.verbForms(INDICATIVE, ACTIVE, Some(PAST), Some(Pronoun.SG_3)).strForm)
    assertEquals("urðu",	verb.verbForms(INDICATIVE, ACTIVE, Some(PAST), Some(Pronoun.PL_3)).strForm)
  }

  @Test
  def testClass3rdFromJaStemFromPastTense(): Unit = {

    val srcType: VerbType = (INDICATIVE, ACTIVE, Some(PAST), Some(Pronoun.SG_3))

    val verb = StrongVerb(STRONG_3RD_CLASS, Map(srcType -> "halp"))

    assertEquals("hjalpa", 	verb.verbForms (INFINITIVE, ACTIVE, None, None).strForm)
    assertEquals("help", 		verb.verbForms(INDICATIVE, ACTIVE, Some(PRESENT), Some(Pronoun.SG_1)).strForm)
    assertEquals("hjǫlpum",	verb.verbForms(INDICATIVE, ACTIVE, Some(PRESENT), Some(Pronoun.PL_1)).strForm)
    assertEquals("halpt", 	verb.verbForms(INDICATIVE, ACTIVE, Some(PAST),    Some(Pronoun.SG_2)).strForm)
  }

  @Test
  def testClass3rdChangeFromJa(): Unit = {

    val srcForm = (INDICATIVE, ACTIVE, Some(PRESENT), Some(Pronoun.PL_3))

    val verb = StrongVerb(STRONG_3RD_CLASS, Map(srcForm -> "hjalpa"))

    val verbFormP3 = verb.verbForms(INDICATIVE, ACTIVE, Some(PRESENT), Some(Pronoun.SG_3))

    assertEquals("helpr", verbFormP3.strForm)
  }


  @Test
  def testClass3rdChangeToJa(): Unit = {

    val srcForm = (INDICATIVE, ACTIVE, Some(PRESENT), Some(Pronoun.SG_3))

    val verb = StrongVerb(STRONG_3RD_CLASS, Map(srcForm -> "helpr"))

    val verbFormP3 = verb.verbForms(INDICATIVE, ACTIVE, Some(PRESENT), Some(Pronoun.PL_3))

    assertEquals("hjalpa", verbFormP3.strForm)
  }

	@Test
	def testClass3rdUUmlaut(): Unit = {

		val presStem = StrongVerbStem.fromStrRepr("sekkv", STRONG_3RD_CLASS, PRESENT_STEM)

		assertEquals("søkkr", 	StrongVerbForm.verbFrom(presStem, Pronoun.SG_3, PRESENT, INDICATIVE, ACTIVE).strForm)
		assertEquals("søkkva", 	StrongVerbForm.verbFrom(presStem, Pronoun.PL_3, PRESENT, INDICATIVE, ACTIVE).strForm)

		val pastSgStem = StrongVerbStem.fromStrRepr("sakkv", STRONG_3RD_CLASS, PRETERITE_SINGULAR_STEM)
		assertEquals("sǫkk", 	StrongVerbForm.verbFrom(pastSgStem, Pronoun.SG_3, PAST, INDICATIVE, ACTIVE).strForm)

		val pastPlStem = StrongVerbStem.fromStrRepr("sukk", STRONG_3RD_CLASS, PRETERITE_PLURAL_STEM)
		assertEquals("sukku", StrongVerbForm.verbFrom(pastPlStem, Pronoun.PL_3, PAST, INDICATIVE, ACTIVE).strForm)
	}

  @Test
  def testClass3rdUUmlaut2(): Unit = {

    val srcForm = (INDICATIVE, ACTIVE, Some(PRESENT), Some(Pronoun.PL_3))
    val irregularPerfect = ( PARTICIPLE, ACTIVE, Some(PAST), None)

    val verb = StrongVerb(STRONG_3RD_CLASS, Map(srcForm -> "søkkva", irregularPerfect -> "sokkinn"))

    assertEquals("søkkr", 	verb.verbForms(INDICATIVE, ACTIVE,  Some(PRESENT), Some(Pronoun.SG_3)).strForm)
    assertEquals("søkkva", 	verb.verbForms(INDICATIVE, ACTIVE,  Some(PRESENT), Some(Pronoun.PL_3)).strForm)

    assertEquals("sǫkk", 	  verb.verbForms(INDICATIVE, ACTIVE,  Some(PAST), Some(Pronoun.SG_3)).strForm)
    assertEquals("sukku",   verb.verbForms(INDICATIVE, ACTIVE,  Some(PAST), Some(Pronoun.PL_3)).strForm)

    assertEquals("sokkinn", verb.verbForms(PARTICIPLE, ACTIVE, Some(PAST),    None).strForm)
  }

  @Test
  def testClass3rdSongva(): Unit = {

    val srcForm = (INDICATIVE, ACTIVE, Some(PRESENT), Some(Pronoun.PL_3))
    val irregularPerfect = ( PARTICIPLE, ACTIVE, Some(PAST), None)

    val verb = StrongVerb(STRONG_3RD_CLASS, Map(srcForm -> "syngva", irregularPerfect -> "sunginn"))

		assertEquals("syngva", 	verb.verbForms (INFINITIVE, ACTIVE, None, None).strForm)
		assertEquals("syngr", 	verb.verbForms(INDICATIVE, ACTIVE, Some(PRESENT), Some(Pronoun.SG_3)).strForm)
    assertEquals("sǫng",    verb.verbForms(INDICATIVE, ACTIVE, Some(PAST),    Some(Pronoun.SG_3)).strForm)

    assertEquals("sunginn",  verb.verbForms( PARTICIPLE, ACTIVE, Some(PAST),    None).strForm)
  }

	@Test
	def testClass3rdUmlautedAblautExtraction(): Unit = {

		val verb = StrongVerbForm.fromStringRepr("syngva", STRONG_3RD_CLASS, (INDICATIVE, ACTIVE, Some(PRESENT), Some(Pronoun.PL_3)))

		val StrongVerbForm(_, verbStem) = verb

		assertEquals("singv", verbStem.stringForm())
	}

  @Test
  def testClass3rdUmlautedAblautExtraction2(): Unit = {

    val verb = StrongVerbForm.fromStringRepr("syngr", STRONG_3RD_CLASS, (INDICATIVE, ACTIVE, Some(PRESENT), Some(Pronoun.SG_3)))

    val StrongVerbForm(_, verbStem) = verb

    assertEquals("singv", verbStem.stringForm())
  }

  @Test
  def testClass3rdInflectionCornerCases(): Unit = {

    val verb = StrongVerbForm.fromStringRepr("batzt", STRONG_3RD_CLASS, (INDICATIVE, ACTIVE, Some(PAST), Some(Pronoun.SG_2)))

    val StrongVerbForm((_, verbStem)) = verb

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

    val srcForm = (INDICATIVE, ACTIVE, Some(PRESENT), Some(Pronoun.SG_2))

    val verb = StrongVerb(STRONG_5TH_CLASS, Map(srcForm -> "liggr"))

    assertEquals("liggr",   verb.verbForms(INDICATIVE, ACTIVE, Some(PRESENT), Some(Pronoun.SG_3)).strForm)
    assertEquals("liggjum", verb.verbForms(INDICATIVE, ACTIVE, Some(PRESENT), Some(Pronoun.PL_1)).strForm)

    assertEquals("lá",      verb.verbForms(INDICATIVE, ACTIVE, Some(PAST),    Some(Pronoun.SG_1)).strForm)
    // gemination doubles the 't'
    assertEquals("látt",    verb.verbForms(INDICATIVE, ACTIVE, Some(PAST),    Some(Pronoun.SG_2)).strForm)
    assertEquals("lágum",   verb.verbForms(INDICATIVE, ACTIVE, Some(PAST),    Some(Pronoun.PL_1)).strForm)
    assertEquals("leginn",  verb.verbForms(PARTICIPLE, ACTIVE, Some(PAST),    None).strForm)
  }

  @Test
  def testClass5thVerbReka(): Unit = {

    val inf = (INFINITIVE, ACTIVE, None, None)

    val verb = StrongVerb(VerbClassEnum.STRONG_5TH_CLASS, Map(inf -> "reka"))

    assertEquals("rek",  verb.verbForms(INDICATIVE, ACTIVE, Some(PRESENT), Some(Pronoun.SG_1)).strForm)
    assertEquals("rekr", verb.verbForms(INDICATIVE, ACTIVE, Some(PRESENT), Some(Pronoun.SG_3)).strForm)

    assertEquals("rekum",  verb.verbForms(INDICATIVE, ACTIVE, Some(PRESENT), Some(Pronoun.PL_1)).strForm)
    assertEquals("rekið",  verb.verbForms(INDICATIVE, ACTIVE, Some(PRESENT), Some(Pronoun.PL_2)).strForm)
    assertEquals("reka",   verb.verbForms(INDICATIVE, ACTIVE, Some(PRESENT), Some(Pronoun.PL_3)).strForm)

    assertEquals("rak",   verb.verbForms(INDICATIVE, ACTIVE, Some(PAST), Some(Pronoun.SG_1)).strForm)
    assertEquals("rakt",  verb.verbForms(INDICATIVE, ACTIVE, Some(PAST), Some(Pronoun.SG_2)).strForm)
    assertEquals("rak",   verb.verbForms(INDICATIVE, ACTIVE, Some(PAST), Some(Pronoun.SG_3)).strForm)

    assertEquals("rákum",  verb.verbForms(INDICATIVE, ACTIVE, Some(PAST), Some(Pronoun.PL_1)).strForm)
    assertEquals("rákuð",  verb.verbForms(INDICATIVE, ACTIVE, Some(PAST), Some(Pronoun.PL_2)).strForm)
    assertEquals("ráku",   verb.verbForms(INDICATIVE, ACTIVE, Some(PAST), Some(Pronoun.PL_3)).strForm)
  }

  @Test
  def testClass5thVerbVega(): Unit = {

    val srcForm =  (INFINITIVE, ACTIVE, None, None)

    val verb = StrongVerb(STRONG_5TH_CLASS, Map(srcForm -> "vega"))

    assertEquals("vegr", verb.verbForms(INDICATIVE, ACTIVE, Some(PRESENT), Some(Pronoun.SG_3)).strForm)
    assertEquals("vegum",verb.verbForms(INDICATIVE, ACTIVE, Some(PRESENT), Some(Pronoun.PL_1)).strForm)
    assertEquals("vá",   verb.verbForms(INDICATIVE, ACTIVE, Some(PAST),    Some(Pronoun.SG_3)).strForm)
    assertEquals("vágum",verb.verbForms(INDICATIVE, ACTIVE, Some(PAST),    Some(Pronoun.PL_1)).strForm)
  }

  @Test
  def testClass5thVerbStressShift(): Unit = {

    val inf = (INFINITIVE, ACTIVE, None, None)

    val verb = StrongVerb(VerbClassEnum.STRONG_5TH_CLASS, Map(inf -> "sjá"))

    assertEquals("sé",  verb.verbForms(INDICATIVE, ACTIVE, Some(PRESENT), Some(Pronoun.SG_1)).strForm)
    assertEquals("sér", verb.verbForms(INDICATIVE, ACTIVE, Some(PRESENT), Some(Pronoun.SG_3)).strForm)
    // [sé + um] -> [sj + óum] -> [sj + áum] -> sjáum --(vowel-deletion)-> sjám
    // [sé + um] ---> [sj + áum] -> sjáa --(vowel-deletion)-> sjá
    //assertEquals("sjám", verb.verbForms(INDICATIVE, ACTIVE, Some(PRESENT), Some(Pronoun.PL_1)).strForm)

    assertEquals("sátt", verb.verbForms(INDICATIVE, ACTIVE, Some(PAST), Some(Pronoun.SG_2)).strForm)
    assertEquals("sá", verb.verbForms(INDICATIVE, ACTIVE, Some(PAST), Some(Pronoun.SG_3)).strForm)
    assertEquals("sám", verb.verbForms(INDICATIVE, ACTIVE, Some(PAST), Some(Pronoun.PL_1)).strForm)
    // 'sá' or 'sáu'
    val pastP3 = verb.verbForms(INDICATIVE, ACTIVE, Some(PAST), Some(Pronoun.PL_3)).strForm
    assertTrue(pastP3 == "sáu" || pastP3 == "sá")

    assertEquals("sénn",  verb.verbForms( PARTICIPLE, ACTIVE, Some(PAST),    None).strForm)
  }

  @Test
  def testClass6VerbInflection() {

    val svStemPR = StrongVerbStem("tak", STRONG_6TH_CLASS, PRESENT_STEM)
    val svStemPS = StrongVerbStem("tók", STRONG_6TH_CLASS, PRETERITE_SINGULAR_STEM)
    val svStemPP = StrongVerbStem("tók", STRONG_6TH_CLASS, PRETERITE_PLURAL_STEM)
    val svStemPF = StrongVerbStem("tak", STRONG_6TH_CLASS, PERFECT_STEM)

    assertEquals("taka",		verbFrom(svStemPR, None, INFINITIVE, ACTIVE).strForm)

    assertEquals("tekinn", 	verbFrom(svStemPF, Some(PAST), 		PARTICIPLE, ACTIVE).strForm)
    assertEquals("takandi", verbFrom(svStemPR, Some(PRESENT), PARTICIPLE, ACTIVE).strForm)

    assertEquals("tek", 		verbFrom(svStemPR, Pronoun.SG_1, PRESENT,	INDICATIVE, ACTIVE).strForm)
    assertEquals("tekr", 		verbFrom(svStemPR, Pronoun.SG_2, PRESENT,	INDICATIVE, ACTIVE).strForm)
    assertEquals("tekr", 		verbFrom(svStemPR, Pronoun.SG_3, PRESENT,	INDICATIVE, ACTIVE).strForm)

    assertEquals("tǫkum", 	verbFrom(svStemPR, Pronoun.PL_1, PRESENT,	INDICATIVE, ACTIVE).strForm)
    assertEquals("takið", 	verbFrom(svStemPR, Pronoun.PL_2, PRESENT,	INDICATIVE, ACTIVE).strForm)
    assertEquals("taka",		verbFrom(svStemPR, Pronoun.PL_3, PRESENT,	INDICATIVE, ACTIVE).strForm)

    assertEquals("tók", 		verbFrom(svStemPS, Pronoun.SG_1, PAST, 		INDICATIVE, ACTIVE).strForm)
    assertEquals("tókt" /*"tókst"*/, verbFrom(svStemPS, Pronoun.SG_2, PAST,	INDICATIVE, ACTIVE).strForm)
    assertEquals("tók", 		verbFrom(svStemPS, Pronoun.SG_3, PAST,    INDICATIVE, ACTIVE).strForm)

    assertEquals("tókum",		verbFrom(svStemPP, Pronoun.PL_1, PAST, INDICATIVE, ACTIVE).strForm)
    assertEquals("tókuð",		verbFrom(svStemPP, Pronoun.PL_2, PAST, INDICATIVE, ACTIVE).strForm)
    assertEquals("tóku",		verbFrom(svStemPP, Pronoun.PL_3, PAST, INDICATIVE, ACTIVE).strForm)
  }

  @Test
  def testClass73VowelDeletion(): Unit = {

    val stem = StrongVerbStem.fromStrRepr("fá", STRONG_7_3_CLASS, PRESENT_STEM)
    assertEquals("fá", verbFrom(stem, None, INFINITIVE, ACTIVE).strForm)


    val srcForm =  (INFINITIVE, ACTIVE, None, None)
    val verb = StrongVerb(STRONG_7_3_CLASS, Map(srcForm -> "fá"))

    assertEquals("fá", verb.verbForms (INFINITIVE, ACTIVE, None, None).strForm)
  }

  @Test
  def testClass71(): Unit = {

    val srcForm = (INDICATIVE, ACTIVE, Some(PRESENT), Some(Pronoun.PL_3))

    val verb = StrongVerb(STRONG_7_1_CLASS, Map(srcForm -> "heita"))

    assertEquals("heita", verb.verbForms (INFINITIVE, ACTIVE, None, None).strForm)
    assertEquals("hézt", 	verb.verbForms(INDICATIVE, ACTIVE, Some(PAST), Some(Pronoun.SG_2)).strForm)
    assertEquals("hétuð", verb.verbForms(INDICATIVE, ACTIVE, Some(PAST), Some(Pronoun.PL_2)).strForm)
  }

  @Test
  def testClass72a(): Unit = {

    val srcForm =  (INFINITIVE, ACTIVE, None, None)

    val verb = StrongVerb(STRONG_7_2A_CLASS, Map(srcForm -> "auka"))

    assertEquals("jók", 	verb.verbForms(INDICATIVE, ACTIVE, Some(PAST), Some(Pronoun.SG_1)).strForm)
    assertEquals("jókum", verb.verbForms(INDICATIVE, ACTIVE, Some(PAST), Some(Pronoun.PL_1)).strForm)
    assertEquals("aukinn",verb.verbForms( PARTICIPLE, ACTIVE, Some(PAST), None).strForm)
  }

  @Test
  def testClass72b(): Unit = {

    val srcForm =  (INFINITIVE, ACTIVE, None, None)
    val irregularPastPlural = (INDICATIVE, ACTIVE, Some(PAST), Some(Pronoun.PL_3))
    val irregularPastSingular = (INDICATIVE, ACTIVE, Some(PAST), Some(Pronoun.SG_3))

    val verb = StrongVerb(STRONG_7_2B_CLASS, Map(srcForm -> "búa", irregularPastPlural -> "bjuggu", irregularPastSingular -> "bjó"))

    // past-plural stem has preference over the present stem for past-single stem
    assertEquals("bjó", 	  verb.verbForms(INDICATIVE, ACTIVE, Some(PAST), Some(Pronoun.SG_1)).strForm)

    // impossible to compute this form: the ancestor PGmc verb was a weak verb, probably its dental suffix
    //  changed as *būdēdum > bjuggum
    assertEquals("bjuggum", verb.verbForms(INDICATIVE, ACTIVE, Some(PAST), Some(Pronoun.PL_1)).strForm)
    assertEquals("búinn",   verb.verbForms( PARTICIPLE, ACTIVE, Some(PAST), None).strForm)
  }

  @Test
  def testClass72bPast(): Unit = {

    val srcForm = (INDICATIVE, ACTIVE, Some(PAST), Some(Pronoun.SG_1))
    val irregularPastPlural = (INDICATIVE, ACTIVE, Some(PAST), Some(Pronoun.PL_1))

    val verb = StrongVerb(STRONG_7_2B_CLASS, Map(srcForm -> "bjó", irregularPastPlural -> "bjuggum"))

    // as bjuggum is not a regular form of the verb búa, it's not expectad that
    // the engine can compute forms other than past plural

    assertEquals("búa", 	  verb.verbForms (INFINITIVE, ACTIVE, None, None).strForm)
    assertEquals("bý", 	    verb.verbForms(INDICATIVE, ACTIVE, Some(PRESENT), Some(Pronoun.SG_1)).strForm)
    assertEquals("býr", 	  verb.verbForms(INDICATIVE, ACTIVE, Some(PRESENT), Some(Pronoun.SG_3)).strForm)
    assertEquals("bjótt",   verb.verbForms(INDICATIVE, ACTIVE, Some(PAST), Some(Pronoun.SG_2)).strForm)
    assertEquals("bjuggum", verb.verbForms(INDICATIVE, ACTIVE, Some(PAST), Some(Pronoun.PL_1)).strForm)
    assertEquals("búinn",   verb.verbForms( PARTICIPLE, ACTIVE, Some(PAST), None).strForm)
  }

  @Test
  def testClass72bHoggva(): Unit = {

    val srcForm =  (INFINITIVE, ACTIVE, None, None)
    val pp = ( PARTICIPLE, ACTIVE, Some(PAST), None)

    val verb = StrongVerb(STRONG_7_2B_CLASS, Map(srcForm -> "hǫggva", pp -> "hǫggvinn"))

    assertEquals("hǫggva",  verb.verbForms (INFINITIVE, ACTIVE, None, None).strForm)
    // FIXME: haggv -> heggv -> høggv, should both of I and U umlaut applied ?
    assertEquals("hegg",    verb.verbForms(INDICATIVE, ACTIVE, Some(PRESENT), Some(Pronoun.SG_1)).strForm)
    //assertEquals("hjó",     verb.verbForms(INDICATIVE, ACTIVE, Some(PAST), Some(Pronoun.SG_3)).strForm)
    assertEquals("hjuggum", verb.verbForms(INDICATIVE, ACTIVE, Some(PAST), Some(Pronoun.PL_1)).strForm)
    assertEquals("hǫggvinn",   verb.verbForms( PARTICIPLE, ACTIVE, Some(PAST), None).strForm)
  }

  case class Format[K, V](m: Map[K, V]) {

    override def toString: String = m.toSeq.sortBy(_._1.toString).mkString("\n")
  }

  @Test
  def testClass72bHoggvaPast(): Unit = {

    val presentS3 = (INDICATIVE, ACTIVE, Some(PRESENT), Some(Pronoun.SG_3))
    val pastS3    = (INDICATIVE, ACTIVE, Some(PAST), Some(Pronoun.SG_3))
    val pastP3    = (INDICATIVE, ACTIVE, Some(PAST), Some(Pronoun.PL_3))
    val perf      = ( PARTICIPLE, ACTIVE, Some(PAST), None)

    val verb = StrongVerb(STRONG_7_2B_CLASS, Map(presentS3 -> "heggr", pastS3 -> "hjó", pastP3 -> "hjuggu", perf -> "hǫggvinn"))

    assertEquals("hǫggva",  verb.verbForms (INFINITIVE, ACTIVE, None, None).strForm)
    assertEquals("hegg",    verb.verbForms(INDICATIVE, ACTIVE, Some(PRESENT), Some(Pronoun.SG_1)).strForm)
    assertEquals("heggr",   verb.verbForms(INDICATIVE, ACTIVE, Some(PRESENT), Some(Pronoun.SG_3)).strForm)
    assertEquals("hjó",     verb.verbForms(INDICATIVE, ACTIVE, Some(PAST), Some(Pronoun.SG_3)).strForm)
    assertEquals("hjuggum", verb.verbForms(INDICATIVE, ACTIVE, Some(PAST), Some(Pronoun.PL_1)).strForm)
    assertEquals("hǫggvinn",   verb.verbForms( PARTICIPLE, ACTIVE, Some(PAST), None).strForm)
  }

  @Test
  def testClass73(): Unit = {

    val srcForm =  (INFINITIVE, ACTIVE, None, None)

    val verb = StrongVerb(STRONG_7_3_CLASS, Map(srcForm -> "falla"))

    assertEquals("fell", 	verb.verbForms(INDICATIVE, ACTIVE, Some(PAST), Some(Pronoun.SG_1)).strForm)
    assertEquals("fellum", verb.verbForms(INDICATIVE, ACTIVE, Some(PAST), Some(Pronoun.PL_1)).strForm)
    assertEquals("fallinn",verb.verbForms( PARTICIPLE, ACTIVE, Some(PAST), None).strForm)
  }

  @Test
  def testClass73Past(): Unit = {

    val srcForm = (INDICATIVE, ACTIVE, Some(PAST), Some(Pronoun.PL_3))

    val verb = StrongVerb(STRONG_7_3_CLASS, Map(srcForm -> "fengu"))

    assertEquals("fekk",    verb.verbForms(INDICATIVE, ACTIVE, Some(PAST), Some(Pronoun.SG_3)).strForm)
    assertEquals("fengum",  verb.verbForms(INDICATIVE, ACTIVE, Some(PAST), Some(Pronoun.PL_1)).strForm)
    assertEquals("fenginn", verb.verbForms( PARTICIPLE, ACTIVE, Some(PAST), None).strForm)
  }

  @Test
  def testClass73PastInverse(): Unit = {

    val srcForm = (INDICATIVE, ACTIVE, Some(PAST), Some(Pronoun.SG_3))

    val verb = StrongVerb(STRONG_7_3_CLASS, Map(srcForm -> "fekk"))

    assertEquals("fengu",    verb.verbForms(INDICATIVE, ACTIVE, Some(PAST), Some(Pronoun.PL_3)).strForm)
  }

  @Test
  def testClass74Present(): Unit = {

    val srcForm =  (INFINITIVE, ACTIVE, None, None)

    val verb = StrongVerb(STRONG_7_4_CLASS, Map(srcForm -> "fá"))

    assertEquals("fæ", verb.verbForms(INDICATIVE, ACTIVE, Some(PRESENT), Some(Pronoun.SG_1)).strForm)
    assertEquals("fám", verb.verbForms(INDICATIVE, ACTIVE, Some(PRESENT), Some(Pronoun.PL_1)).strForm)
  }

  @Test
  def testClass74VowelDeletion(): Unit = {

    val stem = StrongVerbStem.fromStrRepr("fá", STRONG_7_4_CLASS, PRESENT_STEM)

    assertEquals("fá", verbFrom(stem, None, INFINITIVE, ACTIVE).strForm)
  }

	@Test
	def sanityCheck() {

		val svStemPR = StrongVerbStem("tak", STRONG_6TH_CLASS, PRESENT_STEM)

		try {

			verbFrom(svStemPR, Some(PAST), 		PARTICIPLE, ACTIVE)

			fail("Past participle should not be constructible from present stem")
		}
		catch {
			case e: RuntimeException => ()
		}
	}
}
