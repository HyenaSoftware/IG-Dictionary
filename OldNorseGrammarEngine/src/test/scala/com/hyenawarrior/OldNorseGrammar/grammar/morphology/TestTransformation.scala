package com.hyenawarrior.OldNorseGrammar.grammar.morphology

import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.ProductiveTransforms.{StressShift, _}
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.StemTransform.{DevoiceAfterLateral, JAugment, NasalAssimilation, Raising}
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.{I_Umlaut, StemTransform}
import org.junit.Assert._
import org.junit.Test

/**
	* Created by HyenaWarrior on 2017.10.20..
	*/
class TestTransformation {

	@Test
	def testSemivowelDeletion(): Unit = {

		assertEquals("krjúp", SemivowelDeletion("krjúp"))
		assertEquals("krýp", SemivowelDeletion("krjýp"))
		assertEquals("urðu", SemivowelDeletion("vurðu"))

		assertEquals("teli", SemivowelDeletion("telji"))
		assertEquals("ey", SemivowelDeletion("eyj"))
		assertEquals("stefs", SemivowelDeletion("stefjs"))
		assertEquals("hǫggum", SemivowelDeletion("hǫggvum"))
		assertEquals("sǫng", SemivowelDeletion("sǫngv"))
		assertEquals("fǫlr", SemivowelDeletion("fǫlvr"))
	}

	@Test
	def testConsonantAssimilation(): Unit = {

		assertEquals("batzt", ConsonantAssimilation("battst"))
		assertEquals("brauzt", ConsonantAssimilation("brautst"))
		assertEquals("brauzk", ConsonantAssimilation("brautstsk"))
		assertEquals("kallask", ConsonantAssimilation("kallarsk"))
		assertEquals("kallizk", ConsonantAssimilation("kalliðsk"))
	}

	@Test
	def testConsonantAssimilationReverse(): Unit = {

		assertEquals(Some("battst"), ConsonantAssimilation.unapply("batzt"))
		assertEquals(Some("brautst"), ConsonantAssimilation.unapply("brauzt"))

		// unpredicateble: when we should double the 'z'-s? (-z- > -zz- > -tsts-)
    // assertEquals(Some("brautstsk"), ConsonantAssimilation.unapply("brauzk"))

    // -sk also a correct suffix, it's not always correct to restore it
		// assertEquals(Some("kallarsk"), ConsonantAssimilation.unapply("kallask"))

		assertEquals(Some("kalliðsk"), ConsonantAssimilation.unapply("kalliðsk"))
	}

	@Test
	def testConsonantAssimilationReverse2(): Unit = {

		assertEquals(Some("mennr"), ConsonantAssimilation.unapply("mennn"))
	}

	@Test
	def testConsonantAssimilationShouldIgnore(): Unit = {

		assertEquals("hestr", ConsonantAssimilation("hestr"))
	}

	@Test
	def testConsonantAssimilationR(): Unit = {

		assertEquals("maðr",  ConsonantAssimilation("mannr"))
		assertEquals("stóll", ConsonantAssimilation("stólr"))
		assertEquals("groenn", ConsonantAssimilation("groenr"))
		assertEquals("less",   ConsonantAssimilation("lesr"))
	}

	@Test
	def testConsonantAssimilationBlockR(): Unit = {

		assertEquals("dalr",  ConsonantAssimilation("dalr"))
		assertEquals("vinr", ConsonantAssimilation("vinr"))
	}

	@Test
	def testConsonantAssimilationCombo(): Unit = {

		assertEquals("nagl",   ConsonantAssimilation("naglr"))
		assertEquals("menn",   ConsonantAssimilation("mennr"))
		assertEquals("skipti", ConsonantAssimilation("skiptti"))
	}

	@Test
	def testConsonantAssimilation2(): Unit = {

		val result = ConsonantAssimilation.transform("kallizk")

		assertEquals(Set("kallitsk", "kalliðsk"), result.toSet)
	}

	@Test
	def testGemination(): Unit = {

		assertEquals("ný" -> "tt", 	Gemination("ný", "t"))
		assertEquals("fá" -> "rri", Gemination("fá", "ri"))

		assertEquals("lét" -> "um", Gemination("lét", "um"))
		assertEquals("Cá" -> "s", 	Gemination("Cá", "s"))
	}

  @Test
  def testJuToJoAtClass2Verbs(): Unit = {

    assertEquals("brjóta",  StemTransform.JuToJo("brjúta").getOrElse("brjúta"))
    assertEquals("ljóta",   StemTransform.JuToJo("ljúta").getOrElse("ljúta"))
    assertEquals("skjóta",  StemTransform.JuToJo("skjúta").getOrElse("skjúta"))
    assertEquals("bjóða",   StemTransform.JuToJo("bjúða").getOrElse("bjúða"))
  }

  @Test
  def testNotChangeJuToJoAtClass2Verbs(): Unit = {

    assertEquals("fljúga",  StemTransform.JuToJo("fljúga").getOrElse("fljúga"))
    assertEquals("smjúga",  StemTransform.JuToJo("smjúga").getOrElse("smjúga"))
    assertEquals("krjúpa",  StemTransform.JuToJo("krjúpa").getOrElse("krjúpa"))
  }

	@Test
	def testEToJaAtClass3Verbs(): Unit = {

		assertEquals("gjald", StemTransform.Breaking("geld").getOrElse("geld"))
		assertEquals("sjarf", StemTransform.Breaking("serf").getOrElse("serf"))
		assertEquals("gemd", StemTransform.Breaking("gemd").getOrElse("gemd"))
	}

	@Test
	def testEToJaAtClass3VerbsReverse(): Unit = {

		assertEquals("geld", StemTransform.Breaking.unapply("gjald").getOrElse("gjald"))
		assertEquals("gerd", StemTransform.Breaking.unapply("gjard").getOrElse("gjard"))
		assertEquals("gjamd", StemTransform.Breaking.unapply("gjamd").getOrElse("gjamd"))
	}

	@Test
	def testEToJaAndIUmlautInversion(): Unit = {

		val stemStr = StemTransform.Breaking("geld").get
		val stemStr2 = I_Umlaut(stemStr).get
		val stemStr3 = SemivowelDeletion(stemStr2)

		assertEquals("geld", stemStr3)
	}

	/** Fracture does not occur at all if *e is preceded by v, l, or r, e.g. verða, leðr. */
	@Test
	def testDoNotBreak(): Unit = {

		assertEquals("verða", StemTransform.Breaking("verða").getOrElse("verða"))
	}

  @Test
  def testVowelDeletion(): Unit = {

    assertEquals("fá", VowelDeletion("fáa"))
		assertEquals("ám", VowelDeletion("áum"))
  }

  @Test
  def testRaisingFor3rdClassVerbs(): Unit = {

    assertEquals("brinna", Raising("brenna").get)
  }

  @Test
  def testRaisingFor5thClassVerbs(): Unit = {

    assertEquals(Some("biðj"), Raising("beðj"))
    assertEquals(Some("beðj"), Raising.unapply("biðj"))
  }

	@Test
	def testNasalAssimilationFor3rdClassVerbs(): Unit = {

		assertEquals("batt", NasalAssimilation("band").getOrElse("band"))

		val NasalAssimilation(word) = "batt"
		assertEquals("band", word)
	}

  @Test
  def testDevoiceAfterLateral3rdClassVerbs(): Unit = {

    assertEquals(Some("galt"), DevoiceAfterLateral("gald"))

    val DevoiceAfterLateral(word) = "galt"
    assertEquals("gald", word)
  }

	@Test
	def testStemAugmentation(): Unit = {

    assertEquals(Some("liggj"), JAugment("leg"))

    val JAugment(str) = "liggj"
    assertEquals("leg", str)
	}

  @Test
	def testStressShift(): Unit = {

		assertEquals("fjár" -> "ár", StressShift("féar", 2))
		assertEquals("sjúm" -> "úm", StressShift("séum", 2))
    assertEquals("sjá"  -> "á",  StressShift("séa", 1))
    assertEquals("tré"  -> "",  StressShift("tré", 0))
    assertEquals("trjúm"  -> "úm",  StressShift("tréum", 2))

		assertEquals(Some("fjár"), StressShift("féar").map(VowelDeletion(_)))
		assertEquals(Some("sjám"), StressShift("séum").map(VowelDeletion(_))) // it might be sjóm
		assertEquals(Some("sjá"),  StressShift("séa").map(VowelDeletion(_)))
		assertEquals(None,  			 StressShift("tré"))
		assertEquals(Some("trjám"),  StressShift("tréum").map(VowelDeletion(_)))
	}

	@Test
	def testStressShiftInv(): Unit = {

    assertEquals(Some("féar" -> "ar"), StressShift unapply "fjár" -> 2)
    assertEquals(Some("séum" -> "um"), StressShift unapply "sjúm" -> 2)
    assertEquals(Some("séa" -> "a"), StressShift unapply "sjá" -> 1)

		assertEquals(Some("féar"), StressShift unapply "fjár")
		assertEquals(Some("séum"), StressShift unapply "sjúm")
		assertEquals(Some("séa"), StressShift unapply "sjá")
	}

	@Test
	def testNoStressShift(): Unit = {

		assertEquals(None, StressShift unapply "fá")
		assertEquals(None, StressShift unapply "tré")
	}
}
