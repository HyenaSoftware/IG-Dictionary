package com.hyenawarrior.OldNorseGrammar.grammar.morphology

import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.ProductiveTransforms.{StressShift, _}
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.StemTransform.{DevoiceAfterLateral, JAugment, NasalAssimilation, Raising}
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.{I_Umlaut, StemTransform}
import com.hyenawarrior.OldNorseGrammar.grammar.phonology.Word
import org.junit.Assert._
import org.junit.Test

/**
	* Created by HyenaWarrior on 2017.10.20..
	*/
class TestTransformation {

	def consAssim(wordStr: String, suffix: String): Seq[String] = ConsonantAssimilation2
		.transform2(Word(wordStr).splitAt(-1 - suffix.length))
		.values
		.map(_.asString)
    .toSeq

	def consAssimReverse(wordStr: String, suffix: String): Seq[String] = ConsonantAssimilation2
		.reverse2(Word(wordStr).splitAt(-1 - suffix.length))
		.values
		.map(_.asString)
    .toSeq

	def consAssim(wordStr: String, suffix: String, suffix2: String): Seq[String] = {

		val word = Word(wordStr).splitAt(-1 - suffix2.length)

		val words = ConsonantAssimilation2.transform2(word).values.toSeq

		val words2 = if(words.nonEmpty) words else Seq(word)

		words2
			.map(w => {
				val wi = Word(w.morphemes.init).splitAt(-1 - suffix.length)
				val ws = ConsonantAssimilation2.transform2(wi).values.map(_ + w.morphemes.last)

				if(ws.nonEmpty) ws else Seq(w)
			})
			.flatMap(ws => ws.map(_.asString))
	}

	def consAssimReverse(wordStr: String, suffix: String, suffix2: String): Seq[String] = {

		val word = Word(wordStr).splitAt(-1 - suffix2.length)

		val words = ConsonantAssimilation2.reverse2(word).values.toSeq

		val words2 = if(words.nonEmpty) words else Seq(word)

		words2
			.map(w => {
        val wi = Word(w.morphemes.init).splitAt(-1 - suffix.length)
        val ws = ConsonantAssimilation2.reverse2(wi).values.map(_ + w.morphemes.last)

				if(ws.nonEmpty) ws else Seq(w)
      })
			.flatMap(ws => ws.map(_.asString))
	}

	private def assertContains[T](expected: Seq[T], acceptable: Seq[T], given: Seq[T]) = {

		val mustBeAcceptable = given.filterNot(expected.contains(_))
		val unacceptableValues = mustBeAcceptable.filterNot(acceptable.contains(_))
		assertTrue(s"These forms should not be exted: ${unacceptableValues.mkString("[", ", ", "]")}", unacceptableValues.isEmpty)

		assertEquals(expected, given.intersect(expected))
	}

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

	@deprecated
	@Test
	def testConsonantAssimilation(): Unit = {

		assertEquals("batzt", ConsonantAssimilation("battst"))
		assertEquals("brauzt", ConsonantAssimilation("brautst"))
		assertEquals("brauzk", ConsonantAssimilation("brautstsk"))
		assertEquals("kallask", ConsonantAssimilation("kallarsk"))
		assertEquals("kallizk", ConsonantAssimilation("kalliðsk"))
	}

	@Test
	def testConsonantAssimilationNew(): Unit = {

		// batt+st > batzt
		assertContains(Seq("battst"), Seq("batðst"), consAssim("batzt", "st"))
		assertContains(Seq("brautst"), Seq("brauðst"), consAssim("brauzt", "st"))

		// brautstsk > braut+st+sk
		assertContains(Seq("brautstsk"), Seq("brauðsk", "brautsk", "brauttsk", "brauðstsk"), consAssim("brauzk", "st", "sk"))

		assertContains(Seq("kallask"), Seq(), consAssim("kallarsk", "sk"))
		assertContains(Seq("kalliðsk"), Seq("kallitsk", "kallitstsk"), consAssim("kallizk", "ið", "sk"))
	}

  @Test
  def testConsonantAssimilationZedRev(): Unit = {

		// [batzt > battst]1 > batt+st (past of binda)
		// 1: it is automatic so no need to have anythin on the left side
		assertContains(Seq("battst"), Seq("batðst"), consAssimReverse("batzt", "st"))
		// brauzt > braut+st
		assertContains(Seq("brautst"), Seq("brauðst"), consAssimReverse("brauzt", "st"))

		// probably it is a past participle:
		// first approach: -r is stored as already merged into -sk: -sk instead of -rsk
		// Q: how can it works with adjective endings + reflexive suffix?
		// second approach: -r inflectional ending is not "pre-merged"
		// kallaðr+sk > *kallaðrsk > *kallaðsk > *kallazk
		// proposed way: kallazk > kallað-r-sk
		// kallazk > kallað-sk
		assertContains(Seq("kallaðrsk"), Seq("kallatrsk", "kallaðsk", "kallatstsk"), consAssimReverse("kallazk", "r", "sk"))
  }

	@Test
	def testConsonantAssimilationTwoPhaseRev(): Unit = {

		assertContains(Seq("brautstsk"), Seq("brautrsk", "brauðrsk", "brauðsk", "brautstsk"), consAssimReverse("brauzk", "st", "sk"))
	}

	@Test
	def testConsonantAssimilationReverse2Ignored(): Unit = {

		// -sk also a correct suffix, it's not always correct to restore it
		assertEquals(Seq("kallarsk"), consAssimReverse("kallask", "r", "sk"))
	}

	@Test
	def testConsonantAssimilationShouldIgnore(): Unit = {

		assertEquals("hestr", ConsonantAssimilation("hestr"))

		assertEquals(Seq(), consAssim("hestr", "r"))
		assertEquals(Seq(), consAssim("hamarr", "r"))
	}

	@Test
	def testConsonantAssimilationShouldIgnoreRev(): Unit = {

		assertEquals(Seq(), consAssimReverse("hestr", "r"))
		assertEquals(Seq(), consAssimReverse("hamarr", "r"))
	}

	@deprecated
	@Test
	def testConsonantAssimilationR(): Unit = {

		assertEquals("maðr",  ConsonantAssimilation("mannr"))
		assertEquals("stóll", ConsonantAssimilation("stólr"))
		assertEquals("groenn", ConsonantAssimilation("groenr"))
		assertEquals("less",   ConsonantAssimilation("lesr"))
	}

	@Test
	def testConsonantAssimilationNewR(): Unit = {

		// If the stem ends in a short stressed syllable, r does not assimilate to l or n, only
		//	to s, as in less (in contrast to dalr ‘valley.n’, vinr ‘friend.n’).
		//assertEquals(Seq("CaCall"),	ConsonantAssimilation2.transform("CaCalr", "r"))

    assertContains(Seq("maðr"), Seq("mann"), consAssim("mannr", "r"))
		assertEquals(Seq("stóll"), consAssim("stólr", "r"))
		assertEquals(Seq("grœnn"), consAssim("groenr", "r"))
		assertEquals(Seq("less"), consAssim("lesr", "r"))

		assertEquals(Seq(), consAssim("annarr", "r"))
		assertEquals(Seq(), consAssim("dýrr", "r"))
	}

	@Test
	def testConsonantAssimilationNewRRev(): Unit = {

		assertContains(Seq("mannr"), Seq("maðrr"),		consAssimReverse("maðr", "r"))
		assertEquals(Seq("stólr",	"stóllr"),	consAssimReverse("stóll", "r"))
		assertEquals(Seq("grœnr","grœnnr"), 	consAssimReverse("groenn", "r"))
		assertEquals(Seq("lesr", "lessr"), 		consAssimReverse("less", "r"))

		// If the stem ends in a short stressed syllable, r does not assimilate to l or n, only
		//	to s, as in less (in contrast to dalr ‘valley.n’, vinr ‘friend.n’).
		//assertEquals(Seq("CaCall"),	ConsonantAssimilation2.transform("CaCalr", "r"))

		assertEquals(Seq(), consAssimReverse("annarr", "r"))
	}

	@deprecated
	@Test
	def testConsonantAssimilationBlockR(): Unit = {

		assertEquals("dalr",  ConsonantAssimilation("dalr"))
		assertEquals("vinr", ConsonantAssimilation("vinr"))
	}

	@Test
	def testConsonantAssimilationNewBlockR(): Unit = {

		assertEquals(Seq(), consAssim("dalr", "r"))
		assertEquals(Seq(), consAssim("vinr", "r"))
	}

	@Test
	def testConsonantAssimilationNewReverseBlockR(): Unit = {

		assertEquals(Seq(), consAssimReverse("dalr", "r"))
		assertEquals(Seq(), consAssimReverse("vinr", "r"))
	}

	@Test
	def testConsonantAssimilationVakr(): Unit = {

		assertContains(Seq("vakr"), Seq(), consAssim("vakrr", "r"))
	}

	@Test
	def testConsonantAssimilationVakrRev() = {

		assertContains(Seq("vakrr"), Seq(), consAssimReverse("vakr", "r"))
	}

	@deprecated
	@Test
	def testConsonantAssimilationCombo(): Unit = {

		assertEquals("nagl",   ConsonantAssimilation("naglr"))
		assertEquals("menn",   ConsonantAssimilation("mennr"))
		assertEquals("skipti", ConsonantAssimilation("skiptti"))
	}

	@Test
	def testConsonantAssimilationNewImpl(): Unit = {

		assertContains(Seq("nagl"), Seq(), consAssim("naglr", "r"))
		assertContains(Seq("menn"), Seq(), consAssim("mennr", "r"))
		assertContains(Seq("skipti"), Seq(), consAssim("skiptti", "ti"))
		assertContains(Seq("gamall"), Seq(), consAssim("gamalr", 	"r"))
		assertContains(Seq(), Seq(), consAssim("hamarr", 	"r"))
	}

	@Test
	def testConsonantAssimilationNewReverseImpl(): Unit = {

		assertContains(Seq("naglr"), Seq("nagll", "naglr"), consAssimReverse("nagl", "r"))
		assertEquals(Seq("mennr"), consAssimReverse("menn", "r"))
		assertContains(Seq(), Seq("góðr"), consAssimReverse("góðr", "r"))

		// it is probably predictible: do not mix voiced and non-voiced consonants in one context
		assertContains(Seq("skiptti"), Seq("skipðti"), consAssimReverse("skipti", "ti"))
		assertContains(Seq("gamalr"), Seq("gamallr"), consAssimReverse("gamall", "r"))
	}

	@Test
	def testConsonantAssimilationMedioPassiveRev(): Unit = {

		assertContains(Seq("kalliðsk"), Seq("kallitsk"), consAssimReverse("kallizk", "ið", "sk"))
	}

	@Test
	def testConsonantAssimilation3(): Unit = {

		assertContains(Seq("gamalli"), Seq(), consAssim("gamalri", "ri"))
	}

	@Test
	def testConsonantAssimilation3Rev(): Unit = {

		assertContains(Seq("gamalri"), Seq("gamallli"), consAssimReverse("gamalli", "ri"))
	}

	@Test
	def testConsonantAssimilationVoicedDentalRegressive(): Unit = {

		assertContains(Seq("fœtt"), Seq(), consAssim("foeddt", "t"))
		assertContains(Seq("kallat"), Seq(), consAssim("kallaðt", "t"))
	}

	@Test
	def testConsonantAssimilationVoicedDentalRegressiveRev(): Unit = {

		assertContains(Seq("fœddt"), Seq("fœddt", "fœnt", "fœt", "fœtðt"), consAssimReverse("foett", "t"))

		assertContains(Seq("kallaðt"), Seq(), consAssimReverse("kallat", "t")) // supine
	}

	@Test
	def testConsonantAssimilationNasalDental(): Unit = {

		assertContains(Seq("hitt"), Seq(), consAssim("hint",   "t"))
		assertContains(Seq(),       Seq(), consAssim("annart", "t"))
	}

	@Test
	def testConsonantAssimilationNasalDentalRev(): Unit = {

		// FIXME: make difference between voiced and voiceless consonants,
		// 	avoid to mix them in the same context (or consonant cluster)
		assertContains(Seq("hint"), Seq("hiddt", "hitðt"), consAssimReverse("hitt", "t"))
	}

	@Test
	def testConsonantAssimilationNasalDentalNegRev(): Unit = {

		assertContains(Seq(), Seq(), consAssimReverse("atnart", "t"))
	}

	@Test
	def testConsonantAssimilationDD(): Unit = {

		assertContains(Seq("gladdi"), Seq(), consAssim("glaðði", "ði"))
	}

	@Test
	def testConsonantAssimilationDDRev(): Unit = {

		assertEquals(Seq("glaðði"), consAssimReverse("gladdi", "ði"))
	}

	@Test
	def testConsonantAssimilationNN2Eth(): Unit = {

		assertEquals(Seq("ǫðru"), consAssim("ǫnnru", "u"))
	}

	@Test
	def testConsonantAssimilationNN2EthRev(): Unit = {

		assertEquals(Seq("ǫnnru"), consAssimReverse("ǫðru", "u"))
	}

	@Test
	def testConsonantAssimilationGemination(): Unit = {

		assertEquals(Seq("nýtt"), consAssim("nýt", "t"))
		assertEquals(Seq("fárri"), consAssim("fári", "ri"))
	}

	@Test
	def testConsonantAssimilationGeminationRev(): Unit = {

		assertContains(Seq("nýt"), Seq("nýddt", "nýnt", "nýtðt"), consAssimReverse("nýtt", "t"))

		assertEquals(Seq("fári"), consAssimReverse("fárri", "ri"))
	}

	@Test
	def testConsonantAssimilationGeminationRev2(): Unit = {

		assertContains(Seq("dýr"), Seq("dýrr"), consAssimReverse("dýrr", "r"))
	}

	@Test
	def testConsonantAssimilationHvass(): Unit = {

		assertEquals(Seq("hvasr", "hvassr"), consAssimReverse("hvass", "r"))
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
