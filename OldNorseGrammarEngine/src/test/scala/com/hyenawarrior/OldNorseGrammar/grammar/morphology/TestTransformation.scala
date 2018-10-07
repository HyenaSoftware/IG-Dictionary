package com.hyenawarrior.OldNorseGrammar.grammar.morphology

import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.ProductiveTransforms.{StressShift, _}
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.StemTransform.{DevoiceAfterLateral, JAugment, NasalAssimilation, Raising}
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.{I_Umlaut, StemTransform}
import org.junit.Assert._
import org.junit.{Ignore, Test}

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
		assertEquals(Seq("batzt"),  ConsonantAssimilation2.transform("battst", "st"))
		assertEquals(Seq("brauzt"), ConsonantAssimilation2.transform("brautst", "st"))
		assertEquals(Seq("brauzk"),	 ConsonantAssimilation2.transform("brautstsk", "sk"))

		// ignore
		assertEquals(Seq("kallask"), ConsonantAssimilation2.transform("kallarsk", "sk"))
		assertEquals(Seq("kallizk"), ConsonantAssimilation2.transform("kalliðsk", "sk"))
	}

  @deprecated
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
  def testConsonantAssimilationZedRev(): Unit = {

		// batzt > batt+st
    assertEquals(Seq("battst",  "batðst"),	ConsonantAssimilation2.reverse("batzt", "st"))
		// brauzt > braut+st
    assertEquals(Seq("brautst", "brauðst"), ConsonantAssimilation2.reverse("brauzt", "st"))

		// probably it is a past participle:
		// first approach: -r is stored as already merged into -sk: -sk instead of -rsk
		// Q: how can it works with adjective endings + reflexive suffix?
		assertEquals(Seq("kallatsk", "kallaðsk"), ConsonantAssimilation2.reverse("kallazk", "sk")) 	// kallazk > kallað-sk

		// second approach: -r inflectional ending is not "pre-merged"
    assertEquals(Seq("kallaðrsk"), ConsonantAssimilation2.reverse("kallazk", "rsk"))	// kallazk > kallað-r-sk

		// proposed way:
		//assertEquals(Seq("kallaðrsk"), ConsonantAssimilation2.reverse("kallazk", "r", "sk"))	// kallazk > kallað-r-sk
  }

	@Ignore("it would require some kind of conditional rule chaining, it's not trivial")
	@Test
	def testConsonantAssimilationTwoPhaseRev(): Unit = {

		// unpredicateble: when we should double the 'z'-s? (-z- > -zz- > -tsts-)
		assertEquals(Seq("brautstsk"), ConsonantAssimilation2.reverse("brauzk", "sk"))
		//assertEquals(Seq("brautstsk"), ConsonantAssimilation2.reverse("brauzk", "st", "sk"))
	}

	@Ignore("suffix is already stored as -sk instead of -rsk, it makes the *rsk > *sk conversion pointless")
	@Test
	def testConsonantAssimilationReverse2Ignored(): Unit = {

		// -sk also a correct suffix, it's not always correct to restore it
		assertEquals(Seq("kallarsk"), ConsonantAssimilation2.reverse("kallask", "rsk"))
	}

  @deprecated
	@Test
	def testConsonantAssimilationReverse3(): Unit = {

		assertEquals(Some("mennr"), ConsonantAssimilation.unapply("mennn"))
	}

	@Test
	def testConsonantAssimilationShouldIgnore(): Unit = {

		assertEquals("hestr", ConsonantAssimilation("hestr"))

    assertEquals(Seq(), ConsonantAssimilation2.transform("hestr", "r"))
		assertEquals(Seq(), ConsonantAssimilation2.transform("hamarr", "r"))
	}

	@Test
	def testConsonantAssimilationShouldIgnoreRev(): Unit = {

		assertEquals(Seq(), ConsonantAssimilation2.reverse("hestr", "r"))
		assertEquals(Seq(), ConsonantAssimilation2.reverse("hamarr", "r"))
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

		assertEquals(Seq("maðr", "mann"),		ConsonantAssimilation2.transform("mannr", "r"))
		assertEquals(Seq("stóll"),	ConsonantAssimilation2.transform("stólr", "r"))
		assertEquals(Seq("groenn"),	ConsonantAssimilation2.transform("groenr", "r"))
		assertEquals(Seq("less"),		ConsonantAssimilation2.transform("lesr", "r"))

		// If the stem ends in a short stressed syllable, r does not assimilate to l or n, only
		//	to s, as in less (in contrast to dalr ‘valley.n’, vinr ‘friend.n’).
		//assertEquals(Seq("CaCall"),	ConsonantAssimilation2.transform("CaCalr", "r"))

		assertEquals(Seq(),		ConsonantAssimilation2.transform("annarr", "r"))
		assertEquals(Seq(),		ConsonantAssimilation2.transform("dýrr", "r"))
	}

	@Test
	def testConsonantAssimilationNewRRev(): Unit = {

		assertEquals(Seq("mannr", "maðrr"),		ConsonantAssimilation2.reverse("maðr", "r"))
		assertEquals(Seq("stólr",	"stóllr"),	ConsonantAssimilation2.reverse("stóll", "r"))
		assertEquals(Seq("groenr","groennr"),	ConsonantAssimilation2.reverse("groenn", "r"))
		assertEquals(Seq("lesr", "lessr"),		ConsonantAssimilation2.reverse("less", "r"))

		// If the stem ends in a short stressed syllable, r does not assimilate to l or n, only
		//	to s, as in less (in contrast to dalr ‘valley.n’, vinr ‘friend.n’).
		//assertEquals(Seq("CaCall"),	ConsonantAssimilation2.transform("CaCalr", "r"))

		assertEquals(Seq(),	ConsonantAssimilation2.reverse("annarr", "r"))
		assertEquals(Seq(),	ConsonantAssimilation2.reverse("dýrr", "r"))
	}

	@deprecated
	@Test
	def testConsonantAssimilationBlockR(): Unit = {

		assertEquals("dalr",  ConsonantAssimilation("dalr"))
		assertEquals("vinr", ConsonantAssimilation("vinr"))
	}

	@Test
	def testConsonantAssimilationNewBlockR(): Unit = {

		assertEquals(Seq(),  ConsonantAssimilation2.transform("dalr", "r"))
		assertEquals(Seq(), ConsonantAssimilation2.transform("vinr", "r"))
	}

	@Test
	def testConsonantAssimilationNewReverseBlockR(): Unit = {

		assertEquals(Seq(), ConsonantAssimilation2.reverse("dalr", "r"))
		assertEquals(Seq(),	ConsonantAssimilation2.reverse("vinr", "r"))
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

		assertEquals(Seq("nagl"), ConsonantAssimilation2.transform("naglr", "r"))
		assertEquals(Seq("menn"), ConsonantAssimilation2.transform("mennr", "r"))
		assertEquals(Seq("skipti"), ConsonantAssimilation2.transform("skiptti", "ti"))
		assertEquals(Seq("gamall"), ConsonantAssimilation2.transform("gamalr", "r"))
		assertEquals(Seq(), ConsonantAssimilation2.transform("hamarr", "r"))
	}

	@Test
	def testConsonantAssimilationNewReverseImpl(): Unit = {

		assertEquals(Seq("naglr"),   ConsonantAssimilation2.reverse("nagl",	"r"))
		assertEquals(Seq("mennr"),   ConsonantAssimilation2.reverse("menn",	"r"))

		// it is probably predictible: do not mix voiced and non-voiced consonants in one context
		assertEquals(Seq("skipðti", "skiptti"), ConsonantAssimilation2.reverse("skipti","ti"))
		assertEquals(Seq("gamalr"), 	ConsonantAssimilation2.reverse("gamall", "r"))
	}

	@Test
	def testConsonantAssimilationMedioPassiveRev(): Unit = {

		// suffix is given as -izk
		assertEquals(Seq(), ConsonantAssimilation2.reverse("kallizk", "izk"))
	}

	@Test
	def testConsonantAssimilation3(): Unit = {

		assertEquals(Seq("gamalli"), ConsonantAssimilation2.transform("gamalri", "ri"))
	}

	@Test
	def testConsonantAssimilation3Rev(): Unit = {

		assertEquals(Seq("gamalri"), ConsonantAssimilation2
			.reverse("gamalli", "ri")
			.filter(_ != "gamallli"))
	}

	@Test
	def testConsonantAssimilationVoicedDentalRegressive(): Unit = {

		assertEquals(Seq("foett"), ConsonantAssimilation2.transform("foeddt", "t"))
		assertEquals(Seq("kallat"), ConsonantAssimilation2.transform("kallaðt", "t"))
	}

	@Test
	def testConsonantAssimilationVoicedDentalRegressiveRev(): Unit = {

		assertEquals(Seq("foeddt"), ConsonantAssimilation2.reverse("foett", "t"))

		assertEquals(Seq("kallaðt"), ConsonantAssimilation2.reverse("kallat", "t"))
	}

	@Test
	def testConsonantAssimilationNasalDental(): Unit = {

		assertEquals(Seq("hitt"), ConsonantAssimilation2.transform("hint", "t"))

		assertEquals(Seq(),       ConsonantAssimilation2.transform("annart", "t"))
	}

	@Test
	def testConsonantAssimilationNasalDentalRev(): Unit = {

		// FIXME: make difference between voiced and voiceless consonants,
		// 	avoid to mix them in the same context (or consonant cluster)
		assertEquals(Seq("hint"), ConsonantAssimilation2.reverse("hitt", "t"))
	}

	@Test
	def testConsonantAssimilationNasalDentalNegRev(): Unit = {

		assertEquals(Seq(), ConsonantAssimilation2.reverse("atnart", "t"))
	}

	@Test
	def testConsonantAssimilationDD(): Unit = {

		assertEquals(Seq("gladdi"), ConsonantAssimilation2.transform("glaðði", "ði"))
	}

	@Test
	def testConsonantAssimilationDDRev(): Unit = {

		assertEquals(Seq("glaðði"), ConsonantAssimilation2.reverse("gladdi", "ði"))
	}

	@Test
	def testConsonantAssimilationNN2Eth(): Unit = {

		assertEquals(Seq("ǫðru"), ConsonantAssimilation2.transform("ǫnnru", "u"))
	}

	@Test
	def testConsonantAssimilationNN2EthRev(): Unit = {

		assertEquals(Seq("ǫnnru"), ConsonantAssimilation2.reverse("ǫðru", "u"))
	}

	@Test
	def testConsonantAssimilationGemination(): Unit = {

		assertEquals(Seq("nýtt"), ConsonantAssimilation2.transform("nýt", "t"))
		assertEquals(Seq("fárri"), ConsonantAssimilation2.transform("fári", "ri"))
	}

	@Test
	def testConsonantAssimilationGeminationRev(): Unit = {

		assertEquals(Seq("nýt"), ConsonantAssimilation2.reverse("nýtt", "t"))
		assertEquals(Seq("fári"), ConsonantAssimilation2.reverse("fárri", "ri"))
	}

	@Test
	def testConsonantAssimilationGeminationRev2(): Unit = {

		assertEquals(Seq("dýr", "dýrr"), ConsonantAssimilation2.reverse("dýrr", "r"))
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
