package com.hyenawarrior.OldNorseGrammar.grammar.morphology

import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.ProductiveTransforms.{SemivowelDeletion, VowelDeletion}
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.StemTransform.Raising
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.{Explicit_I_Umlaut, StemTransform}
import org.junit.Assert.assertEquals
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

		assertEquals("gjald", StemTransform.EToJa("geld").getOrElse("geld"))
		assertEquals("sjarf", StemTransform.EToJa("serf").getOrElse("serf"))
		assertEquals("gemd", StemTransform.EToJa("gemd").getOrElse("gemd"))
	}

	@Test
	def testEToJaAtClass3VerbsReverse(): Unit = {

		assertEquals("geld", StemTransform.EToJa.unapply("gjald").getOrElse("gjald"))
		assertEquals("gerd", StemTransform.EToJa.unapply("gjard").getOrElse("gjard"))
		assertEquals("gjamd", StemTransform.EToJa.unapply("gjamd").getOrElse("gjamd"))
	}

	@Test
	def testEToJaAndIUmlautInversion(): Unit = {

		val stemStr = StemTransform.EToJa("geld").get
		val stemStr2 = Explicit_I_Umlaut(stemStr)
		val stemStr3 = SemivowelDeletion(stemStr2)

		assertEquals("geld", stemStr3)
	}

	/** Fracture does not occur at all if *e is preceded by v, l, or r, e.g. verða, leðr. */
	@Test
	def testDoNotBreak(): Unit = {

		assertEquals("verða", StemTransform.EToJa("verða").getOrElse("verða"))
	}

  @Test
  def testVowelDeletion(): Unit = {

    assertEquals("fá", VowelDeletion("fáa"))
  }

  @Test
  def testRaisingFor3rdClassVerbs(): Unit = {

    assertEquals("brinna", Raising("brenna").get)
  }

  @Test
  def testRaisingFor5thClassVerbs(): Unit = {

    assertEquals("biðja", Raising("beðjan").getOrElse("beðjan"))
  }
}
