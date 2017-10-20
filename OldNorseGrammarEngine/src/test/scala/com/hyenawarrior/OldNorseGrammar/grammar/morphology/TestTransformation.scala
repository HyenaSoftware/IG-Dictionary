package com.hyenawarrior.OldNorseGrammar.grammar.morphology

import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.ProductiveTransforms.SemivowelDeletion
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
		assertEquals("höggum", SemivowelDeletion("höggvum"))
		assertEquals("söng", SemivowelDeletion("söngv"))
		assertEquals("fölr", SemivowelDeletion("fölvr"))
	}

	@Test
	def testEToJaAtClass3Verbs(): Unit = {

		assertEquals("gjald", StemTransform.EToJa("geld"))
		assertEquals("sjarf", StemTransform.EToJa("serf"))
		assertEquals("gemd", StemTransform.EToJa("gemd"))
	}

	@Test
	def testEToJaAtClass3VerbsReverse(): Unit = {

		assertEquals("geld", StemTransform.EToJa.unapply("gjald").get)
		assertEquals("gerd", StemTransform.EToJa.unapply("gjard").get)
		assertEquals("gjamd", StemTransform.EToJa.unapply("gjamd").get)
	}

	@Test
	def testEToJaAndIUmlautInversion(): Unit = {

		val stemStr = StemTransform.EToJa("geld")
		val stemStr2 = Explicit_I_Umlaut(stemStr)
		val stemStr3 = SemivowelDeletion(stemStr2)

		assertEquals("geld", stemStr3)
	}
}
