package com.hyenawarrior.OldNorseGrammar.grammar.morphology

import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.U_Umlaut
import org.junit.Test
import org.junit.Assert._

/**
	* Created by HyenaWarrior on 2017.10.18..
	*/
class UmlautTest {

	@Test
	def testBasicUUmlaut() {

		val umlautedStr = U_Umlaut("tattattu")

		assertEquals("töttuttu", umlautedStr)
	}

	@Test
	def testUUmlautWithoutChange() {

		assertEquals("tøttyttu", U_Umlaut("tettittu"))
		assertEquals("tottøttu", U_Umlaut("tottettu"))
		assertEquals("tyttyttu", U_Umlaut("tittyttu"))
	}
}
