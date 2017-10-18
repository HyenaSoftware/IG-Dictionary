package com.hyenawarrior.OldNorseGrammar.grammar.morphology

import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.{Explicit_I_Umlaut, U_Umlaut}
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
	def testBasicUUmlautWithVTrigger() {

		val umlautedStr = U_Umlaut("tattattv")

		assertEquals("töttuttv", umlautedStr)
	}

	@Test
	def testUUmlautWithoutChange() {

		assertEquals("tøttittu", U_Umlaut("tettittu"))
		assertEquals("tottettu", U_Umlaut("tottettu"))
		assertEquals("tyttyttu", U_Umlaut("tittyttu"))
	}

	@Test
	def testBasicIUmlaut() {

		assertEquals("tet", Explicit_I_Umlaut("tat"))
		assertEquals("tæt", Explicit_I_Umlaut("tát"))

		assertEquals("tøt", Explicit_I_Umlaut("tot"))
		assertEquals("tœt", Explicit_I_Umlaut("tót"))

		assertEquals("tyt", Explicit_I_Umlaut("tut"))
		assertEquals("týt", Explicit_I_Umlaut("tút"))
	}
}
