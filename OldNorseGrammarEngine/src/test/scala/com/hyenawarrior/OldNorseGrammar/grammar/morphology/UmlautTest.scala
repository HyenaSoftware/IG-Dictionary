package com.hyenawarrior.OldNorseGrammar.grammar.morphology

import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.{Explicit_I_Umlaut, U_Umlaut}
import org.junit.Assert._
import org.junit.Test

/**
	* Created by HyenaWarrior on 2017.10.18..
	*/
class UmlautTest {

	@Test
	def testBasicUUmlaut() {

		val umlautedStr = U_Umlaut("tattattu")

		assertEquals("tǫttuttu", umlautedStr)
	}

	@Test
	def testBasicUUmlautWithVTrigger() {

		assertEquals("tǫttuttv", U_Umlaut("tattattv"))
    assertEquals("tøttittv", U_Umlaut("tettittv"))
    assertEquals("tyttyttv", U_Umlaut("tittyttv"))
	}

	@Test
	def testUUmlautWithoutChange() {

		assertEquals("tettittu", U_Umlaut("tettittu"))
		assertEquals("tottettu", U_Umlaut("tottettu"))
		assertEquals("tittyttu", U_Umlaut("tittyttu"))
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
