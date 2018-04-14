package com.hyenawarrior.OldNorseGrammar.grammar.morphology

import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.{Explicit_I_Umlaut, U_Umlaut, V_Umlaut}
import org.junit.Assert._
import org.junit.Test

/**
	* Created by HyenaWarrior on 2017.10.18..
	*/
class UmlautTest {

	@Test
	def testBasicUUmlaut() {

		val umlautedStr = U_Umlaut("tattattu")

		assertEquals(Some("tǫttuttu"), umlautedStr)
	}

	@Test
	def testBasicUUmlautWithVTrigger() {

    assertEquals(Some("tǫttuttv"), V_Umlaut("tattattv"))
    assertEquals(Some("tøttittv"), V_Umlaut("tettittv"))
    assertEquals(Some("tyttyttv"), V_Umlaut("tittyttv"))
	}

	@Test
	def testUUmlautWithoutChange() {

		assertEquals(None, U_Umlaut("tettittu"))
		assertEquals(None, U_Umlaut("tottettu"))
		assertEquals(None, U_Umlaut("tittyttu"))
	}

	@Test
	def testBasicIUmlaut() {

		assertEquals(Some("tet"), Explicit_I_Umlaut("tat"))
		assertEquals(Some("tæt"), Explicit_I_Umlaut("tát"))

		assertEquals(Some("tøt"), Explicit_I_Umlaut("tot"))
		assertEquals(Some("tœt"), Explicit_I_Umlaut("tót"))

		assertEquals(Some("tyt"), Explicit_I_Umlaut("tut"))
		assertEquals(Some("týt"), Explicit_I_Umlaut("tút"))
	}
}
