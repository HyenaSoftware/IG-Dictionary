package com.hyenawarrior

import com.hyenawarrior.OldNorseGrammar.grammar._
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.U_Umlaut
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stemclasses.StrongVerbStemClasses
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.{StrongVerb, VerbClassEnum, VerbModeEnum, VerbTenseEnum}
import org.junit.Assert.{assertEquals, assertSame}
import org.junit.{Assert, Test}

/**
  * Created by HyenaWarrior on 2017.04.15..
  */
class ExampleUnitTest {

  case class MockPoS(str: String) extends PoS {

    override def strForm: String = str

    override def descriptorFlags: List[DescriptorFlag] = List()
  }

  @Test
  def testInactiveUmlaut() {

    val mp = MockPoS("kallada")

    val w = Word(mp, List(U_Umlaut))

    assertEquals("kallada", w.strForm())
  }

  @Test
  def testActiveUmlaut() {

    val mp = MockPoS("kalladu")

    val w = Word(mp, List(U_Umlaut))

    assertEquals("kölludu", w.strForm())
  }



	@Test
  def testAblaut() {

    val givenStrongVerb = StrongVerb("brunnum", VerbClassEnum.STRONG_3RD_CLASS, Pronoun.PL_1, VerbTenseEnum.PAST)

		val finitiveParams = (Pronoun.SG_2, VerbModeEnum.INDICATIVE, VerbTenseEnum.PAST)

    val strongVerbResult = StrongVerbStemClasses.convertTo(givenStrongVerb, Left(finitiveParams))

		strongVerbResult match {

			case Some(StrongVerb(str, clazz, pronoun, tense)) =>
				assertEquals("brannt", str)
				assertSame(VerbClassEnum.STRONG_3RD_CLASS, clazz)
				assertSame(Pronoun.SG_2, pronoun)
				assertSame(VerbTenseEnum.PAST, tense)

			case _ => Assert.fail()
		}
  }
}