package com.hyenawarrior

import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.U_Umlaut
import com.hyenawarrior.OldNorseGrammar.grammar.{Case, DescriptorFlag, Number, PoS, Root, Word}
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.Noun
import org.junit.Assert.assertEquals
import org.junit.Test

/**
  * Created by HyenaWarrior on 2017.04.15..
  */
class ExampleUnitTest {

  case class MockPoS(str: String) extends PoS {

    override def strForm: String = str

    override def descriptorFlags: List[DescriptorFlag] = List()
  }

  @Test
  def testInactiveUmlaut {

    val mp = MockPoS("kallada")

    val w = Word(mp, List(U_Umlaut))

    assertEquals("kallada", w.strForm())
  }

  @Test
  def testActiveUmlaut {

    val mp = MockPoS("kalladu")

    val w = Word(mp, List(U_Umlaut))

    assertEquals("kölludu", w.strForm())
  }
}