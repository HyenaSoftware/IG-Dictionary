package com.hyenawarrior.OldNorseGrammar.grammar.morphology

import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.ProductiveTransforms.SieversLaw
import org.junit.Assert.assertEquals
import org.junit.Test

/**
  * Created by HyenaWarrior on 2018.03.18..
  */
class SieversLawTest {

  @Test
  def testRestore(): Unit = {

    val str = SieversLaw restore "ríkj"

    assertEquals("ríkij", str.get)
  }

  @Test
  def testRestore2(): Unit = assertEquals(None, SieversLaw restore "ríkij")

  @Test
  def testRestore3(): Unit = assertEquals(None, SieversLaw restore "ríkija")

  @Test
  def testRestore4(): Unit = {

    val str = SieversLaw restore "ríkja"

    assertEquals("ríkija", str.get)
  }
}
