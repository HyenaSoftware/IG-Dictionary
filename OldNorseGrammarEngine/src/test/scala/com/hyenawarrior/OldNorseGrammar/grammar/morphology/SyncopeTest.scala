package com.hyenawarrior.OldNorseGrammar.grammar.morphology

import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.ProductiveTransforms.Syncope
import org.junit.Assert.assertEquals
import org.junit.Test

/**
  * Created by HyenaWarrior on 2018.03.11..
  */
class SyncopeTest {

  @Test def testSyncopeForAptnar() = assertEquals("aptnar", Syncope("aptinar"))
  @Test def testSyncopeForGamlir() = assertEquals("gamlir", Syncope("gamalir"))
  @Test def testSyncopeForDrepnum() = assertEquals("drepnum", Syncope("drepinum"))
  @Test def testSyncopeForHirdar() = assertEquals("hirðar", Syncope("hirðiar"))
  @Test def testSyncopeForHiminar() = assertEquals("himnar", Syncope("himinar"))

  @Test def testSyncopeForLukli() = assertEquals("lukli", Syncope("lukili"))

  @Test def testSyncopeForUlfunum() = assertEquals("úlfumnum", Syncope("úlfuminum"))

  // negative tests
  @Test def testSyncopeForLykils() = assertEquals("lykils", Syncope("lykils"))
  @Test def testSyncopeForKetill() = assertEquals("ketill", Syncope("ketill"))
}
