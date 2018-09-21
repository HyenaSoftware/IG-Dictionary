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

  //
  @Test def testSyncopeForAptnar2() = assertEquals("aptnar", Syncope.transform("aptinar", "ar"))
  @Test def testSyncopeForGamlir2() = assertEquals("gamlir", Syncope.transform("gamalir", "ir"))
  @Test def testSyncopeForDrepnum2() = assertEquals("drepnum", Syncope.transform("drepinum", "um"))
  @Test def testSyncopeForHirdar2() = assertEquals("hirðar", Syncope.transform("hirðiar", "ar"))
  @Test def testSyncopeForHiminar2() = assertEquals("himnar", Syncope.transform("himinar", "ar"))

  @Test def testSyncopeForLukli2() = assertEquals("lukli", Syncope.transform("lukili", "i"))

  @Test def testSyncopeForUlfunum2() = assertEquals("úlfumnum", Syncope.transform("úlfuminum", "inum"))

  @Test def testSyncopeForDyra() = assertEquals("dýra", Syncope.transform("dýra", "a"))

  @Test def testSyncopeForNyrri() = assertEquals("nýrri", Syncope.transform("nýrari", "ari"))

  // negative tests
  @Test def testSyncopeForLykils() = assertEquals("lykils", Syncope("lykils"))
  @Test def testSyncopeForKetill() = assertEquals("ketill", Syncope("ketill"))

  @Test def testSyncopeForLykils2() = assertEquals("lykils", Syncope.transform("lykils", "s"))
  @Test def testSyncopeForKetill2() = assertEquals("ketill", Syncope.transform("ketill", "r"))
}
