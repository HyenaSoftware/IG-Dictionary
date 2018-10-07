package com.hyenawarrior.OldNorseGrammar.grammar.phonology

import org.hamcrest.CoreMatchers.equalTo
import org.junit.rules.ErrorCollector
import org.junit.{Rule, Test}

/**
  * Created by HyenaWarrior on 2018.09.30..
  */
class TestWord {

  val collector = new ErrorCollector

  @Rule
  def collectorDef = collector

  private def equalToIn = (mhs: Seq[Morpheme], i: Int) => equalTo(mhs.lift(i).map(_.asString()).getOrElse("<MISSING>"))

  @Test
  def testSplit(): Unit = {

    val WORD = Word("abde")

    val m1 = WORD.splitAt(0).morphemes
    collector.checkThat("", equalToIn(m1, 0))
    collector.checkThat("abde", equalToIn(m1, 1))

    val m2 = WORD.splitAt(1).morphemes
    collector.checkThat("a", equalToIn(m2, 0))
    collector.checkThat("bde", equalToIn(m2, 1))

    val m3 = WORD.splitAt(2).morphemes
    collector.checkThat("ab", equalToIn(m3, 0))
    collector.checkThat("de", equalToIn(m3, 1))

    val m4 = WORD.splitAt(3).morphemes
    collector.checkThat("abd", equalToIn(m4, 0))
    collector.checkThat("e", equalToIn(m4, 1))

    val m5 = WORD.splitAt(4).morphemes
    collector.checkThat("abde", equalToIn(m5, 0))
    collector.checkThat("", equalToIn(m5, 1))
  }

  @Test
  def testSplitRev(): Unit = {

    val WORD = Word("abde")

    val mhs1 = WORD.splitAt(-5).morphemes
    collector.checkThat("", equalToIn(mhs1, 0))
    collector.checkThat("abde", equalToIn(mhs1, 1))

    val mhs2 = WORD.splitAt(-4).morphemes
    collector.checkThat("a", equalToIn(mhs2, 0))
    collector.checkThat("bde", equalToIn(mhs2, 1))

    val mhs3 = WORD.splitAt(-3).morphemes
    collector.checkThat("ab", equalToIn(mhs3, 0))
    collector.checkThat("de", equalToIn(mhs3, 1))

    val mhs4 = WORD.splitAt(-2).morphemes
    collector.checkThat("abd", equalToIn(mhs4, 0))
    collector.checkThat("e", equalToIn(mhs4, 1))

    val mhs5 = WORD.splitAt(-1).morphemes
    collector.checkThat("abde", equalToIn(mhs5, 0))
    collector.checkThat("", equalToIn(mhs5, 1))
  }
}
