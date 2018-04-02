package com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.ProductiveTransforms

import com.hyenawarrior.OldNorseGrammar.grammar.Syllable.Length.SHORT
import com.hyenawarrior.OldNorseGrammar.grammar.Syllables
import com.hyenawarrior.OldNorseGrammar.grammar.phonology.Consonant.isConsonant

import scala.language.postfixOps

/**
  * Created by HyenaWarrior on 2017.10.31..
  */
object ConsonantAssimilation {

  def apply(str: String): String = loop(str, reduce lift)

  def unapply(str: String): Option[String] = Some(loop(str, restore lift))

  def invert(str: String): Seq[String] = {

    val a = inverseTransformation.flatMap(_.lift(str))

    a
  }

  /**
    * main loop, which apply the reduce function while it's necessary
    * @param value input string as list of characters
    * @return transformed string as list of characters
    */
  private def loop(str: String, f: String => Option[String]): String = f(str) match {

    case Some(newStr) => loop(newStr, f)
    case None => str
  }

  private val rx_tsC_or_zzC = "^(.+)(?:ts|zz)(.)(.*)$".r
  private val rx_rsk = "^(.+)rsk(.*)$".r
  private val rx_dsk = "^(.+)ðsk(.*)$".r
  private val rx_nn2eth = "^(.*a)nn(r.*)$".r
  private val rx_r2ln = "^(.*([ln]))r$".r
  private val rx_sr2ss = "^(.*)sr$".r
  private val rx_2cs = "^(.*)([bdðfghjklmnprstvxzþ]{2})([bdðfghjklmnprstvxzþ])(.*)$".r

  private val reduce: PartialFunction[String, String] = {

    case rx_tsC_or_zzC(p, c, s) if isConsonant(c.head) => s"${p}z$c$s"
    case rx_rsk(s, p) => s"${s}sk$p"
    case rx_dsk(s, p) => s"${s}zk$p"
    case rx_nn2eth(a, b) => s"${a}ð$b"
    //case s if s.contains("annr") => s.replace("annr", "aðr")
    case rx_r2ln(str, c) if !isShortMonosyllabic(str) => s"$str$c"
    case rx_sr2ss(a) => s"${a}ss"
    case rx_2cs(p, c2, c, s) if c2.last == c.head => s"$p$c2$s"
  }

  private def isShortMonosyllabic(str: String): Boolean = {

    // in such stems, like vǫll-, hall-, the first syllable should be counted as short
    val Syllables(syls) = str.replace("ll", "l").concat("a")

    // the second syllable is extrametrical, don't count it
    val isShortMonosyllabic = syls.size == 2 && syls.head.length == SHORT

    isShortMonosyllabic
  }

  private val rx_z = "^(.+)z(.+)$".r
  private val rx_eth2nn = "^(.*a)ðr$".r
  private val rx_ln2r = "^(.+)([ln])$".r
  //private val rx_3cs = "^(.*)([bdðfghjklmnprstvxzþ]{2})$".r

  private val restore: PartialFunction[String, String] = {

    case rx_z(s, p) => s"${s}ts$p"
    case rx_eth2nn(s) => s"${s}nnr"

    //case rx_3cs(str, c2) if c2.head == c2.last => s"$str$c2${c2.last}"
    case rx_ln2r(str, c) if str.last == c.head => s"${str}r"
  }

  private val inverseTransformation = Seq[PartialFunction[String, String]](

    { case rx_z(s, p) => s"${s}ts$p" },
    { case rx_eth2nn(s) => s"${s}nnr" },
    { case s => s }, // identity
    { case rx_ln2r(str, c) if str.last == c.head => s"${str}r" }
  )
}
