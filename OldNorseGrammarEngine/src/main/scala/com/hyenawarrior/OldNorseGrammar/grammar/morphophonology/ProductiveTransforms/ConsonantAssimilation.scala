package com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.ProductiveTransforms

import com.hyenawarrior.OldNorseGrammar.grammar.phonology.Consonant.isConsonant

/**
  * Created by HyenaWarrior on 2017.10.31..
  */
object ConsonantAssimilation {

  def apply(str: String): String = loop(str, reduce lift)

  def unapply(str: String): Option[String] = Some(loop(str, restore lift))

  /**
    * main loop, which apply the reduce function while it's necessary
    * @param str input string as list of characters
    * @return transformed string as list of characters
    */
  private def loop(str: String, f: String => Option[String]): String = f(str) match {

    case Some(newStr) => loop(newStr, f)
    case None => str
  }

  private val rx_tsC_or_zzC = "^(.+)(?:ts|zz)(.)(.*)$".r
  private val rx_rsk = "^(.+)rsk(.*)$".r
  private val rx_dsk = "^(.+)ðsk(.*)$".r

  private val reduce: PartialFunction[String, String] = {

    case rx_tsC_or_zzC(p, c, s) if isConsonant(c.head) => s"${p}z$c$s"
    case rx_rsk(s, p) => s"${s}sk$p"
    case rx_dsk(s, p) => s"${s}zk$p"
  }

  private val rx_z = "^(.+)z(.+)$".r

  private val restore: PartialFunction[String, String] = {

    case rx_z(s, p) => s"${s}ts$p"
  }
}
