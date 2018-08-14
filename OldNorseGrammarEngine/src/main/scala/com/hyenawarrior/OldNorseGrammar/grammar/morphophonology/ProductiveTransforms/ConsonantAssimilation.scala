package com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.ProductiveTransforms

import com.hyenawarrior.OldNorseGrammar.grammar.Syllable.Length.SHORT
import com.hyenawarrior.OldNorseGrammar.grammar.Syllables
import com.hyenawarrior.OldNorseGrammar.grammar.phonology.Consonant.isConsonant

import scala.language.postfixOps
import scala.util.matching.Regex

/**
  * Created by HyenaWarrior on 2017.10.31..
  */
object ConsonantAssimilation {

  def apply(str: String): String = loop(str, reduce lift)

  def unapply(str: String): Option[String] = Some(loop(str, restore lift))

  @deprecated
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

  @deprecated
  private val restore: PartialFunction[String, String] = {

    case rx_z(s, p) => s"${s}ts$p"
    case rx_eth2nn(s) => s"${s}nnr"

    //case rx_3cs(str, c2) if c2.head == c2.last => s"$str$c2${c2.last}"
    case rx_ln2r(str, c) if str.last == c.head => s"${str}r"
  }

  @deprecated
  private val inverseTransformation = Seq[PartialFunction[String, String]](

    { case rx_z(s, p) => s"${s}ts$p" },
    { case rx_eth2nn(s) => s"${s}nnr" },
    { case s => s }, // identity
    { case rx_ln2r(str, c) if str.last == c.head => s"${str}r" }
  )

  //
  case class Transformation(regex: Regex, suffixes: Seq[String])

  private val transform_dr_2_nn = Transformation("^(.*a)ðr$".r,     Seq("nn", "r"))
  private val transform_ll_2_lr  = Transformation("^(.+(?:l(?=l)|n(?=n))).$".r,  Seq("r"))
  private val transform_ll_2_llr  = Transformation("^(.+(?:ll|nn))$".r,  Seq("r"))
  private val transform_zk_2_tsk = Transformation("^(.+)zk$".r, Seq("t", "sk"))
  private val transform_zk_2_dsk = Transformation("^(.+)zk$".r, Seq("ð", "sk"))

  private val edges = Seq(transform_dr_2_nn, transform_ll_2_lr, transform_ll_2_llr, transform_zk_2_tsk, transform_zk_2_dsk)

  case class Path(nodes: Seq[(Transformation, String)])
  object BeginOfPath extends Path(Seq())

  def transform(str: String): Seq[String] = transform(str, (a, b) => true)

  def transform(str: String, validator: (String, String) => Boolean): Seq[String] = {

    def transformAndValidate(s: String, tr: Transformation): Option[String] = transformStr(s, tr) match {

      case Some(s2) if validator(s, s2) => Some(s2)
      case _ => None
    }

    val allEdges = edges.toSet

    def loop(str: String, p: Path): Seq[Path] = {

      val possibleTransformations = allEdges -- p.nodes.map(_._1)

      val outputEdges = possibleTransformations.toSeq
        .flatMap(tr => transformAndValidate(str, tr).map(s => tr -> s))

      outputEdges match {

        case Seq() => Seq(p)
        case _ => outputEdges.flatMap { case e @ (tr, s) => loop(s, Path(p.nodes :+ e)) }
      }
    }

    val leaves = loop(str, BeginOfPath)
    val newStrs = leaves.flatMap(_.nodes.lastOption)

    newStrs.map(_._2)
  }

  def transformStr(str: String, tr: Transformation): Option[String] = {

    val Transformation(rgx, newStrs) = tr

    str match {

      case rgx(s) => Some(newStrs.mkString(s, "", ""))
      case _ => None
    }
  }
}
