package com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.ProductiveTransforms

import com.hyenawarrior.OldNorseGrammar.grammar.phonology.MorphemeProperty.{Stem, Suffix}
import com.hyenawarrior.OldNorseGrammar.grammar.phonology.PhonemeProperty.Syncopated
import com.hyenawarrior.OldNorseGrammar.grammar.phonology._

import scala.util.matching.Regex

/**
  * Created by HyenaWarrior on 2018.09.05..
  */
object ConsonantAssimilation2 {

  // TODO: source these from the Vowel and Consonant objects
  private val C = "[bdðfghjklmnprstvxzþ]"
  private val NC = "[^bdðfghjklmnprstvxzþ]"
  private val V = "(?:[aeioøǫuyáæéíóœúý]|oe|ae|au|ey|ei)"
  private val SV = "[aeioøǫuy]"
  private val LV = "(?:[áæéíóœúý]|oe|ae|au|ey|ei)"
  private val UV = "[aiu]"

  // -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  // infectional ending, that ends by consonant, like -rar, can't cause syncope, so (in theory) there is no need to
  // reverse this transformation there
  // mað-r    mann-r    mann-
  // aðr-ar   annr-ar   annar-
  //
  //  akr  > akr-r
  //  nagl > nagl-l (> nagl-r)
  //  menn > menn-n (> menn-r)
  //  skipti > skipt-ti
  //
  //  TODO, avoid it: *gamall > gamalll
  //
  //  (nagl >) nagl-l > nagl-r
  //  gamal-lar > gamal-rar
  //  kallazk -> kallatsk

  // -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  private case class WordRegexTransform(regex: Regex
                                , replacement: String
                                , description: String
                                , predicate: Seq[SimpleMorpheme] => Boolean = _ => true) {

    override def toString: String = description
  }

  private val FWD_ITEMS2 = Seq(

    WordRegexTransform(s"((?<=$SV$C$UV)[ln]|(?<=$LV)[ln]|s)-r".r, "$1-$1", "Cr > CC"),

    WordRegexTransform( "(?<=[aǫ])nn-r".r,  "ð-r", "(a)nnr > (a)ðr"),
    WordRegexTransform( "(?<=[aǫ])nnr-".r,  "ðr-", "(a)nnr > (a)ðr"),
    WordRegexTransform("ð-ð".r,             "d-d", "ðð > dd"),
    WordRegexTransform("dd-t".r,            "t-t", "ddt > tt"),

    WordRegexTransform("n-t".r,         "t-t", "nt > tt"),
    WordRegexTransform(s"(?<=$V)ð-t".r, "-t",  "ðt > t"),

    WordRegexTransform("z1-z2".r, "ð-s", "z > ðs"),
    WordRegexTransform("z1-z2".r, "t-s", "z > ts"),

    WordRegexTransform("-z2k".r, "-z2z1z2k", "z(k) > zz(k)"),

    WordRegexTransform("r-sk".r, "-sk", "rsk > sk"),

    WordRegexTransform(s"(?<=$SV)($C)(?!\\1)($C)-\\2".r, "$1-$2", "CC -> C"),

    WordRegexTransform(s"(?<=$LV)-([tr])".r, "$1-$1",  "Vr, Vt > Vrr, Vtt"),

    WordRegexTransform(s"(?<=$C)([lns])-r".r, "-$1", "Cr > C")
  )

  private val REV_ITEMS2 = Seq(

    WordRegexTransform(s"(?<=$UV[ln]|$LV[ln]|s)(-[lns])".r, "-r", "CC > Cr"),

    WordRegexTransform( "(?<=[aǫ])ð-r$".r,        "nn-r", "aðr > annr"),
    WordRegexTransform(s"(?<=[aǫ])ðr-(?=$UV)".r,  "nnr-", "aðr > annr"),
    WordRegexTransform("d-d".r,                   "ð-ð",  "dd  > ðð"),
    WordRegexTransform("t-t".r,                   "dd-t", "tt > ddt"),

    WordRegexTransform("t-t".r, "n-t", "tt > nt"),
    WordRegexTransform(s"(?<=$V)-t".r, "ð-t", "t > ðt"),

    WordRegexTransform("-z1".r,  "ð-r", "t(sk) > ðr(sk)"),
    WordRegexTransform("-z1".r,  "t-r", "t(sk) > tr(sk)"),

    WordRegexTransform("-z1".r, "z1-z2z1", "t > tst"),
    WordRegexTransform("z1-z2".r, "z1-z2",  "t-s > t-s"),
    WordRegexTransform("z1-z2".r, "ð-z2",   "t-s > ð-s"),

    WordRegexTransform("-sk".r, "r-sk", "sk > rsk"),

    WordRegexTransform(s"(?<=$SV)($C)-(?!\\1)($C)".r, "$1$2-$2", "C -> CC"),

    WordRegexTransform(s"(?<=$LV)([tr])-\\1".r, "-$1",  "Vrr, Vtt > Vr, Vt"),

    WordRegexTransform(s"(?<=$V$C)-([lns])$$".r, "$1-r", "C > Cr")
  )

  def transform2(word: Word): Map[String, Word] = {

    val (wLast :: wFirst) = word.morphemes.reverse

    FWD_ITEMS2.flatMap(t => transformImpl2(t, wFirst.reverse, wLast))
      .map { case (description, wNewInit) => description -> new Word(wNewInit) }
      .toMap
  }

  def reverse2(word: Word): Map[String, Word] = {

    val (wLast :: wFirst) = word.morphemes.reverse

    REV_ITEMS2.flatMap(t => transformImpl2(t, wFirst.reverse, wLast))
      .map { case (description, wNewInit) => description -> new Word(wNewInit) }
      .toMap
  }

  private def transformImpl2(transformation: WordRegexTransform, word: Seq[SimpleMorpheme], suffix: SimpleMorpheme)
    : Option[(String, Seq[SimpleMorpheme])] = {

    val rgx = transformation.regex
    val rpl = transformation.replacement
    val prd = transformation.predicate

    if(prd(word)) {

      val wordStr = Phoneme.asStringRepr(word.last.phonemes.filter(ph => ph.phonemeProperty != Syncopated))
      val suffixStr = Phoneme.asStringRepr(suffix.phonemes)

      val compositeWord = s"$wordStr-$suffixStr"

      if (rgx.findFirstIn(compositeWord).nonEmpty) {

        val resultStr = rgx.replaceFirstIn(compositeWord, rpl)

        val resultArray = resultStr.split('-').padTo(2, "")

        val newSuffix = Phoneme.fromStringRepr(resultArray(1), Suffix)
        val newWordLast = Phoneme.fromStringRepr(resultArray(0), Stem)

        Some(transformation.description -> (word.init :+ newWordLast :+ newSuffix))

      } else {

        None
      }
    } else {

      None
    }
  }
}
