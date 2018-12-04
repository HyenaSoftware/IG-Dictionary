package com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.ProductiveTransforms

import java.lang.Math._

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

  // TODO: should mediopassive endings be indicated as -rsk, -stsk/-szk, etc. instead of -sk ?
  // TODO: is it possible?: private val transform_CC_2_DDC = Transformation(...)

  // TODO: does it needed? kallaðr + sk > kallaðzk > kallazk
  @SuppressWarnings(Array("unused"))
  private val transform_rev_tsk_2_trsk  = ReverseTransformation("$".r -> None, "^zk$".r -> Some("rsk"), description = "zk > trsk")

  /* -- re-doubling ---

    forward:

      l|r > l|l   ^r > ^l
      n|r > n|n   ^r > ^n
      s|r > s|s   ^r > ^s
    backward:
      dd|t > t|t  stressed
        [^C]C+dd$,^t
      n|t > t|t   stressed
        [^C]C+n$,^t
      ð|ð > d|d
        ð$,^ð
      ann|r > að|r
        ann$,^r
    merging:
      ð|t > [t]   unstressed
        [^C]C+[^C]C+ð$,^t
      t|s > zk
      Cd|d > Cd
        Cd$,^d
      Ct|t > Ct
        Ct$,^t
      Cr|r > r
        Cr$,^r
    forward+merging
      Cl|r > Cl|l > Cl  ^l > ^r
        Cl$,^r

    akr  > akr-r
    nagl > nagl-l (> nagl-r)
    menn > menn-n (> menn-r)
    skipti > skipt-ti

    TODO, avoid it: *gamall > gamalll

    (nagl >) nagl-l > nagl-r
    gamal-lar > gamal-rar
    kallazk -> kallatsk
  */

  // -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  // TODO: should overlapping characters appear in both patterns of stem and inflectional ending?
  @Deprecated
  private trait Transformation {

    val description: String
    val stemReplacement: (Regex, Option[String])
    val suffixReplacement: (Regex, Option[String])
    val overlappingCharacters: Int
    val assimilatedCharacters: Int

    override def toString: String = description
  }

  @Deprecated
  private object Transformation {

    def unapply(tr: Transformation): Option[((Regex, Option[String]), (Regex, Option[String]), Int, Int)] = {

      Some((tr.stemReplacement, tr.suffixReplacement, tr.overlappingCharacters, tr.assimilatedCharacters))
    }
  }

  @Deprecated
  private case class ForwardTransformation(stemReplacement: (Regex, Option[String]), suffixReplacement: (Regex, Option[String]),
    overlappingCharacters: Int = 0, assimilatedCharacters: Int = 0, description: String) extends Transformation

  @Deprecated
  private case class ReverseTransformation(stemReplacement: (Regex, Option[String]), suffixReplacement: (Regex, Option[String]),
    overlappingCharacters: Int = 0, assimilatedCharacters: Int = 0, description: String) extends Transformation

  // -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  private val FWD_ITEMS = Seq(
    // forward
    ForwardTransformation(s"$C*$SV$C$UV[ln]|$LV[ln]|s$$".r -> None, "^([lns])r".r -> Some("$1"), 1, description = "Cr > CC"),

    // backward
    ForwardTransformation("([ðd])(\\1)$".r             -> Some("t"), "^t".r -> None, description = "ddt > tt"),
    ForwardTransformation("(?<=[aǫ])(nn)$".r           -> Some("ð"), "^r".r -> None, description = "ann|r > aðr"),
    ForwardTransformation("(?<=[aǫ])(nn)(?=r$)".r      -> Some("ð"), s"^$UV".r -> None, description = "annr| > aðr"),
    ForwardTransformation("r$".r -> Some(""),  "^sk$".r -> None,      description = "rsk > sk"),
    ForwardTransformation("ð$".r -> Some("d"), "^ð".r   -> Some("d"), description = "ðð > dd"),

    // merging
    ForwardTransformation("ð$".r -> Some("t"),    "^t".r          -> Some(""),  description = "ðt > t"),
    ForwardTransformation("n$".r -> Some("t"),    "^t".r          -> None,      description = "nt > tt"),
    ForwardTransformation("tst$".r -> Some("z"),  "^s(?=[kt]$)".r -> Some(""),  description = "t > ðt"),
    ForwardTransformation("(?<!ts)[tð]$".r      -> Some("z"), "^s(?=[kt]$)".r         -> Some(""),    description = "t > ðt"),
    ForwardTransformation(s"($C)(?!\\1)$C$$".r  -> None,      s"^($C)\\1(?=$NC|$$)".r -> Some(""), 1, description = "CC > C"),

    ForwardTransformation(s"^$C*$LV$$".r -> None,  "^([tr])".r -> Some("$1$1"),  description = "Vr|t > rr|tt"),

    // forward+merging
    ForwardTransformation(s"$C[lns]$$".r -> None, s"^r".r -> Some(""), description = "Cr > C"))

  private val REV_ITEMS = Seq(
    // forward
    ReverseTransformation(s"$C*$SV$C($UV[ln])|($LV[ln])|s$$".r -> None, "^[lns]".r -> Some("r"), description = "CC > Cr"),

    // backward
    ReverseTransformation("(?<=[aǫ])(ð)$".r           -> Some("nn"),  "^r".r    -> None, description = "aðr > annr"),
    ReverseTransformation("(?<=[aǫ])(ð)(?=r$)".r      -> Some("nn"),  s"^$UV".r -> None, description = "aðr > annr"),
    ReverseTransformation("$".r                       -> None,        "^sk$".r  -> Some("rsk"), 0, 1, description = "sk > rsk"),
    ReverseTransformation("d$".r                      -> Some("ð"),   "^d".r    -> Some("ð"),   description = "dd > ðð"),
    ReverseTransformation("t$".r                      -> Some("dd"),  "^t".r    -> None,        description = "tt > ddt"),

    // merging
    ReverseTransformation("t$".r                  -> Some("n"), "^t".r -> None,       description = "tt > nt"),
    ReverseTransformation(s"(?<=^$C{0,9}$V$C)t$$".r   -> Some("ð"), "^t".r -> None, 1, 1, description = "t > ðt"),
    ReverseTransformation("z$".r                  -> Some("t"), "^z([kt])$".r   -> Some("s$1"), 1, 1, description = "zk > tsk"),
    ReverseTransformation("z$".r                  -> Some("ð"), "^z([kt])$".r   -> Some("s$1"), 1, 1, description = "zk > ðsk"),
    ReverseTransformation(s"^$C*$V($C)(?!z|\\1)$C$$".r  -> None,      s"^(?!z)($C)".r -> Some("$1"),  1, 1, description = "C -> CC"),

    ReverseTransformation(s"(?<=^$C{0,9}$LV)[tr]$$".r  -> Some(""), "^([tr])(?=\\1)".r  -> Some(""), 1, description = "Vrr|tt > r|t"),
    // edge case for gemination+simplification: [dýr+r] > [dýrr+r:geminated] > [dýr+r:simplified]
    ReverseTransformation(s"(?<=^$C{0,9}$LV([tr]))\\1$$".r  -> Some(""), "^([tr])(?=\\1|$)".r  -> None, 1, 1, description = "Vrr|tt > rr|tt"),

    // forward+merging
    ReverseTransformation(s"^$C*$V$C*[lns]$$".r   -> None,  "^[lns]$".r  -> Some("r"),   1, 1, description = "C > Cr")
  )

  @Deprecated
  def transform(str: String, suffix: String): Seq[String] = syncopeRouting(str, suffix, FWD_ITEMS).map { case (a, b) => a + b }
  @Deprecated
  def reverse(str: String, suffix: String): Seq[String] = syncopeRouting(str, suffix, REV_ITEMS).map { case (a, b) => a + b }

  @Deprecated
  def transform(word: Word): Seq[Word] = {

    val suffixStr = word.selectMorpheme(Suffix).map(_.asString()).getOrElse("")

    syncopeRouting(word.asString, suffixStr, FWD_ITEMS).map {

      case (stem, suffix) => new Word(Seq(Morpheme(stem, Stem), Morpheme(suffix, Suffix)))
    }
  }

  @Deprecated
  def reverse(word: Word, expectedSuffix: String): Seq[Word] = {

    syncopeRouting(word.asString, expectedSuffix, REV_ITEMS).map {

      case (stem, suffix) => new Word(Seq(Morpheme(stem, Stem), Morpheme(suffix, Suffix)))
    }
  }

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

    // (?=.)
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

  @Deprecated
  private def syncopeRouting(str: String, suffix: String, items: Seq[Transformation]): Seq[(String, String)] = {

    /*
     TODO: apply here a sanity check like:
      stem without suffix: '^C*V(C{1,2}(V(C))?)?$'
      1st capture group is optional as word can be monosyllabic without code, like 'á' - 'river'
      2nd capture group is optional as a word might be monosyllabic with coda, like 'stól'
        Q: the coda of a stressed syllable may not have more than 2 consonants
      3rd capture group here can't be 'C+' as unstressed syllables might not have multiple consonants in their coda
     */

    items.flatMap(t => transformImpl(t, str, suffix))
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

  @Deprecated
  private def transformImpl(transformation: Transformation, word: String, knownInfl: String): Option[(String, String)] = {

    val Transformation((stemRgx, stemRepl), (inflRgx, inflRepl), overlappingCharacters, assimilatedCharacters) = transformation

    // word = "nagl", knownInfl = "r", transformation = "C > Cr", assimilatedCharacters = 1, overlappingCharacters = 1
    val inflLength = max(knownInfl.length - assimilatedCharacters, 0)
    // inflLength = 1 - 1 = 0
    val endPoint = word.length - inflLength
    // endPoint = 4 - 0 = 4
    val beginPoint = endPoint - overlappingCharacters
    // beginPoint = 4 - 1 = 3

    if(beginPoint < 0) return None

    val realStem = word.substring(0, endPoint)
    // realStem = "nagl"
    val suffix = word.substring(beginPoint, word.length)  // for regex operations
    // suffix = "l"
    val realInfl = word.substring(endPoint, word.length)  // only for validation
    // realInfl = ""

    if(validateInflection(PRE, transformation, knownInfl, realInfl)) {
    // pass

      val stemChangeIsFine = stemRgx.findFirstIn(realStem).nonEmpty
      val inflChangeIsFine = inflRgx.findFirstIn(suffix).nonEmpty

      if (stemChangeIsFine && inflChangeIsFine) {
      // pass

        val newStem = stemRepl.map(repl => stemRgx.replaceFirstIn(realStem, repl)) getOrElse realStem
        // None:      realStem = "nagl", newStem = "nagl"
        val newInfl = inflRepl.map(repl => inflRgx.replaceFirstIn(suffix, repl)) getOrElse suffix
        // Some("r"): suffix = "l", newInfl = "r"

        if (validateInflection(POST, transformation, knownInfl, newInfl)) {
        // pass

          // "nagl" + "r"
          Some(newStem -> newInfl)

        } else None

      } else None

    } else None
  }

  @Deprecated
  private case class ValidationType(id: Int) extends AnyVal
  private val PRE = ValidationType(0)
  private val POST = ValidationType(1)

  @Deprecated
  private def validateInflection(when: ValidationType, t: Transformation, expected: String, given: String): Boolean = {

    val strEq = expected == given

    (when, t) match {

      case (PRE,  _: ForwardTransformation) => strEq
      case (POST, _: ReverseTransformation) => strEq
      case _ => true
    }
  }
}
