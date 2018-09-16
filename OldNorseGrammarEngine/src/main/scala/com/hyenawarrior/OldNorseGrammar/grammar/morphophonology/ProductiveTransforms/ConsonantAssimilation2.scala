package com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.ProductiveTransforms

import java.lang.Math._

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
  private trait Transformation {

    val description: String
    val stemReplacement: (Regex, Option[String])
    val suffixReplacement: (Regex, Option[String])
    val overlappingCharacters: Int
    val assimilatedCharacters: Int
  }

  private object Transformation {

    def unapply(tr: Transformation): Option[((Regex, Option[String]), (Regex, Option[String]), Int, Int)] = {

      Some((tr.stemReplacement, tr.suffixReplacement, tr.overlappingCharacters, tr.assimilatedCharacters))
    }
  }

  private case class ForwardTransformation(stemReplacement: (Regex, Option[String]), suffixReplacement: (Regex, Option[String]),
    overlappingCharacters: Int = 0, assimilatedCharacters: Int = 0, description: String) extends Transformation

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

    // forward+merging
    ForwardTransformation(s"$C[lns]$$".r -> None, s"^r".r -> Some(""), description = "Cr > C"))

  private val REV_ITEMS = Seq(
    // forward
    ReverseTransformation(s"$C*$SV$C$UV[ln]|$LV[ln]|s$$".r -> None, "^[lns]".r -> Some("r"), description = "CC > Cr"),

    // backward
    ReverseTransformation("(?<=[aǫ])(ð)$".r           -> Some("nn"),  "^r".r    -> None, description = "aðr > annr"),
    ReverseTransformation("(?<=[aǫ])(ð)(?=r$)".r      -> Some("nn"),  s"^$UV".r -> None, description = "aðr > annr"),
    ReverseTransformation("$".r                       -> None,        "^sk$".r  -> Some("rsk"), description = "sk > rsk"),
    ReverseTransformation("d$".r                      -> Some("ð"),   "^d".r    -> Some("ð"),   description = "dd > ðð"),
    ReverseTransformation("t$".r                      -> Some("dd"),  "^t".r    -> None,        description = "tt > ddt"),

    // merging
    ReverseTransformation("t$".r                  -> Some("n"), "^t".r -> None,       description = "tt > nt"),
    ReverseTransformation(s"(?<=^$C*$V$C)t$$".r   -> Some("ð"), "^t".r -> None, 1, 1, description = "t > ðt"),
    ReverseTransformation("z$".r                  -> Some("t"), "^z([kt])$".r   -> Some("s$1"), 1, 1, description = "zk > tsk"),
    ReverseTransformation("z$".r                  -> Some("ð"), "^z([kt])$".r   -> Some("s$1"), 1, 1, description = "zk > ðsk"),
    ReverseTransformation(s"^$C*$V($C)(?!z|\\1)$C$$".r  -> None,      s"^(?!z)($C)".r -> Some("$1"),  1, 1, description = "C -> CC"),

    // forward+merging
    ReverseTransformation(s"^$C*$V$C*[lns]$$".r   -> None,      "^[lns]$".r     -> Some("r"),   1, 1, description = "C > Cr")
  )

  def transform(str: String, suffix: String): Seq[String] = syncopeRouting(str, suffix, FWD_ITEMS)

  def reverse(str: String, suffix: String): Seq[String] = syncopeRouting(str, suffix, REV_ITEMS)

  private def syncopeRouting(str: String, suffix: String, items: Seq[Transformation]): Seq[String] = {

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

  private def transformImpl(transformation: Transformation, word: String, knownInfl: String): Option[String] = {

    val Transformation((stemRgx, stemRepl), (inflRgx, inflRepl), overlappingCharacters, assimilatedCharacters) = transformation

    val inflLength = max(knownInfl.length - assimilatedCharacters, 0)
    val endPoint = word.length - inflLength
    val beginPoint = endPoint - overlappingCharacters

    val realStem = word.substring(0, endPoint)
    val suffix = word.substring(beginPoint, word.length)
    val realInfl = word.substring(endPoint, word.length)

    if(validateInflection(PRE, transformation, knownInfl, realInfl)) {

      val stemChangeIsFine = stemRgx.findFirstIn(realStem).nonEmpty
      val inflChangeIsFine = inflRgx.findFirstIn(suffix).nonEmpty

      if (stemChangeIsFine && inflChangeIsFine) {

        val newStem = stemRepl.map(repl => stemRgx.replaceFirstIn(realStem, repl)) getOrElse realStem
        val newInfl = inflRepl.map(repl => inflRgx.replaceFirstIn(suffix, repl)) getOrElse suffix

        if (validateInflection(POST, transformation, knownInfl, newInfl)) {

          Some(newStem + newInfl)

        } else None

      } else None

    } else None
  }

  private case class ValidationType(id: Int) extends AnyVal
  private val PRE = ValidationType(0)
  private val POST = ValidationType(1)

  private def validateInflection(when: ValidationType, t: Transformation, expected: String, given: String): Boolean = {

    val strEq = expected == given

    (when, t) match {

      case (PRE,  _: ForwardTransformation) => strEq
      case (POST, _: ReverseTransformation) => strEq
      case _ => true
    }
  }
}
