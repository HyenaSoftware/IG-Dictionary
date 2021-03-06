package com.hyenawarrior.OldNorseGrammar.grammar.morphophonology

import com.hyenawarrior.OldNorseGrammar.grammar.Syllable.Length.SHORT
import com.hyenawarrior.OldNorseGrammar.grammar.{Syllable, Syllables}
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.ProductiveTransforms.VowelLengthening
import com.hyenawarrior.OldNorseGrammar.grammar.phonology.Consonant
import com.hyenawarrior.OldNorseGrammar.grammar.phonology.Consonant._
import com.hyenawarrior.auxiliary.&

/**
  * Created by HyenaWarrior on 2017.10.20..
  */
object StemTransform {

  trait Transformation {

    def apply(stemStr: String): Option[String]

    def unapply(stemStr: String): Option[String]
  }

  trait NucleusTransformation extends Transformation {

    protected val SRC_NUCLEUS: String
    protected val DST_NUCLEUS: String

    final def apply(stemStr: String): Option[String] = transform(stemStr, SRC_NUCLEUS, DST_NUCLEUS)
    final def unapply(stemStr: String): Option[String] = transform(stemStr, DST_NUCLEUS, SRC_NUCLEUS)

    protected def transform(stemStr: String, nucleus: String, newNucleus: String): Option[String]
  }

  object Breaking extends NucleusTransformation {

    // assume that a stem has only one syllable (?)
    val SRC_NUCLEUS: String = "e"
    val DST_NUCLEUS: String = "ja"

    override def transform(stemStr: String, nucleus: String, newNucleus: String): Option[String] = {

      val idxOfJa =	stemStr indexOf nucleus
      if(idxOfJa == -1) return None

      val idxOfNucleusEnd = idxOfJa + nucleus.length
      val onset = stemStr.substring(0, idxOfJa)
      val coda = stemStr substring idxOfNucleusEnd

      if (!isEligible(onset, coda)) return None

      Some(onset + newNucleus + coda)
    }

    /**
      https://lrc.la.utexas.edu/eieol/norol/20#grammar_1398
      https://lrc.la.utexas.edu/eieol/norol/60#grammar_1454

      > This rule only applies to [the infinitive and present plural forms]* of verbs whose stem ends in
      a consonant cluster beginning with l or r.
      > Fracture does not occur at all if *e is preceded by v, l, or r, e.g. verða, leðr.

      * Sg 1-3 has I-umlaut, that reverses -ja- to -e- with the help of semivowel-deletion.
        So I assume that it's applied to the whole present stem.
      */
    private def isEligible(onset: String, coda: String): Boolean = {

      val prevCons = onset.lastOption.getOrElse(' ')
      val clusterFirstCons = coda.charAt(0)
      val clusterSecondCons = coda.charAt(1)

      val firstIsVLR = "vlr" contains prevCons
      val secondIsLR = "lr" contains clusterFirstCons
      val thirdIsCons = isConsonant(clusterSecondCons)

      !firstIsVLR && secondIsLR && thirdIsCons
    }
  }

  // TODO: Should it be renamed as A-mutation?
  object JuToJo extends NucleusTransformation {

    protected val SRC_NUCLEUS: String = "jú"
    protected val DST_NUCLEUS: String = "jó"

    protected def transform(stemStr: String, nucleus: String, newNucleus: String): Option[String] = {

      val idxOfNucleus = stemStr.indexOf(nucleus)
      val idxOfNucleusEnd = idxOfNucleus + nucleus.length

      if(isEligible(stemStr, idxOfNucleusEnd)) {

        val onset = stemStr.substring(0, idxOfNucleus)
        val coda  = stemStr.substring(idxOfNucleusEnd)

        Some(onset + newNucleus + coda)

      } else None
    }

    private def isEligible(stemStr: String, idxOfNucleusEnd: Int): Boolean = (stemStr.length > idxOfNucleusEnd) && {

      val nextLetter = stemStr.charAt(idxOfNucleusEnd)

      Consonant.isDental(nextLetter)
    }
  }

  object Raising extends NucleusTransformation {

    val SRC_NUCLEUS: String = "e"
    val DST_NUCLEUS: String = "i"

    override def transform(stemStr: String, nucleus: String, newNucleus: String): Option[String] = {

      val idxOfE = stemStr.indexOf(nucleus)

      val coda = stemStr.substring(idxOfE + nucleus.length)

      if(idxOfE > 0 && (coda.startsWith("n") || coda.endsWith("j"))) {

        val onset = stemStr.substring(0, idxOfE)

        Some(onset + newNucleus + coda)

      } else None
    }
  }

  // TODO: make the code generic
  object PerfectRaising extends NucleusTransformation {

    val SRC_NUCLEUS: String = "o"
    val DST_NUCLEUS: String = "u"

    override def transform(stemStr: String, nucleus: String, newNucleus: String): Option[String] = {

      val idxOfE = stemStr.indexOf(nucleus)

      val coda = stemStr.substring(idxOfE + nucleus.length)

      if(idxOfE > 0 && coda.startsWith("n")) {

        val onset = stemStr.substring(0, idxOfE)

        Some(onset + newNucleus + coda)

      } else None
    }
  }

  object NasalAssimilation extends Transformation {

    override def apply(stemStr: String): Option[String] = {

      val (prefix, lastChars) = split(stemStr)

      val newSuffix = lastChars.toSeq match {

        case _ :+ c :+ d if isNasal(c) && isVoicedStop(d) => Some(s"${devoice(d)}" * 2)
        case _ => None
      }

      newSuffix.map(prefix + _)
    }

    private def split(stemStr: String) = stemStr splitAt stemStr.length - 2

    override def unapply(stemStr: String): Option[String] = {

      val (prefix, lastChars) = split(stemStr)

      val suffix = lastChars.toSeq match {

        case _ :+ c :+ d if c==d && isVoicelessStop(c) =>
          val n = if(c=='p') 'm' else 'n'
          Some(s"$n${voice(d)}")
        case _ => None
      }

      suffix.map(prefix + _)
    }
  }

  object DevoiceAfterLateral extends Transformation {

    override def apply(stemStr: String): Option[String] = swap(stemStr, "ld", "lt")

    override def unapply(stemStr: String): Option[String] = swap(stemStr, "lt", "ld")

    private def swap(stemStr: String, from: String, to: String): Option[String] = {

      val (prefix, lastChars) = stemStr splitAt stemStr.length - 2

      if(lastChars == from) Some(prefix + to) else None
    }
  }

  object ReduceStemFinalG extends Transformation {

    private val reduceFinalG = "^(.+?)g$".r

    override def apply(stemStr: String): Option[String] = Some {

      stemStr match {

        case reduceFinalG(reducedStemStr) => VowelLengthening(reducedStemStr)
        case _ => stemStr
      }
    }

    override def unapply(stemStr: String): Option[String] = Some {

      val VowelLengthening(shortenedStemStr) = stemStr

      if (!isConsonant(shortenedStemStr.last))
        shortenedStemStr + "g"
      else
        shortenedStemStr
    }
  }

  // root <-> stem
  object JAugment extends Transformation {

    private val singleG = "(?<!g)g$".r
    private val doubleG = "gg$".r

    override def apply(stemStr: String): Option[String] = {

      val hasE = stemStr.indexOf('e') != -1

      if(hasE) {

        val augmentedStem = singleG.replaceFirstIn(stemStr, "gg") + "j"

        Raising(augmentedStem)

      } else {

        None
      }
    }

    override def unapply(stemStr: String): Option[String] = stemStr match {

      case Raising(unRaisedStemStr) =>

        val withOutAugment = unRaisedStemStr stripSuffix "j"

        Some(doubleG.replaceAllIn(withOutAugment, "g"))

      case _ => None
    }
  }

  /**
    *  restore the J-augmented stem
    *  The only purpose of it, is to undo the semivowel deletion
    */
  object FixJAugmentation {

    def unapply(stemStr: String): Option[String] = {

      val Syllables(sys @ (sy :: _)) = stemStr

      val endsSingleCons = sys.length == 1 && sys.last.coda.length == 1

      sy.nucleus match {
        case "i" | "í" if endsSingleCons || stemStr.endsWith("gg") => Some(if(stemStr endsWith "j") stemStr else stemStr + "j")
        case _ => None
      }
    }
  }

  /**
    * restore the V-augmented stem
    * The only purpose of it, is to undo the semivowel deletion
    *
    * [https://lrc.la.utexas.edu/eieol/norol/10#grammar_1389]
    * The w (v in the orthography) of the stem remained only when it followed
    * - a short syllable,
    * - a g, or a k,
    * - and preceded a or u.
    *
    * <SHORT|"[:alpha:]+[kg]">w<"[au][:alpha:]*">
    */
  @deprecated(message = "Use the split versions below")
  object FixVAugmentation {

    // this regex also prevent to add a final -v to a stem that already has augmentation
    private val velarEnd = "^(.+(?:ng|gg|kk))$".r

    def unapply(stemStr: String): Option[String] = stemStr match {

      // first syllable is affected by (V-augmented) U-Umlaut and stem ends in a velar consonant
      case Syllables(Syllable(_, "a" | "y", _, _, _) :: _) & velarEnd(_) => Some(stemStr + "v")

      // the last syllable of the stem is short
      case Syllables(_ :+ Syllable(_, _, _, _, SHORT)) => Some(stemStr + "v")

      case _ => None
    }
  }

  object FixVAugmentatAfterShortSyllable {

    def unapply(stemStr: String): Option[String] = stemStr match {

      case s if s.endsWith("v") => Some(stemStr)
      // the last syllable of the stem is short
      case Syllables(_ :+ Syllable(_, "a" | "i"  | "e", _, _, SHORT)) => Some(stemStr + "v")
      case _ => None
    }
  }

  object FixVAugmentatAfterVelar {

    // this regex also prevent to add a final -v to a stem that already has augmentation
    private val velarEnd = "^(.+(?:ng|gg|kk))$".r

    def unapply(stemStr: String): Option[String] = stemStr match {

      case s if s.endsWith("v") => Some(stemStr)
      // first syllable is affected by (V-augmented) U-Umlaut and stem ends in a velar consonant
      case Syllables(Syllable(_, "a" | "i"  | "e", _, _, _) :: _) & velarEnd(_) => Some(stemStr + "v")
      case _ => None
    }
  }

  object VelarIUmlaut extends Transformation {

    private def stemEndsInVelar(stemStr: String) = Consonant isVelar stemStr.last

    override def apply(stemStr: String): Option[String] = if(stemEndsInVelar(stemStr)) I_Umlaut(stemStr) else None

    override def unapply(stemStr: String): Option[String] = stemStr match {

      case I_Umlaut(normalizedStem) if stemEndsInVelar(stemStr) => Some(normalizedStem)

      case _ => None
    }
  }
}
