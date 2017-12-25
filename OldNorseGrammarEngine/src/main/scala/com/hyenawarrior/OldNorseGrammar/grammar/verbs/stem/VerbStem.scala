package com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem

import java.lang.String.format

import com.hyenawarrior.OldNorseGrammar.grammar.Root
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.StemTransform._
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.TransformationMode.{Disabled, EnabledFor, Undefined}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbClassEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.EnumVerbStem.{PERFECT_STEM, PRESENT_STEM, PRETERITE_SINGULAR_STEM}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.{StrongVerbClassEnum, TransformationMode}

/**
	* Created by HyenaWarrior on 2017.04.22..
	*/
abstract class VerbStem(stemType: EnumVerbStem) {

	def stringForm(): String

	def getStemType(): EnumVerbStem = stemType
}

abstract class CommonStrongVerbStem(normalizedStem: String, verbClass: StrongVerbClassEnum, stemType: EnumVerbStem
                                    , appliedTransform: TransformationMode = Undefined)
	extends VerbStem(stemType) {

	def getAblautGrade(): AblautGrade

  /**
    * stem is regular if
    * - its ablaut is matching to the ablaut grade of its verb class
    *
    * @return
    */
  def isRegular: Boolean = {

    val currentAblautGrade = Ablaut.getAblautGradeFrom(normalizedStem)
    val ablautGradeOfPresentStem = StrongVerbStem.ABLAUTS(verbClass).presentAblautGrade

    currentAblautGrade == ablautGradeOfPresentStem
  }

  // get the denormalized stem
  override def stringForm(): String = {

    /*
      if the current stem is
      - present stem and it's regular: just validate the ablaut
      - irregular: ignore the ablaut
    */

    StrongVerbStem.denormalize(normalizedStem, verbClass, stemType, appliedTransform) getOrElse normalizedStem
  }

  def getRoot(): Root
}

object CommonStrongVerbStem {

	def unapply(strongVerbStem: CommonStrongVerbStem): Option[(String, StrongVerbClassEnum, EnumVerbStem, TransformationMode)]
		= strongVerbStem match {

		case svs: StrongVerbStem => Some((svs.normalizedStem, svs.verbClass, svs.getStemType(), svs.appliedTransform))
		case svs7: StrongVerbStemClass7th => Some((svs7.normalizedStem, STRONG_7TH_CLASS, svs7.getStemType(), Undefined))
		case _ => None
	}
}

// instead of root it may have its string representation
// 	+ no need to reset roots
//	+ no need to have separate class for 7th verbs to represent their ablautGrades
//	+ for class 1-6, code could do validation
case class StrongVerbStem(normalizedStem: String, verbClass: StrongVerbClassEnum, stemType: EnumVerbStem
  , appliedTransform: TransformationMode = Undefined)
	extends CommonStrongVerbStem(normalizedStem, verbClass, stemType, appliedTransform) {

  /**
    * @return Ablaut grade of the stem, that can be irregular.
    */
	def getAblautGrade() = Ablaut.getAblautGradeFrom(normalizedStem)

  /**
    * Be aware: it can't reflect the ablaut of an irregular present stem
    * @return
    */
  override def getRoot(): Root = {
      /*
                                  (stem relative root)
        (denormalized/decayed)    (normalized)   (root)   (ablaut)
        liggj-                    leg-           leg-
        lá-                       lág-           leg-
        lág-                      lág-           leg-

        hǫggv-                    haggv-         hag- ?
        hjógv-                    hjóggv-        hag- ?

        normalized stem:
          - doesn't have stem transformations, like:
            - root vowel doesn't change; e.g. breaking (as these interfere with ablaut changing)
            - nasal assimilation; ng -> kk
            - lt -> lt
            - J-augmentation; it affects only present stems, but other stems can be flagged
            - umlauts, as these interfere with ablaut changing
          - although it *does* have
            - V-augmentation as it appears also in past and perfect stems next to the present stem

        denormalized stem:
          - does *not* have I/U-umlauts, as these can interfere with I/U-umlauts of verb conjugation
              - in present singular, that's incorrect: haggv- -> hǫggv-(u) -> høggr(i)
              - the correct form is: haggv- -> heggr(i)
          - J/V-augmentations only have their markers at the end of the stem
          - stem transformations, and their vowel transformations
        */

    // present stem is identical to the root
    val ablautGradeOfPresentStem = StrongVerbStem.ABLAUTS(verbClass).presentAblautGrade
    val rootStrRepr = Ablaut.transform(normalizedStem, getAblautGrade(), ablautGradeOfPresentStem).get

    Root(rootStrRepr)
  }
}

case class StrongVerbStemClass7th(normalizedStem: String, stemType: EnumVerbStem, ablautGrade: AblautGrade)
	extends CommonStrongVerbStem(normalizedStem, STRONG_7TH_CLASS, stemType) {

	def getAblautGrade() = ablautGrade

  override def getRoot(): Root = Root(normalizedStem)
}

object StrongVerbStem {

	val ABLAUTS: Map[StrongVerbClassEnum, StaticAblaut] = Map(
		STRONG_1ST_CLASS -> StaticAblaut("í",	"ei", "i", "i"),
		STRONG_2ND_CLASS -> StaticAblaut("jú", "au", "u", "o"),
		STRONG_3RD_CLASS -> StaticAblaut("e", 	"a", 	"u", "o"),

		STRONG_4TH_CLASS -> StaticAblaut("e",	"a", 	"á", "o"),
		STRONG_5TH_CLASS -> StaticAblaut("e", "a", 	"á", "e"),
		STRONG_6TH_CLASS -> StaticAblaut("a", "ó", 	"ó", "a"),
		STRONG_7TH_CLASS -> null,
    STRONG_7_1_CLASS -> StaticAblaut("ei", "é", "é", "ei"),
    STRONG_7_2A_CLASS -> StaticAblaut("au", "jó", "jó", "au"),
    STRONG_7_2B_CLASS -> StaticAblaut("ú", "jó", "ju", "ú"),
    STRONG_7_3_CLASS -> StaticAblaut("a", "e", "e", "a"),
    STRONG_7_4_CLASS -> StaticAblaut("á", "é", "é", "á"),
    STRONG_7_5_CLASS -> StaticAblaut("ó", "é", "é", "ó")
	)

  /**
      Root -> Stem
   */
  def fromRoot(root: Root, verbClass: StrongVerbClassEnum, stemType: EnumVerbStem): StrongVerbStem = {

    // present stem is identical to the root
    val rootRepr = root.toString

    val ablautGradeOfPresentStem = Ablaut.getAblautGradeFrom(rootRepr)

    val targetAblautGrade = StrongVerbStem.ABLAUTS(verbClass).grades(stemType)

    val myStemRepr = Ablaut.transform(rootRepr, ablautGradeOfPresentStem, targetAblautGrade).get

    // TODO: do we need to do V/J augmentation here?
    //  like: lágum (past) -> leg (root) -> leggj (present)

    StrongVerbStem(myStemRepr, verbClass, stemType)
  }

	/**
		* Create a stem from a string representation
		*
		* Do not use it for:
		* 	- creating a custom/irregular stem
		*
		* @param stemStr    denormalized or decayed string representation
		* @param verbClass  class of the current verb stem
		* @param stemType   it's corresponding with the ablaut grades
		* @return
		* @throws RuntimeException if the ablaut grade of the strong verb class is not matching on the nucleus of
		*                          the first syllable
		*/
	def fromStrRepr(stemStr: String, verbClass: StrongVerbClassEnum, stemType: EnumVerbStem, subjectOfIUmalut: Boolean = false)
  : CommonStrongVerbStem = {

    verbClass match {

      case STRONG_7TH_CLASS =>
        val ablaut = Ablaut.getAblautGradeFrom(stemStr)
        StrongVerbStemClass7th(stemStr, stemType, ablaut)

      case _ =>
        val augmentedStem = augment(stemStr, verbClass, stemType, subjectOfIUmalut)
        val (optTransformation, normalizedStemStr) = normalize(augmentedStem, verbClass, stemType)
        //validateAblautGrade(verbClass, stemType, normalizedStemStr)

        StrongVerbStem(normalizedStemStr, verbClass, stemType, optTransformation)
    }
  }

  private object InverseBreaking { def unapply(arg: String): Option[String] = Breaking(arg) }

  /**
    * verb-form -> non-inflected, non-augmented stem -> denormalized stem
    */
  private def augment(stemStr: String, verbClass: StrongVerbClassEnum, stemType: EnumVerbStem, subjectOfIUmalut: Boolean): String
    = (verbClass, stemType, stemStr) match {

      // helpr <-[I-umlaut + SVD]-- hjalp- --[braking]-> help-
      // the effect of the next line is inverted back during the stem normalization, but the normalization also add a flag
      //  to indicate that this stem does have breaking - so, yes, it's redundant but it's important to have the flag
    case (STRONG_3RD_CLASS, PRESENT_STEM, InverseBreaking(s)) if subjectOfIUmalut => s

      /* do not fix the augmentation in any other cases:
        FIN    PST-STEM    PRS-STEM
        lá  -> lág-     -> liggj-
        bjó -> bjó-     -> bú-
        hjó -> hjóggv-  -> haggv-

        West Germanic gemination
          * does lágum (past) have only one 'g', because past stem is not J-augmented?
          *
          * https://lrc.la.utexas.edu/eieol/norol/70
          * The augment does not appear in the past forms; the past stems ended in a single consonant,
           * which disappeared in the singular forms by the time of the ON texts.
      */
    case (STRONG_5TH_CLASS | STRONG_7_2B_CLASS, PRESENT_STEM, FixStemAugmentation(origStemStr)) => origStemStr
    case _ => stemStr
  }

  private def normalize(stemStr: String, verbClass: StrongVerbClassEnum, stemType: EnumVerbStem): (TransformationMode, String)
		= (verbClass, stemType, stemStr) match {

    // normalize stem before processing it
    case (STRONG_2ND_CLASS, PRESENT_STEM, JuToJo(origStemStr)) => EnabledFor(JuToJo) -> origStemStr

		case (STRONG_3RD_CLASS,  PRESENT_STEM,  Breaking(origStemStr))  => EnabledFor(Breaking) -> origStemStr
		case (STRONG_3RD_CLASS,  PRESENT_STEM,  Raising(origStemStr))   => EnabledFor(Raising) -> origStemStr
    case (STRONG_3RD_CLASS,  PERFECT_STEM,  PerfectRaising(origStemStr))   => EnabledFor(PerfectRaising) -> origStemStr
		case (STRONG_3RD_CLASS | STRONG_7_3_CLASS, PRETERITE_SINGULAR_STEM, NasalAssimilation(origStemStr))    => EnabledFor(NasalAssimilation) -> origStemStr
		case (STRONG_3RD_CLASS | STRONG_7_3_CLASS, PRETERITE_SINGULAR_STEM, DevoiceAfterLateral(origStemStr))  => EnabledFor(DevoiceAfterLateral) -> origStemStr

    case (STRONG_5TH_CLASS | STRONG_7_2B_CLASS, PRESENT_STEM, JAugment(origStemStr)) => EnabledFor(JAugment) -> origStemStr

    case (STRONG_6TH_CLASS | STRONG_7_3_CLASS, PERFECT_STEM, VelarIUmlaut(origStemStr)) => EnabledFor(VelarIUmlaut) -> origStemStr

    case _ => Disabled -> stemStr
	}

  private[stem] def denormalize(stemRepr: String, verbClass: StrongVerbClassEnum, stemType: EnumVerbStem
                                , allowTransform: TransformationMode): Option[String]
    = (verbClass, stemType, allowTransform) match {

    case (STRONG_2ND_CLASS, PRESENT_STEM, _) => JuToJo(stemRepr)

    case (STRONG_3RD_CLASS | STRONG_7_3_CLASS, PRESENT_STEM, Undefined | EnabledFor(Breaking | Raising)) => Breaking(stemRepr) orElse Raising(stemRepr)
    case (STRONG_3RD_CLASS | STRONG_7_3_CLASS, PRETERITE_SINGULAR_STEM, _)  => NasalAssimilation(stemRepr) orElse DevoiceAfterLateral(stemRepr)
    case (STRONG_3RD_CLASS,  PERFECT_STEM,  Undefined | EnabledFor(PerfectRaising))   => PerfectRaising(stemRepr)

    case (STRONG_5TH_CLASS, PRESENT_STEM, EnabledFor(JAugment))             => JAugment(stemRepr)
    case (STRONG_5TH_CLASS | STRONG_7_2B_CLASS, PRETERITE_SINGULAR_STEM, _) => ReduceStemFinalG(stemRepr)

    case (STRONG_6TH_CLASS | STRONG_7_3_CLASS, PERFECT_STEM, Undefined | EnabledFor(VelarIUmlaut)) => VelarIUmlaut(stemRepr)

    case _ => None
  }

  // TODO: check that it's really needed or delete it if not
  private def validateAblautGrade(verbClass: StrongVerbClassEnum, stemType: EnumVerbStem, stemStr: String): Unit = {

    val givenSrcAblautGrade = Ablaut.getAblautGradeFrom(stemStr)
    val expectedSrcAblautGrade = ABLAUTS(verbClass).grades(stemType)

    if (givenSrcAblautGrade != expectedSrcAblautGrade) {

      throw new RuntimeException(format(
        "Cannot create verb stem object from string representation '%s'. The ablaut of the representation '%s'" +
          " is different than the '%s' %s stem ablaut grade of %s.",
        stemStr, givenSrcAblautGrade, expectedSrcAblautGrade, stemType.name, verbClass.name))
    }
  }
}