package com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem

import java.lang.String.format

import com.hyenawarrior.OldNorseGrammar.grammar.Root
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.StemTransform._
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.{Ablaut, AblautGrade, StaticAblaut}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.StrongVerbClassEnum
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbClassEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.EnumVerbStem.{PRESENT_STEM, PRETERITE_SINGULAR_STEM}

/**
	* Created by HyenaWarrior on 2017.04.22..
	*/
abstract class VerbStem(stemType: EnumVerbStem) {

	def stringForm(): String

	def getStemType(): EnumVerbStem = stemType
}

abstract class CommonStrongVerbStem(normalizedStem: String, verbClass: StrongVerbClassEnum, stemType: EnumVerbStem)
	extends VerbStem(stemType) {

	def getAblautGrade(): AblautGrade

  // get the denormalized stem
  override def stringForm(): String = {

		// present stem is identical to the root
		val ablautGradeOfPresentStem = Ablaut.getAblautGradeFrom(normalizedStem)

		val myStemRepr = Ablaut.transform(normalizedStem, ablautGradeOfPresentStem, getAblautGrade()).get

    StrongVerbStem.denormalize(myStemRepr, verbClass, stemType) getOrElse myStemRepr
  }

  def getRoot(): Root = {

    val normalizedStemWithoutJAugment = normalizedStem match {

      case JAugment(s) => s
      case s => s
    }

    val ablautGradeOfPresentStem = StrongVerbStem.ABLAUTS(verbClass).presentAblautGrade
    val rootStrRepr = Ablaut.transform(normalizedStemWithoutJAugment, getAblautGrade(), ablautGradeOfPresentStem).get

    Root(rootStrRepr)
  }
}

object CommonStrongVerbStem {

	def unapply(strongVerbStem: CommonStrongVerbStem): Option[(Root, StrongVerbClassEnum, EnumVerbStem)]
		= strongVerbStem match {

		case svs: StrongVerbStem => Some((svs.getRoot(), svs.verbClass, svs.getStemType()))
		case svs7: StrongVerbStemClass7th => Some((svs7.getRoot(), STRONG_7TH_CLASS, svs7.getStemType()))
		case _ => None
	}
}

// instead of root it may have its string representation
// 	+ no need to reset roots
//	+ no need to have separate class for 7th verbs to represent their ablautGrades
//	+ for class 1-6, code could do validation
case class StrongVerbStem(normalizedStem: String, verbClass: StrongVerbClassEnum, stemType: EnumVerbStem)
	extends CommonStrongVerbStem(normalizedStem, verbClass, stemType) {

	def getAblautGrade() = StrongVerbStem.ABLAUTS(verbClass).grades(stemType)
}

case class StrongVerbStemClass7th(normalizedStem: String, stemType: EnumVerbStem, ablautGrade: AblautGrade)
	extends CommonStrongVerbStem(normalizedStem, STRONG_7TH_CLASS, stemType) {

	def getAblautGrade() = ablautGrade
}

object StrongVerbStem {

	val ABLAUTS: Map[StrongVerbClassEnum, StaticAblaut] = Map(
		STRONG_1ST_CLASS -> StaticAblaut("í",	"ei", "i", "i"),
		STRONG_2ND_CLASS -> StaticAblaut("jú", "au", "u", "o"),
		STRONG_3RD_CLASS -> StaticAblaut("e", 	"a", 	"u", "o"),

		STRONG_4TH_CLASS -> StaticAblaut("e",	"a", 	"á", "o"),
		STRONG_5TH_CLASS -> StaticAblaut("e", 	"a", 	"á", "e"),
		STRONG_6TH_CLASS -> StaticAblaut("a", 	"ó", 	"ó", "a"),
		STRONG_7TH_CLASS -> null
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

    StrongVerbStem(myStemRepr, verbClass, stemType)
  }

	/**
		* Create a stem from a string representation
		*
		* Do not use it for:
		* 	- creating a custom/irregular stem
		*
		* @param stemStr
		* @param verbClass
		* @param stemType
		* @return
		* @throws RuntimeException if the ablaut grade of the strong verb class is not matching on the nucleus of
		*                          the first syllable
		*/
	def fromStrRepr(stemStr: String, verbClass: StrongVerbClassEnum, stemType: EnumVerbStem): CommonStrongVerbStem = {

    verbClass match {

      case STRONG_7TH_CLASS =>
        val ablaut = Ablaut.getAblautGradeFrom(stemStr)
        StrongVerbStemClass7th(stemStr, stemType, ablaut)

      case _ =>
        val augmentedStem = augment(stemStr, verbClass, stemType)
        val normalizedStemStr = normalize(augmentedStem, verbClass, stemType)
        extractVerbFrom(verbClass, stemType, normalizedStemStr)
    }
  }

  /**
    * verb-form -> non-inflected, non-augmented stem -> denormalized stem
    *
    * @param stemStr
    * @param verbClass
    * @param stemType
    * @return
    */
  private def augment(stemStr: String, verbClass: StrongVerbClassEnum, stemType: EnumVerbStem): String
    = (verbClass, stemType, stemStr) match {

    case (STRONG_5TH_CLASS, PRESENT_STEM, FixJAugmentedWord(origStemStr)) => origStemStr
    case _ => stemStr
  }

  private def normalize(stemStr: String, verbClass: StrongVerbClassEnum, stemType: EnumVerbStem): String
		= (verbClass, stemType, stemStr) match {

    // normalize stem before processing it
    case (STRONG_2ND_CLASS, PRESENT_STEM, JuToJo(origStemStr)) => origStemStr

		case (STRONG_3RD_CLASS, PRESENT_STEM, Breaking(origStemStr)) => origStemStr
		case (STRONG_3RD_CLASS | STRONG_5TH_CLASS, PRESENT_STEM, Raising(origStemStr)) => origStemStr
		case (STRONG_3RD_CLASS, PRETERITE_SINGULAR_STEM, NasalAssimilation(origStemStr)) => origStemStr
		case (STRONG_3RD_CLASS, PRETERITE_SINGULAR_STEM, DevoiceAfterLateral(origStemStr)) => origStemStr

    //case (STRONG_5TH_CLASS, PRESENT_STEM, Raising(origStemStr)) => origStemStr
    case (STRONG_5TH_CLASS, PRETERITE_SINGULAR_STEM, ReduceStemFinalG(origStemStr)) => origStemStr

    case _ => stemStr
	}

  private[stem] def denormalize(stemRepr: String, verbClass: StrongVerbClassEnum, stemType: EnumVerbStem): Option[String]
    = (verbClass, stemType) match {

    case (STRONG_2ND_CLASS, PRESENT_STEM) => JuToJo(stemRepr)

    case (STRONG_3RD_CLASS, PRESENT_STEM) => Breaking(stemRepr) orElse Raising(stemRepr)
    case (STRONG_3RD_CLASS, PRETERITE_SINGULAR_STEM) => NasalAssimilation(stemRepr) orElse DevoiceAfterLateral(stemRepr)

    case (STRONG_5TH_CLASS, PRESENT_STEM)            => Raising(stemRepr)
    case (STRONG_5TH_CLASS, PRETERITE_SINGULAR_STEM) => ReduceStemFinalG(stemRepr)
    case _ => None
  }

  private def extractVerbFrom(verbClass: StrongVerbClassEnum, stemType: EnumVerbStem, stemStr: String): StrongVerbStem = {

    val givenSrcAblautGrade = Ablaut.getAblautGradeFrom(stemStr)
    val expectedSrcAblautGrade = ABLAUTS(verbClass).grades(stemType)

    if (givenSrcAblautGrade != expectedSrcAblautGrade) {

      throw new RuntimeException(format(
        "Cannot create verb stem object from string representation '%s'. The ablaut of the representation '%s'" +
          " is different than the '%s' %s stem ablaut grade of %s.",
        stemStr, givenSrcAblautGrade, expectedSrcAblautGrade, stemType.name, verbClass.name))
    }

    StrongVerbStem(stemStr, verbClass, stemType)
  }
}