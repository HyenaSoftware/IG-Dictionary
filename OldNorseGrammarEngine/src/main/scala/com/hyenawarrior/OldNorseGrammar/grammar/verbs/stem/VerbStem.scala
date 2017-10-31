package com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem

import java.lang.String.format

import com.hyenawarrior.OldNorseGrammar.grammar.Root
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.StemTransform._
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.{Ablaut, AblautGrade, StaticAblaut}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.StrongVerbClassEnum
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbClassEnum._

/**
	* Created by HyenaWarrior on 2017.04.22..
	*/
abstract class VerbStem(root: Root, stemType: EnumVerbStem) {

	def stringForm(): String

	def getStemType(): EnumVerbStem = stemType
}

abstract class CommonStrongVerbStem(root: Root, verbClass: StrongVerbClassEnum, stemType: EnumVerbStem)
	extends VerbStem(root, stemType) {

	def getAblautGrade(): AblautGrade

  private def applyPhonologicalChanges(stemRepr: String): String = verbClass match {

    case STRONG_2ND_CLASS => JuToJo(stemRepr) getOrElse stemRepr
    case STRONG_3RD_CLASS => Breaking(stemRepr) orElse Raising(stemRepr) getOrElse stemRepr
    case _ => stemRepr
  }

  override def stringForm(): String = {

		// present stem is identical to the root
		val rootRepr = root.toString

		val ablautGradeOfPresentStem = Ablaut.getAblautGradeFrom(rootRepr)

		val myStemRepr = Ablaut.transform(rootRepr, ablautGradeOfPresentStem, getAblautGrade()).get

    applyPhonologicalChanges(myStemRepr)
  }
}

object CommonStrongVerbStem {

	def unapply(strongVerbStem: CommonStrongVerbStem): Option[(Root, StrongVerbClassEnum, EnumVerbStem)]
		= strongVerbStem match {

		case svs: StrongVerbStem => Some((svs.root, svs.verbClass, svs.getStemType()))
		case svs7: StrongVerbStemClass7th => Some((svs7.root, STRONG_7TH_CLASS, svs7.getStemType()))
		case _ => None
	}
}

// instead of root it may have its string representation
// 	+ no need to reset roots
//	+ no need to have separate class for 7th verbs to represent their ablautGrades
//	+ for class 1-6, code could do validation
case class StrongVerbStem(root: Root, verbClass: StrongVerbClassEnum, stemType: EnumVerbStem)
	extends CommonStrongVerbStem(root, verbClass, stemType) {

	def getAblautGrade() = StrongVerbStem.ABLAUTS(verbClass).grades(stemType)
}

case class StrongVerbStemClass7th(root: Root, stemType: EnumVerbStem, ablautGrade: AblautGrade)
	extends CommonStrongVerbStem(root, STRONG_7TH_CLASS, stemType) {

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
	def fromStrRepr(stemStr: String, verbClass: StrongVerbClassEnum, stemType: EnumVerbStem): CommonStrongVerbStem
		= (verbClass, stemStr) match {

		case (STRONG_7TH_CLASS, _) =>
			val ablaut = Ablaut.getAblautGradeFrom(stemStr)
			StrongVerbStemClass7th(Root(stemStr), stemType, ablaut)

    case (STRONG_2ND_CLASS, JuToJo(origStemStr)) => extractVerbFrom(verbClass, stemType, origStemStr)

      // normalize stem before processing it
		case (STRONG_3RD_CLASS, Breaking(origStemStr)) => extractVerbFrom(verbClass, stemType, origStemStr)
    case (STRONG_3RD_CLASS, Raising(origStemStr)) => extractVerbFrom(verbClass, stemType, origStemStr)

    case _ => extractVerbFrom(verbClass, stemType, stemStr)
	}

  private def extractVerbFrom(verbClass: StrongVerbClassEnum, stemType: EnumVerbStem, stemStr: String): StrongVerbStem
    = {

    val givenSrcAblautGrade = Ablaut.getAblautGradeFrom(stemStr)
    val expectedSrcAblautGrade = ABLAUTS(verbClass).grades(stemType)

    if (givenSrcAblautGrade != expectedSrcAblautGrade) {

      throw new RuntimeException(format(
        "Cannot create verb stem object from string representation '%s'. The ablaut of the representation '%s'" +
          " is different than the '%s' %s stem ablaut grade of %s.",
        stemStr, givenSrcAblautGrade, expectedSrcAblautGrade, stemType.name, verbClass.name))
    }

    val ablautGradeOfPresentStem = ABLAUTS(verbClass).presentAblautGrade
    val rootStrRepr = Ablaut.transform(stemStr, expectedSrcAblautGrade, ablautGradeOfPresentStem).get

    StrongVerbStem(Root(rootStrRepr), verbClass, stemType)
  }
}