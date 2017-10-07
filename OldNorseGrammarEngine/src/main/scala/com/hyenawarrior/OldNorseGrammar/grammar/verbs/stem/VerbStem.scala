package com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem

import com.hyenawarrior.OldNorseGrammar.grammar.Root
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.{Ablaut, AblautGrade, StaticAblaut}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.StrongVerbClassEnum
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbClassEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.EnumVerbStem.{PERFECT_STEM, PRESENT_STEM, PRETERITE_PLURAL_STEM, PRETERITE_SINGULAR_STEM}

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

	override def stringForm(): String = {

		// present stem is identical to the root
		val rootRepr = root.toString

		val ablautGradeOfPresentStem = Ablaut.getAblautGradeFrom(rootRepr).get

		val myStemRepr = Ablaut.transform(rootRepr, ablautGradeOfPresentStem, getAblautGrade()).get

		myStemRepr
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
		* Create a stem from a strign representation
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
		= verbClass match {

		case STRONG_7TH_CLASS =>
			val ablaut = Ablaut.getAblautGradeFrom(stemStr).get
			StrongVerbStemClass7th(Root(stemStr), stemType, ablaut)

		case _ =>
			val givenSrcAblautGrade = Ablaut.getAblautGradeFrom(stemStr).get
			val expectedSrcAblautGrade = ABLAUTS(verbClass).grades(stemType)

			if(givenSrcAblautGrade != expectedSrcAblautGrade)
				throw  new RuntimeException()

			val ablautGradeOfPresentStem = ABLAUTS(verbClass).presentAblautGrade
			val optRootStrRepr = Ablaut.transform(stemStr, expectedSrcAblautGrade, ablautGradeOfPresentStem)

			optRootStrRepr match {

				case Some(rootStrRepr) => StrongVerbStem(Root(rootStrRepr), verbClass, stemType)
				case None => throw new RuntimeException()
			}

	}

	def stemFromRoot(root: Root, verbStemEnum: EnumVerbStem) = verbStemEnum match {

		case PRESENT_STEM =>
	}

	// no, it rather related to weak verbs
	def stemSuffixOf(verbStemEnum: EnumVerbStem) = verbStemEnum match {

		// present stem is identical to the root
		case PRESENT_STEM => ""

		case PRETERITE_SINGULAR_STEM => ""	// +ablaut
		case PRETERITE_PLURAL_STEM => ""		// +ablaut
		case PERFECT_STEM => ""							// +ablaut
	}
}