package com.hyenawarrior.OldNorseGrammar.grammar.nouns

import com.hyenawarrior.OldNorseGrammar.grammar.Case.{ACCUSATIVE, DATIVE, GENITIVE, NOMINATIVE}
import com.hyenawarrior.OldNorseGrammar.grammar.Number.{PLURAL, SINGULAR}
import com.hyenawarrior.OldNorseGrammar.grammar.{Case, DescriptorFlag, I_Umlaut, Number, PoS, Root, Stem, U_Umlaut, Word, WordTransformation}
import com.hyenawarrior.dictionaryLoader.{GenderFlag, MinorFlag}

/**
	* Created by HyenaWarrior on 2017.03.20..
	*/
class Noun(str: String, meaningId: Int, val decl: (Number, Case), root: Option[Root]) extends Stem(root) with PoS
{
	// mostly for debug
	override def toString = s"$str (Noun) [${decl._1}, ${decl._2}] [root:${super.toString}]"

	// testing or searching, perhaps it will be deprecated
	override def strForm = str

	override def descriptorFlags = List(decl._1, decl._2)
}

trait NounGenerator
{
	def apply(root: Root, meaningId: Int, decl: (Number, Case)): Word =
	{
		val str = inflect(root, decl)
		val nn = new Noun(str, meaningId, decl, Some(root))
		Word(nn, transormationsFor(decl))
	}

	def transormationsFor(decl: (Number, Case)): List[WordTransformation] = List()

	protected def inflect(root: Root, decl: (Number, Case)): String = root.word + inflection(decl)

	protected def inflection(decl: (Number, Case)): String

	def unapply(declForm: String, decl: (Number, Case)): Root = Root(declForm.stripSuffix(inflection(decl)))
}

object NullStemClass extends NounGenerator
{
	override protected def inflection(decl: (Number, Case)): String = ""
}

object StrongStemClassMascA extends NounGenerator
{
	override protected def inflection(decl: (Number, Case)) = decl match
	{
		case (SINGULAR, NOMINATIVE) => "r"
		case (SINGULAR, ACCUSATIVE)	=> ""
		case (SINGULAR, DATIVE)			=> "i"
		case (SINGULAR, GENITIVE)		=> "s"	 // "s"

		case (PLURAL, NOMINATIVE)		=> "ar"
		case (PLURAL, ACCUSATIVE)		=> "a"
		case (PLURAL, DATIVE)				=> "um"
		case (PLURAL, GENITIVE)			=> "a"	 // "s"
	}
}

object StrongStemClassMascR extends NounGenerator
{
	override protected def inflection(decl: (Number, Case)) = decl match
	{
		case (SINGULAR, NOMINATIVE) => "r"
		case (SINGULAR, ACCUSATIVE)	=> ""
		case (SINGULAR, DATIVE)			=> "i"
		case (SINGULAR, GENITIVE)		=> "s"

		case (PLURAL, NOMINATIVE)		=> "ar"
		case (PLURAL, ACCUSATIVE | GENITIVE)		=> "a"
		case (PLURAL, DATIVE)		=> "um"
	}
}

object StrongStemClassFeminineR extends NounGenerator
{
	override def transormationsFor(decl: (Number, Case)) =  decl match
	{
		case (SINGULAR, cs) if cs != GENITIVE => List(U_Umlaut)
		case (PLURAL, NOMINATIVE | ACCUSATIVE) => List(I_Umlaut)
		case (PLURAL, DATIVE) => List(U_Umlaut)
		case _ => List.empty
	}

	protected override def inflection(decl: (Number, Case)) = decl match
	{
		// auto umlaut for SNG-NOM and SNG-ACC
		// root -> u-umlaut
		case (SINGULAR, cs) if cs != GENITIVE		=> ""
		case (PLURAL, NOMINATIVE | ACCUSATIVE)	=> "r"
		case (PLURAL, DATIVE)										=> "um"
		case (SINGULAR, GENITIVE)								=> "ar"
		case (PLURAL, GENITIVE)									=> "a"
	}
}


object NounGenerator
{

	val ALL_DECLENSION = List(SINGULAR, PLURAL).flatMap(num => Case.values.map(cs => num -> cs))

	private def extractRootFrom(overrides: Map[(Number, Case), String])(implicit genderStemClass: (GenderFlag, MinorFlag)): Root =
	{
		val nounGen = nounGeneratorOf

		val possibleRoots = overrides.map{ case (num, cs) => nounGen.unapply(cs, num)}

		possibleRoots.head
	}

	private def extend(overrides: Map[(Option[Number], Option[Case]), String]): Map[(Number, Case), String] =
	{
		overrides.flatMap(ncw => ncw match
		{
			case ((None, Some(cs)), word) => Number.conventionalValues.map(_ -> cs -> word)
			case ((Some(num), None), word) => Case.values.map(num -> _ -> word)
			case ((Some(num), Some(cs)), word) => List(num -> cs -> word)
		})
	}

	private def nounGeneratorOf(implicit genderStemClass: (GenderFlag, MinorFlag)) = genderStemClass match
	{
		case (GenderFlag.MASCULINE, MinorFlag.A) => StrongStemClassMascA
		case (GenderFlag.MASCULINE, MinorFlag.R) => StrongStemClassMascR
		case (GenderFlag.FEMININE, MinorFlag.R) => StrongStemClassFeminineR
		case _ => NullStemClass
	}

	private def inflectionFor(root: Root, meaningId: Int, decl: (Number, Case))(implicit genderStemClass: (GenderFlag, MinorFlag)): Word =
	{
		nounGeneratorOf.apply(root, meaningId, decl)
	}

	def apply(givenRoot: Option[Root], overrides: Map[(Option[Number], Option[Case]), String], meaningId: Int, gender: GenderFlag, stemClass: MinorFlag): List[Word] =
	{
		implicit val genderAndStemClass = (gender, stemClass)

		val extOverrides = extend(overrides)

		val root = givenRoot.getOrElse(extractRootFrom(extOverrides))

		val infections = ALL_DECLENSION
			  .filter(d => !extOverrides.keys.exists(d == _))
				.map(decl => inflectionFor(root, meaningId, decl))

		val irregularInflections = extOverrides
			.map
			{
				case(decl, str) => val nounGen = nounGeneratorOf

				Word(new Noun(str, meaningId, decl, Some(root)), nounGen.transormationsFor(decl))
			}
		  .toList

		irregularInflections ++ infections
	}
}
