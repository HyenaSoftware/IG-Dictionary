package com.hyenawarrior.OldNorseGrammar.grammar.nouns

import com.hyenawarrior.OldNorseGrammar.grammar.GNumber.{PLURAL, SINGULAR}
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses._
import com.hyenawarrior.OldNorseGrammar.grammar._
import com.hyenawarrior.dictionaryLoader.{GenderFlag, MinorFlag}



object NounGenerator
{
	val ALL_DECLENSION = List(SINGULAR, PLURAL).flatMap(num => Case.values.map(cs => num -> cs))

	private def extractRootFrom(overrides: Map[(GNumber, Case), String])(implicit genderStemClass: (GenderFlag, MinorFlag)): Root =
	{
		val nounGen = nounGeneratorOf

		val possibleRoots = overrides.flatMap
		{
			case (num, cs) => nounGen.unapply(cs, num)
		}

		possibleRoots.head
	}

	private def extend(overrides: Map[(Option[GNumber], Option[Case]), String]): Map[(GNumber, Case), String] =
	{
		overrides.flatMap(ncw => ncw match
		{
			case ((None, Some(cs)), word) => GNumber.conventionalValues.map(_ -> cs -> word)
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

	private def inflectionFor(root: Root, meaningId: Int, decl: (GNumber, Case))(implicit genderStemClass: (GenderFlag, MinorFlag)): Word =
	{
		nounGeneratorOf.apply(root, meaningId, decl)
	}

	def apply(givenRoot: Option[Root], overrides: Map[(Option[GNumber], Option[Case]), String], meaningId: Int, gender: GenderFlag, stemClass: MinorFlag): List[Word] =
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
				case(decl, str) => Word(new Noun(str, meaningId, decl, root, nounGeneratorOf))
			}
		  .toList

		irregularInflections ++ infections
	}
}
