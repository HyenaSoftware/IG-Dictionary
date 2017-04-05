package com.hyenawarrior.OldNorseGrammar.grammar

import com.hyenawarrior.OldNorseGrammar.grammar.nouns._
import com.hyenawarrior.dictionaryLoader._

/**
	* Created by HyenaWarrior on 2017.03.25..
	*/
case class Language(str: String) extends AnyVal

class Database(defs: Map[Language, List[MeaningDefinition]])
{
	trait FlagProp[T, P] {

		def asEnum(x: Set[P]): Option[T]
	}

	trait WordFlagProp[T] extends FlagProp[T, WordDescFlags]
	trait MeaningFlagProp[T] extends FlagProp[T, MeaningDescFlags]

	implicit object CaseFlagProp extends WordFlagProp[Case] {

		def asEnum(flags: Set[WordDescFlags]) = flags.find(_.isInstanceOf[CaseFlag]) match {

			case Some(CaseFlag.NOMINATIVE)	=> Some(Case.NOMINATIVE)
			case Some(CaseFlag.ACCUSATIVE)	=> Some(Case.ACCUSATIVE)
			case Some(CaseFlag.DATIVE)			=> Some(Case.DATIVE)
			case Some(CaseFlag.GENITIVE)		=> Some(Case.GENITIVE)
			case _ => None
		}
	}

	implicit object NumFlagProp extends WordFlagProp[Number] {

		def asEnum(flags: Set[WordDescFlags]) = flags.find(_.isInstanceOf[NumFlag]) match {

			case Some(NumFlag.SINGULAR) => Some(Number.SINGULAR)
			case Some(NumFlag.PLURAL) => Some(Number.PLURAL)
			case _ => None
		}
	}

	/*implicit object GenderFlagProp extends MeaningFlagProp[Gender] {

		def asEnum(flags: Set[MeaningDescFlags]) = flags.find(_.isInstanceOf[GenderFlag]) match {

			case Some(GenderFlag.MASCULINE) => Some(Gender.MASCULINE)
			case Some(GenderFlag.FEMININE) => Some(Gender.FEMININE)
			case Some(GenderFlag.NEUTER) => Some(Gender.NEUTER)
			case _ => None
		}
	}*/

	private def posTypeOf(md: MeaningDefinition) = md.meaningDescFlags.filter(_.isInstanceOf[PosFlag]).head
	private def posSubTypeOf(md: MeaningDefinition) = md.meaningDescFlags.filter(_.isInstanceOf[PosSubFlag]).head

	private def createNounFrom(md: MeaningDefinition): List[Word] = posSubTypeOf(md) match
	{
		// strong noun
		case PosSubFlag.STRONG =>  {

			val rootWordDef = md.wordDefinition.find(_.wordDescFlags.contains(MiscFlags.ROOT))
			val nonRootWordDefs = md.wordDefinition.filterNot(_.wordDescFlags.contains(MiscFlags.ROOT))

			//val gd = implicitly[MeaningFlagProp[Gender]].asEnum(md.meaningDescFlags).get
			val gd = md.meaningDescFlags.filter(_.isInstanceOf[GenderFlag]).map(_.asInstanceOf[GenderFlag]).head
			val mnr = md.meaningDescFlags.filter(_.isInstanceOf[MinorFlag]).map(_.asInstanceOf[MinorFlag]).head

			val optRoot = rootWordDef.map(rw => Root(rw.word))

			val caseOverrides = nonRootWordDefs.map(wd => {

				val cs = implicitly[WordFlagProp[Case]].asEnum(wd.wordDescFlags)
				val num = implicitly[WordFlagProp[Number]].asEnum(wd.wordDescFlags)

				(num, cs) -> wd.word
			}).toMap

			NounGenerator(optRoot, caseOverrides, md.meaningId, gd, mnr)
		}


		case PosSubFlag.WEAK => List()
			// weak noun
	}

	private def createVerbFrom(md: MeaningDefinition): List[Word] = List()

	private def load(md: MeaningDefinition): List[Word] =	posTypeOf(md) match
	{
		case PosFlag.NOUN => createNounFrom(md)
		case PosFlag.VERB => createVerbFrom(md)
	}

	private def loadAndWrapIntoMeaning(meaningDefs: List[MeaningDefinition]): List[WordGroup]
		= meaningDefs.map(md => WordGroup(load(md), md.meaningId, md.meaningDescFlags))

	private val definitions: Map[Language, List[WordGroup]] = defs.map { case (lang, words) => lang -> loadAndWrapIntoMeaning(words) }

	def findBy(searchStr: String): Map[WordGroup, List[Word]] =
	{
		val res = definitions
			.flatMap(_._2)
			.map(wg => wg -> wg.wordsThoseStartsWith(searchStr))
			.filter { case (_, list) => list.nonEmpty }
		  .toMap
		res
	}
}

case class WordGroup(words: List[Word], meaningId: Int, meaningDescFlags: Set[MeaningDescFlags])
{
	def isPrimary(w: Word) = w.pos match
	{
		case nn: Noun => (Number.SINGULAR, Case.NOMINATIVE) == nn.decl
		case _ => false
	}

	def primaryWord: Word = words.filter(w => isPrimary(w)).head

	def wordsThoseStartsWith(str: String) = words.filter(_.strForm.startsWith(str))
}
