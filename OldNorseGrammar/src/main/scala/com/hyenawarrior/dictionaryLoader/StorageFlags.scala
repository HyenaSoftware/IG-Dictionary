package com.hyenawarrior.dictionaryLoader

import com.hyenawarrior.auxiliary.EnumLike



abstract class AbstractFlag(name: String) {

	AbstractFlag.add(name -> this)

	override def toString = name
}

object AbstractFlag extends EnumLike[String, AbstractFlag]
{
	// force init
	MiscFlags
	PosFlag
	PosSubFlag
	CaseFlag
	NumFlag
	GenderFlag
	MinorFlag
}

object MeaningDescFlags extends EnumLike[String, MeaningDescFlags]
object WordDescFlags extends EnumLike[String, WordDescFlags]

class MeaningDescFlags			(name: String) extends AbstractFlag(name)  		{ MeaningDescFlags	.add(name -> this) }
class WordDescFlags					(name: String) extends AbstractFlag(name)  		{ WordDescFlags			.add(name -> this) }
final case class MiscFlags	(name: String) extends WordDescFlags(name)  	{ MiscFlags					.add(name -> this) }
final case class PosFlag		(name: String) extends MeaningDescFlags(name) { PosFlag						.add(name -> this) }
final case class PosSubFlag	(name: String) extends MeaningDescFlags(name) { PosSubFlag				.add(name -> this) }
final case class CaseFlag		(name: String) extends WordDescFlags(name) 		{ CaseFlag					.add(name -> this) }
final case class NumFlag		(name: String) extends WordDescFlags(name) 		{ NumFlag						.add(name -> this) }
@deprecated("use Gender instead") final case class GenderFlag	(name: String) extends MeaningDescFlags(name)	{ GenderFlag				.add(name -> this) }
final case class MinorFlag	(name: String) extends MeaningDescFlags(name)	{ MinorFlag 				.add(name -> this) }

object MiscFlags extends EnumLike[String, MiscFlags] {

	val ROOT = MiscFlags("ROOT")
}

object PosFlag extends EnumLike[String, PosFlag]
{
	val NOUN = new PosFlag("NOUN")
	val VERB = new PosFlag("VERB")
}

object PosSubFlag extends EnumLike[String, PosSubFlag]
{
	val STRONG = PosSubFlag("STRNG")
	val WEAK = PosSubFlag("WK")
}

object CaseFlag extends EnumLike[String, CaseFlag]
{
	val NOMINATIVE = new CaseFlag("NOM")
	val ACCUSATIVE = new CaseFlag("ACC")
	val DATIVE = new CaseFlag("DAT")
	val GENITIVE = new CaseFlag("GEN")
}

object NumFlag extends EnumLike[String, NumFlag] {

	val SINGULAR = new NumFlag("SNG")
	val PLURAL = new NumFlag("PLR")
}

object GenderFlag extends EnumLike[String, GenderFlag] {

	val MASCULINE = new GenderFlag("MASC")
	val FEMININE = new GenderFlag("FEM")
	val NEUTER = new GenderFlag("NEUTER")
}

object MinorFlag extends EnumLike[String, MinorFlag] {

	val A = MinorFlag("A")
	val R = MinorFlag("R")
}



case class NounFlags(mdFlags: List[MeaningDescFlags]) extends AnyVal {

	def posTypeOf = mdFlags.filter(_.isInstanceOf[PosFlag]).head
	def posSubTypeOf = mdFlags.filter(_.isInstanceOf[PosSubFlag]).head
}

