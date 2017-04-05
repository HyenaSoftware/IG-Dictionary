package com.hyenawarrior.OldNorseGrammar.grammar

/**
	* Created by HyenaWarrior on 2017.03.01..
	*/
case class Word(pos: PoS, transformations: List[WordTransformation])
{
	def underlyingPoS = pos

	// useful for lookup
	val strForm = transformations.foldLeft(pos.strForm)((str, trn) => trn applyIfEligible str)

	// formatted description
	val description = "[not yet]"

	val traits: List[DescriptorFlag] = pos.descriptorFlags

	override def toString = s"$strForm [$pos + ${transformations.map(_.toString)}]"
}


trait WordTransformation
{
	final def applyIfEligible(str: String) = if(isEligible(str)) forceApply(str) else str

	def forceApply(str: String): String

	protected def isEligible(str: String): Boolean
}

trait Umlaut extends WordTransformation
{
	override def forceApply(str: String) = str.map(c => umlautTransformation.getOrElse(c, c))

	override protected def isEligible(str: String): Boolean = true

	val umlautTransformation: Map[Char, Char]
}

object U_Umlaut extends Umlaut
{
	override val umlautTransformation = Map('a' -> 'ö' /* u */ )

	override def toString = "U umlaut"
}

object I_Umlaut extends Umlaut
{
	override val umlautTransformation = Map(
		'a' -> 'e',
		'á' -> 'æ',
		'u' -> 'y',
		'ú' -> 'ý'
		//"au" -> "ey"
	)

	override def toString = "I umlaut"
}
