package com.hyenawarrior.OldNorseGrammar.grammar.morphophonology

/**
	* Created by HyenaWarrior on 2017.04.19..
	*/
case class AblautGrade(rootVowel: String)
{
	def occuresIn(str: String): Boolean = str.contains(rootVowel)

	override def toString: String = rootVowel
}

object AblautGrade
{
	implicit def AblautGradeFromStrin(str: String): AblautGrade = AblautGrade(str)
}

object AblautTransformation
{
	//def isEligibleForTransformation(str: String, ablaut: Ablaut): Boolean = str.contains(ablaut.rootVowel)

	def apply(str: String, srcAblaut: AblautGrade, dstAblaut: AblautGrade): Option[String] =
	{
		val where = str.indexOf(srcAblaut.rootVowel)

		if (where != -1)
		{

			val length = srcAblaut.rootVowel.length

			val prefixStr = str.substring(0, where)
			val suffixStr = str.substring(where + length)

			Some(s"$prefixStr${dstAblaut.rootVowel}$suffixStr")

			//val rootVowel = root.rootVowel
			//root.word.replace(rootVowel, ablautVowel)
		} else None
	}

/*	def forceApply(syllables: List[Syllable]): List[Syllable] = {

		syllables.headOption
			.map(firstSyll => firstSyll.stressed -> firstSyll.letters)
		  .map{case (isS, str) => Syllable(str, isS)}
			.map(newSyl => newSyl +: syllables.tail)
			.getOrElse(List())
	}
*/

}
