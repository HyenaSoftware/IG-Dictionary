package com.hyenawarrior.OldNorseGrammar.grammar

/**
	* Created by HyenaWarrior on 2017.06.21..
	*/
package object soundex
{
	val SUFFIX_PRESENT_PARTICIPLE = "(.+)andi".r

	val SUFFIX_ARTICLE_NOM_SG = "(.+)rinn".r
	val SUFFIX_ARTICLE_ACC_SG = "(.+)inn".r
	val SUFFIX_ARTICLE_DAT_SG = "(.+)inum".r
	val SUFFIX_ARTICLE_GEN_SG = "(.+)sins".r

	val SUFFIX_ARTICLE_NOM_PL = "(.+)arnir".r
	val SUFFIX_ARTICLE_ACC_PL = "(.+)ana".r
	val SUFFIX_ARTICLE_DAT_PL = "(.+)unum".r
	val SUFFIX_ARTICLE_GEN_PL = "(.+)anna".r

	val SUFFIX_LABIAL_UM = "(.+[á])m".r
	val SUFFIX_UM = "(.+)um".r

	val SUFFIX_AR = "(.+)ar".r
	val SUFFIX_IR = "(.+)ir".r
	val SUFFIX_IÐ = "(.*[^e])ið".r
	val SUFFIX_UÐ = "(.+)uð".r
	val SUFFIX_ST = "(.+)st".r


	val SUFFIX_NOUN_NOM_SG = "(.+)r".r
	val SUFFIX_NOUN_DAT_SG = "(.+)i".r
	val SUFFIX_NOUN_GEN_SG = "(.+)s".r

	val SUFFIX_NOUN_ACC_GEN_PL = "(.+)a".r
	val SUFFIX_U = "(.+)u".r

	def soundexCodeOf(str: String): String = transformConsonant(removeVowels(addStartingV(removeSuffix(str))))

	private def removeSuffix(str: String) =	str match
	{
		case SUFFIX_PRESENT_PARTICIPLE(word) => word

		// articles	+ inflections
		case SUFFIX_ARTICLE_NOM_SG(word) => word
		case SUFFIX_ARTICLE_ACC_SG(word) => word
		case SUFFIX_ARTICLE_DAT_SG(word) => word
		case SUFFIX_ARTICLE_GEN_SG(word) => word

		case SUFFIX_ARTICLE_NOM_PL(word) => word
		case SUFFIX_ARTICLE_ACC_PL(word) => word
		case SUFFIX_ARTICLE_DAT_PL(word) => word
		case SUFFIX_ARTICLE_GEN_PL(word) => word

		// inflections
		case SUFFIX_LABIAL_UM(word) => word
		case SUFFIX_UM(word) => word

		case SUFFIX_AR(word) => word // it has higher priority than SUFFIX_NOUN_NOM_SG
		case SUFFIX_IR(word) => word // it has higher priority than SUFFIX_NOUN_NOM_SG
		case SUFFIX_IÐ(word) => word
		case SUFFIX_UÐ(word) => word
		case SUFFIX_ST(word) => word
		case SUFFIX_NOUN_NOM_SG(word) => word
		case SUFFIX_NOUN_DAT_SG(word) => word
		case SUFFIX_NOUN_GEN_SG(word) => word

		case SUFFIX_NOUN_ACC_GEN_PL(word) => word
		case SUFFIX_U(word) => word
		case _ => str
	}

	private val STARTING_V = "([oóuúyý].+)".r

	private def addStartingV(str: String) = str match
	{
		case STARTING_V(word) => s"v$word"
		case _ => str
	}

	private val VOWELS = "aæáeéiíjoœøóöuúyý" // counts half-vowels too

	private def removeVowels(str: String): String =
	{
		val idxOfFirst = str.indexWhere(VOWELS.contains(_))

		str.zipWithIndex.filterNot
		{
			case (c, i) => i >= idxOfFirst && VOWELS.contains(c)
		}
			.map(_._1).mkString
	}

	private val TRANSFORM_NN = "(.*?)n{2,}(.*)".r

	private def transformConsonant(str: String): String = str match
	{
		case TRANSFORM_NN(pre,post) => s"${pre}ð$post"
		case _ => str
	}
}
