package com.hyenawarrior

import org.junit.Test
import org.junit.Assert.{assertEquals, assertNotSame, assertSame}
import com.hyenawarrior.OldNorseGrammar.grammar.soundex.soundexCodeOf

/**
	* Created by HyenaWarrior on 2017.06.21..
	*/
class SoundexTest
{
	val transform: String => String = soundexCodeOf

	@Test
	def testDiversity(): Unit =
	{
		compareDiversiveSoundexCode(
			Array("ríða", "flúið", "verða", "úlfr")
		)
	}

	@Test
	def testStrongVerbClass1(): Unit =
	{
		compareSoundexCode(
			Array("ríða","ríðið", "ríðandi")
				++ Array("ríð",		"ríðr", 	"ríðr", "ríðum","ríðið","ríða")
				++ Array("reið",	"reiðst", "reið", "riðum","riðuð","riðu")
				++ Array("ríði",	"ríðir", 	"ríði", "ríðum","ríðið","ríði")
				++ Array("riði",	"riðir", 	"riði", "riðum","riðuð","riðu")
		)
	}

	@Test
	def testStrongVerbClass2(): Unit =
	{
		compareSoundexCode(
			Array("flúið","flýjandi")
			++ Array("flý",		"flýrð",	"flýr", 	"flýjum",	"flýið",	"flýja")
			++ Array("flúð",	"flúðir",	"flúði",	"flúðum",	"flúðuð",	"flúðu")
			++ Array("flýi",	"flýir",	"flýi",		"flýjum",	"flýið",	"flýi")
			++ Array("flýði",	"flýðir",	"flýði",	"flýðum",	"flýðuð",	"flýðu")
		)
	}

	@Test
	def testStrongVerbClass3(): Unit =
	{
		compareSoundexCode(
			Array("verða",		"orðið", 	"verðandi")
			++ Array("verð",	"verðr",	"verðr",	"verðum", "verðið", "verða")
			++ Array("varð",	"varðst",	"varð",		"urðum",	"urðuð",	"urðu")
			++ Array("verði", "verðir",	"verði",	"verðum",	"verðið",	"verði")
			++ Array("yrði",	"yrðir",	"yrði",		"yrðum",	"yrðuð",	"yrðu")
		)
	}

	@Test
	def testNounStrongAMasc(): Unit =
	{
		compareSoundexCode(
			Array("úlfr",		"úlf",	"úlfi",	"úlfs")
			++ Array("úlfrinn",		"úlfinn",	"úlfinum",	"úlfsins")
			++ Array("úlfar",			"úlfa",		"úlfum",		"úlfa")
			++ Array("úlfarnir",	"úlfana",	"úlfunum",	"úlfanna")
		)
	}

	@Test
	def testNounStrongARasc(): Unit =
	{
		compareSoundexCode(
			Array("maðr",			"mann",		"manni",		"manns") ++
			Array("maðrinn",	"mannin",	"manninum",	"mannsins") ++
			Array("menn",			"menn",		"mönnum",		"manna") ++
			Array("menninir",	"mennina","mönnunum",	"mannanna")
		)
	}

	def compareSoundexCode(forms: Array[String]): Unit =
	{
		val allForms = forms.map(x => transform(x) -> x)
			.groupBy{ case(k, _) => k }
			.mapValues(_.map(_._2))
		  .map
			{
				case (k, v) => println(s"$k -> ${v.mkString("(", ", ", ")")}"); k
			}

		assertEquals(s"${allForms} must have only one element", 1, allForms.size)
	}

	def compareDiversiveSoundexCode(forms: Array[String]): Unit =
	{
		val allForms = forms.map(transform).toSet

		assertEquals(forms.toSet.size, allForms.size)
	}
}
