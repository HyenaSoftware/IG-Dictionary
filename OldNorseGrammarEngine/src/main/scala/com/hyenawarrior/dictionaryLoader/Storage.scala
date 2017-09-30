package com.hyenawarrior.dictionaryLoader

import scala.io.BufferedSource

/**
	* Created by HyenaWarrior on 2017.03.10..
	*/

case class WordDefinition(word: String, wordDescFlags: Set[WordDescFlags])

case class MeaningDefinition(meaningId: Int, meaningDescFlags: Set[MeaningDescFlags], wordDefinition: List[WordDefinition])


abstract class Storage
{
	/* Checks if external storage is available for read and write */
	def isExternalStorageWritable: Boolean

	/* Checks if external storage is available to at least read */
	def isExternalStorageReadable: Boolean

	def availableLanguages: List[String]

	private def parseWordDef(wordDescStr: String): WordDefinition =
	{
		val Array(wordDescs, qword) = wordDescStr.split('=')
		val parts = wordDescs.split('.').toList

		val word = qword.stripPrefix("\"").stripSuffix("\"")

		//val flags = parts.flatMap(AbstractFlag.findByName[WordDescFlags]).toSet

		WordDefinition(word,  Set()) //flags)
	}

	private def extractDescFromRecord(fieldsOfRecord: Array[String]): (Set[MeaningDescFlags], List[WordDefinition]) =
	{
		val meaningDescStrs = fieldsOfRecord.head.split('.').toList

		//val mdSet = meaningDescStrs.flatMap(AbstractFlag.findByName[MeaningDescFlags]).toSet

		(/*mdSet*/ Set(), fieldsOfRecord.tail.map(parseWordDef).toList)
	}

	def meanings(language: String): List[MeaningDefinition] =
	{
		val src = getEntries(language)

		try
		{
			src.getLines.map(line =>
			{
				val Array(meaningIdStr, trunk) = line split ':'

				val meaningId = meaningIdStr.toInt

				val parts = trunk split  ']' map ( _ drop 1 )

				val (meaningDescSet, wordDescList) = extractDescFromRecord(parts)

				MeaningDefinition(meaningId, meaningDescSet, wordDescList)
			})
			.toList
		}
		finally
		{
			src.close
		}
	}

	protected def getEntries(language: String): BufferedSource
}
