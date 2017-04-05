package com.hyenawarrior

import java.io.File

import com.hyenawarrior.OldNorseGrammar.grammar.{Database, Language}
import com.hyenawarrior.dictionaryLoader.Storage

import scala.io.Source

/**
	* Created by HyenaWarrior on 2017.03.21..
	*/
object StorageTestRunner
{
	def main(args: Array[String]): Unit =
	{
		object Storage extends Storage with HDDStorage

		val words = Storage.meanings("Old Norse")

		words foreach println

		implicit val db = new Database(Map(Language("Old Norse") -> words))

		print("> ")
		io.Source.stdin.getLines takeWhile(_ != "exit") foreach process

		println("exiting...")
	}

	def process(lookupStr: String)(implicit db: Database): Unit =
	{
		db.findBy(lookupStr).foreach(w => println(s" * $w"))
		print("> ")
	}
}

trait HDDStorage
{
	/* Checks if external storage is available for read and write */
	def isExternalStorageWritable = false

	/* Checks if external storage is available to at least read */
	def isExternalStorageReadable = true

	def availableLanguages = List("Old Norse")

	protected def getEntries(language: String) =
	{
		val path = new File(".").getAbsolutePath

		Source.fromFile("res/data.txt")
	}
}
