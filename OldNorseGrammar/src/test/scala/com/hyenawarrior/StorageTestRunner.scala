package com.hyenawarrior

import java.io.File

import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.U_Umlaut
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.Noun
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.NounStemClassEnum
import com.hyenawarrior.OldNorseGrammar.grammar.{Case, Database, Language, Number, Root, Word}
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


    io.Source.stdin.getLines
        .takeWhile(_ != "exit")
        .foreach {
          str => try process(str) catch
          {
            case e: Exception => println(e.toString)
          }
          print("> ")
        }


		println("exiting...")
	}

	def process(line: String)(implicit db: Database): Unit =
  {
    line match
    {
      case cmd if cmd.startsWith("find") =>
        for (str <- cmd.split(' ').tail)
        {
          db.findBy(str).foreach(w => println(s" * $w"))
        }

      case cmd if cmd.startsWith("sy") =>
        val cmds = cmd.split(' ')
        val word = cmds(1)
        val r = Root(word)
        val n = new Noun(word, -1, (Number.SINGULAR, Case.NOMINATIVE), Some(r))
        val w = Word(n, List(U_Umlaut))
        println(s" * $w")

      case cmd if cmd.startsWith("gen") =>
      {
        val cmds = cmd.split(' ')
        if (cmds.length < 3) throw new IllegalStateException("too few argument")

        val strCs = cmds(1)
        val strNum = cmds(2)
        val word = cmds(3)

        val cs = strCs match {
          case "nom" => Case.NOMINATIVE
          case "acc" => Case.ACCUSATIVE
          case "dat" => Case.DATIVE
          case "gen" => Case.GENITIVE
          case _ => throw new IllegalStateException("Case missing")
        }

        val num = strNum match {
          case "sg" => Number.SINGULAR
          case "pl" => Number.PLURAL
          case _ => throw new IllegalStateException("Number missing")
        }

        NounStemClassEnum.values
          .filter(_.nounStemClass != null)
          .map(ncl => ncl.name -> ncl.nounStemClass(Root(word), -1, (num, cs)))
          .foreach({ case (ncl, w) => println(s" * $w ($ncl)") })
      }
      case _ => ()
    }
	}

	def generate: Unit =
	{

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
