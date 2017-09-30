package com.hyenawarrior.oldnorsedictionary.model.database.marshallers

import android.content.ContentValues
import android.database.{Cursor, DatabaseUtils}
import com.hyenawarrior.oldnorsedictionary.model.database._

/**
  * Created by HyenaWarrior on 2017.05.26..
  */
trait TMarshaller[T <: dbrecord]
{
  type jint =  java.lang.Integer

	val TABLE_NAME: String

  def apply(data: T): ContentValues
	def unapply(record: ContentValues): Option[T]

	def unapply(cursor: Cursor): Option[T] =
	{
		val record = new ContentValues

		DatabaseUtils.cursorRowToContentValues(cursor, record)

		unapply(record)
	}
}

package object marshallers
{
	implicit object WordMarshaller extends TMarshaller[Word]
	{
		override def apply(word: Word): ContentValues =
		{
			val record = new ContentValues

			record.put("WordId", word.wordId: jint)
			record.put("MeaningId", word.meaningId: jint)
			record.put("PosId", word.pos.id: jint)

			record
		}

		def unapply(record: ContentValues): Option[Word] =
		{
			val wordId = record.getAsInteger("WordId")
			val meaningId = record.getAsInteger("MeaningId")
			val pos = PosType.findByName(record.getAsInteger("PosId"))

			Some(Word(wordId, meaningId, pos.head))
		}

		override val TABLE_NAME: String = "Word"
	}

	implicit object WordFormMarshaller extends TMarshaller[WordForm]
	{
		override val TABLE_NAME: String = "WordForms"

		override def apply(data: WordForm): ContentValues =
		{
			val record = new ContentValues

			record.put("Form",		data.form)
			record.put("WordId",	data.wordId: jint)
			record.put("FormId",	data.posForm.id: jint)
			record.put("PosId",		data.posType.id: jint)

			record
		}

		override def unapply(record: ContentValues): Option[WordForm] =
		{
			val form = record.getAsString("Form")
			val wordId = record.getAsInteger("WordId")
			val formId = record.getAsInteger("FormId")
			val posId = record.getAsInteger("PosId")

			val optPosType: Option[PosType] = PosType.findByName(posId)

			val posForm = optPosType.flatMap
			{
				case _: VerbType => VerbForm.findByName[PosForm](formId)
				case _: NounType => NounForm.findByName[PosForm](formId)
			}

			posForm.zip(optPosType).map
			{
				case (f, t) => WordForm(form, wordId, f, t)
			}
				.headOption
		}
	}

	implicit object ExampleMarshaller extends TMarshaller[Example]
	{
		override def apply(data: Example): ContentValues =
		{
			val record = new ContentValues

			record.put("ExampleGroupId", data.groupId: jint)
			record.put("Example", data.text)

			record
		}

		override def unapply(record: ContentValues): Option[Example] =
		{
			val groupId = record.getAsInteger("ExampleGroupId")
			val example = record.getAsString("Example")

			Some(Example(groupId, example))
		}

		override val TABLE_NAME: String = "Examples"
	}

	implicit object LanguageMarshaller extends TMarshaller[Language]
	{
		override def apply(data: Language): ContentValues =
		{
			val record = new ContentValues

			record.put("LangId", data.id: jint)
			record.put("LangName", data.name)

			record
		}

		override def unapply(record: ContentValues): Option[Language] =
		{
			val langId = record.getAsInteger("LangId")
			val langName = record.getAsString("LangName")

			Some(Language(langId, langName))
		}

		override val TABLE_NAME: String = "Langs"
	}

	implicit object MeaningMarshaller extends TMarshaller[Meaning]
	{
		override val TABLE_NAME: String = "Meaning"

		override def apply(data: Meaning): ContentValues =
		{
			val record = new ContentValues

			record.put("MeaningId",			data.meaningId: jint)
			record.put("LangId",				data.langId: jint)
			record.put("Meaning",				data.meaning)
			record.put("Note",					data.note)

			if(data.exampleId.isDefined)
				record.put("ExampleGroupId",	data.exampleId.get: jint)
			else
				record.putNull("ExampleGroupId")

			record
		}

		override def unapply(record: ContentValues): Option[Meaning] =
		{
			val meaningId = record.getAsInteger("MeaningId")
			val langId = record.getAsInteger("LangId")
			val meaning = record.getAsString("Meaning")
			val note = record.getAsString("Note")
			val exampleId = Option(record.getAsInteger("ExampleGroupId")).map(i => i: Int)

			Some(Meaning(meaningId, langId, meaning, note, exampleId))
		}
	}
}