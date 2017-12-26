package com.hyenawarrior.oldnorsedictionary.model.database

import android.content.Context
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.StrongVerb
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbModeEnum.INFINITIVE
import com.hyenawarrior.oldnorsedictionary.model.DictionaryEntry
import com.hyenawarrior.oldnorsedictionary.model.SupportedLanguages.OldNorse
import com.hyenawarrior.oldnorsedictionary.model.database.IGPersister.lookupTable
import com.hyenawarrior.oldnorsedictionary.model.persister.Serializer
import com.hyenawarrior.oldnorsedictionary.model.persister.database.DBLayer.ColumnDefinition
import com.hyenawarrior.oldnorsedictionary.model.persister.database.{AndroidSDBLayer, DatabasePersister, Table}

/**
  * Created by HyenaWarrior on 2017.11.16..
  */
object IGPersister {

  val lookupTable = Table("Lookup", IndexedSeq(
    ColumnDefinition("TextId", Serializer.ClassOfInt),
    ColumnDefinition("LangId", Serializer.ClassOfInt),
    ColumnDefinition("ObjId", Serializer.ClassOfInt),
    ColumnDefinition("IsPrimary", Serializer.ClassOfBoolean)
  ))
}

class IGPersister(ctx: Context) {

  implicit val dbLayer = new AndroidSDBLayer(ctx)
  import serializers._

  private val persister = DatabasePersister(dbLayer)

  def lookup(text: String): Seq[DictionaryEntry] = {

    val records = DatabasePersister.Texts.select(List("Id"), Array(s"$text%"), "Text like ?", isDictinct = true)
    val textIds = records.map { case List(id: Int) => id }

    val whereClause = List.fill(textIds.size)("?").mkString("LangId = ? AND TextId IN (", ", ", ")")
    val args = OldNorse.id +: textIds.toList

    val objIds = lookupTable
      .select(List("ObjId", "IsPrimary"), args.toArray, whereClause, isDictinct = true)
      .map{ case List(o: Int, isPri: Boolean) => o }

    objIds.flatMap(load[DictionaryEntry](_))
  }

  def save(obj: DictionaryEntry): Unit = {

    val objId = persister.store(obj)

    addLookupTexts(obj, objId)
  }

  private def addLookupTexts(de: DictionaryEntry, objId: Int): Unit = de.word match {

    case sv: StrongVerb =>
      val strIds = sv.verbForms
        .map {
          case ((INFINITIVE, None, None), v) => v -> true
          case (_, v) => v -> false
        }
        .map { case(v, p) => persister.stringInterner.getOrStore(v.strForm) -> p }

      // create lookup entries
      for((strId, isPri) <- strIds) {

        lookupTable.insert(Array(strId, OldNorse.id, objId, isPri))
      }
  }

  def loadObject(objId: Int): Option[Any] = {

    val typeId: Int = persister.typeOf(objId).getOrElse(throw new RuntimeException(s"Type of object $objId is unknown."))

    val optSerializer = ALL_SERIALIZER.values.find(_.typeId == typeId)

    optSerializer match {

      case Some(ser) => Some(persister.load(objId)(ser))
      case None => None
    }
  }

  def load[T](objId: Int)(implicit serializer: Serializer[T]): Option[T] = Some(persister.load(objId)(serializer))
}
