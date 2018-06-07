package com.hyenawarrior.oldnorsedictionary.model.database

import android.content.Context
import com.hyenawarrior.OldNorseGrammar.grammar.{PoSForm, Pos}
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
      .select(List("ObjId"), args.toArray, whereClause, isDictinct = true)
      .map{ case List(o: Int) => o }

    objIds.flatMap(load[DictionaryEntry](_))
  }

  def save(obj: DictionaryEntry): Unit = {

    val objId = persister.store(obj)

    val posObj = obj.word.asInstanceOf[Pos[_, _ <: PoSForm]]
    addLookupTexts(posObj, objId)
  }

  def delete(obj: DictionaryEntry): Boolean = {

    val hashCode = DictionaryEntryHashCode(obj)

    // TODO make sure the deleted blob is the same as the argument

    val deletedRecords = lookupTable.delete(Array(hashCode), "ObjId = ?")

    persister.delete(obj)
  }

  def deleteAll(): Unit = persister.deleteAll()

  private def addLookupTexts[K, F <: PoSForm](pos: Pos[K, F], objId: Int): Unit = {

    val strIds = pos.forms
      .map {
        case (pos.PRIMARY_KEY, v) => v -> true
        case (_, v) => v -> false
      }
      .map { case (v, p) => persister.stringInterner.getOrStore(v.strRepr) -> p }

    // create lookup entries
    for ((strId, isPri) <- strIds) {

      lookupTable.insert(Array(strId, OldNorse.id, objId, isPri))
    }
  }

  def load[T](objId: Int)(implicit serializer: Serializer[T]): Option[T] = Some(persister.load(objId)(serializer))
}
