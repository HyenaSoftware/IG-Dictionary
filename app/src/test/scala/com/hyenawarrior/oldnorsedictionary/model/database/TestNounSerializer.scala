package com.hyenawarrior.oldnorsedictionary.model.database

import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.StrongStemClassMascA
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.{NounForm, _}
import com.hyenawarrior.OldNorseGrammar.grammar.enums.{Case, GNumber}
import com.hyenawarrior.oldnorsedictionary.model.persister.Reader
import org.junit.Assert._
import org.junit.Test

import scala.reflect.ClassTag

/**
  * Created by HyenaWarrior on 2018.02.17..
  */
class TestNounSerializer {

  case class MyReader(data: List[Any]) extends Reader {

    private val indexedData = data.toIndexedSeq
    private var i = -1

    override def apply[T]()(implicit clazz: ClassTag[T]): T = {

      i = i+1
      indexedData(i).asInstanceOf[T]
    }
  }

  @Test
  def testSerializeAndBack(): Unit = {

    val serializer = serializers.NounMarshaller

    val GIVEN_FORMS = Seq(NounForm("úlfr", GNumber.SINGULAR -> Case.NOMINATIVE, false))
      .map(nf => nf.declension -> nf.isDefinite -> nf).toMap

    val GEN_VERB_FORMS = Seq(
      NounForm("úlfar", GNumber.PLURAL -> Case.NOMINATIVE, false),
      NounForm("úlf",   GNumber.SINGULAR -> Case.ACCUSATIVE, false),
      NounForm("úlfa",  GNumber.PLURAL -> Case.ACCUSATIVE, false))
      .map(nf => nf.declension -> nf.isDefinite -> nf).toMap

    val OVR_VERB_FORMS = Seq(
      NounForm("úlfi",  GNumber.SINGULAR -> Case.DATIVE, false),
      NounForm("úlfum", GNumber.PLURAL -> Case.DATIVE, false))
      .map(nf => nf.declension -> nf.isDefinite -> nf).toMap

    val noun = Noun(NounStem("", StrongStemClassMascA), GIVEN_FORMS, GEN_VERB_FORMS, OVR_VERB_FORMS)
    val data = serializer.marshall(noun)

    val sameNoun = serializer.unmarshall(MyReader(data))

    assertEquals(noun.stem, sameNoun.stem)
    assertEquals(noun.givenForms, sameNoun.givenForms)
    assertEquals(noun.generatedForms, sameNoun.generatedForms)
    assertEquals(noun.overridenForms, sameNoun.overridenForms)
  }
}
