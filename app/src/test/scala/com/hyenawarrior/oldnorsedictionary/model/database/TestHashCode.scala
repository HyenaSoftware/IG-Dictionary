package com.hyenawarrior.oldnorsedictionary.model.database

import com.hyenawarrior.OldNorseGrammar.grammar.enums.{Case, GNumber}
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.StrongStemClassMascA
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.{Noun, NounForm, NounStem}
import com.hyenawarrior.oldnorsedictionary.model.DictionaryEntry
import com.hyenawarrior.oldnorsedictionary.model.database.serializers.DictionaryEntryHashCode
import com.hyenawarrior.oldnorsedictionary.new_word.pages.MeaningDef
import org.junit.Test

/**
  * Created by HyenaWarrior on 2018.06.06..
  */
class TestHashCode {

  @Test
  def testHashCodeComputation(): Unit = {

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

    val meanings = List(
      MeaningDef("either meaing", "some note", Seq("first example", "second example")),
      MeaningDef("another meaing", "another note", Seq("antoher first example", "another second example"))
    )

    val d = DictionaryEntry(noun, meanings)

    val hc = DictionaryEntryHashCode(d)

    //print(new RichInt(hc).toHexString)
    print(f"0x$hc%x")
  }
}
