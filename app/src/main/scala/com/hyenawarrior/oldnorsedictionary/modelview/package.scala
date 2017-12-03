package com.hyenawarrior.oldnorsedictionary

import android.view.View
import android.widget.TextView
import com.hyenawarrior.OldNorseGrammar.grammar.Pronoun
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.StrongVerbContext
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbModeEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbTenseEnum.{unapply => _, _}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.EnumVerbStem._

/**
  * Created by HyenaWarrior on 2017.12.03..
  */
package object modelview {

  private val VERB_TEXTVIEWS = Seq(

    (INFINITIVE, None, None) -> R.id.tv_addword_verb_Infinitive,
    (INDICATIVE, Some(PRESENT), Some(Pronoun.SG_1)) -> R.id.tv_addword_verb_PresSg1,
    (INDICATIVE, Some(PRESENT), Some(Pronoun.SG_2)) -> R.id.tv_addword_verb_PresSg2,
    (INDICATIVE, Some(PRESENT), Some(Pronoun.SG_3)) -> R.id.tv_addword_verb_PresSg3,

    (INDICATIVE, Some(PRESENT), Some(Pronoun.PL_1)) -> R.id.tv_addword_verb_PresPl1,
    (INDICATIVE, Some(PRESENT), Some(Pronoun.PL_2)) -> R.id.tv_addword_verb_PresPl2,
    (INDICATIVE, Some(PRESENT), Some(Pronoun.PL_3)) -> R.id.tv_addword_verb_PresPl3,

    (INDICATIVE, Some(PAST), Some(Pronoun.SG_1)) -> R.id.tv_addword_verb_PastSg1,
    (INDICATIVE, Some(PAST), Some(Pronoun.SG_2)) -> R.id.tv_addword_verb_PastSg2,
    (INDICATIVE, Some(PAST), Some(Pronoun.SG_3)) -> R.id.tv_addword_verb_PastSg3,

    (INDICATIVE, Some(PAST), Some(Pronoun.PL_1)) -> R.id.tv_addword_verb_PastPl1,
    (INDICATIVE, Some(PAST), Some(Pronoun.PL_2)) -> R.id.tv_addword_verb_PastPl2,
    (INDICATIVE, Some(PAST), Some(Pronoun.PL_3)) -> R.id.tv_addword_verb_PastPl3,

    (PARTICIPLE, Some(PRESENT), None) -> R.id.tv_addword_verb_PresPart,
    (PARTICIPLE, Some(PAST), None) -> R.id.tv_addword_verb_PastPart
  )

  def setDeclensionsTo(sv: StrongVerbContext, targetView: View): Unit = sv match {
    case StrongVerbContext(cl, ablautGrade, verbForms) =>

      val stemName = targetView.findViewById(R.id.tv_addword_verb_stemName).asInstanceOf[TextView]
      stemName.setText(cl.name)

      val ablautDesc = Seq(PRESENT_STEM, PRETERITE_SINGULAR_STEM, PRETERITE_PLURAL_STEM, PERFECT_STEM)
        .map(ablautGrade)
        .mkString("(", " - ", ")")

      // set ablaut grades
      val tv_addword_verb_AblautGrades = targetView.findViewById(R.id.tv_addword_verb_AblautGrades).asInstanceOf[TextView]
      tv_addword_verb_AblautGrades.setText(ablautDesc)

      for((vt, id) <- VERB_TEXTVIEWS) {

        val f = verbForms(vt)
        val tv = targetView.findViewById(id).asInstanceOf[TextView]
        tv.setText(f.strForm)
      }
  }
}
