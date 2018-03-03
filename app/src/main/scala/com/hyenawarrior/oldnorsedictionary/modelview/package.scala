package com.hyenawarrior.oldnorsedictionary

import android.view.View
import android.widget.TextView
import com.hyenawarrior.OldNorseGrammar.grammar.{PoSForm, Pos}
import com.hyenawarrior.OldNorseGrammar.grammar.enums.Case.{unapply => _, _}
import com.hyenawarrior.OldNorseGrammar.grammar.enums.GNumber.{unapply => _, _}
import com.hyenawarrior.OldNorseGrammar.grammar.enums.Pronoun
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.AblautGrade
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.Noun
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.enum.NounStemClassEnum
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbModeEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbTenseEnum.{unapply => _, _}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbVoice.{ACTIVE, MEDIO_PASSIVE}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.{FinitiveMood, StrongVerbClassEnum, VerbVoice}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.enum.EnumVerbStem
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.enum.EnumVerbStem._

/**
  * Created by HyenaWarrior on 2017.12.03..
  */
package object modelview {

  private val NOUN_EDIT_TEXTS = Seq(
    R.id.tvNewWord_Nom_Sg -> (SINGULAR, NOMINATIVE),
    R.id.tvNewWord_Acc_Sg -> (SINGULAR, ACCUSATIVE),
    R.id.tvNewWord_Dat_Sg -> (SINGULAR, DATIVE),
    R.id.tvNewWord_Gen_Sg -> (SINGULAR, GENITIVE),

    R.id.tvNewWord_Nom_Pl -> (PLURAL, NOMINATIVE),
    R.id.tvNewWord_Acc_Pl -> (PLURAL, ACCUSATIVE),
    R.id.tvNewWord_Dat_Pl -> (PLURAL, DATIVE),
    R.id.tvNewWord_Gen_Pl -> (PLURAL, GENITIVE)
  )
  
  private val VERB_NON_FINITIVE_IDS = Seq(

    (INFINITIVE, None,          None) -> R.id.tv_addword_verb_Infinitive,
    (PARTICIPLE, Some(PRESENT), None) -> R.id.tv_addword_verb_PresPart,
    (PARTICIPLE, Some(PAST),    None) -> R.id.tv_addword_verb_PastPart
  )

  private val VERB_FINITIVE_IDS = Seq(

    (PRESENT, Pronoun.SG_1) -> R.id.tv_addword_verb_PresSg1,
    (PRESENT, Pronoun.SG_2) -> R.id.tv_addword_verb_PresSg2,
    (PRESENT, Pronoun.SG_3) -> R.id.tv_addword_verb_PresSg3,

    (PRESENT, Pronoun.PL_1) -> R.id.tv_addword_verb_PresPl1,
    (PRESENT, Pronoun.PL_2) -> R.id.tv_addword_verb_PresPl2,
    (PRESENT, Pronoun.PL_3) -> R.id.tv_addword_verb_PresPl3,

    (PAST, Pronoun.SG_1) -> R.id.tv_addword_verb_PastSg1,
    (PAST, Pronoun.SG_2) -> R.id.tv_addword_verb_PastSg2,
    (PAST, Pronoun.SG_3) -> R.id.tv_addword_verb_PastSg3,

    (PAST, Pronoun.PL_1) -> R.id.tv_addword_verb_PastPl1,
    (PAST, Pronoun.PL_2) -> R.id.tv_addword_verb_PastPl2,
    (PAST, Pronoun.PL_3) -> R.id.tv_addword_verb_PastPl3
  )

  def setDeclensionsTo(noun: Noun, targetView: View): Unit = {

    val descView   = targetView.findViewById(R.id.tv_noun_class).asInstanceOf[TextView]
    val indefView = targetView.findViewById(R.id.frame_noun_indef)
    val defView   = targetView.findViewById(R.id.frame_noun_def)

    descView.setText(noun.stem.stemClass.toString)

    setDeclensionsTo(noun, indefView, isDefinite = false)
    setDeclensionsTo(noun, defView, isDefinite = true)
  }

  def setDeclensionsTo(noun: Noun, nscEnum: NounStemClassEnum, targetView: View, isDefinite: Boolean): Unit = {

    val tvNounDeclDesc = targetView.findViewById(R.id.tvNounDeclDesc).asInstanceOf[TextView]
    tvNounDeclDesc.setText(nscEnum.name)

    setDeclensionsTo(noun, targetView, isDefinite)
  }

  def setDeclensionsTo(noun: Noun, targetView: View, isDefinite: Boolean): Unit = {

    for((id, nf) <- NOUN_EDIT_TEXTS) {

      val tvNC = targetView.findViewById(id).asInstanceOf[TextView]
      val ncTextForm = noun.nounForms.get(nf).map(_.strRepr).getOrElse("...")
      tvNC.setText(ncTextForm)
    }
  }

  def setDeclensionsTo[K, F <: PoSForm](pos: Pos[K, F], targetView: View): Unit = pos match {

    case noun: Noun       => setDeclensionsTo(noun, targetView)
    case verb: StrongVerb => setDeclensionsTo(verb, targetView)
  }

  /**
    * Show only nonfinitives + indicative or subjunctive conjugation
    * @param sv
    * @param targetView
    * @param mood
    * @param voice
    */
  def setDeclensionsTo(sv: StrongVerb, targetView: View, mood: FinitiveMood, voice: VerbVoice): Unit = sv match {
    case StrongVerb(cl, ablautGrade, _, _, _) =>

    setVerbConjugationDetailsTo(targetView, cl, ablautGrade)

    setInfinitiveConjugationsTo(sv, targetView, voice)
    setFinitiveConjugationTo(sv, targetView, mood, voice)
  }

  private def setVerbConjugationDetailsTo(targetView: View, cl: StrongVerbClassEnum, ablautGrade: Map[EnumVerbStem, AblautGrade]): Unit = {
    val stemName = targetView.findViewById(R.id.tv_addword_verb_stemName).asInstanceOf[TextView]
    stemName.setText(cl.name)

    val ablautDesc = Seq(PRESENT_STEM, PRETERITE_SINGULAR_STEM, PRETERITE_PLURAL_STEM, PERFECT_STEM)
      .map(ablautGrade)
      .mkString("(", " - ", ")")
      .replace("ǫ", "ö")

    // set ablaut grades
    val tv_addword_verb_AblautGrades = targetView.findViewById(R.id.tv_addword_verb_AblautGrades).asInstanceOf[TextView]
    tv_addword_verb_AblautGrades.setText(ablautDesc)
  }

  /**
    * Show only nonfinitives + indicative or subjunctive conjugation
    *
    * @param sv
    */
  def setDeclensionsTo(sv: StrongVerb, rootView: View): Unit = sv match {

    case StrongVerb(cl, ablautGrade, _, _, _) =>

      setVerbConjugationDetailsTo(rootView, cl, ablautGrade)

      val frame_active_non = rootView.findViewById(R.id.frame_active_nonfinitive)
      val frame_active_ind = rootView.findViewById(R.id.frame_active_indicatives)
      val frame_active_subj = rootView.findViewById(R.id.frame_active_subjunctives)

      val frame_medpass_non = rootView.findViewById(R.id.frame_mediopassive_nonfinitive)
      val frame_medpass_ind = rootView.findViewById(R.id.frame_mediopassive_indicative)
      val frame_medpass_subj = rootView.findViewById(R.id.frame_mediopassive_subjunctive)

      setInfinitiveConjugationsTo(sv, frame_active_non, ACTIVE)
      setFinitiveConjugationTo(sv, frame_active_ind, INDICATIVE, ACTIVE)
      setFinitiveConjugationTo(sv, frame_active_subj, SUBJUNCTIVE, ACTIVE)

      setInfinitiveConjugationsTo(sv, frame_medpass_non, MEDIO_PASSIVE)
      setFinitiveConjugationTo(sv, frame_medpass_ind, INDICATIVE, MEDIO_PASSIVE)
      setFinitiveConjugationTo(sv, frame_medpass_subj, SUBJUNCTIVE, MEDIO_PASSIVE)
  }

  def setInfinitiveConjugationsTo(sv: StrongVerb, targetView: View, voice: VerbVoice): Unit = {

    for ((vt, id) <- VERB_NON_FINITIVE_IDS) {

      val wt = vt match {

        case (m, oT, oP) => (m, voice, oT, oP)
      }

      val f = sv.verbForms(wt)
      val tv = targetView.findViewById(id).asInstanceOf[TextView]
      tv.setText(f.strRepr.replace("ǫ", "ö"))
    }
  }

  def setFinitiveConjugationTo(sv: StrongVerb, targetView: View, mood: FinitiveMood, voice: VerbVoice): Unit = {

    for ((vt, id) <- VERB_FINITIVE_IDS) {

      val wt = vt match {

        case (t, p) => (mood, voice, Some(t), Some(p))
      }

      val f = sv.verbForms(wt)
      val tv = targetView.findViewById(id).asInstanceOf[TextView]
      tv.setText(f.strRepr.replace("ǫ", "ö"))
    }
  }
}
