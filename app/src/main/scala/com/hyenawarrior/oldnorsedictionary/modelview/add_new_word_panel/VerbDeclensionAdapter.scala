package com.hyenawarrior.oldnorsedictionary.modelview.add_new_word_panel

import android.app.Activity
import android.view.{View, ViewGroup}
import android.widget.{Button, TextView}
import com.hyenawarrior.OldNorseGrammar.grammar.Pronoun.{unapply => _}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbTenseEnum.{unapply => _}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.{StrongVerbClassDesc, VerbClassDesc, VerbClassEnum}
import com.hyenawarrior.oldnorsedictionary.R
import com.hyenawarrior.oldnorsedictionary.model.database.marshallers.VerbForm
import com.hyenawarrior.oldnorsedictionary.modelview.CustomAdapter

/**
	* Created by HyenaWarrior on 2017.04.22..
	*/
object VerbDeclensionAdapter
{
	val VERB_TEXTVIEWS = List(
		R.id.tv_addword_verb_PresSg1 -> VerbForm.VERB_INDICATIVE_PRESENT_1ST_SG,
		R.id.tv_addword_verb_PresSg2 -> VerbForm.VERB_INDICATIVE_PRESENT_2ND_SG,
		R.id.tv_addword_verb_PresSg3 -> VerbForm.VERB_INDICATIVE_PRESENT_3RD_SG,

		R.id.tv_addword_verb_PresPl1 -> VerbForm.VERB_INDICATIVE_PRESENT_1ST_PL,
		R.id.tv_addword_verb_PresPl2 -> VerbForm.VERB_INDICATIVE_PRESENT_2ND_PL,
		R.id.tv_addword_verb_PresPl3 -> VerbForm.VERB_INDICATIVE_PRESENT_3RD_PL,

		R.id.tv_addword_verb_PastSg1 -> VerbForm.VERB_INDICATIVE_PAST_1ST_SG,
		R.id.tv_addword_verb_PastSg2 -> VerbForm.VERB_INDICATIVE_PAST_2ND_SG,
		R.id.tv_addword_verb_PastSg3 -> VerbForm.VERB_INDICATIVE_PAST_3RD_SG,

		R.id.tv_addword_verb_PastPl1 -> VerbForm.VERB_INDICATIVE_PAST_1ST_PL,
		R.id.tv_addword_verb_PastPl2 -> VerbForm.VERB_INDICATIVE_PAST_2ND_PL,
		R.id.tv_addword_verb_PastPl3 -> VerbForm.VERB_INDICATIVE_PAST_3RD_PL,

		R.id.tv_addword_verb_Infinitive -> VerbForm.VERB_INFINITIVE,
		R.id.tv_addword_verb_PresPart		-> VerbForm.VERB_PRESENT_PARTICIPLE,
		R.id.tv_addword_verb_PastPart		-> VerbForm.VERB_PAST_PARTICIPLE
	)
}

class VerbDeclensionAdapter(activity: Activity) extends CustomAdapter[(VerbClassDesc, Map[VerbForm, String])](activity)
{
	override protected def getNewView(i: Int, viewGroup: ViewGroup): View = {

		val view = inflater.inflate(R.layout.verb_declension, viewGroup, false)

		val (vcDesc, map) = itemAt(i)

		// set declensions
		val tv_addword_verb_stemName = view.findViewById(R.id.tv_addword_verb_stemName).asInstanceOf[TextView]
		tv_addword_verb_stemName.setText(vcDesc.className)

		for ((tvCtrlId, key) <- VerbDeclensionAdapter.VERB_TEXTVIEWS)
		{
			val tvVerbCtrl = view.findViewById(tvCtrlId).asInstanceOf[TextView]
			val text = map.getOrElse(key, "...")
			tvVerbCtrl.setText(text)
		}

		val ablautDesc = vcDesc match
		{
			case a: StrongVerbClassDesc => a.ablaut.toString
			case _ => "n/a"
		}

		// set ablaut grades
		val tv_addword_verb_AblautGrades = view.findViewById(R.id.tv_addword_verb_AblautGrades).asInstanceOf[TextView]
		tv_addword_verb_AblautGrades.setText(ablautDesc)

    // tag the select button
    val tv_addword_verb_Select = view.findViewById(R.id.tv_addword_verb_Select).asInstanceOf[Button]
    tv_addword_verb_Select.setTag(vcDesc)

		view
	}

  def getSelectorTagOf(view: View): Option[VerbClassDesc] = view match
  {
    case btn: Button => Option(btn.getTag.asInstanceOf[VerbClassDesc])
    case _ => None
  }
}