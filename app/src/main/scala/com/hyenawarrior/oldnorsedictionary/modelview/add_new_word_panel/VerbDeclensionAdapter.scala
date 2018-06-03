package com.hyenawarrior.oldnorsedictionary.modelview.add_new_word_panel

import android.app.Activity
import android.view.{View, ViewGroup}
import android.widget.Button
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbModeEnum.INDICATIVE
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbTenseEnum.{unapply => _}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbVoice.{ACTIVE, MEDIO_PASSIVE}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.{FinitiveMood, VerbClassEnum}
import com.hyenawarrior.oldnorsedictionary.R
import com.hyenawarrior.oldnorsedictionary.modelview._

/**
	* Created by HyenaWarrior on 2017.04.22..
	*/
class VerbDeclensionAdapter(activity: Activity, listView: ViewGroup)
  extends CustomAdapter[(VerbClassEnum, Verb)](activity, listView, R.layout.verb_declension)
{
  private var currentMood = INDICATIVE
  private var currentVoice = ACTIVE

  def setFinitiveMood(mood: FinitiveMood) = {

    currentMood = mood

    invalidateValues()
  }

  def showMediopassives(show: Boolean) = {

    currentVoice = if(show) MEDIO_PASSIVE else ACTIVE

    invalidateValues()
  }

  def resetView(i: Int, item: (VerbClassEnum, Verb), view: View): Unit = {

    val (vcDesc, verb) = item

		// set declensions
		setDeclensionsTo(verb, view, currentMood, currentVoice)

    // tag the select button
    val tv_addword_verb_Select = view.findViewById[Button](R.id.tv_addword_verb_Select)
    tv_addword_verb_Select.setTag(vcDesc)
	}

  def getSelectorTagOf(view: View): Option[VerbClassEnum] = view match
  {
    case btn: Button => Option(btn.getTag.asInstanceOf[VerbClassEnum])
    case _ => None
  }
}
