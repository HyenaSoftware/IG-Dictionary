package com.hyenawarrior.oldnorsedictionary.modelview.add_new_word_panel

import android.app.Activity
import android.view.{View, ViewGroup}
import android.widget.Button
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbModeEnum.INDICATIVE
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbTenseEnum.{unapply => _}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbVoice.{ACTIVE, MEDIO_PASSIVE}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs._
import com.hyenawarrior.oldnorsedictionary.R
import com.hyenawarrior.oldnorsedictionary.modelview._

/**
	* Created by HyenaWarrior on 2017.04.22..
	*/
class VerbDeclensionAdapter(activity: Activity, listView: ViewGroup)
  extends CustomAdapter[(VerbClassEnum, StrongVerb)](activity, listView, R.layout.verb_declension)
{
  private var currentMood = INDICATIVE
  private var currentVoice = ACTIVE

  def setFinitiveMood(mood: FinitiveMood) = {

    currentMood = mood

    for(i <- 0 until getCount) {

      val v = getView(i, null, null)

      resetView(i, v)
    }
  }

  def showMediopassives(show: Boolean) = {

    currentVoice = if(show) MEDIO_PASSIVE else ACTIVE

    for(i <- 0 until getCount) {

      val v = getView(i, null, null)

      resetView(i, v)
    }
  }

  def resetView(i: Int, view: View): Unit = {

    val (vcDesc, strongVerb) = itemAt(i)

		// set declensions
		setDeclensionsTo(strongVerb, view, currentMood, currentVoice)

    // tag the select button
    val tv_addword_verb_Select = view.findViewById(R.id.tv_addword_verb_Select).asInstanceOf[Button]
    tv_addword_verb_Select.setTag(vcDesc)
	}

  def getSelectorTagOf(view: View): Option[VerbClassEnum] = view match
  {
    case btn: Button => Option(btn.getTag.asInstanceOf[VerbClassEnum])
    case _ => None
  }
}
