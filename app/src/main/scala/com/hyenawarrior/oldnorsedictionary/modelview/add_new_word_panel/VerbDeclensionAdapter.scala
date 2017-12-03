package com.hyenawarrior.oldnorsedictionary.modelview.add_new_word_panel

import android.app.Activity
import android.view.{View, ViewGroup}
import android.widget.Button
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbTenseEnum.{unapply => _}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.{StrongVerbContext, VerbClassEnum}
import com.hyenawarrior.oldnorsedictionary.R
import com.hyenawarrior.oldnorsedictionary.modelview._

/**
	* Created by HyenaWarrior on 2017.04.22..
	*/
class VerbDeclensionAdapter(activity: Activity) extends CustomAdapter[(VerbClassEnum, StrongVerbContext)](activity)
{
	override protected def getNewView(i: Int, viewGroup: ViewGroup): View = {

		val view = inflater.inflate(R.layout.verb_declension, viewGroup, false)

		val (vcDesc, strongVerb) = itemAt(i)

		// set declensions
		setDeclensionsTo(strongVerb, view)

    // tag the select button
    val tv_addword_verb_Select = view.findViewById(R.id.tv_addword_verb_Select).asInstanceOf[Button]
    tv_addword_verb_Select.setTag(vcDesc)

		view
	}

  def getSelectorTagOf(view: View): Option[VerbClassEnum] = view match
  {
    case btn: Button => Option(btn.getTag.asInstanceOf[VerbClassEnum])
    case _ => None
  }
}
