package com.hyenawarrior.oldnorsedictionary.modelview.add_new_word_panel

import android.app.Activity
import android.view.{View, ViewGroup}
import android.widget.Button
import com.hyenawarrior.OldNorseGrammar.grammar.enums.Case.{unapply => _}
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.Noun
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.enum.NounStemClassEnum
import com.hyenawarrior.oldnorsedictionary.R
import com.hyenawarrior.oldnorsedictionary.modelview.{CustomAdapter, setDeclensionsTo}

/**
	* Created by HyenaWarrior on 2017.04.12..
	*/
class NounDeclensionAdapter(activity: Activity, listView: ViewGroup)
  extends CustomAdapter[(NounStemClassEnum, Noun)](activity, listView, R.layout.noun_declension)
{
	private var showDefiniteForms = false

	protected def resetView(i: Int, item: (NounStemClassEnum, Noun), view: View): Unit = {

		val (nscEnum, noun) = item

		setDeclensionsTo(noun, nscEnum, view, showDefiniteForms)

    // tag the select button
    val tv_addword_noun_Select = view.findViewById[Button](R.id.tv_addword_noun_Select)
    tv_addword_noun_Select.setTag(nscEnum)
	}

	def setDefinitness(isDefinite: Boolean): Unit = {

		showDefiniteForms = isDefinite

		invalidateValues()
	}

  def getSelectorTagOf(view: View): Option[NounStemClassEnum] = view match
  {
    case btn: Button => Option(btn.getTag.asInstanceOf[NounStemClassEnum])
    case _ => None
  }
}
