package com.hyenawarrior.oldnorsedictionary.modelview.add_new_word_panel

import android.app.Activity
import android.view.{View, ViewGroup}
import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.Adjective
import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.enums.AdjectiveType
import com.hyenawarrior.oldnorsedictionary.R
import com.hyenawarrior.oldnorsedictionary.modelview._

/**
  * Created by HyenaWarrior on 2018.10.15..
  */
class AdjectiveDeclensionAdapter(activity: Activity, listView: ViewGroup)
  extends CustomAdapter[(AdjectiveType, Adjective)](activity, listView, R.layout.adjective_declension) {

  override protected def resetView(i: Int, adjAndType: (AdjectiveType, Adjective), view: View): Unit = {

    val (adjectiveType, adjective) = adjAndType

    setDeclensionsTo(adjective, view, adjectiveType)
  }
}