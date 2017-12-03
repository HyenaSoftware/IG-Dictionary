package com.hyenawarrior.oldnorsedictionary.model

import com.hyenawarrior.oldnorsedictionary.model.DictionaryListItem.DescedString
import com.hyenawarrior.oldnorsedictionary.new_word.pages.MeaningDef

/**
  * Created by HyenaWarrior on 2017.11.18..
  */
object DictionaryListItem {

  type DescedString = (String, String)
}

case class DictionaryListItem(otherForms: Seq[DescedString], posType: String, posObj: Any, meanings: List[MeaningDef])
