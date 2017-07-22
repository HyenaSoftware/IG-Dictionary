package com.example.hyenawarrior.myapplication.new_word.pages

import com.example.hyenawarrior.dictionary.model.database.marshallers.{PosForm, PosType}

/**
  * Created by HyenaWarrior on 2017.04.30..
  */

case class WordData(posType: PosType, forms: Map[PosForm, String], meanings: List[MeaningDef])
