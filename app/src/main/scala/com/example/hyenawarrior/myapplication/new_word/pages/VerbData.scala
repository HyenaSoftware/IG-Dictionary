package com.example.hyenawarrior.myapplication.new_word.pages

import com.example.hyenawarrior.dictionary.model.database.marshallers.VerbForm
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.{VerbClassEnum, VerbModeEnum}

/**
  * Created by HyenaWarrior on 2017.04.30..
  */
case class VerbData(verbClass: VerbClassEnum, forms: Map[VerbForm, String]) extends WordData
