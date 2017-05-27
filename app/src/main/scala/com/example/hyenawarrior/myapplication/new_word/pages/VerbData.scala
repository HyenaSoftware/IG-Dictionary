package com.example.hyenawarrior.myapplication.new_word.pages

import com.example.hyenawarrior.myapplication.new_word.new_pos_helpers.AddNewVerbHelper.Declension
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbClassEnum

/**
  * Created by HyenaWarrior on 2017.04.30..
  */
case class VerbData(verbClass: VerbClassEnum, data: Map[Declension, String]) extends WordData
