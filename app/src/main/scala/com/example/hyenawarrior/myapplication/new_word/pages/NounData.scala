package com.example.hyenawarrior.myapplication.new_word.pages

import com.example.hyenawarrior.myapplication.new_word.new_pos_helpers.AddNewNounHelper.Declension
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.NounStemClassEnum

/**
  * Created by HyenaWarrior on 2017.04.30..
  */
case class NounData(nscEnum: NounStemClassEnum, data: Map[Declension, String]) extends WordData
