package com.example.hyenawarrior.myapplication.new_word.pages

import com.example.hyenawarrior.myapplication.new_word.new_pos_helpers.AddNewNounHelper.Declension

/**
  * Created by HyenaWarrior on 2017.04.30..
  */
case class NounData(data: Map[Declension, String]) extends WordData
