package com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.levels

import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.ShortCoded

/**
  * Created by HyenaWarrior on 2018.09.20..
  */
trait Level extends ShortCoded {

  override def toString: String = this.getClass.getSimpleName
}
