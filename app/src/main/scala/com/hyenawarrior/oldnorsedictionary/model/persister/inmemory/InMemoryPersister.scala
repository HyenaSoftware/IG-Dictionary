package com.hyenawarrior.oldnorsedictionary.model.persister.inmemory

import com.hyenawarrior.oldnorsedictionary.model.persister.Persister

/**
  * Created by HyenaWarrior on 2017.11.16..
  */
class InMemoryPersister extends Persister {

  private val myserData = new InMemorySerData
  private val myStringInterner = new InMemoryStringInterner

  override def stringInterner = myStringInterner
  override def serData = myserData
}
