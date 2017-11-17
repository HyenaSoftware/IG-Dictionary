package com.hyenawarrior.oldnorsedictionary.model.persister.inmemory

import com.hyenawarrior.oldnorsedictionary.model.persister.Persister

/**
  * Created by HyenaWarrior on 2017.11.16..
  */
object InMemoryPersister extends Persister {

  override def stringInterner = InMemoryStringInterner
  override def serData = InMemorySerData
}
