package com.hyenawarrior.oldnorsedictionary.model.persister.inmemory

import com.hyenawarrior.oldnorsedictionary.model.persister.{Persister, Serializer}

/**
  * Created by HyenaWarrior on 2017.11.16..
  */
class InMemoryPersister(implicit serializers: Map[Class[_], Serializer[Any]]) extends Persister(serializers) {

  private val myserData = new InMemorySerData
  private val myStringInterner = new InMemoryStringInterner

  override def stringInterner = myStringInterner
  override def serData = myserData
}
