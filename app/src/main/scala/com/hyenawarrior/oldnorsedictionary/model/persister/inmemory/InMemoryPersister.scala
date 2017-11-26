package com.hyenawarrior.oldnorsedictionary.model.persister.inmemory

import com.hyenawarrior.oldnorsedictionary.model.persister.{Persister, Serializer}

/**
  * Created by HyenaWarrior on 2017.11.16..
  */
case class InMemoryPersister(implicit serializers: Map[Class[_], Serializer[Any]]) extends Persister(serializers) {

  override def stringInterner = InMemoryStringInterner
  override def serData = InMemorySerData
}
