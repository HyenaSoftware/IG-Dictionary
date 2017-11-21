package com.hyenawarrior.auxiliary

/**
	* Created by HyenaWarrior on 2017.03.21..
	*/
abstract class EnumLike[K, V]
{
	private var entries: Map[K, V] = Map()
	private var ids: Map[V, Int] = Map()

	def add(e: (K, V)) = {

    entries += e
    ids += e._2 -> ids.size
  }

  def idOf(e: V): Int = ids(e)
  def fromId[W >: V](id: Int): Option[W] = ids.collectFirst { case (e, i) if i == id => e }

	def findByName[W >: V](name: K): Option[W] = entries.get(name).asInstanceOf[Option[W]]
	def values = entries.values.toList
}
