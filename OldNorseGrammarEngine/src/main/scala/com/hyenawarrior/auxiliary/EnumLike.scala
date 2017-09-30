package com.hyenawarrior.auxiliary

/**
	* Created by HyenaWarrior on 2017.03.21..
	*/
class EnumLike[K, V]
{
	private var entries: Map[K, V] = Map()

	def add(e: (K, V)) = entries += e
	def findByName[W >: V](name: K): Option[W] = entries.get(name).asInstanceOf[Option[W]]
	def values = entries.values.toList
}
