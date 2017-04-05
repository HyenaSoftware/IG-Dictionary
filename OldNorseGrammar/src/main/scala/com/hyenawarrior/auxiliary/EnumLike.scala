package com.hyenawarrior.auxiliary

/**
	* Created by HyenaWarrior on 2017.03.21..
	*/
class EnumLike[E] {

	private var entries: Map[String, E] = Map()

	def add(e: (String, E)) = entries += e
	def findByName[T <: E](name: String): Option[T] = entries.get(name).asInstanceOf[Option[T]]
	def values = entries.values.toList
}
