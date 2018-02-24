package com.hyenawarrior.auxiliary.enum

/**
  * Created by HyenaWarrior on 2018.02.17..
  */
case class Id(i: Int) extends AnyVal

abstract class EnumConst[E <: EnumConst[E]](implicit enumLike: EnumLike[E]) extends Serializable {

  enumLike.CONSTANTS = enumLike.CONSTANTS :+ this.asInstanceOf[E]

  def id(): Int = enumLike.INDICES(this.asInstanceOf[E])
}

trait EnumLike[E <: EnumConst[E]] {

  private[enum] lazy val INDICES = CONSTANTS.zipWithIndex.toMap
  private[enum] var CONSTANTS = IndexedSeq[E]()

  def values = CONSTANTS

  def idOf(enumConst: E): Int = enumConst.id()

  def findById(id: Int): Option[E] = if(id < CONSTANTS.size) Some(CONSTANTS(id)) else None

  def findBy[K](key: K)(implicit lookup: EnumLookup[E, K]): Option[E] = lookup find key

}

abstract class EnumLookup[E <: EnumConst[E], K](implicit enumLike: EnumLike[E]) {

  private val MAP = enumLike.CONSTANTS.map(e => keyOf(e) -> e ).toMap

  final def find(key: K): Option[E] = MAP.get(key)

  def keyOf(enumConst: E): K
}