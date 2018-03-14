package com.hyenawarrior.auxiliary

/**
  * Created by HyenaWarrior on 2018.01.14..
  */
object TupleEx {

  implicit class Operators2[A, B](val t2: (A, B)) extends AnyVal {

    def :+[C](c: C): (A, B, C) = (t2._1, t2._2, c)
    def +:[C](c: C): (C, A, B) = (c, t2._1, t2._2)
  }

  implicit class Operators3[A, B, C](val t: (A, B, C)) extends AnyVal {

    def :+[D](d: D): (A, B, C, D) = (t._1, t._2, t._3, d)
    def +:[D](d: D): (D, A, B, C) = (d, t._1, t._2, t._3)
  }

  implicit class Operators4[A, B, C, D](val t: (A, B, C, D)) extends AnyVal {

    def :+[E](d: E): (A, B, C, D, E) = (t._1, t._2, t._3, t._4, d)
    def +:[E](d: E): (E, A, B, C, D) = (d, t._1, t._2, t._3, t._4)
  }
}