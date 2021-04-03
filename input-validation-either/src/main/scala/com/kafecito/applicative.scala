package com.kafecito

import model.EItherWithErrorList


object applicative {

  // This is the building block for all larger zips. In here we intelligently decide how to chain errors
  def zip[A, B](a: EItherWithErrorList[A], b: EItherWithErrorList[B]): EItherWithErrorList[(A, B)] = {
    (a, b) match {
      case (Left(errorsA), Left(errorsB)) => Left(errorsA ++ errorsB)
      case (Right(goodA), Left(errorsB)) => Left(errorsB)
      case (Left(errorsA), Right(goodB)) => Left(errorsA)
      case (Right(goodA), Right(goodB)) => Right((goodA, goodB))
    }
  }

  // map2
  def multiMap[A, B, Y](a: EItherWithErrorList[A], b: EItherWithErrorList[B])(to_y: (A, B) => Y): EItherWithErrorList[Y] =
  // tupled method allows the function to take a tuple instead of a list of arguments
    zip(a, b).map(to_y.tupled)


  // WIth the building blocks above we can write bigger maps
  // zip3
  def zip[A, B, C](a: EItherWithErrorList[A], b: EItherWithErrorList[B], c: EItherWithErrorList[C]): EItherWithErrorList[(A, B, C)] =
    zip(zip(a, b), c).map(t => (t._1._1, t._1._2, t._2))


  // zip4
  def zip[A, B, C, D](a: EItherWithErrorList[A], b: EItherWithErrorList[B], c: EItherWithErrorList[C], d: EItherWithErrorList[D]):
  EItherWithErrorList[(A, B, C, D)] =
    zip(zip(a, b), zip(c, d)).map(t => (t._1._1, t._1._2, t._2._1, t._2._2))

  // zip5
  def zip[A, B, C, D, E](a: EItherWithErrorList[A], b: EItherWithErrorList[B], c: EItherWithErrorList[C], d: EItherWithErrorList[D],
                         e: EItherWithErrorList[E]):
  EItherWithErrorList[(A, B, C, D, E)] =
    zip(zip(a, b), zip(c, d, e)).map(t => (t._1._1, t._1._2, t._2._1, t._2._2, t._2._3))


  // zip6
  def zip[A, B, C, D, E, F](a: EItherWithErrorList[A], b: EItherWithErrorList[B], c: EItherWithErrorList[C], d: EItherWithErrorList[D],
                            e: EItherWithErrorList[E], f: EItherWithErrorList[F]):
  EItherWithErrorList[(A, B, C, D, E, F)] =
    zip(zip(a, b, c), zip(d, e, f)).map(t => (t._1._1, t._1._2, t._1._3, t._2._1, t._2._2, t._2._3))


  // zip7
  def zip[A, B, C, D, E, F, G](a: EItherWithErrorList[A], b: EItherWithErrorList[B], c: EItherWithErrorList[C], d: EItherWithErrorList[D],
                               e: EItherWithErrorList[E], f: EItherWithErrorList[F], g: EItherWithErrorList[G]):
  EItherWithErrorList[(A, B, C, D, E, F, G)] =
    zip(zip(a, b, c), zip(d, e, f, g)).map(t => (t._1._1, t._1._2, t._1._3, t._2._1, t._2._2, t._2._3, t._2._4))

  // zip8
  def zip[A, B, C, D, E, F, G, H](a: EItherWithErrorList[A], b: EItherWithErrorList[B], c: EItherWithErrorList[C], d: EItherWithErrorList[D],
                                  e: EItherWithErrorList[E], f: EItherWithErrorList[F], g: EItherWithErrorList[G],
                                  h: EItherWithErrorList[H]
                                 ):
  EItherWithErrorList[(A, B, C, D, E, F, G, H)] =
    zip(zip(a, b, c, d), zip(e, f, g, h)).map(t => (t._1._1, t._1._2, t._1._3, t._1._4, t._2._1, t._2._2, t._2._3, t._2._4))


  // zip9
  def zip[A, B, C, D, E, F, G, H, I](a: EItherWithErrorList[A], b: EItherWithErrorList[B], c: EItherWithErrorList[C], d: EItherWithErrorList[D],
                                     e: EItherWithErrorList[E], f: EItherWithErrorList[F], g: EItherWithErrorList[G],
                                     h: EItherWithErrorList[H], i: EItherWithErrorList[I]
                                    ):
  EItherWithErrorList[(A, B, C, D, E, F, G, H, I)] =
    zip(zip(a, b, c, d), zip(e, f, g, h, i)).map(t => (t._1._1, t._1._2, t._1._3, t._1._4, t._2._1, t._2._2, t._2._3, t._2._4, t._2._5))

  // zip10
  def zip[A, B, C, D, E, F, G, H, I, J](a: EItherWithErrorList[A], b: EItherWithErrorList[B], c: EItherWithErrorList[C], d: EItherWithErrorList[D],
                                        e: EItherWithErrorList[E], f: EItherWithErrorList[F], g: EItherWithErrorList[G],
                                        h: EItherWithErrorList[H], i: EItherWithErrorList[I], j: EItherWithErrorList[J]
                                       ):
  EItherWithErrorList[(A, B, C, D, E, F, G, H, I, J)] =
    zip(zip(a, b, c, d, e), zip(f, g, h, i, j)).map(t => (t._1._1, t._1._2, t._1._3, t._1._4, t._1._5, t._2._1, t._2._2, t._2._3, t._2._4, t._2._5))


  // zip11
  def zip[A, B, C, D, E, F, G, H, I, J, K](a: EItherWithErrorList[A], b: EItherWithErrorList[B], c: EItherWithErrorList[C], d: EItherWithErrorList[D],
                                           e: EItherWithErrorList[E], f: EItherWithErrorList[F], g: EItherWithErrorList[G],
                                           h: EItherWithErrorList[H], i: EItherWithErrorList[I], j: EItherWithErrorList[J],
                                           k: EItherWithErrorList[K]
                                          ):
  EItherWithErrorList[(A, B, C, D, E, F, G, H, I, J, K)] =
    zip(zip(a, b, c, d, e), zip(f, g, h, i, j, k)).map(t => (t._1._1, t._1._2, t._1._3, t._1._4, t._1._5, t._2._1, t._2._2, t._2._3, t._2._4, t._2._5, t._2._6))


  // zip12
  def zip[A, B, C, D, E, F, G, H, I, J, K, L](a: EItherWithErrorList[A], b: EItherWithErrorList[B], c: EItherWithErrorList[C], d: EItherWithErrorList[D],
                                              e: EItherWithErrorList[E], f: EItherWithErrorList[F], g: EItherWithErrorList[G],
                                              h: EItherWithErrorList[H], i: EItherWithErrorList[I], j: EItherWithErrorList[J],
                                              k: EItherWithErrorList[K], l: EItherWithErrorList[L]
                                             ):
  EItherWithErrorList[(A, B, C, D, E, F, G, H, I, J, K, L)] =
    zip(zip(a, b, c, d, e, f), zip(g, h, i, j, k, l)).map(t => (t._1._1, t._1._2, t._1._3, t._1._4, t._1._5, t._1._6, t._2._1, t._2._2, t._2._3, t._2._4, t._2._5, t._2._6))


  def multiMap[A, B, C, Y](a: EItherWithErrorList[A], b: EItherWithErrorList[B], c: EItherWithErrorList[C])(to_y: (A, B, C) => Y): EItherWithErrorList[Y] =
    zip(a, b, c).map(to_y.tupled)

  def multiMap[A, B, C, D, Y](a: EItherWithErrorList[A], b: EItherWithErrorList[B], c: EItherWithErrorList[C],
                         d: EItherWithErrorList[D]
                        )
                        (to_y: (A, B, C, D) => Y):
  EItherWithErrorList[Y] =
    zip(a, b, c, d).map(to_y.tupled)

  def multiMap[A, B, C, D, E, Y](a: EItherWithErrorList[A], b: EItherWithErrorList[B], c: EItherWithErrorList[C],
                            d: EItherWithErrorList[D], e: EItherWithErrorList[E]
                           )
                           (to_y: (A, B, C, D, E) => Y):
  EItherWithErrorList[Y] =
    zip(a, b, c, d, e).map(to_y.tupled)


  def multiMap[A, B, C, D, E, F, Y](a: EItherWithErrorList[A], b: EItherWithErrorList[B], c: EItherWithErrorList[C],
                               d: EItherWithErrorList[D], e: EItherWithErrorList[E], f: EItherWithErrorList[F]
                              )
                              (to_y: (A, B, C, D, E, F) => Y):
  EItherWithErrorList[Y] =
    zip(a, b, c, d, e, f).map(to_y.tupled)


  def multiMap[A, B, C, D, E, F, G, Y](a: EItherWithErrorList[A], b: EItherWithErrorList[B], c: EItherWithErrorList[C],
                                  d: EItherWithErrorList[D], e: EItherWithErrorList[E], f: EItherWithErrorList[F],
                                  g: EItherWithErrorList[G]
                                 )
                                 (to_y: (A, B, C, D, E, F, G) => Y):
  EItherWithErrorList[Y] =
    zip(a, b, c, d, e, f, g).map(to_y.tupled)


  def multiMap[A, B, C, D, E, F, G, H, Y](a: EItherWithErrorList[A], b: EItherWithErrorList[B], c: EItherWithErrorList[C],
                                     d: EItherWithErrorList[D], e: EItherWithErrorList[E], f: EItherWithErrorList[F],
                                     g: EItherWithErrorList[G], h: EItherWithErrorList[H]
                                    )
                                    (to_y: (A, B, C, D, E, F, G, H) => Y):
  EItherWithErrorList[Y] =
    zip(a, b, c, d, e, f, g, h).map(to_y.tupled)


  def multiMap[A, B, C, D, E, F, G, H, I, Y](a: EItherWithErrorList[A], b: EItherWithErrorList[B], c: EItherWithErrorList[C],
                                        d: EItherWithErrorList[D], e: EItherWithErrorList[E], f: EItherWithErrorList[F],
                                        g: EItherWithErrorList[G], h: EItherWithErrorList[H], i: EItherWithErrorList[I]
                                       )
                                       (to_y: (A, B, C, D, E, F, G, H, I) => Y):
  EItherWithErrorList[Y] =
    zip(a, b, c, d, e, f, g, h, i).map(to_y.tupled)


  def multiMap[A, B, C, D, E, F, G, H, I, J, Y](a: EItherWithErrorList[A], b: EItherWithErrorList[B], c: EItherWithErrorList[C],
                                           d: EItherWithErrorList[D], e: EItherWithErrorList[E], f: EItherWithErrorList[F],
                                           g: EItherWithErrorList[G], h: EItherWithErrorList[H], i: EItherWithErrorList[I],
                                           j: EItherWithErrorList[J]
                                          )
                                          (to_y: (A, B, C, D, E, F, G, H, I, J) => Y):
  EItherWithErrorList[Y] =
    zip(a, b, c, d, e, f, g, h, i, j).map(to_y.tupled)


  def multiMap[A, B, C, D, E, F, G, H, I, J, K, Y](a: EItherWithErrorList[A], b: EItherWithErrorList[B], c: EItherWithErrorList[C],
                                           d: EItherWithErrorList[D], e: EItherWithErrorList[E], f: EItherWithErrorList[F],
                                           g: EItherWithErrorList[G], h: EItherWithErrorList[H], i: EItherWithErrorList[I],
                                           j: EItherWithErrorList[J], k: EItherWithErrorList[K]
                                          )
                                          (to_y: (A, B, C, D, E, F, G, H, I, J, K) => Y):
  EItherWithErrorList[Y] =
    zip(a, b, c, d, e, f, g, h, i, j, k).map(to_y.tupled)


  def multiMap[A, B, C, D, E, F, G, H, I, J, K, L, Y](a: EItherWithErrorList[A], b: EItherWithErrorList[B], c: EItherWithErrorList[C],
                                              d: EItherWithErrorList[D], e: EItherWithErrorList[E], f: EItherWithErrorList[F],
                                              g: EItherWithErrorList[G], h: EItherWithErrorList[H], i: EItherWithErrorList[I],
                                              j: EItherWithErrorList[J], k: EItherWithErrorList[K], l: EItherWithErrorList[L]
                                             )
                                             (to_y: (A, B, C, D, E, F, G, H, I, J, K, L) => Y):
  EItherWithErrorList[Y] =
    zip(a, b, c, d, e, f, g, h, i, j, k, l).map(to_y.tupled)

}
