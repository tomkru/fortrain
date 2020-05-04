package com.tomkru.scalatrain

object Fortrain extends App {
  import ForComprehensionTut._

  require(for1 == maps1)
  require(for2 == maps2)
  require(for3 == maps3)
  require(for4 == maps4)
  require(for5 == maps5)
}

object MapTut {
  /*
   * Mam nieco typu A zabalene v kontexte ktory ma definovanu funkciu
   * map a funkciu ktora urobi z typu A typ B a chcem dostat typ B
   * zabaleny v danom kontexte. Na A a B nekladieme ziadne dalsie poziadavky(mozu byt rovnake).
   *
   * Vo FP sa tento kontext ktory ma definovanu funkciu map nazyva funktor.
   * Functor je jeden zo zakladnych typovych tried (type class) vo FP.
   *
   * Map As to Bs:
   *  ______                                                             ______
   * |      |                                                           |      |
   * |  As  |  Extract As       Apply As to Bs Function       Wrap Bs   |  Bs  |
   * |      | ----------->  As -------------------------> Bs ---------> |      |
   * |______|                                                           |______|
   *  Functor                                                            Functor
   */
  val noneOpt: Option[String] = None
  val leftEit: Either[String, Int] = Left("Error")
  val list1 = List(1,2,3,4,5).map(x => x*2) // List(2,4,6,8,10)
  val list2 = List(1,2,3,4,5).map(_*2) // List(2,4,6,8,10)
  val seq1 = Seq(1,2,3,4,5).map(x => x*2) // Seq(2,4,6,8,10)
  val some = Some(2).map(_*2) // Some(4)
  val none = noneOpt.map(_*2) // None
  val right = Right(2).map(_*2) // Right(4)
  val left = leftEit.map(_*2) // Left("Error")

  val map = Map(1->"a", 2->"b", 3->"c").map{case (key, value) => (key+1, value)} //Map(2->"a", 3->"b", 4->"c")
    //1->"a" je iba cukor pre konstruktor tuple (1,"a")
}

object FlatMapTut {
  /*
   * Mam nieco typu A zabalene v kontexte ktory ma definovanu funkciu map a flatMap a
   * funkciu ktora urobi z typu A typ B ALE zabaleny v tom istom kontexte. A chcem
   * dostat typ B zabaleny v danom kontexte. Na A a B nekladieme ziadne dalsie poziadavky(mozu byt aj rovnake).
   *
   * Vo FP sa tento kontext ktory ma definovanu funkciu map a flatMap nazyva monada.
   * Monada je jedna zo zakladnych typovych tried (type class) vo FP.
   *
   * FlatMap As to Bs:
   *  ______                                                                 ______              ______
   * |      |                                                               |  __  |            |      |
   * |  As  |  Extract As       Apply As to Bs Function    __     Wrap Bs   | |Bs| |  Flatten   |  Bs  |
   * |      | ----------->  As -------------------------> |Bs|   ---------> | |__| | ---------> |      |
   * |______|                                             |__|              |______|            |______|
   *  Monad                                               Monad               Monad               Monad
   */

  def duplicate[A](i: A): List[A] = List(i,i)

  val list1 = List(1,2,3,4,5).map(duplicate) // List(List(1,1), List(2,2), List(3,3), List(4,4), List(5,5))
  val list2 = List(1,2,3,4,5).flatMap(duplicate) // List(1,1,2,2,3,3,4,4,5,5)


  val pf1: PartialFunction[Int, Int] = {
    case x if x%2 == 0 => x/2
  } //Partial Function from Int to Int
  val liftedPf1 = pf1.lift // Function from Int to Option[Int]

  val evenSome1 = Some(2).map(liftedPf1) // Some(Some(1))
  val evenSome2 = Some(2).flatMap(liftedPf1) // Some(1)
  val oddSome1 = Some(1).map(liftedPf1) // Some(None)
  val oddSome2 = Some(1).flatMap(liftedPf1) // None
  val none1 = None.map(liftedPf1) //None: Option[Option[Int]]
  val none2 = None.flatMap(liftedPf1) //None: Option[Int]


  val list3 = List("Ahoj", "Cau", "Nazdar").map(_.toUpperCase) // List(AHOJ, CAU, NAZDAR)
  val list4 = List("Ahoj", "Cau", "Nazdar").flatMap(_.toUpperCase) // List(A,H,O,J,C,A,U,N,A,Z,D,A,R)
}

object ForComprehensionTut {
  /*
   * For Comprehension je iba syntakticky cukor pre mapy a flatMapy
   *
   * Pretoze je for iba syntakticky cukor pre mapy a flatMapy (+withFilter a foreach)
   * platia urcite pravidla:
   * 1. Na pravej strane generatoru (<-) musi byt vzdy monada
   * 2. Tato monada musi byt vzdy rovnaka
   * 3. Na lavej strane generatoru (<-) je hodnota "vybalena" z monady
   * 4. Argument yieldu je hodnota bez monady, for comprehension ju ale zabali do monady
   *     - Teda vystup for comprehension bude F[V] - kde F je typ monady ktora co for figurovala na pravej strane a V je typ hodoty v yield
   */

  val opta = Some("a")
  val optb = Some("b")
  case class IntStringBool(i: Int, s: String, b: Boolean)
  val list1 = List(1,2,3,4,5)
  val list2 = List("a","b","c")
  val list3 = List(true, false)
  def add3Opt(i: Int): Option[Int] = Some(i+3)
  val opt1 = Some(1)

  val for1 = for {
    a <- opta
    b <- optb
  } yield (a,b) // Some((a,b))
  val maps1 = opta.flatMap(a => optb.map(b => (a,b)))

  val for2 = for {
    x <- list1
    y <- list2
    z <- list3
  } yield IntStringBool(x, y, z)
  val maps2 = list1.flatMap(x => list2.flatMap(y => list3.map(z => IntStringBool(x,y,z))))

  val for3 = for {
    x <- list1
    if x % 2 == 0
    y <- list2
    z <- list3
  } yield IntStringBool(x, y, z)
  val maps3 = list1.withFilter(x => x % 2 == 0).flatMap(x => list2.flatMap(y => list3.map(z => IntStringBool(x, y, z))))

  val for4 = for {
    value <- opt1
    valuePlus4 = value + 4
    valuePlus7 <- add3Opt(valuePlus4)
  } yield valuePlus7
  val maps4 = opt1.flatMap(value => {
    val valuePlus4 = value + 4
    add3Opt(valuePlus4).map(valuePlus7 => valuePlus7)
  })

  val for5 = for {
    x <- list1
    y <- list2
    z <- list3
  } println(IntStringBool(x,y,z))
  val maps5 = list1.foreach(x => list2.foreach(y => list3.foreach(z => println(IntStringBool(x,y,z)))))

}
