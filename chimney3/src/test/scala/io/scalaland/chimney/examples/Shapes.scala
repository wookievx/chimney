package io.scalaland.chimney.examples

object shapes1 {

  case class Point(x: Int, y: Int)

  sealed trait Shape
  case class Triangle(p1: Point, p2: Point, p3: Point) extends Shape
  case class Rectangle(p1: Point, p2: Point) extends Shape
}

object shapes2 {

  case class Point(x: Int, y: Int)

  sealed trait Shape
  case class Polygon(points: List[Point]) extends Shape
}

object shapes3 {

  case class Point(x: Double, y: Double)

  sealed trait Shape
  case class Triangle(p3: Point, p2: Point, p1: Point) extends Shape
  case class Rectangle(p1: Point, p2: Point) extends Shape
}

object shapes4 {

  case class Point(x: Double, y: Double)

  sealed trait Shape

  sealed trait ThreeAnglesShape extends Shape

  sealed trait FourAnglesShape extends Shape

  case class Triangle(p3: Point, p2: Point, p1: Point) extends ThreeAnglesShape
  case class Rectangle(p1: Point, p2: Point) extends FourAnglesShape
}

object shapes5 {
  case class Point(x: Double, y: Double)

  sealed trait Shape

  case class Triangle(p1: Point, p2: Point, p3: Point) extends Shape
  case class Rectangle(p1: Point, p2: Point) extends Shape
  case class Circle(center: Point, rad: Double) extends Shape

  object Inner {
    case class Triangle(p1: Point, p2: Point, p3: Point) extends Shape
    case class Rectangle(p1: Point, p2: Point) extends Shape
    case class Circle(center: Point, rad: Double) extends Shape
  }
}
