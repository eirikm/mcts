package mcts.data

import scala.reflect.ClassTag
import Array2d.{Column, Index, Row}

/**
  * Squeeze a 2D array into a 1D array. Provide an immutable interface.
  */
final class Array2d[@specialized T](val array: Array[T], val numX: Int, val numY: Int) {

  def index(col: Column, row: Row): Index =
    Index(col * numY + row)

  def apply(col: Column, row: Row): T =
    apply(index(col, row))

  def apply(index: Index): T =
    array(index)

  def apply(colRow: (Column, Row)): T =
    apply(colRow._1, colRow._2)

  def updated(col: Column, row: Row, value: T): Array2d[T] =
    updated(index(col, row), value)

  def map[@specialized U: ClassTag](f: (Column, Row, T) => U): Array2d[U] = {
    val newArray = Array.ofDim[U](array.length)

    var col = 0
    while (col < numX) {

      var row = 0
      while (row < numY) {
        val idx = index(col, row)
        newArray(idx) = f(col, row, array(idx))
        row += 1
      }
      col += 1
    }

    new Array2d[U](newArray, numX, numY)
  }

  def updated(idx: Index, value: T): Array2d[T] = {
    val cloned = array.clone()
    cloned(idx) = value
    new Array2d(cloned, numX, numY)
  }
}

object Array2d {
  def apply[T: ClassTag](numX: Int, numY: Int, empty: T): Array2d[T] =
    new Array2d[T](Array.fill(numX * numY)(empty), numX, numY)

  type Column = Int
  type Row    = Int
  type Index  = Int

  def Column(value: Int): Column = value.asInstanceOf[Column]
  def Row(value:    Int): Row    = value.asInstanceOf[Row]
  def Index(value:  Int): Index  = value.asInstanceOf[Index]
}
