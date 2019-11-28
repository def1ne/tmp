package com.github.tmp

import scala.annotation.tailrec

/**
 * Основная идея в том, чтобы сгенерить заранее все квадраты на промежутке всех возможных значений.
 * После чего при помощи бинарного поиска в массиве квадратов можно будет находить границы начала и конца требуемого
 * нам промежутка.
 *
 * Created by a.isachenkov on 11/28/2019.
 */
object MaxDepthSearch extends App {

  //наш "словарь" квадратов в промежутке допустимых значений поиска, отсортирован по возрастанию, все значения уникальны.
  val perfectSquares: Array[Int] = perfectSquaresGenerateRec(2, 1000000, List.empty)

  @tailrec
  def perfectSquaresGenerateRec(current: Int, limit: Int, acc: List[Int]): Array[Int] = {
    val square: Int = current * current
    if (square > limit) acc.reverse.toArray else perfectSquaresGenerateRec(current + 1, limit, square :: acc)
  }

  /**
   * Реализация бинарного поиска позиции в словаре. Ищется позиция элемента равного заданному или наиболее близкого
   * заданному. Вернуться может позиция элемента как больше, так и меньше заданного.
   *
   * @param element элемент для поиска.
   * @param start   начальная позиция поиска.
   * @param end     конечноая позиция поиска.
   * @return позиция в словаре.
   */
  @tailrec
  def searchPositionRec(element: Int, start: Int, end: Int): Int = {
    if (start == end) {
      start
    } else {
      val middle: Int = (start + end) / 2
      val middleElement = perfectSquares(middle)
      if (middleElement == element) {
        middle
      } else if (element < middleElement) {
        searchPositionRec(element, start, middle - 1)
      } else {
        searchPositionRec(element, middle + 1, end)
      }
    }
  }

  def searchPosition(element: Int): Int = {
    if (element < perfectSquares(0)) {
      0
    } else if (element > perfectSquares(perfectSquares.length - 1)) {
      perfectSquares.length - 1
    } else {
      searchPositionRec(element, 0, perfectSquares.length - 1)
    }
  }

  /**
   * Поиск позиции в словаре, начиная с которой (включительно) все элементы словаря больше или равны заданному числу.
   *
   * @param element число для поиска.
   * @return позиция в словаре.
   */
  def getStartPosition(element: Int): Int = {
    val position = searchPosition(element)
    if (element > perfectSquares(position)) position + 1 else position
  }

  /**
   * Поиск позиции в словаре, до которой (включительно) все элементы словаря меньше или равны заданному числу.
   *
   * @param element число для поиска.
   * @return позиция в словаре.
   */
  def getEndPosition(element: Int): Int = {
    val position = searchPosition(element)
    if (element < perfectSquares(position)) position - 1 else position
  }

  def getSquareRange(start: Int, end: Int): Array[Int] = {
    java.util.Arrays.copyOfRange(perfectSquares, getStartPosition(start), getEndPosition(end) + 1)
  }

  /**
   * Вычисляет "глубину" квадрата - сколько раз можно применить извлечение квадратного корня так,
   * чтобы результат оставался целым числом.
   *
   * @param element число текущей итерации.
   * @param depth   глубина текущей итерации.
   * @return "глубину" квадрата.
   */
  @tailrec
  def getDepthRec(element: Double, depth: Int): Int = {
    val sqrt = Math.sqrt(element)
    if (!sqrt.isNaN && !sqrt.isInfinity && sqrt == Math.rint(sqrt)) getDepthRec(sqrt, depth + 1) else depth
  }

  def solution(start: Int, end: Int): Int = {
    getSquareRange(start, end).map(getDepthRec(_, 0)).max
  }

  println(solution(2, 1000000)) //prints 4
  println(solution(10, 20)) //prints 2
  println(solution(6000, 7000)) //prints 3
}
