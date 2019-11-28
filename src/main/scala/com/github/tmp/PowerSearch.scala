package com.github.tmp

/**
 * Для всех чисел больше 10 нам по-сути нужно число равное 10 в степени количества знаков оригинального числа.
 * Т.е. самая большая степень 10-ки меньшая или равная заданному числу.
 * Так как десятичный логарифм - возрастающая функция на аргументах больше нуля, мы можем взять десятичный логарифм
 * от заданного числа и округлить его до целого снизу, тем самым получив наиболее близкую степень 10ки.
 * Created by a.isachenkov on 11/28/2019.
 */
object PowerSearch extends App {

  def solution(number: Int): Int = {
    if (number < 10) 0 else Math.pow(10, Math.floor(Math.log10(number))).toInt
  }

  println(solution(1234567)) //prints 1000000
  println(solution(143)) //prints 100
  println(solution(10)) //prints 10
  println(solution(1)) //prints 0
}
