
package org.sigfigs
package util

trait PrettyPrinter {

  def toSubscript(i: Int): String = toSubscript(i.toString)

  def toSubscript(s: String): String = s.map { c => c match {
    case '0' => '₀'
    case '1' => '₁'
    case '2' => '₂'
    case '3' => '₃'
    case '4' => '₄'
    case '5' => '₅'
    case '6' => '₆'
    case '7' => '₇'
    case '8' => '₈'
    case '9' => '₉'
    case '+' => '₊'
    case '-' => '₋'
    case c => c
  }}

  def toSuperscript(i: Int): String = toSuperscript(i.toString)

  def toSuperscript(s: String): String = s.map { c => c match {
    case '0' => '⁰'
    case '1' => '¹'
    case '2' => '²'
    case '3' => '³'
    case '4' => '⁴'
    case '5' => '⁵'
    case '6' => '⁶'
    case '7' => '⁷'
    case '8' => '⁸'
    case '9' => '⁹'
    case '+' => '⁺'
    case '-' => '⁻'
    case c => c
  }}

  def toSansSerifBold(i: Int): String = toSuperscript(i.toString)

  // weird problem with closing unicode for the bold characters causes us to
  // have to use a flatMap here instead of the usual map
  def toSansSerifBold(s: String): String = s.flatMap { c => c match {
    case '0' => "𝟬"
    case '1' => "𝟭"
    case '2' => "𝟮"
    case '3' => "𝟯"
    case '4' => "𝟰"
    case '5' => "𝟱"
    case '6' => "𝟲"
    case '7' => "𝟳"
    case '8' => "𝟴"
    case '9' => "𝟵"
    case c => c.toString
  }}

  def toBold(i: Int): String = toSuperscript(i.toString)

  // weird problem with closing unicode for the bold characters causes us to
  // have to use a flatMap here instead of the usual map
  def toBold(s: String): String = s.flatMap { c => c match {
    case '0' => "𝟎"
    case '1' => "𝟏"
    case '2' => "𝟐"
    case '3' => "𝟑"
    case '4' => "𝟒"
    case '5' => "𝟓"
    case '6' => "𝟔"
    case '7' => "𝟕"
    case '8' => "𝟖"
    case '9' => "𝟗"
    case c => c.toString
  }}
}



