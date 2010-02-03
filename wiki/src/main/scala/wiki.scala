package wiki{

import scala.util.parsing.combinator._

case class WikiResult(s:String){
    def print() = {
       println(WikiParsers.parse(WikiParsers.wiki,s).get)
    }
    override def toString() = {
       WikiParsers.parse(WikiParsers.wiki,s).get.toString()
    }
}

object Wiki {
   def wiki(s:String):WikiResult = {
       return WikiResult(s)
   }
}

trait Base

case class Block(s:Base) extends Base{
   override def toString = { s.toString() }
}
case class H1(item:Base) extends Base{
   override def toString = { "<h1>" + item + "</h1>" }
}
case class H2(item:Base) extends Base{
   override def toString = { "<h2>" + item + "</h2>" }
}
case class H3(item:Base) extends Base{
   override def toString = { "<h3>" + item + "</h3>" }
}
case class P (item:Base) extends Base{
   override def toString = { "<p>" + item + "</p>" }
}
case class Style(item:StyleItem) extends Base{
   override def toString = { "<span class='"+item.attr + "'>" + item.item + "</span>" }
}
case class StyleOption(item:StyleItem) extends Base{
   override def toString = { "<span style='"+item.attr + "'>" + item.item + "</span>" }
}
case class StyleItem(attr:Ident,item:Base){
}
case class Link(item:LinkItem) extends Base{
   override def toString = { "<a href='"+item.attr + "'>" + item.item + "</a>" }
}
case class LinkItem(attr:Ident,item:Base) extends Base{
}
case class Ident(item:String) extends Base{
   override def toString = { item.toString() }
}
case class Meta(item:String) extends Base{
   override def toString = { item match {
       case "<" => "&lt;" 
       case ">" => "&gt;" 
       case "&" => "&amp;" 
       case "\"" => "&quot;" 
       case "'" => "'" 
       case _ => "" 
   } }
}
case class Item(item:Base) extends Base{
   override def toString = { item.toString() }
}
case class Items(item:List[Base]) extends Base{
  override def toString = { join(item) }
  def join(l:List[Any]) = {("" /: l )(_+""+_)}
}

object WikiParsers extends RegexParsers{

  def wiki:Parser[Base] = ( h3 | h2 | h1 | paragraph )^^ Block
   
  def h1:Parser[H1] = header ~> tokens ^^ H1
  def h2:Parser[H2] = (header ~ header ) ~> tokens ^^ H2
  def h3:Parser[H3] = (header ~ header  ~ header ) ~> tokens ^^ H3
  def paragraph:Parser[P] = tokens ^^ P 

  def tokens:Parser[Items] = rep(token) ^^ Items

  def token:Parser[Item] = (item | special) ^^ Item

  def special:Parser[Ident] = (header | escape) ^^ Ident

  def item:Parser[Item] = (style | link | styleOption | linkOption | ident | meta ) ^^ Item

  def style:Parser[Style] = styleBegin ~> styleItem <~ styleEnd ^^ Style
  def link:Parser[Link] = linkBegin  ~> linkItem <~ linkEnd ^^ Link

  def styleOption:Parser[Base] = (styleBegin ~  styleBegin) ~> styleItem <~ (styleEnd ~ styleEnd) ^^ StyleOption
  def linkOption:Parser[Base] = (linkBegin ~ linkBegin) ~> linkItem <~ (linkEnd ~ linkEnd) ^^ Link

  def styleItem:Parser[StyleItem] = ident ~ separate ~ item ^^ {case a~b~c => StyleItem(a,c) }
  def linkItem:Parser[LinkItem] = ident ~ separate ~ item ^^ {case a~b~c => LinkItem(a,c) }
 
  def meta = ( tagBegin | tagEnd | amp | quote | dquote ) ^^ Meta 

  def header     = "!"
  def styleBegin = "{"
  def styleEnd   = "}"
  def linkBegin  = "["
  def linkEnd    = "]"
  def separate   = "|"
  def escape      = """\"""

  def tagBegin   = "<"
  def tagEnd     = ">"
  def amp        = "&"
  def dquote     = "\""  
  def quote      = "'"
 
  def ident = """[\u0020-\uffff&&[^!{}<>&'"\[\]|\\]]+""".r ^^ Ident

  def join(l:List[Any]) = {("" /: l )(_+""+_)}
}

}


