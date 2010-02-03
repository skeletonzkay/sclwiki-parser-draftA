package parseLine
{
  import scala.util.parsing.combinator._

  object A{}

  object LineProcessor{
     def run(s:String):LineDocuments = {
       return LineParsers.parse(LineParsers.lines,s).get
     }
  }

  case class LineDocuments(cs:List[Any]){
  }
  case class Line(s:String) {override def toString = {s} }
  case class Lines(cs:List[Line]) 

  object LineParsers extends RegexParsers{

    override def skipWhitespace = false

    def lines:Parser[LineDocuments] = lineWithNoReturn ~ lineWithReturns ^^ { case a ~ b => LineDocuments(List(List(a.s),b.cs).flatten) }

    def lineWithReturns:Parser[Lines] = (lineWithReturn).* ^^ { case cs => Lines(cs) } 

    def lineWithNoReturn:Parser[Line] = line ^^ { case a => Line(a) }

    def lineWithReturn:Parser[Line] = ret ~ line ^^ { case r ~ a => Line(a) }

    def line:Parser[String] = rep1(nor | quotedret | quote) ^^ { case a => "" + join(a) + "" }

    def quotedret = quote ~ ret ^^ { case a ~ b =>  qret(a,b) }

    def nor = """[\u0020-\uffff&&[^\\]]+""".r

    def quote = "\\" 

    def ret = "\r\n" | "\r" | "\n" ^^ { case a => "[ret]" + a }

    def join(l:List[Any]) = {("" /: l )(_+""+_)}

    def qret(a:String,b:String) = { "" }//{ a + "" + b }
  }
}
