package wikiProc{

  object WikiProcessor{

      def line(s:String,fun:String=>String) = {
         parseLine.LineProcessor.run(s).cs.map {
            i => fun(i.toString())
         }
      }

      def proc(s:String,fun:String=>String) = {
         line(s
         , { i => wiki.Wiki.wiki(i.toString()).toString() } )
         .map{ i => fun (i.toString() ) }
     }

     def procToString(s:String):String = {
       join(line(s
         , { i => wiki.Wiki.wiki(i.toString()).toString() } ))
     }

     def join(l:List[Any]) = {("" /: l )(_+""+_)}
    
  }

}

