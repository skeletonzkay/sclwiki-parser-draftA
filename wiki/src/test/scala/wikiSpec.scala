
wiki.Wiki.wiki("""!!!header""").print()

wiki.Wiki.wiki("""say hello!""").print()


wiki.Wiki.wiki("""!nothing{every|thing}abc[d|e]f""").print()

wiki.Wiki.wiki("""nothing{every|thing}abc[d|e]f""").print()

wiki.Wiki.wiki("""nothing{red bgBlack|everything}abc[http://www.google.com|google]f""").print()

wiki.Wiki.wiki("""everything{{background-color:red;|in it's right }}place
nothing
""").print()

wiki.Wiki.wiki("""<everything>&"'
nothing
""").print()

def line(s:String,fun:String=>String)= {
   parseLine.LineProcessor.run(s).cs.map {
      i => fun(i.toString())
   }
}

line("""!!!{white bgBlack|e}{black bgRed|ve}{white bgBlack|ry}thing
in it's \
right place
"""
, { i => wiki.Wiki.wiki(i.toString()).toString() } )
.map{i => println (i)}


