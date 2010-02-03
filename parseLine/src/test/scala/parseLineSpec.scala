
def line(s:String) = {
  parseLine.LineProcessor.run(s).cs.map { 
    i => println(i) 
  }
}

line("""everything
in it's \
right 
place \.""")

