Config { font = "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"
       , borderColor = "black"
       , border = TopB
       , bgColor = "0e0e0e"
       , fgColor = "e0e0e0"
       , position = Top
       , lowerOnStart = True
       , commands = [ Run Memory ["-t","Mem: <usedratio>%"] 10
    		        , Run Date "%a %b %_d %Y %H:%M:%S" "date" 10
                    , Run StdinReader
                    ]
       , sepChar  = "%"
       , alignSep = "}{"
       , template = "%StdinReader% }{ %memory% %date%"
       }
