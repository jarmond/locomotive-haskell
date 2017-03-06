module Loco.Identifiers where

reservedIdentifiers = commaSepIdentifiers ++
                      hyphenSepIdentifiers ++
                      dimIdentifiers ++
                      forIdentifiers ++
                      ifIdentifiers ++
                      strCmdIdentifiers ++
                      streamCmdIdentifiers

commaSepIdentifiers = [ -- Three word commands first
                        "ON BREAK CONT"
                      , "ON BREAK GOSUB"
                      , "ON BREAK STOP"
                      , "ON ERROR GOTO"
                        -- Two word commands
                      , "CHAIN MERGE"
                      , "DEF FN"
                      , "GRAPHICS PEN"
                      , "GRAPHICS PAPER"
                      , "LINE INPUT"
                      , "ON SQ"
                      , "SYMBOL AFTER"
                      , "PRINT USING"
                        -- One word commands
                      , "AFTER"
                      , "AUTO"
                      , "BORDER"
                      , "CALL"
                      , "CAT"
                      , "CHAIN"
                      , "CLEAR"
                      , "CLG"
                      , "CLOSEIN"
                      , "CLOSEOUT"
                      , "CLS"
                      , "CONT"
                      , "DATA"
                      , "DEFINT"
                      , "DEFREAL"
                      , "DEFSTR"
                      , "DEG"
                      , "DI"
                      , "DRAW"
                      , "EDIT"
                      , "EI"
                      , "END"
                      , "ENT"
                      , "ENV"
                      , "ERL"
                      , "ERROR"
                      , "EVERY"
                      , "FILL"
                      , "FN"
                      , "FRAME"
                      , "GOSUB"
                      , "GOTO"
                      , "INK"
                      , "INPUT"
                      , "KEY"
                      , "LOAD"
                      , "LOCATE"
                      , "MASK"
                      , "MEMORY"
                      , "MERGE"
                      , "MODE"
                      , "MOVE"
                      , "MOVER"
                      , "NEW"
                      , "ON"
                      , "OPENIN"
                      , "OPENOUT"
                      , "ORIGIN"
                      , "OUT"
                      , "PAPER"
                      , "PEN"
                      , "PLOT"
                      , "PLOTR"
                      , "POKE"
                      , "PRINT"
                      , "RAD"
                      , "RANDOMIZE"
                      , "READ"
                      , "RELEASE"
                      , "REM"
                      , "RENUM"
                      , "RESTORE"
                      , "RESUME"
                      , "RETURN"
                      , "RUN"
                      , "SAVE"
                      , "SOUND"
                      , "SPC"
                      , "SPEED"
                      , "SQ"
                      , "STOP"
                      , "SWAP"
                      , "SYMBOL"
                      , "TAB"
                      , "TAG"
                      , "TAGOFF"
                      , "TROFF"
                      , "TRON"
                      , "WAIT"
                      , "WEND"
                      , "WHILE"
                      , "WIDTH"
                      , "ZONE"
                      ]

hyphenSepIdentifiers = [ "DELETE"
                       , "LIST"
                       ]
isHyphenCmd = (flip elem) hyphenSepIdentifiers

dimIdentifiers = [ "DIM", "ERASE" ]
forIdentifiers = [ "FOR", "TO", "NEXT", "STEP" ]
ifIdentifiers  = [ "IF", "THEN", "ELSE" ]
strCmdIdentifiers = [ "MID$", "CHR$" ]

streamCmdIdentifiers = [ "WINDOW"
                       , "WRITE"
                       ]
