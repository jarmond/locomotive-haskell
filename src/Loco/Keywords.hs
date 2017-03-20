module Loco.Keywords where

keywords = builtinCmds ++ builtinFuncs

builtinCmds = [
  "AFTER",
  "AND",
  "AUTO",
  "BORDER",
  "CALL",
  "CAT",
  "CHAIN",
  "CLEAR",
  "CLG",
  "CLOSEIN",
  "CLOSEOUT",
  "CLS",
  "CONT",
  "DATA",
  "DATA",
  "DEF",
  "DEFINT",
  "DEFREAL",
  "DEFSTR",
  "DEG",
  "DELETE",
  "DI",
  "DIM",
  "DRAW",
  "DRAWR",
  "EDIT",
  "EI",
  "ELSE",
  "ERROR",
  "EVERY",
  "FN",
  "FOR",
  "GOSUB",
  "GOTO",
  "HIMEM",
  "IF",
  "INK",
  "INPUT",
  "KEY",
  "LET",
  "LINE",
  "LIST",
  "LOAD",
  "LOCATE",
  "MEMORY",
  "MERGE",
  "MID$",
  "MOD",
  "MODE",
  "MOVE",
  "MOVER",
  "NEXT",
  "NEW",
  "NOT",
  "ON",
  "ON BREAK",
  "ON ERROR BREAK",
  "ON ERROR GOTO",
  "GOTO",
  "ON SQ",
  "OPENIN",
  "OPENOUT",
  "OR",
  "ORIGIN",
  "OUT",
  "PAPER",
  "PEN",
  "PLOT",
  "PLOTR",
  "POKE",
  "PRINT",
  "RAD",
  "RANDOMIZE",
  "READ",
  "RELEASE",
  "REM",
  "RENUM",
  "RESTORE",
  "RESUME",
  "RETURN",
  "RUN",
  "SAVE",
  "SOUND",
  "SPC",
  "SPEED",
  "SPC",
  "STEP",
  "STOP",
  "SWAP",
  "SYMBOL",
  "TAB",
  "TAG",
  "TAGOFF",
  "THEN",
  "TO",
  "TROFF",
  "TRON",
  "USING",
  "WAIT",
  "WEND",
  "WHILE",
  "WIDTH",
  "WINDOW",
  "WRITE",
  "XOR",
  "ZONE"]

builtinFuncs = [
  "ABS",
  "ASC",
  "ATN",
  "BIN$",
  "CHR$",
  "CINT",
  "COS",
  "CREAL",
  "EOF",
  "EXP",
  "FIX",
  "FRE",
  "HEX$",
  "INKEY",
  "INKEY$",
  "INP",
  "INSTR",
  "INT",
  "JOY",
  "LEFT$",
  "LEN",
  "LOG",
  "LOG10",
  "LOWER$",
  "MAX",
  "MID$",
  "MIN",
  "PEEK",
  "PI",
  "POS",
  "REMAIN",
  "RIGHT$",
  "RND",
  "ROUND",
  "SGN",
  "SIN",
  "SPACE$",
  "SQ",
  "SQR",
  "STR$",
  "STRING$",
  "TAN",
  "TEST",
  "TESTR",
  "TIME",
  "UNT",
  "UPPER$",
  "VAL",
  "XPOS",
  "YPOS",
  "VPOS"]
