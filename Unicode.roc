interface Unicode
    exposes [
        Character,
        utf8,
        lineEnding,
        whitespace,
    ]
    imports [
        Parser.Core.{Parser, parsePartial, buildPrimitiveParser, eatWhile, eatWhileNot},
    ]

Character : [
    # Line Endings
    LF, # Line Feed
    CR, # Carriage Return
    CRLF, # Carriage Return followed by Line Feed

    # Whitespace
    FF, # Form Feed
    TAB,
    SPACE,
    NSBP,
    OSM, 
    ENQUAD, 
    EMQUAD, 
    ENSPACE, 
    EMSPACE, 
    THREEPEREMSPACE, 
    FOURPEREMSPACE, 
    SICPEREMSPACE,
    FIGURESPACE, 
    PUNCTUATIONSPACE, 
    THINSPACE, 
    HAIRSPACE, 
    NNBSP, 
    MMSP, 
    IDEOGRAPHICSPACE,

]

utf8 : Character -> List U8
utf8 = \c ->
    when c is 
        LF -> Str.toUtf8 "\u(000A)"
        FF -> Str.toUtf8 "\u(000C)"
        CR -> Str.toUtf8 "\u(000D)"
        CRLF -> Str.toUtf8 "\u(000D)\u(000A)"
        TAB -> Str.toUtf8 "\u(0009)"
        SPACE -> Str.toUtf8 "\u(0020)"
        NSBP -> Str.toUtf8 "\u(00A0)"
        OSM -> Str.toUtf8 "\u(1680)" 
        ENQUAD -> Str.toUtf8 "\u(2000)" 
        EMQUAD -> Str.toUtf8 "\u(2001)" 
        ENSPACE -> Str.toUtf8 "\u(2002)" 
        EMSPACE -> Str.toUtf8 "\u(2003)" 
        THREEPEREMSPACE -> Str.toUtf8 "\u(2004)" 
        FOURPEREMSPACE -> Str.toUtf8 "\u(2005)" 
        SICPEREMSPACE -> Str.toUtf8 "\u(2006)"
        FIGURESPACE -> Str.toUtf8 "\u(2007)" 
        PUNCTUATIONSPACE -> Str.toUtf8 "\u(2008)" 
        THINSPACE -> Str.toUtf8 "\u(2009)" 
        HAIRSPACE -> Str.toUtf8 "\u(200A)" 
        NNBSP -> Str.toUtf8 "\u(202F)" 
        MMSP -> Str.toUtf8 "\u(205F)" 
        IDEOGRAPHICSPACE -> Str.toUtf8 "\u(3000)"

# [LINE ENDING](https://spec.commonmark.org/0.30/#line-ending)
lineEnding : Parser (List U8) (List U8)
lineEnding = 
    input <- buildPrimitiveParser

    when input is 
        [10, ..] -> Ok {val : utf8 LF, input : List.drop input 1}
        [13,10, ..] -> Ok {val : utf8 CRLF, input : List.drop input 2}
        [13, ..] -> Ok {val : utf8 CR, input : List.drop input 1}
        _ -> Err (ParsingFailure "not a line ending")

expect parsePartial lineEnding (Str.toUtf8 "\na") == Ok {val : utf8 LF, input : ['a']}
expect parsePartial lineEnding (Str.toUtf8 "\ra") == Ok {val : utf8 CR, input : ['a']}
expect parsePartial lineEnding (Str.toUtf8 "\r\na") == Ok {val : utf8 CRLF, input : ['a']}
expect parsePartial lineEnding (Str.toUtf8 "X") == Err (ParsingFailure "not a line ending")

# [WHITESPACE](https://spec.commonmark.org/0.30/#unicode-whitespace-character)
whitespace : Parser (List U8) (List U8)
whitespace = 
    input <- buildPrimitiveParser

    when input is
        [9,..] -> Ok {val : utf8 TAB, input : List.drop input 1}
        [10, ..] -> Ok {val : utf8 LF, input : List.drop input 1}
        [12, ..] -> Ok {val : utf8 FF, input : List.drop input 1}
        [13, ..] -> Ok {val : utf8 CR, input : List.drop input 1}
        [32,..] -> Ok {val : utf8 SPACE, input : List.drop input 1} 
        [194,160] -> Ok {val : utf8 NSBP, input : List.drop input 2}
        [225, 154, 128] -> Ok {val : utf8 OSM, input : List.drop input 3}
        [226, 128, 128] -> Ok {val : utf8 ENQUAD, input : List.drop input 3}
        [226, 128, 129] -> Ok {val : utf8 EMQUAD, input : List.drop input 3}
        [226, 128, 130] -> Ok {val : utf8 ENSPACE, input : List.drop input 3}
        [226, 128, 131] -> Ok {val : utf8 EMSPACE, input : List.drop input 3}
        [226, 128, 132] -> Ok {val : utf8 THREEPEREMSPACE, input : List.drop input 3}
        [226, 128, 133] -> Ok {val : utf8 FOURPEREMSPACE, input : List.drop input 3}
        [226, 128, 134] -> Ok {val : utf8 SICPEREMSPACE, input : List.drop input 3}
        [226, 128, 135] -> Ok {val : utf8 FIGURESPACE, input : List.drop input 3}
        [226, 128, 136] -> Ok {val : utf8 PUNCTUATIONSPACE, input : List.drop input 3}
        [226, 128, 137] -> Ok {val : utf8 THINSPACE, input : List.drop input 3}
        [226, 128, 138] -> Ok {val : utf8 HAIRSPACE, input : List.drop input 3}
        [226, 128, 175] -> Ok {val : utf8 NNBSP, input : List.drop input 3}
        [226, 129, 159] -> Ok {val : utf8 MMSP, input : List.drop input 3}
        [227, 128, 128] -> Ok {val : utf8 IDEOGRAPHICSPACE, input : List.drop input 3}
        _ -> Err (ParsingFailure "not whitSPACE")

expect parsePartial whitespace (Str.toUtf8 " ab") == Ok {val : [32], input : ['a','b']} 

expect 
    input = Str.toUtf8 " \n\t  abc"
    expected = Str.toUtf8 " \n\t  "
    got = parsePartial (eatWhile whitespace) input 
    got == Ok {val : expected, input : ['a','b','c']} 

expect
    input = Str.toUtf8 "asdf234234\nabc"
    expected = Str.toUtf8 "asdf234234"
    got = parsePartial (eatWhileNot whitespace) input 
    got == Ok {val : expected, input : ['\n','a','b','c']} 