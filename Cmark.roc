interface Cmark
    exposes [
        parse,
        render,
    ]
    imports [
        Parser.Core.{Parser, buildPrimitiveParser, oneOf, many},
    ]

UnparsedInline : List U8

RawBlock : [
    Heading Nat UnparsedInline,
    # Paragraph (List Inline)
]

# PARSER
parse : Str -> Result (List RawBlock) [SomethingWentWrong Str]
parse = \str ->
    when Parser.Core.parse blockParser (Str.toUtf8 str) List.isEmpty is
        Ok a -> Ok a
        Err (ParsingFailure _) -> Err (SomethingWentWrong "Parsing failure")
        Err (ParsingIncomplete leftover) -> 
            msg = when Str.fromUtf8 leftover is 
                Ok a -> a 
                Err _ -> leftover |> List.map Num.toStr |> Str.joinWith ","
            Err (SomethingWentWrong "Parsing incomplete '\(msg)'")


blockParser : Parser (List U8) (List RawBlock)
blockParser = 
    many (oneOf [
        headingParser
    ])

headingParser : Parser (List U8) RawBlock
headingParser =
    buildPrimitiveParser \input ->
        when input is
            ['#', '#', ' ', ..] -> 
                prefixLength = "## " |> Str.toUtf8 |> List.len
                when chompUntilEOL input is 
                    Ok r -> 
                        content = r.val |> List.drop prefixLength
                        Ok {val : Heading 2 content, input : (List.drop r.input 1)} 
                    Err NotFound ->
                        content = input |> List.drop prefixLength
                        Ok {val : Heading 2 content, input : []} 
            ['#', ' ', ..] -> 
                prefixLength = "# " |> Str.toUtf8 |> List.len
                when chompUntilEOL input is 
                    Ok r -> 
                        content = r.val |> List.drop prefixLength
                        Ok {val : Heading 1 content, input : (List.drop r.input 1)} 
                    Err NotFound ->
                        content = input |> List.drop prefixLength
                        Ok {val : Heading 1 content, input : []} 
            _ -> Err (ParsingFailure "not a heading")

chompUntilEOL = chompUntil '\n'

# Returns the chomped content
chompUntil : U8 -> (List U8 -> Result {val : List U8, input : List U8} [NotFound])
chompUntil = \target ->
    \input ->
        check = \x -> Bool.isEq target x
        when List.findFirstIndex input check is
            Err _ -> Err NotFound
            Ok index -> 
                val = List.sublist input {start : 0, len : index }
                Ok {val, input : List.drop input index }

expect 
    chomper = chompUntil '\n'
    input = "# H\nR" |> Str.toUtf8
    chomper input == Ok {val : ['#', ' ', 'H'], input : ['\n','R']}

expect 
    input = "# H1\n" |> Str.toUtf8
    chompUntilEOL input
    |> Result.map \r -> {val : Heading 1 (List.drop r.val 2), input : r.input} 
    |> Result.mapErr \_ -> ParsingFailure "not a heading"
    |> Bool.isEq (Ok {val :Heading 1 ['H','1'], input : ['\n']})

# RENDER
render : List RawBlock -> Str 
render = \blocks ->
    blocks
    |> List.map \block ->
        when block is 
            Heading level contents -> renderHeading level contents
    |> Str.joinWith "\n"

renderHeading : Nat, List U8 -> Str 
renderHeading = \level, contents ->
    contentStr = 
        when Str.fromUtf8 contents is 
            Ok a -> a
            Err _ -> crash "unable to make str from heading contents"
    when level is 
        1 -> "<h1>\(contentStr)</h1>"
        2 -> "<h2>\(contentStr)</h2>"
        3 -> "<h3>\(contentStr)</h3>"
        4 -> "<h4>\(contentStr)</h4>"
        5 -> "<h5>\(contentStr)</h5>"
        _ -> crash "heading level not supported"