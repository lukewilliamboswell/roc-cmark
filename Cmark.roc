interface Cmark
    exposes [
        parse,
        render,
    ]
    imports [
        Parser.Core.{ Parser, const, buildPrimitiveParser, parsePartial, oneOf, sepBy, keep, eatWhileNot },
        Unicode.{ whitespace, lineEnding },
    ]

UnparsedInline : List U8

RawBlock : [
    Heading HeadingLevel UnparsedInline,
    Paragraph UnparsedInline,
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

# Parse Blocks - 1st Pass
blockParser : Parser (List U8) (List RawBlock)
blockParser =
    blocks = oneOf [
        headingParser, 
        paragraphParser, # must be last will catch everything
    ]

    sepBy blocks lineEnding


# [ATX Headings](https://spec.commonmark.org/0.30/#atx-headings)
headingParser : Parser (List U8) RawBlock
headingParser =
    const (\level -> \content ->
        when level is
            One -> Heading One content
            Two -> Heading Two content
            Three -> Heading Three content
            Four -> Heading Four content
            Five -> Heading Five content
    )
    |> keep headingLevelParser
    |> keep (eatWhileNot whitespace)

HeadingLevel : [One, Two, Three, Four, Five]

headingLevelParser : Parser (List U8) HeadingLevel
headingLevelParser =
    buildPrimitiveParser \input ->
        when input is
            ['#', '#', '#', '#', '#', ' ', ..] -> Ok { val: Five, input: List.drop input 6 }
            ['#', '#', '#', '#', ' ', ..] -> Ok { val: Four, input: List.drop input 5 }
            ['#', '#', '#', ' ', ..] -> Ok { val: Three, input: List.drop input 4 }
            ['#', '#', ' ', ..] -> Ok { val: Two, input: List.drop input 3 }
            ['#', ' ', ..] -> Ok { val: One, input: List.drop input 2 }
            _ -> Err (ParsingFailure "not a heading")

# [Paragraphs](https://spec.commonmark.org/0.30/#paragraphs)
# A sequence of non-blank lines that cannot be interpreted as other kinds of blocks forms a paragraph.
paragraphParser : Parser (List U8) RawBlock
paragraphParser =
    const Paragraph
    |> keep (eatWhileNot lineEnding)

expect 
    input = Str.toUtf8 "abc\n"
    got = parsePartial paragraphParser input
    got == Ok {val : Paragraph input, input : ['\n']}

# RENDER
render : List RawBlock -> Str
render = \blocks ->
    blocks
    |> List.map \block ->
        when block is
            Heading level contents -> renderHeading level contents
            Paragraph contents -> renderParagraph contents
    |> Str.joinWith "\n"

renderParagraph : List U8 -> Str
renderParagraph = \contents ->
    contentStr =
        when Str.fromUtf8 contents is
            Ok a -> a
            Err _ -> crash "unable to make paragraph from contents"

    "<p>\(contentStr)</p>"

renderHeading : HeadingLevel, List U8 -> Str
renderHeading = \level, contents ->
    contentStr =
        when Str.fromUtf8 contents is
            Ok a -> a
            Err _ -> crash "unable to make str from heading contents"

    when level is
        One -> "<h1>\(contentStr)</h1>"
        Two -> "<h2>\(contentStr)</h2>"
        Three -> "<h3>\(contentStr)</h3>"
        Four -> "<h4>\(contentStr)</h4>"
        Five -> "<h5>\(contentStr)</h5>"


