app "cmark-spec-tests"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.1.1/zAoiC9xtQPHywYk350_b7ust04BmWLW00sjb9ZPtSQk.tar.br" }
    imports [
        pf.Stdout,
        pf.Stderr,
        pf.Task.{ Task },
        Cmark.{ parse, render },
        TerminalColor.{ Color, withColor}
    ]
    provides [
        main,
    ] to pf

main : Task {} []
main =
    Task.onFail example62 \_ -> crash "Oops, something went wrong."

pass = withColor "PASS: " Green
fail = withColor "FAIL: " Red


runTest : Str, Str, Str -> Task {} []
runTest = \name, input, expected ->
    when parse input is 
        Ok blocks -> 
            result = render blocks
            if result == expected then 
                Stdout.line ("\(pass)\(name)")
            else 

                Stderr.line "\(fail)\nEXPECTED\n\(expected)\nGOT\n\(result)"
        Err (SomethingWentWrong msg) -> 
            Stderr.line "\(fail)\(msg)"

# ATX Headings

example62 : Task {} []
example62 = 
    input = 
        """
        # foo
        ## foo
        """
    expected = 
        """
        <h1>foo</h1>
        <h2>foo</h2>
        """
    runTest "62 - ATX simple headings" input expected
