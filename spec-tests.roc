app "cmark-spec-tests"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.1.1/zAoiC9xtQPHywYk350_b7ust04BmWLW00sjb9ZPtSQk.tar.br" }
    imports [
        pf.Stdout,
        pf.Stderr,
        pf.Task.{ Task },
        Cmark.{ parse, render },
        TerminalColor.{ Color, withColor },
    ]
    provides [
        main,
    ] to pf

# Setup
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
            Stderr.line "\(fail)\nEXPECTED\n\(expected)\nERROR\n\(msg)"

# Run the tests from the spec
main : Task {} []
main =
    task =
        {} <- example62 |> Task.await
        {} <- example219 |> Task.await
    
        Stdout.line "Tests complete"

    Task.onFail task \_ -> crash "Oops, something went wrong."

# [ATX Headings](https://spec.commonmark.org/0.30/#atx-headings)
example62 : Task {} []
example62 =
    input =
        """
        # foo
        ## foo
        ### foo
        #### foo
        ##### foo
        """
    expected =
        """
        <h1>foo</h1>
        <h2>foo</h2>
        <h3>foo</h3>
        <h4>foo</h4>
        <h5>foo</h5>
        """

    runTest "62 - ATX simple headings" input expected

# [Paragraphs](https://spec.commonmark.org/0.30/#paragraphs)
example219 : Task {} []
example219 =
    input =
        """
        aaa
        
        bbb
        """
    expected =
        """
        <p>aaa</p>
        <p>bbb</p>
        """

    runTest "219 - simple example with two paragraphs" input expected
