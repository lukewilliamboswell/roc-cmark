app "cmark-spec-tests"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.1.1/zAoiC9xtQPHywYk350_b7ust04BmWLW00sjb9ZPtSQk.tar.br" }
    imports [
        pf.Stdout,
        pf.Stderr,
        pf.Task.{ Task },
        Cmark.{ parse, render },
    ]
    provides [
        main,
    ] to pf

main : Task {} []
main =
    Task.onFail example62 \_ -> crash "Oops, something went wrong."

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
    runTest input expected

runTest : Str, Str -> Task {} []
runTest = \input, expected ->
    when parse input is 
        Ok blocks -> 
            result = render blocks
            if result == expected then 
                Stdout.line "PASS:\n\(result)"
            else 
                Stderr.line "FAIL: expected\n\(expected)\ngot:\n\(result)"
        Err (SomethingWentWrong msg) -> 
            Stderr.line "FAIL: \(msg)"