app "example"
    packages {
        cli: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.2.0/dJQSsSmorujhiPNIvJKlQoI92RFIG_JQwUfIxZsCSwE.tar.br",
    }
    imports [
        "input2.txt" as aocInput : Str,
        cli.Stdout,
        parser.Core.{ Parser, buildPrimitiveParser, many },
        parser.String.{ parseStr },
    ]
    provides [main] to cli

main =
  Stdout.line "The answer is: \(solve aocInput |> Num.toStr)"

solve : Str -> Nat
solve =
  \s ->
    lines s
    |> List.map calibrationNumber
    |> List.sum

calibrationNumber : Str -> Nat
calibrationNumber = \s ->
  ds = digits s
  (tt, t) =
    when (List.first ds, List.last ds) is
      (Ok f, Ok l) -> (f, l)
      (Err _, Ok l) -> (l , l)
      (Ok f, Err _) -> (f , f)
      _ -> (0, 0 )
  tt * 10 + t

lines : Str -> List Str
lines = \s -> Str.split s "\n"

values : List (Result Nat Str) -> List Nat
values = \rs ->
  List.walk rs [] List.appendIfOk

digits : Str -> List Nat
digits = \s ->
    result = parseStr (many digitParser) s

    when result is
        Ok chars -> values chars
        Err _ -> []

digitParser : Parser (List U8) (Result Nat Str)
digitParser =
    input <- buildPrimitiveParser

    valResult =
        when input is
            [] -> Err (ParsingFailure "Nothing to parse")
            ['o', 'n', 'e', ..] -> Ok (Ok 1)
            ['t', 'w', 'o', ..] -> Ok (Ok 2)
            ['t', 'h', 'r', 'e', 'e', ..] -> Ok (Ok 3)
            ['f', 'o', 'u', 'r', ..] -> Ok (Ok 4)
            ['f', 'i', 'v', 'e', ..] -> Ok (Ok 5)
            ['s', 'i', 'x', ..] -> Ok (Ok 6)
            ['s', 'e', 'v', 'e', 'n', ..] -> Ok (Ok 7)
            ['e', 'i', 'g', 'h', 't', ..] -> Ok (Ok 8)
            ['n', 'i', 'n', 'e', ..] -> Ok (Ok 9)
            ['0', ..] -> Ok (Ok 0)
            ['1', ..] -> Ok (Ok 1)
            ['2', ..] -> Ok (Ok 2)
            ['3', ..] -> Ok (Ok 3)
            ['4', ..] -> Ok (Ok 4)
            ['5', ..] -> Ok (Ok 5)
            ['6', ..] -> Ok (Ok 6)
            ['7', ..] -> Ok (Ok 7)
            ['8', ..] -> Ok (Ok 8)
            ['9', ..] -> Ok (Ok 9)
            _ -> Ok (Err "")

    valResult
    |> Result.map \val -> { val, input: List.dropFirst input 1 }
