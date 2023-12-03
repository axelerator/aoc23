app "example"
    packages {
        cli: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.2.0/dJQSsSmorujhiPNIvJKlQoI92RFIG_JQwUfIxZsCSwE.tar.br",
        array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.1.0/ssMT0bDIv-qE7d_yNUyCByGQHvpNkQJZsGUS6xEFsIY.tar.br",
    }
    imports [
        "test-input1.txt" as testInput : Str,
        "input1.txt" as input1 : Str,
        cli.Stdout,
        Bool.{true, false},
        parser.Core.{ Parser, skip, keep, const, oneOf, map, sepBy, many, buildPrimitiveParser, apply},
        parser.String.{ parseStr, digits, string },
        array2d.Array2D.{ Array2D },
    ]
    provides [main] to cli

test = solve1 testInput |> Num.toStr
part1 = solve1 input1 |> Num.toStr
part2 = solve2 input1 |> Num.toStr

main =
  Stdout.line "The answer are: test:\(test) part1:\(part1) part2:\(part2)"

solve1 : Str -> Nat
solve1 = \s -> 42

solve2 : Str -> Nat
solve2 = \s -> 43


Parse a : Parser (List U8) a

RowItem : [Dot, Number Nat, Symbol]
Row : List RowItem

Schematic : Array2D RowItem

fromRows : List Row -> Schematic
fromRows = \rows ->
    Array2D.fromLists rows FitShortest


parseSchematic : Parse Schematic
parseSchematic =
    map (sepBy parseRow (string "\n")) fromRows

parseRow : Parse (List RowItem)
parseRow =
    map
        (many (oneOf [parseDot, parseNumber, parseSymbol]))
        List.join

parseDot : Parse (List RowItem)
parseDot =
    const [Dot]
        |> skip (string ".")

spaceAwareNumber : Nat -> List RowItem
spaceAwareNumber = \n ->
    #suffix = List.repeat Dot 2 # (Num.toStr n |> Str.countGraphemes) 
    #List.join [[Dot], [Number n]]
    []

parseNumber : Parse (List RowItem)
parseNumber =
    const spaceAwareNumber
        |> keep digits

parseSymbol : Parse (List RowItem)
parseSymbol =
    input <- buildPrimitiveParser

    valResult =
        when input is
            [] -> Err (ParsingFailure "Nothing to parse")
            [c, ..] -> 
                if c == '\n' then
                    Err (ParsingFailure "Nothing to parse")
                else
                    Ok [Symbol]
    valResult
    |> Result.map \val -> { val, input: List.dropFirst input 1 }

lines : Str -> List Str
lines = \s -> Str.split s "\n"

expect
    actual = parseStr parseDot "."
    actual == Ok [Dot]

#expect
#    actual = parseStr parseNumber "123"
#    actual == Ok [Number 123, Dot, Dot]

#expect
#    actual = parseStr parseRow ".12+."
#    actual == Ok [Dot, Number 12, Symbol, Dot]

#expect
#    actual = parseStr parseRow ".123."
#    actual == Ok [Dot, Number 123, Dot, Dot, Dot]

#expect
#    expected =
#        fromRows [[Dot, Number 1], [Number 2, Symbol]]
#    actual = parseStr parseSchematic ".1\n2$"
#    actual == Ok expected

