app "example"
    packages {
        cli: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.3.0/-e3ebWWmlFPfe9fYrr2z1urfslzygbtQQsl69iH1qzQ.tar.br",
        array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.1.0/ssMT0bDIv-qE7d_yNUyCByGQHvpNkQJZsGUS6xEFsIY.tar.br",
    }
    imports [
        "test-input1.txt" as testInput : Str,
        "input1.txt" as input1 : Str,
        cli.Stdout,
        Bool.{true, false},
        parser.Core.{ Parser, skip, const, oneOf, map, sepBy, many, buildPrimitiveParser},
        parser.String.{ parseStr, digits, string },
        array2d.Array2D.{ Array2D, Index, Shape },
    ]
    provides [main] to cli

test = solve1 testInput |> Num.toStr
part1 = solve1 input1 |> Num.toStr
part2 = solve2 input1 |> Num.toStr

main =
    Stdout.line "The answer are: test:\(test) part1:\(part1) part2:\(part2)"
walkOptions  = {
    direction : Forwards,
}


solve1 : Str -> Nat
solve1 = \s -> 
    when parseStr parseSchematic s is
        Ok grid ->
            Array2D.walk grid 0 walkOptions (\sum, _, idx ->  sum + (value grid idx))
        _ -> 0 

solve2 : Str -> Nat
solve2 = \_ -> 43

Parse a : Parser (List U8) a

RowItem : [Dot, Number Nat Nat, Symbol]
Row : List RowItem

Schematic : Array2D RowItem

fromRows : List Row -> Schematic
fromRows = \rows ->
    Array2D.fromLists (List.reverse rows |> List.dropFirst 1 ) FitShortest
        |> Array2D.rotateClockwise

gridStr : Nat,Nat,Schematic -> Str
gridStr = \width, height, grid ->
    colRange = 
        List.range { start: At 0, end: Before width }
    rowRange = 
        List.range { start: At 0, end: Before height }

    mkRow = \y ->
        Str.joinWith (List.map colRange (\x -> cellStr grid {x: x, y:y})) ""
    gg = Str.joinWith (List.map rowRange mkRow ) "\n"
    shape = Array2D.shape grid
    "Shape: dimX: \(Num.toStr shape.dimX) dimY:\(Num.toStr shape.dimY)\n\(gg)"

cellStr : Schematic, Index -> Str
cellStr = \grid,idx ->
    when Array2D.get grid idx is
        Err _ -> "X"
        Ok (Dot) -> "."
        Ok (Symbol) -> "*"
        Ok(Number n _) -> Num.toStr n

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

parseNumber : Parse (List RowItem)
parseNumber =
    map digits (\n -> 
            width = (Num.toStr n |> Str.countGraphemes )
            extraSpaces =  width - 1
            prefix = List.repeat Dot extraSpaces
            List.join [[Number n width], prefix]
        )

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

adjacentIndices : Shape, Index, Nat -> List Index
adjacentIndices = \{dimX, dimY}, {x,y}, width ->
    left =
        if x == 0 then
            0
        else 
            x - 1
    right = 
        Num.min (x + width) (dimX - 1)
    top = 
        if y == 0 then 
            0
        else
           y - 1
    bottom =
        Num.min (y + 1) (dimY - 1)
    rowIds = List.range { start: At top, end: At bottom }
    colIds = List.range { start: At left, end: At right }

    mkRow = \rowY ->
       if rowY == y then
           List.join
               [ if x > 0 then [{x: x - 1, y: rowY }] else [],
                 if (x + width) < dimX  then [{ x: x + width, y: rowY }] else [] 
               ]
       else
            List.map colIds (\colX -> { x: colX, y: rowY } )
    List.map rowIds mkRow
     |> List.join

isSymbol : Schematic, Index -> Bool
isSymbol = \grid, idx ->
    when Array2D.get grid idx is
        (Ok Symbol) -> true
        _ -> false

value : Schematic, Index -> Nat
value = \grid, idx ->
    cell = Array2D.get grid idx

    when cell is
        Ok (Number n width) ->
            adjacent = adjacentIndices (Array2D.shape grid) idx width
            hasSymbolNeighbour =
                List.any adjacent (\neighbour -> isSymbol grid neighbour)
            if hasSymbolNeighbour then
                n
            else 
                0
        _ -> 0


expect
    actual = parseStr parseDot "."
    actual == Ok [Dot]

expect
    actual = parseStr parseNumber "123"
    actual == Ok [Number 123 3, Dot, Dot]

expect
    actual = parseStr parseRow ".12+."
    actual == Ok [Dot, Number 12 2, Dot, Symbol, Dot]

expect
    actual = parseStr parseRow ".123."
    actual == Ok [Dot, Number 123 3, Dot, Dot, Dot]

expect
    expected =
        fromRows [[Dot, Number 1 1], [Number 2 1, Symbol]]
    actual = parseStr parseSchematic ".1\n2$"
    actual == Ok expected

expect
    actual = parseStr parseSchematic ".123.\n.123+"
    expected =
        fromRows 
            [ [Dot, Number 123 3, Dot, Dot, Dot],
              [Dot, Number 123 3, Dot, Dot, Symbol],
            ]
    actual == Ok expected

expect
    when parseStr parseSchematic "...\n.1.\n..." is
        Ok grid ->
            actual = value grid { x: 1, y: 1 }
            actual == 0
        _ ->
            false

expect
    when parseStr parseSchematic "+..\n.1.\n..." is
        Ok grid ->
            actual = value grid { x: 1, y: 1 }
            actual == 1
        _ ->
            false

expect
    shape = { dimX: 3, dimY: 3 }
    actual = adjacentIndices shape {x: 0, y:0} 1
    expected =
        [ {x: 1, y: 0 }
        , {x: 0, y: 1 }
        , {x: 1, y: 1 }
        ]
    actual == expected

expect
    shape = { dimX: 3, dimY: 3 }
    actual = adjacentIndices shape {x: 1, y:1} 1
    expected =
        [ {x: 0, y: 0 }
        , {x: 1, y: 0 }
        , {x: 2, y: 0 }
        , {x: 0, y: 1 }
        , {x: 2, y: 1 }
        , {x: 0, y: 2 }
        , {x: 1, y: 2 }
        , {x: 2, y: 2 }
        ]
    actual == expected


expect
    shape = { dimX: 4, dimY: 3 }
    actual = adjacentIndices shape {x: 1, y:1} 2
    expected =
        [ {x: 0, y: 0 }
        , {x: 1, y: 0 }
        , {x: 2, y: 0 }
        , {x: 3, y: 0 }
        , {x: 0, y: 1 }
        , {x: 3, y: 1 }
        , {x: 0, y: 2 }
        , {x: 1, y: 2 }
        , {x: 2, y: 2 }
        , {x: 3, y: 2 }
        ]
    actual == expected

expect
    testGrid = parseStr parseSchematic testInput
    when testGrid is
        Ok grid -> 
           (value grid {x: 0, y: 0}) == 467
        _ -> false
    

