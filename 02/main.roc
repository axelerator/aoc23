app "example"
    packages {
        cli: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.2.0/dJQSsSmorujhiPNIvJKlQoI92RFIG_JQwUfIxZsCSwE.tar.br",
    }
    imports [
        "test-input1.txt" as testInput : Str,
        "input1.txt" as actualInput : Str,
        cli.Stdout,
        Bool.{true, false},
        parser.Core.{ Parser, skip, keep, const, oneOf, map2, sepBy},
        parser.String.{ parseStr, digits, string },
    ]
    provides [main] to cli

test = solve testInput |> Num.toStr
part1 = solve actualInput |> Num.toStr
part2 = solve2 actualInput |> Num.toStr
main =
  Stdout.line "The answer are: test:\(test) part1:\(part1) part2:\(part2)"

solve : Str -> Nat
solve = \s -> 
    config =
        Dict.fromList [
            (Red, 12),
            (Green, 13),
            (Blue, 14)
        ]

    parseGames s
        |> List.keepIf (\game -> isPossible config game)
        |> List.map (\{gameId} -> gameId)
        |> List.sum


solve2 : Str -> Nat
solve2 = \s -> 
    parseGames s
        |> List.map power
        |> List.sum

Game : { 
    gameId: Nat,
    draws: List Draws 
}

ColorCount : Dict Color Nat

isPossible : ColorCount, Game -> Bool
isPossible = \cfg, game ->
    walker = 
        \stillGood, color, amount ->
            when Dict.get cfg color is
                Ok max -> stillGood && amount <= max
                _ -> false

    Dict.walk
        (maxOfGame game)
        true
        walker

power : Game -> Nat
power = \game ->
    Dict.values (maxOfGame game)
        |> List.product

parseGames : Str -> List Game
parseGames = \s ->
    result = List.map (lines s) (\line -> parseStr gameParser line)

    values result


maxOfDraw : ColorCount,Draws -> ColorCount
maxOfDraw =
    \init,draws -> 
        walker : ColorCount, SingleDraw -> ColorCount
        walker =
            \colorCount, {color, amount} ->
                replace =
                    when Dict.get colorCount color is
                        Err _ -> true
                        Ok n -> n < amount
                if replace then
                    Dict.insert colorCount color amount 
                else
                    colorCount

        List.walk draws init walker

maxOfGame : Game -> ColorCount
maxOfGame = \{draws} ->
    List.walk draws emptyColors maxOfDraw

emptyColors : ColorCount
emptyColors = 
    Dict.fromList 
        [ (Red, 0),
          (Green, 0),
          (Blue, 0)
        ]


Color : [Red, Green, Blue]
SingleDraw : { color: Color, amount: Nat }
Draws : List SingleDraw

Parse a : Parser (List U8) a

singleNumber : Parse Nat
singleNumber =
    const (\n -> n)
    |> skip (string "Game ")
    |> keep (digits)
    |> skip (string ":")

# Parse a number followed by a newline
gameNumberParser : Parse Nat
gameNumberParser =
    const (\n -> n)
    |> skip (string "Game ")
    |> keep (digits)
    |> skip (string ": ")

parseBlue : Parse Color
parseBlue =
    const Blue
    |> skip (string "blue")

parseRed : Parse Color
parseRed =
    const Red
    |> skip (string "red")

parseGreen : Parse Color
parseGreen =
    const Green
    |> skip (string "green")

parseColor : Parse Color
parseColor =
    oneOf [parseRed, parseBlue, parseGreen]


parseColorWithSpace : Parse Color
parseColorWithSpace =
    const (\c -> c)
        |> skip (string " ")
        |> keep parseColor

singleDrawParser : Parse SingleDraw
singleDrawParser =
     map2 
        digits 
        parseColorWithSpace 
        (\a,b -> { color: b, amount: a } )

multiDrawParser : Parse (List SingleDraw)
multiDrawParser =
    sepBy
        singleDrawParser
        (string ", ")
    
fullDrawParser : Parse (List Draws)
fullDrawParser = 
    sepBy
        multiDrawParser
        (string "; ")

gameParser : Parse Game
gameParser =
    map2 
        gameNumberParser 
        fullDrawParser 
        (\gameId, draws -> { gameId: gameId, draws: draws } )
lines : Str -> List Str
lines = \s -> Str.split s "\n"

values : List (Result a err) -> List a
values = \rs ->
  List.walk rs [] List.appendIfOk



expect
    actual = parseStr singleNumber "Game 1000:"
    actual == Ok 1000

expect
    actual = parseStr parseRed "red"
    actual == Ok Red

expect
    actual = parseStr parseColor "blue"
    actual == Ok Blue

expect
    actual = parseStr parseColorWithSpace " blue"
    actual == Ok Blue

expect
    actual = parseStr singleDrawParser "42 blue"
    actual == Ok { color: Blue, amount: 42 }

expect
    actual = parseStr multiDrawParser "42 blue"
    actual == Ok [{ color: Blue, amount: 42 }]


expect
    actual = parseStr multiDrawParser "42 blue, 23 red"
    actual == Ok [{ color: Blue, amount: 42 }, { color: Red, amount: 23 }]

expect
    actual = parseStr fullDrawParser "42 blue, 23 red; 4711 red"
    actual == Ok [
        [{ color: Blue, amount: 42 }, { color: Red, amount: 23 }],
        [{ color: Red, amount: 4711 }]
        ]


expect
    actual = parseStr gameParser "Game 1: 3 blue, 4 red; 1 green"
    expectedGame : Game
    expectedGame = {
        gameId: 1,
        draws: [
            [{ color: Blue, amount: 3 }, { color: Red, amount: 4 }],
            [{ color: Green, amount: 1 }]
            ]
    }
    actual == Ok expectedGame

expect
    game = {
        gameId: 1,
        draws: [
            [{ color: Blue, amount: 3 }, { color: Red, amount: 4 }],
            [{ color: Green, amount: 1 }]
            ]
    }
    expected = Dict.fromList [(Blue, 3), (Red, 4), (Green, 1)]
    actual = maxOfGame game
    actual == expected

expect
    game = {
        gameId: 1,
        draws: [
            [{ color: Blue, amount: 3 }, { color: Red, amount: 4 }],
            [{ color: Green, amount: 1 }]
            ]
    }
    config = Dict.fromList [(Blue, 5), (Red, 5), (Green, 5)]

    isPossible config game

expect
    game = {
        gameId: 1,
        draws: [
            [{ color: Blue, amount: 3 }, { color: Red, amount: 4 }],
            [{ color: Green, amount: 1 }]
            ]
    }
    config = Dict.fromList [(Blue, 1), (Red, 1), (Green, 1)]
    !(isPossible config game)

expect 
    s = "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
    expected = {
        gameId: 3,
        draws: [
            [ { color: Green, amount: 8 }, { color: Blue, amount: 6 }, { color: Red, amount: 20 }],
            [ { color: Blue, amount: 5 }, { color: Red, amount: 4 }, { color: Green, amount: 13 } ],
            [ { color: Green, amount: 5 }, { color: Red, amount: 1 } ],
            ]
    }
    actual = parseStr gameParser s
    actual == Ok expected

expect
    # Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
    game = {
        gameId: 3,
        draws: [
            [ { color: Red, amount: 20 }],
            [ { color: Blue, amount: 5 }, { color: Red, amount: 4 }, { color: Green, amount: 13 } ],
            [ { color: Green, amount: 5 }, { color: Red, amount: 1 } ],
            ]
    }
    config =
        Dict.fromList [
            (Red, 12),
            (Green, 13),
            (Blue, 14)
        ]

    !(isPossible config game)

