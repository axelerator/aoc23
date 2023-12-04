app "example"
    packages {
        cli: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.3.0/-e3ebWWmlFPfe9fYrr2z1urfslzygbtQQsl69iH1qzQ.tar.br",
    }
    imports [
        "test-input1.txt" as testInput : Str,
        "input1.txt" as input1 : Str,
        cli.Stdout,
        Bool.{ false },
        parser.Core.{ Parser, skip, keep, const,  map3, sepBy, many },
        parser.String.{ parseStr, digits, string },
    ]
    provides [main] to cli

test = solve1 testInput |> Num.toStr
part1 = solve1 input1 |> Num.toStr
part2 = solve2 input1 |> Num.toStr

main =
    Stdout.line "The answer are: test:\(test) part1:\(part1) part2:\(part2)"

solve1 : Str -> Nat
solve1 = \s ->
    when parseStr parseCards s is
        Ok cards ->
            List.map cards points
            |> List.sum

        _ -> 0

solve2 : Str -> Nat
solve2 = \s ->
    when parseStr parseCards s is
        Ok cards ->
            init = List.map cards (\{ id } -> (id, 1)) |> Dict.fromList
            walker = \ocs, card ->
                nextN = matches card |> List.len
                nextIds =
                    if nextN > 0 then
                        List.range { start: At (card.id + 1), end: At (card.id + nextN) }
                    else
                        []
                myOccurrences = Dict.get ocs card.id |> Result.withDefault 0
                addCards = \ocss, id ->
                    beforeCount = Dict.get ocss id |> Result.withDefault 0
                    Dict.insert ocss id (beforeCount + myOccurrences)
                List.walk nextIds ocs addCards

            occurrences = List.walk cards init walker
            Dict.values occurrences |> List.sum

        _ -> 0

Parse a : Parser (List U8) a

CardId : Nat

parseCardId : Parse CardId
parseCardId =
    const (\n -> n)
    |> skip (string "Card")
    |> skip (many (string " "))
    |> keep digits
    |> skip (string ":")
    |> skip (many (string " "))

parseNumbers : Parse (List Nat)
parseNumbers = sepBy digits (many (string " "))

Card : {
    id : CardId,
    winning : Set Nat,
    actual : List Nat,
}

mkCard : CardId, List Nat, List Nat -> Card
mkCard = \id, won, act -> { id: id, winning: Set.fromList won, actual: act }

parseCard : Parse Card
parseCard =
    map3
        parseCardId
        (parseNumbers |> skip (string " |") |> skip (many (string " ")))
        parseNumbers
        mkCard

parseCards : Parse (List Card)
parseCards =
    sepBy parseCard (string "\n")
    |> skip (many (string "\n"))

matches : Card -> List Nat
matches = \{ winning, actual } ->
    List.keepIf actual (\n -> Set.contains winning n)

points : Card -> Nat
points = \card ->
    len = List.len (matches card)
    if len > 0 then
        Num.powInt 2 (len - 1)
    else
        0

expect
    (parseStr parseCardId "Card 1: ") == Ok 1

expect
    (parseStr parseNumbers "1 12 3") == Ok [1, 12, 3]

expect
    (parseStr parseCard "Card 1: 12  3 | 14  5") == Ok ({ id: 1, winning: Set.fromList [12, 3], actual: [14, 5] })

expect
    (parseStr parseCard "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53")
    ==
    Ok ({ id: 1, winning: Set.fromList [41, 48, 83, 86, 17], actual: [83, 86, 6, 31, 17, 9, 48, 53] })
expect
    (parseStr parseCard "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1")
    ==
    Ok ({ id: 3, winning: Set.fromList [1, 21, 53, 59, 44], actual: [69, 82, 63, 72, 16, 21, 14, 1] })
expect
    line1 = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
    line2 = "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
    (parseStr parseCards "\(line1)\n\(line2)")
    ==
    Ok [
        { id: 1, winning: Set.fromList [41, 48, 83, 86, 17], actual: [83, 86, 6, 31, 17, 9, 48, 53] },
        { id: 3, winning: Set.fromList [1, 21, 53, 59, 44], actual: [69, 82, 63, 72, 16, 21, 14, 1] },
    ]
expect
    when parseStr parseCards testInput is
        Ok cs ->
            List.len cs == 6

        Err _ ->
            false

expect
    when parseStr parseCards input1 is
        Ok cs ->
            List.len cs == 202

        Err e ->
            dbg
                e

            false
expect
    when parseStr parseCard "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53" is
        Ok card ->
            (matches card) == [83, 86, 17, 48]

        _ -> false

expect
    when parseStr parseCard "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53" is
        Ok card ->
            (points card) == 8

        _ -> false
expect
    solve1 testInput == 13

expect
    solve2 testInput == 30
