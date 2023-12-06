app "example"
    packages {
        cli: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.3.0/-e3ebWWmlFPfe9fYrr2z1urfslzygbtQQsl69iH1qzQ.tar.br",
    }
    imports [
        "test-input1.txt" as testInput : Str,
        "input1.txt" as input1 : Str,
        "input2.txt" as input2 : Str,
        cli.Stdout,
        Bool.{ false, true },
        parser.Core.{ Parser, skip, keep, const, map2, sepBy, many, oneOf },
        parser.String.{ parseStr, digits, string, codeunit, codeunitSatisfies },
    ]
    provides [main] to cli

test = solve1 testInput |> Num.toStr
part1 = solve1 input1 |> Num.toStr
part2 = solve2 |> Num.toStr

main =
    Stdout.line "The answer are: test:\(test) part1:\(part1) part2:\(part2)"

races : Input -> List (Nat, Nat)
races = \{ times, distances } ->
    List.map2 times distances (\t, d -> (t, d))

solve1 : Str -> Nat
solve1 = \s ->
    when parseStr parseInput s is
        Ok input ->
            races input
            |> List.map waysToBeat
            |> List.map List.len
            |> List.product

        _ -> 0

expect
    solve1 testInput == 288

solve2 : Nat
solve2 =
    when parseStr parseInput input2 is
        Ok input ->
            when (input.times, input.distances) is
                ([time], [distance]) ->
                    waysToBeat (time, distance)
                    |> List.len

                _ -> 0

        _ -> 0

Parse a : Parser (List U8) a

Seeds : List Nat

parseLine : Str -> Parse Seeds
parseLine = \s ->
    const (\ds -> ds)
    |> skip (string s)
    |> skip (string ":")
    |> skip spaces
    |> keep (sepBy digits spaces)

spaces = many (codeunit ' ')

parseTimes = parseLine "Time"
parseDistances = parseLine "Distance"

Input : { times : List Nat, distances : List Nat }

parseInput : Parse Input
parseInput =
    const (\ts -> \ds -> { times: ts, distances: ds })
    |> keep parseTimes
    |> skip (codeunit '\n')
    |> keep parseDistances
    |> skip (many (codeunit '\n'))

expect
    expected = {
        times: [7, 15, 30],
        distances: [9, 40, 200],
    }
    parseStr parseInput testInput == Ok expected

allSpeeds : Nat -> List Nat
allSpeeds = \time ->
    List.range { start: At 1, end: Before time }

calcDistance : Nat -> (Nat -> (Nat, Nat))
calcDistance = \max -> \speed ->
        travelTime = max - speed
        (speed, travelTime * speed)

waysToBeat : (Nat, Nat) -> List Nat
waysToBeat = \(time, bestDistance) ->
    speeds = allSpeeds time
    List.map speeds (calcDistance time)
    |> List.keepIf (\(_, d) -> d > bestDistance)
    |> List.map (\(speed, _) -> speed)

expect
    dbg
        waysToBeat (7, 9)

    waysToBeat (7, 9) == [2, 3, 4, 5]

