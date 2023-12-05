app "example"
    packages {
        cli: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.3.0/-e3ebWWmlFPfe9fYrr2z1urfslzygbtQQsl69iH1qzQ.tar.br",
    }
    imports [
        "test-input1.txt" as testInput : Str,
        "input1.txt" as input1 : Str,
        cli.Stdout,
        Bool.{ false, true },
        parser.Core.{ Parser, skip, keep, const, map2, sepBy, many, oneOf },
        parser.String.{ parseStr, digits, string, codeunit, codeunitSatisfies },
    ]
    provides [main] to cli

test = solve1 testInput |> Num.toStr
part1 = solve1 input1 |> Num.toStr
part2 = solve2 input1 |> Num.toStr

main =
    Stdout.line "The answer are: test:\(test) part1:\(part1) part2:\(part2)"

solve1 : Str -> Nat
solve1 = \s ->
    when parseStr parseInput s is
        Ok input ->
            List.map input.seeds (\seed -> locationNumber input seed)
            |> List.sortAsc
            |> List.first
            |> Result.withDefault 0

        _ -> 0

expect
    solve1 testInput == 35

values : List (Result a e) -> List a
values = \rs ->
    List.walk rs [] List.appendIfOk

toPairs = \ns ->
    walker = \prev, current ->
        when prev is
            [Err n, ..] -> List.append (List.dropFirst prev 1) (Ok (n, current))
            _ -> List.prepend prev (Err current)
    List.walk ns [] walker |> values

expect
    toPairs [1, 2, 3, 4] == [(1, 2), (3, 4)]

locationNumberForRange : Input, (Nat, Nat) -> Nat
locationNumberForRange = \input, (from, len) ->
    walker = \smallest, current ->
        if smallest == 0 || current < smallest then
            current
        else
            smallest
    List.range { start: At from, end: Before (from + len) }
    |> List.map (\seed -> locationNumber input seed)
    |> List.walk 0 walker

solve2 : Str -> Nat
solve2 = \s ->
    when parseStr parseInput s is
        Ok input ->
            toPairs input.seeds
            |> List.map (\range -> locationNumberForRange input range)
            |> List.sortAsc
            |> List.first
            |> Result.withDefault 0

        _ -> 0

expect
    dbg
        solve2 testInput

    solve2 testInput == 46

Parse a : Parser (List U8) a

Seeds : List Nat

parseSeeds : Parse Seeds
parseSeeds =
    const (\ds -> ds)
    |> skip (string "seeds: ")
    |> keep (sepBy digits (string " "))
    |> skip (string "\n\n")

expect
    parseStr parseSeeds "seeds: 79 14 55 13\n\n" == Ok [79, 14, 55, 13]

Map : { name : Str, mappings : List MapRange }

inRange : Nat -> (MapRange -> Bool)
inRange = \src -> \{ srcStart, len } ->
        src >= srcStart && src < (srcStart + len)

lookup : Nat, Map -> Nat
lookup = \src, { mappings } ->
    range = List.findFirst mappings (inRange src)
    when range is
        Ok { srcStart, dstStart } -> dstStart + (src - srcStart)
        _ -> src

expect
    map = {
        name: "seed-to-soil",
        mappings: [
            { dstStart: 50, srcStart: 98, len: 2 },
            { dstStart: 52, srcStart: 50, len: 48 },
        ],
    }
    expected = [
        (0, 0),
        (1, 1),
        (48, 48),
        (49, 49),
        (50, 52),
        (51, 53),
        (96, 98),
        (97, 99),
        (98, 50),
        (99, 51),
    ]
    actual = List.map expected (\(i, _) -> (i, lookup i map))
    actual == expected

locationNumber : Input, Nat -> Nat
locationNumber = \{ maps }, seed ->
    walker : Nat, Map -> Nat
    walker = \currentSeed, currentMap ->
        lookup currentSeed currentMap
    List.walk maps seed walker

expect
    when parseStr parseInput testInput is
        Ok inp ->
            locationNumber inp 79 == 82

        _ ->
            false

MapRange : {
    srcStart : Nat,
    dstStart : Nat,
    len : Nat,
}

parseMapName : Parse Str
parseMapName =
    const (\cs -> Str.fromUtf8 cs |> Result.withDefault "utf8 error")
    |> keep (many (oneOf [codeunit ' ', codeunit '-', codeunitSatisfies isLowerAscii]))

isLowerAscii : U8 -> Bool
isLowerAscii = \c -> c > 96 && c < 123

expect
    parseStr parseMapName "foo-bar" == Ok "foo-bar"

parseMapHeader : Parse Str
parseMapHeader =
    const (\s -> s)
    |> keep parseMapName
    |> skip (string ":\n")

expect
    parseStr parseMapHeader "a-ha:\n" == Ok "a-ha"

parseMapRange : Parse MapRange
parseMapRange =
    const (\d -> \s -> \l -> { srcStart: s, dstStart: d, len: l })
    |> keep digits
    |> skip (codeunit ' ' |> many)
    |> keep digits
    |> skip (codeunit ' ' |> many)
    |> keep digits

expect
    parseStr parseMapRange "1 42 11" == Ok { dstStart: 1, srcStart: 42, len: 11 }

parseMap : Parse Map
parseMap =
    map2
        parseMapHeader
        (sepBy parseMapRange (codeunit '\n'))
        (\name, ms -> { name: name, mappings: ms })

expect
    mapSrc = "foo:\n1 2 3\n4 5 6"
    m1 = { dstStart: 1, srcStart: 2, len: 3 }
    m2 = { dstStart: 4, srcStart: 5, len: 6 }
    expected = {
        name: "foo",
        mappings: [m1, m2],
    }
    actual = parseStr parseMap mapSrc
    actual == Ok expected

parseMaps : Parse (List Map)
parseMaps =
    sepBy parseMap (string "\n\n")

expect
    mapSrc = "foo:\n1 2 3\n4 5 6"
    m1 = { dstStart: 1, srcStart: 2, len: 3 }
    m2 = { dstStart: 4, srcStart: 5, len: 6 }
    expected : Map
    expected = {
        name: "foo",
        mappings: [m1, m2],
    }
    actual = parseStr parseMaps mapSrc
    when actual is
        Ok maps ->
            maps == [expected]

        Err e ->
            dbg
                e

            false

expect
    mapSrc = "foo map:\n1 2 3\n4 5 6"
    m1 = { dstStart: 1, srcStart: 2, len: 3 }
    m2 = { dstStart: 4, srcStart: 5, len: 6 }
    expected : Map
    expected = {
        name: "foo map",
        mappings: [m1, m2],
    }
    mapsSrc = "\(mapSrc)\n\n\(mapSrc)"
    actual = parseStr parseMaps mapsSrc
    when actual is
        Ok maps ->
            maps == [expected, expected]

        Err _ ->
            false

Input : { seeds : Seeds, maps : List Map }

parseInput : Parse Input
parseInput =
    map2
        parseSeeds
        (parseMaps |> skip (codeunit '\n'))
        (\s, ms -> { seeds: s, maps: ms })

expect
    example =
        """
        seeds: 79 14 55 13

        seed-to-soil map:
        50 98 2
        52 50 48

        seed-to-soil map:
        50 98 2
        52 50 48

        """
    when parseStr parseInput example is
        Ok actual ->
            m1 = { dstStart: 50, srcStart: 98, len: 2 }
            m2 = { dstStart: 52, srcStart: 50, len: 48 }
            expectedMap = {
                name: "seed-to-soil map",
                mappings: [m1, m2],
            }
            expected = {
                seeds: [79, 14, 55, 13],
                maps: [expectedMap, expectedMap],
            }
            actual == expected

        e ->
            dbg
                e

            false

expect
    when parseStr parseInput testInput is
        Ok _ ->
            true

        _ ->
            false
