module Widget.ListboxTest exposing (suite)

import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (..)
import Widget.Listbox as Listbox exposing (Entry, Listbox, UpdateConfig)


suite : Test
suite =
    concat
        [ describe "initial Listbox"
            [ fuzz entriesFuzzer "has no hoveredEntry" <|
                Listbox.hoveredEntry updateConfig Listbox.init
                    >> Expect.equal Nothing
            ]
        , describe "focusedEntry"
            [ fuzz entriesFuzzer "on initial Listbox" <|
                Listbox.focusedEntry updateConfig Listbox.init
                    >> Expect.equal Nothing
            , fuzz entriesWithKnownOptionFuzzer "after focusEntry" <|
                \{ knownOption, entries } ->
                    let
                        selection =
                            []
                    in
                    Listbox.focusEntry updateConfig knownOption Listbox.init selection
                        |> Tuple.mapFirst (focusedEntry entries)
                        |> Expect.equal ( Just knownOption, [] )
            , describe "after focusNextOrFirstEntry"
                [ fuzz2 Fuzz.string entriesFuzzer "on initial Listbox" <|
                    \firstOption entries ->
                        let
                            selection =
                                []

                            actualEntries =
                                Listbox.option firstOption :: entries
                        in
                        Listbox.focusNextOrFirstEntry updateConfig
                            actualEntries
                            Listbox.init
                            selection
                            |> Tuple.mapFirst (focusedEntry actualEntries)
                            |> Expect.equal ( Just firstOption, [] )
                ]
            , describe "after focusPreviousOrFirstEntry"
                [ fuzz2 Fuzz.string entriesFuzzer "on initial Listbox" <|
                    \firstOption entries ->
                        let
                            selection =
                                []

                            actualEntries =
                                Listbox.option firstOption :: entries
                        in
                        Listbox.focusPreviousOrFirstEntry updateConfig
                            actualEntries
                            Listbox.init
                            selection
                            |> Tuple.mapFirst (focusedEntry actualEntries)
                            |> Expect.equal ( Just firstOption, [] )
                ]
            ]
        ]



---- HELPER


focusedEntry : List (Entry String divider) -> Listbox -> Maybe String
focusedEntry entries listbox =
    Listbox.focusedEntry updateConfig listbox entries



---- FIXTURES


updateConfig : UpdateConfig String
updateConfig =
    let
        hashEntry =
            identity
    in
    Listbox.updateConfig hashEntry
        { jumpAtEnds = True
        , separateFocus = True
        , selectionFollowsFocus = False
        , handleHomeAndEnd = True
        , typeAhead = Listbox.noTypeAhead
        , minimalGap = 0
        , initialGap = 0
        }



---- FUZZERS


entriesWithKnownOptionFuzzer :
    Fuzzer
        { knownOption : String
        , entries : List (Entry String String)
        }
entriesWithKnownOptionFuzzer =
    Fuzz.map2
        (\knownOption entries ->
            let
                front =
                    List.take (entriesCount // 2) entries

                back =
                    List.drop (entriesCount // 2) entries

                entriesCount =
                    List.length entries
            in
            { knownOption = knownOption
            , entries = front ++ Listbox.option knownOption :: back
            }
        )
        Fuzz.string
        entriesFuzzer


entriesFuzzer : Fuzzer (List (Entry String String))
entriesFuzzer =
    Fuzz.list entryFuzzer


entryFuzzer : Fuzzer (Entry String String)
entryFuzzer =
    Fuzz.frequency
        [ ( 1, dividerFuzzer )
        , ( 10, optionFuzzer )
        ]


optionFuzzer : Fuzzer (Entry String divider)
optionFuzzer =
    Fuzz.string
        |> Fuzz.map Listbox.option


dividerFuzzer : Fuzzer (Entry a String)
dividerFuzzer =
    Fuzz.string
        |> Fuzz.map Listbox.divider
