module Widget.ListboxTest exposing (suite)

import ArchitectureTest
    exposing
        ( TestedApp
        , TestedModel(..)
        , TestedUpdate(..)
        , invariantTest
        , msgTest
        , msgTestWithPrecondition
        )
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Internal.Entries exposing (Entry(..))
import List.Extra as List
import Test exposing (..)
import Widget.Listbox as Listbox exposing (Listbox(..), Msg(..), UpdateConfig)


suite : Test
suite =
    concat
        [ architectureTest
        , describe "initial Listbox"
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
                    \option entries ->
                        let
                            selection =
                                []

                            actualEntries =
                                Listbox.option option :: entries
                        in
                        Listbox.focusNextOrFirstEntry updateConfig
                            actualEntries
                            Listbox.init
                            selection
                            |> Tuple.mapFirst (focusedEntry actualEntries)
                            |> Expect.equal ( Just option, [] )
                ]
            , describe "after focusPreviousOrFirstEntry"
                [ fuzz2 Fuzz.string entriesFuzzer "on initial Listbox" <|
                    \option entries ->
                        let
                            selection =
                                []

                            actualEntries =
                                Listbox.option option :: entries
                        in
                        Listbox.focusPreviousOrFirstEntry updateConfig
                            actualEntries
                            Listbox.init
                            selection
                            |> Tuple.mapFirst (focusedEntry actualEntries)
                            |> Expect.equal ( Just option, [] )
                ]
            ]
        ]


architectureTest : Test
architectureTest =
    concat
        [ invariantTest "maybeKeyboardFocus"
            listboxApp
            (\_ _ { listbox } ->
                case .maybeKeyboardFocus (data listbox) of
                    Nothing ->
                        Expect.pass

                    Just keyboardFocus ->
                        options
                            |> List.member keyboardFocus
                            |> Expect.true
                                ("Expected '" ++ keyboardFocus ++ "' to be a valid option")
            )
        , invariantTest "maybeMouseFocus"
            listboxApp
            (\_ _ { listbox } ->
                case .maybeMouseFocus (data listbox) of
                    Nothing ->
                        Expect.pass

                    Just mouseFocus ->
                        options
                            |> List.member mouseFocus
                            |> Expect.true
                                ("Expected '" ++ mouseFocus ++ "' to be a valid option")
            )
        , msgTestWithPrecondition "arrowDown moves keyboardFocus to next option"
            listboxApp
            listArrowDownPressed
            noPendingKeyboardFocus
            (\before _ after ->
                let
                    beforeMaybeKeyboardFocus =
                        before.listbox
                            |> data
                            |> .maybeKeyboardFocus

                    afterMaybePendingKeyboardFocus =
                        after.listbox
                            |> data
                            |> .maybePendingKeyboardFocus

                    afterMaybeKeyboardFocus =
                        after.listbox
                            |> data
                            |> .maybeKeyboardFocus
                in
                case ( beforeMaybeKeyboardFocus, afterMaybePendingKeyboardFocus ) of
                    ( Just focusBefore, Just focusAfter ) ->
                        let
                            maybeNextOption =
                                options
                                    |> List.splitWhen ((==) focusBefore)
                                    |> Maybe.andThen (Tuple.second >> List.drop 1 >> List.head)
                        in
                        case maybeNextOption of
                            Nothing ->
                                focusAfter
                                    |> Expect.equal focusBefore

                            Just nextOption ->
                                nextOption
                                    |> Expect.equal focusAfter

                    ( Nothing, Just afterPendingKeyboardFocus ) ->
                        afterPendingKeyboardFocus
                            |> Expect.equal firstOption

                    ( Just focusBefore, Nothing ) ->
                        case afterMaybeKeyboardFocus of
                            Nothing ->
                                Expect.fail "Expected the listbox to not loose its keyboardFocus"

                            Just afterKeyboardFocus ->
                                afterKeyboardFocus
                                    |> Expect.equal focusBefore

                    ( Nothing, Nothing ) ->
                        Expect.fail "Expected the listbox to have a keyboardFocus"
            )
        , msgTestWithPrecondition "arrowUp moves keyboardFocus to previous option"
            listboxApp
            listArrowUpPressed
            noPendingKeyboardFocus
            (\before _ after ->
                let
                    beforeMaybeKeyboardFocus =
                        before.listbox
                            |> data
                            |> .maybeKeyboardFocus

                    afterMaybePendingKeyboardFocus =
                        after.listbox
                            |> data
                            |> .maybePendingKeyboardFocus

                    afterMaybeKeyboardFocus =
                        after.listbox
                            |> data
                            |> .maybeKeyboardFocus
                in
                case ( beforeMaybeKeyboardFocus, afterMaybePendingKeyboardFocus ) of
                    ( Just focusBefore, Just focusAfter ) ->
                        let
                            maybePreviousOption =
                                options
                                    |> List.splitWhen ((==) focusBefore)
                                    |> Maybe.andThen (Tuple.first >> List.reverse >> List.head)
                        in
                        case maybePreviousOption of
                            Nothing ->
                                focusAfter
                                    |> Expect.equal focusBefore

                            Just previousOption ->
                                previousOption
                                    |> Expect.equal focusAfter

                    ( Nothing, Just afterPendingKeyboardFocus ) ->
                        afterPendingKeyboardFocus
                            |> Expect.equal firstOption

                    ( Just focusBefore, Nothing ) ->
                        case afterMaybeKeyboardFocus of
                            Nothing ->
                                Expect.fail "Expected the listbox to not loose its keyboardFocus"

                            Just afterKeyboardFocus ->
                                afterKeyboardFocus
                                    |> Expect.equal focusBefore

                    ( Nothing, Nothing ) ->
                        Expect.fail "Expected the listbox to have a keyboardFocus"
            )
        , msgTestWithPrecondition "arrowHome moves keyboardFocus to first option"
            listboxApp
            listHomePressed
            noPendingKeyboardFocus
            (\_ _ after ->
                after.listbox
                    |> data
                    |> .maybePendingKeyboardFocus
                    |> Expect.equal (Just firstOption)
            )
        , msgTestWithPrecondition "arrowEnd moves keyboardFocus to first option"
            listboxApp
            listEndPressed
            noPendingKeyboardFocus
            (\_ _ after ->
                after.listbox
                    |> data
                    |> .maybePendingKeyboardFocus
                    |> Expect.equal (Just lastOption)
            )
        , msgTest "controlA selects/deselects all options"
            listboxApp
            listControlAPressed
            (\before _ after ->
                if List.sort before.selection == List.sort options then
                    after.selection
                        |> Expect.equalLists []
                else
                    after.selection
                        |> List.sort
                        |> Expect.equalLists (List.sort options)
            )
        ]



-- SETUP


listboxApp : TestedApp Model (Listbox.Msg String)
listboxApp =
    { model = ConstantModel (Model Listbox.init [])
    , update = NormalUpdate update
    , msgFuzzer = msgFuzzer
    , msgToString = Debug.toString
    , modelToString =
        \{ listbox, selection } ->
            let
                (Listbox d) =
                    listbox
            in
            String.join "\n"
                [ "listbox ="
                , "      { preventScroll  = "
                    ++ Debug.toString d.preventScroll
                    ++ ",    maybeKeyboardFocus        = "
                    ++ Debug.toString d.maybeKeyboardFocus
                , "      , query          = "
                    ++ Debug.toString d.query
                    ++ ",  maybePendingKeyboardFocus = "
                    ++ Debug.toString d.maybePendingKeyboardFocus
                , "      , ulScrollTop    = "
                    ++ Debug.toString d.ulScrollTop
                    ++ ",        maybeMouseFocus           = "
                    ++ Debug.toString d.maybeMouseFocus
                , "      , ulClientHeight = "
                    ++ Debug.toString d.ulClientHeight
                    ++ ",     maybeLastSelectedEntry    = "
                    ++ Debug.toString d.maybeLastSelectedEntry
                , "      }"
                , "\n    selection ="
                , "      " ++ Debug.toString selection
                ]
    }


type alias Model =
    { listbox : Listbox
    , selection : List String
    }


update : Listbox.Msg String -> Model -> ( Model, Cmd (Listbox.Msg String) )
update msg model =
    let
        ( newListbox, listboxCmd, newSelection ) =
            Listbox.update updateConfig
                onlyOptions
                msg
                model.listbox
                model.selection
    in
    ( { model
        | listbox = newListbox
        , selection = newSelection
      }
    , listboxCmd
    )


id : String
id =
    "listbox"



---- HELPER


expectValidOption : List (Entry String String) -> String -> Expectation
expectValidOption entries option =
    entries
        |> List.any (\entry -> entry == Option option)
        |> Expect.true ("'" ++ option ++ "' is not a valid option")


noPendingKeyboardFocus : Model -> Bool
noPendingKeyboardFocus { listbox } =
    listbox
        |> data
        |> .maybePendingKeyboardFocus
        |> (==) Nothing


focusedEntry : List (Entry String divider) -> Listbox -> Maybe String
focusedEntry entries listbox =
    Listbox.focusedEntry updateConfig listbox entries


data (Listbox d) =
    d



---- FIXTURES


updateConfig : UpdateConfig String
updateConfig =
    let
        hashEntry =
            identity
    in
    Listbox.updateConfig hashEntry
        { jumpAtEnds = False
        , separateFocus = True
        , selectionFollowsFocus = False
        , handleHomeAndEnd = True
        , typeAhead = Listbox.noTypeAhead
        , minimalGap = 0
        , initialGap = 0
        }



-- ENTRIES


options : List String
options =
    List.concat
        [ [ firstOption ]
        , List.range 0 42
            |> List.map String.fromInt
            |> List.map ((++) "option-")
        , [ lastOption ]
        ]


firstOption : String
firstOption =
    "first-option"


lastOption : String
lastOption =
    "last-option"


onlyOptions : List (Entry String divider)
onlyOptions =
    List.map Listbox.option options



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



-- MSG


listArrowDownPressed : Fuzzer (Listbox.Msg String)
listArrowDownPressed =
    Fuzz.constant (ListArrowDownPressed id False)


listArrowUpPressed : Fuzzer (Listbox.Msg String)
listArrowUpPressed =
    Fuzz.constant (ListArrowUpPressed id False)


listHomePressed : Fuzzer (Listbox.Msg String)
listHomePressed =
    Fuzz.constant (ListHomePressed id)


listEndPressed : Fuzzer (Listbox.Msg String)
listEndPressed =
    Fuzz.constant (ListEndPressed id)


listControlAPressed : Fuzzer (Listbox.Msg String)
listControlAPressed =
    Fuzz.constant ListControlAPressed


msgFuzzer : Fuzzer (Listbox.Msg String)
msgFuzzer =
    Fuzz.frequency
        [ -- LIST
          ( 1, Fuzz.constant ListMouseDown )
        , ( 1, Fuzz.constant ListMouseUp )
        , ( 1, Fuzz.constant (ListFocused id) )
        , ( 1, Fuzz.constant ListBlured )
        , ( 10, Fuzz.map (ListArrowUpPressed id) Fuzz.bool )
        , ( 10, Fuzz.map (ListArrowDownPressed id) Fuzz.bool )
        , ( 1, Fuzz.constant (ListEnterPressed id) )
        , ( 1, Fuzz.constant (ListSpacePressed id) )
        , ( 1, Fuzz.constant (ListShiftSpacePressed id) )
        , ( 1, Fuzz.constant (ListHomePressed id) )
        , ( 1, Fuzz.constant (ListControlShiftHomePressed id) )
        , ( 1, Fuzz.constant (ListEndPressed id) )
        , ( 1, Fuzz.constant (ListControlShiftEndPressed id) )
        , ( 1, Fuzz.constant ListControlAPressed )

        -- QUERY
        , ( 1
          , "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                |> String.toList
                |> List.map (String.fromChar >> Fuzz.constant)
                |> Fuzz.oneOf
                |> Fuzz.map (ListKeyPressed id)
          )

        -- ENTRY
        , ( 1
          , options
                |> List.map Fuzz.constant
                |> Fuzz.oneOf
                |> Fuzz.map EntryMouseEntered
          )
        , ( 1, Fuzz.constant EntryMouseLeft )
        , ( 10
          , options
                |> List.map Fuzz.constant
                |> Fuzz.oneOf
                |> Fuzz.map EntryClicked
          )
        ]
