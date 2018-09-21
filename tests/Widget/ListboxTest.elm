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
import Time exposing (Posix)
import Widget.Listbox as Listbox exposing (Effect(..), Listbox(..), Msg(..), UpdateConfig)


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
        [ invariantTest "maybeKeyboardFocus" listboxApp <|
            \_ _ { listbox } ->
                listbox
                    |> data
                    |> .maybeKeyboardFocus
                    |> expectValidOption
        , invariantTest "maybeMouseFocus" listboxApp <|
            \_ _ { listbox } ->
                listbox
                    |> data
                    |> .maybeMouseFocus
                    |> expectValidOption
        , describe "listBlured"
            [ msgTest "keeps keyboardFocus" listboxApp (Fuzz.constant ListBlured) <|
                \before _ after ->
                    expectUnchangedFocus before after
            , msgTest "keeps mouseFocus" listboxApp (Fuzz.constant ListBlured) <|
                \before _ after ->
                    expectUnchangedHover before after
            ]
        , describe "arrowDown"
            [ msgTest "moves keyboardFocus to next option" listboxApp listArrowDownPressed <|
                \before _ after ->
                    let
                        beforeMaybeKeyboardFocus =
                            before.listbox
                                |> data
                                |> .maybeKeyboardFocus

                        afterMaybeKeyboardFocus =
                            after.listbox
                                |> data
                                |> .maybeKeyboardFocus

                        afterMaybeLastSelectedEntry =
                            after.listbox
                                |> data
                                |> .maybeLastSelectedEntry
                    in
                    case ( beforeMaybeKeyboardFocus, afterMaybeKeyboardFocus ) of
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

                        ( Nothing, Just afterKeyboardFocus ) ->
                            case afterMaybeLastSelectedEntry of
                                Nothing ->
                                    afterKeyboardFocus
                                        |> Expect.equal firstOption

                                Just afterLastSelectedEntry ->
                                    afterKeyboardFocus
                                        |> Expect.equal afterLastSelectedEntry

                        ( Just focusBefore, Nothing ) ->
                            case afterMaybeKeyboardFocus of
                                Nothing ->
                                    Expect.fail "Expected the listbox to not loose its keyboardFocus"

                                Just afterKeyboardFocus ->
                                    afterKeyboardFocus
                                        |> Expect.equal focusBefore

                        ( Nothing, Nothing ) ->
                            Expect.fail "Expected the listbox to have a keyboardFocus"
            ]
        , describe "arrowUp"
            [ msgTest "moves keyboardFocus to previous option" listboxApp listArrowUpPressed <|
                \before _ after ->
                    let
                        beforeMaybeKeyboardFocus =
                            before.listbox
                                |> data
                                |> .maybeKeyboardFocus

                        afterMaybeKeyboardFocus =
                            after.listbox
                                |> data
                                |> .maybeKeyboardFocus

                        afterMaybeLastSelectedEntry =
                            after.listbox
                                |> data
                                |> .maybeLastSelectedEntry
                    in
                    case ( beforeMaybeKeyboardFocus, afterMaybeKeyboardFocus ) of
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

                        ( Nothing, Just afterKeyboardFocus ) ->
                            case afterMaybeLastSelectedEntry of
                                Nothing ->
                                    afterKeyboardFocus
                                        |> Expect.equal firstOption

                                Just afterLastSelectedEntry ->
                                    afterKeyboardFocus
                                        |> Expect.equal afterLastSelectedEntry

                        ( Just focusBefore, Nothing ) ->
                            case afterMaybeKeyboardFocus of
                                Nothing ->
                                    Expect.fail "Expected the listbox to not loose its keyboardFocus"

                                Just afterKeyboardFocus ->
                                    afterKeyboardFocus
                                        |> Expect.equal focusBefore

                        ( Nothing, Nothing ) ->
                            Expect.fail "Expected the listbox to have a keyboardFocus"
            ]
        , describe "home"
            [ msgTest "moves keyboardFocus to first option" listboxApp listHomePressed <|
                \_ _ after ->
                    expectFirstOptionFocused after
            ]
        , describe "end"
            [ msgTest "moves keyboardFocus to last option" listboxApp listEndPressed <|
                \_ _ after ->
                    expectLastOptionFocused after
            ]
        , describe "controlA"
            [ msgTest "controlA selects/deselects all options" listboxApp listControlAPressed <|
                \before _ after ->
                    if List.sort before.selection == List.sort options then
                        expectNothingSelected after
                    else
                        expectEverythingSelected after
            ]
        ]



---- EXPECTATIONS


expectValidOption : Maybe String -> Expectation
expectValidOption maybeOption =
    case maybeOption of
        Nothing ->
            Expect.pass

        Just option ->
            options
                |> List.any ((==) option)
                |> Expect.true ("'" ++ option ++ "' is not a valid option")


expectUnchangedFocus : Model -> Model -> Expectation
expectUnchangedFocus before after =
    before.listbox
        |> data
        |> .maybeKeyboardFocus
        |> Expect.equal
            (after.listbox
                |> data
                |> .maybeKeyboardFocus
            )


expectUnchangedHover : Model -> Model -> Expectation
expectUnchangedHover before after =
    before.listbox
        |> data
        |> .maybeMouseFocus
        |> Expect.equal
            (after.listbox
                |> data
                |> .maybeMouseFocus
            )


expectFirstOptionFocused : Model -> Expectation
expectFirstOptionFocused { listbox } =
    listbox
        |> data
        |> .maybeKeyboardFocus
        |> Expect.equal (Just firstOption)


expectLastOptionFocused : Model -> Expectation
expectLastOptionFocused { listbox } =
    listbox
        |> data
        |> .maybeKeyboardFocus
        |> Expect.equal (Just lastOption)


expectNothingSelected : Model -> Expectation
expectNothingSelected { selection } =
    selection
        |> Expect.equalLists []


expectEverythingSelected : Model -> Expectation
expectEverythingSelected { selection } =
    selection
        |> List.sort
        |> Expect.equalLists (List.sort options)



---- SETUP


listboxApp : TestedApp Model (Listbox.Msg String)
listboxApp =
    { model = ConstantModel (Model Listbox.init [] (Time.millisToPosix 0))
    , update = UpdateWithoutCmds update
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
    , now : Posix
    }


update : Listbox.Msg String -> Model -> Model
update msg model =
    let
        ( newListbox, effect, newSelection ) =
            Listbox.internalUpdate updateConfig
                onlyOptions
                msg
                model.listbox
                model.selection

        newModel =
            { model
                | listbox = newListbox
                , selection = newSelection
                , now =
                    model.now
                        |> Time.posixToMillis
                        |> (+) 123
                        |> Time.millisToPosix
            }
    in
    case effect of
        CmdNone ->
            newModel

        TimeNow toMsg ->
            update (toMsg model.now) newModel

        DomSetViewportOf _ _ _ ->
            newModel

        DomFocus targetId ->
            if targetId == Listbox.printListId id then
                update (ListFocused id) newModel
            else
                newModel

        ScrollListToTop toMsg _ ->
            update
                (toMsg
                    { scene = listboxScene
                    , viewport =
                        { x = 0
                        , y = 0
                        , width = 100
                        , height = 80
                        }
                    }
                )
                newModel

        ScrollListToBottom toMsg _ ->
            update
                (toMsg
                    { scene = listboxScene
                    , viewport =
                        { x = 0
                        , y = 30 * toFloat (List.length onlyOptions) - 80
                        , width = 100
                        , height = 80
                        }
                    }
                )
                newModel

        AdjustScrollTop toMsg _ entryId ->
            let
                viewportOfList =
                    { scene = browserScene
                    , viewport =
                        { x = 0
                        , y = 0
                        , width = 100
                        , height = 80
                        }
                    }
            in
            update
                (toMsg
                    viewportOfList
                    elementOfList
                    (elementOfEntry entryId)
                )
                newModel

        AdjustScrollTopNew toMsg _ entryId previousEntryId ->
            let
                viewportOfList =
                    { scene = browserScene
                    , viewport =
                        { x = 0
                        , y = 0
                        , width = 100
                        , height = 80
                        }
                    }
            in
            update
                (toMsg
                    viewportOfList
                    elementOfList
                    (elementOfEntry entryId)
                    (elementOfEntry previousEntryId)
                )
                newModel


id : String
id =
    "listbox"



-- DOM LENGTHS


browserScene =
    { width = 100
    , height = 80
    }


listboxScene =
    { width = 100
    , height = 30 * toFloat (List.length onlyOptions)
    }


browserViewport =
    { x = 0
    , y = 0
    , width = browserScene.width
    , height = browserScene.height
    }


elementOfList =
    { scene = browserScene
    , viewport = browserViewport
    , element = browserViewport
    }


elementOfEntry entryId =
    let
        position =
            options
                |> List.takeWhile ((/=) entryId)
                |> List.length
    in
    { scene = browserScene
    , viewport = browserViewport
    , element =
        { x = 0
        , y = toFloat position * 30
        , width = 100
        , height = 30
        }
    }



---- HELPER


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
