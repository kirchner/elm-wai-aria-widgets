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
import Internal.Listbox as Listbox
    exposing
        ( Effect(..)
        , Entry
        , Listbox
        , Msg(..)
        , UpdateConfig
        )
import List.Extra as List
import Test exposing (..)
import Time exposing (Posix)
import Widget.Listbox


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
                                Widget.Listbox.option option :: entries
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
                                Widget.Listbox.option option :: entries
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
                listbox.focus
                    |> expectValidFocus
        , invariantTest "hover" listboxApp <|
            \_ _ { listbox } ->
                listbox.hover
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
            [ msgTest "moves keyboardFocus to next option" listboxApp listArrowDownDown <|
                \before _ after ->
                    let
                        beforeMaybeKeyboardFocus =
                            before.listbox.focus

                        afterMaybeKeyboardFocus =
                            after.listbox.focus

                        afterMaybeLastSelectedEntry =
                            after.listbox.maybeLastSelectedEntry
                    in
                    case ( beforeMaybeKeyboardFocus, afterMaybeKeyboardFocus ) of
                        ( Listbox.Focus focusBefore, Listbox.Focus focusAfter ) ->
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

                        ( _, Listbox.Focus afterKeyboardFocus ) ->
                            case afterMaybeLastSelectedEntry of
                                Nothing ->
                                    afterKeyboardFocus
                                        |> Expect.equal firstOption

                                Just afterLastSelectedEntry ->
                                    afterKeyboardFocus
                                        |> Expect.equal afterLastSelectedEntry

                        ( Listbox.Focus focusBefore, _ ) ->
                            case afterMaybeKeyboardFocus of
                                Listbox.Focus afterKeyboardFocus ->
                                    afterKeyboardFocus
                                        |> Expect.equal focusBefore

                                _ ->
                                    Expect.fail "Expected the listbox to not loose its keyboardFocus"

                        ( _, _ ) ->
                            Expect.fail "Expected the listbox to have a keyboardFocus"
            ]
        , describe "arrowUp"
            [ msgTest "moves keyboardFocus to previous option" listboxApp listArrowUpDown <|
                \before _ after ->
                    let
                        beforeMaybeKeyboardFocus =
                            before.listbox.focus

                        afterMaybeKeyboardFocus =
                            after.listbox.focus

                        afterMaybeLastSelectedEntry =
                            after.listbox
                                |> .maybeLastSelectedEntry
                    in
                    case ( beforeMaybeKeyboardFocus, afterMaybeKeyboardFocus ) of
                        ( Listbox.Focus focusBefore, Listbox.Focus focusAfter ) ->
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

                        ( _, Listbox.Focus afterKeyboardFocus ) ->
                            case afterMaybeLastSelectedEntry of
                                Nothing ->
                                    afterKeyboardFocus
                                        |> Expect.equal firstOption

                                Just afterLastSelectedEntry ->
                                    afterKeyboardFocus
                                        |> Expect.equal afterLastSelectedEntry

                        ( Listbox.Focus focusBefore, _ ) ->
                            case afterMaybeKeyboardFocus of
                                Listbox.Focus afterKeyboardFocus ->
                                    afterKeyboardFocus
                                        |> Expect.equal focusBefore

                                _ ->
                                    Expect.fail "Expected the listbox to not loose its keyboardFocus"

                        ( _, _ ) ->
                            Expect.fail "Expected the listbox to have a keyboardFocus"
            ]
        , describe "home"
            [ msgTest "moves keyboardFocus to first option" listboxApp listHomeDown <|
                \_ _ after ->
                    expectFirstOptionFocused after
            ]
        , describe "end"
            [ msgTest "moves keyboardFocus to last option" listboxApp listEndDown <|
                \_ _ after ->
                    expectLastOptionFocused after
            ]
        , describe "controlA"
            [ msgTest "controlA selects/deselects all options" listboxApp listControlADown <|
                \before _ after ->
                    if List.sort before.selection == List.sort options then
                        expectNothingSelected after

                    else
                        expectEverythingSelected after
            ]
        ]



---- EXPECTATIONS


expectValidFocus : Listbox.Focus -> Expectation
expectValidFocus f =
    case f of
        Listbox.NoFocus ->
            Expect.pass

        Listbox.Focus option ->
            options
                |> List.any ((==) option)
                |> Expect.true ("'" ++ option ++ "' is not a valid option")

        Listbox.Pending { next } ->
            options
                |> List.any ((==) next)
                |> Expect.true ("'" ++ next ++ "' is not a valid option")


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
    before.listbox.focus
        |> Expect.equal after.listbox.focus


expectUnchangedHover : Model -> Model -> Expectation
expectUnchangedHover before after =
    before.listbox.hover
        |> Expect.equal after.listbox.hover


expectFirstOptionFocused : Model -> Expectation
expectFirstOptionFocused { listbox } =
    listbox.focus
        |> Expect.equal (Listbox.Focus firstOption)


expectLastOptionFocused : Model -> Expectation
expectLastOptionFocused { listbox } =
    listbox.focus
        |> Expect.equal (Listbox.Focus lastOption)


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


listboxApp : TestedApp Model (Msg String)
listboxApp =
    { model = ConstantModel (Model Listbox.init [] (Time.millisToPosix 0))
    , update = UpdateWithoutCmds update
    , msgFuzzer = msgFuzzer
    , msgToString = Debug.toString
    , modelToString =
        \{ listbox, selection } ->
            String.join "\n"
                [ "listbox ="
                , "      { preventScroll  = "
                    ++ Debug.toString listbox.preventScroll
                    ++ ",    maybeKeyboardFocus        = "
                    ++ Debug.toString listbox.focus
                , "      , query          = "
                    ++ Debug.toString listbox.query
                , "      , ulScrollTop    = "
                    ++ Debug.toString listbox.ulScrollTop
                    ++ ",        hover           = "
                    ++ Debug.toString listbox.hover
                , "      , ulClientHeight = "
                    ++ Debug.toString listbox.ulClientHeight
                    ++ ",     maybeLastSelectedEntry    = "
                    ++ Debug.toString listbox.maybeLastSelectedEntry
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


update : Msg String -> Model -> Model
update msg model =
    let
        ( newListbox, effect, newSelection ) =
            Listbox.update updateConfig
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
                    { viewportList = viewportOfList
                    , elementList = elementOfList
                    , elementLi = elementOfEntry entryId
                    }
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
                    { viewportList = viewportOfList
                    , elementList = elementOfList
                    , elementLi = elementOfEntry entryId
                    , elementPreviousLi = elementOfEntry previousEntryId
                    }
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



---- FIXTURES


updateConfig : UpdateConfig String
updateConfig =
    { uniqueId = identity
    , behaviour =
        { jumpAtEnds = False
        , separateFocus = True
        , selectionFollowsFocus = False
        , handleHomeAndEnd = True
        , typeAhead = Listbox.NoTypeAhead
        , minimalGap = 0
        , initialGap = 0
        }
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
    List.map Widget.Listbox.option options



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
            , entries = front ++ Widget.Listbox.option knownOption :: back
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
        |> Fuzz.map Widget.Listbox.option


dividerFuzzer : Fuzzer (Entry a String)
dividerFuzzer =
    Fuzz.string
        |> Fuzz.map Widget.Listbox.divider



-- MSG


listArrowDownDown : Fuzzer (Msg String)
listArrowDownDown =
    Fuzz.constant (ListArrowDownDown id)


listArrowUpDown : Fuzzer (Msg String)
listArrowUpDown =
    Fuzz.constant (ListArrowUpDown id)


listHomeDown : Fuzzer (Msg String)
listHomeDown =
    Fuzz.constant (ListHomeDown id)


listEndDown : Fuzzer (Msg String)
listEndDown =
    Fuzz.constant (ListEndDown id)


listControlADown : Fuzzer (Msg String)
listControlADown =
    Fuzz.constant ListControlADown


msgFuzzer : Fuzzer (Msg String)
msgFuzzer =
    Fuzz.frequency
        [ -- LIST
          ( 1, Fuzz.constant ListMouseDown )
        , ( 1, Fuzz.constant ListMouseUp )
        , ( 1, Fuzz.constant (ListFocused id) )
        , ( 1, Fuzz.constant ListBlured )
        , ( 10, Fuzz.constant (ListArrowUpDown id) )
        , ( 10, Fuzz.constant (ListShiftArrowUpDown id) )
        , ( 10, Fuzz.constant (ListArrowDownDown id) )
        , ( 10, Fuzz.constant (ListShiftArrowDownDown id) )
        , ( 1, Fuzz.constant (ListEnterDown id) )
        , ( 1, Fuzz.constant (ListSpaceDown id) )
        , ( 1, Fuzz.constant (ListShiftSpaceDown id) )
        , ( 1, Fuzz.constant (ListHomeDown id) )
        , ( 1, Fuzz.constant (ListControlShiftHomeDown id) )
        , ( 1, Fuzz.constant (ListEndDown id) )
        , ( 1, Fuzz.constant (ListControlShiftEndDown id) )
        , ( 1, Fuzz.constant ListControlADown )

        -- QUERY
        , ( 1
          , "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                |> String.toList
                |> List.map (String.fromChar >> Fuzz.constant)
                |> Fuzz.oneOf
                |> Fuzz.map (ListKeyDown id)
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
