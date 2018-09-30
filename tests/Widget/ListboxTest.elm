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
import Set
import Test exposing (..)
import Time exposing (Posix)
import Widget.Listbox


suite : Test
suite =
    let
        config behaviour =
            { uniqueId = identity
            , behaviour = behaviour
            }

        behaviours =
            List.map (\behaviour -> behaviour Listbox.NoTypeAhead 0 0)
                [ Listbox.Behaviour False False False False
                , Listbox.Behaviour False False False True
                , Listbox.Behaviour False False True False
                , Listbox.Behaviour False False True True
                , Listbox.Behaviour False True False False
                , Listbox.Behaviour False True False True
                , Listbox.Behaviour False True True False
                , Listbox.Behaviour False True True True
                , Listbox.Behaviour True False False False
                , Listbox.Behaviour True False False True
                , Listbox.Behaviour True False True False
                , Listbox.Behaviour True False True True
                , Listbox.Behaviour True True False False
                , Listbox.Behaviour True True False True
                , Listbox.Behaviour True True True False
                , Listbox.Behaviour True True True True
                ]

        behaviourToTest behaviour =
            describe (behaviourToString behaviour)
                [ architectureTests (config behaviour)
                , functionTests (config behaviour)
                ]

        behaviourToString behaviour =
            String.join ", "
                [ with "jumpAtEnds" behaviour.jumpAtEnds
                , with "separateFocus" behaviour.separateFocus
                , with "selectionFollowsFocus" behaviour.selectionFollowsFocus
                , with "handleHomeAndEnd" behaviour.handleHomeAndEnd
                ]

        with name value =
            if value then
                "with " ++ name

            else
                "without " ++ name
    in
    behaviours
        |> List.map behaviourToTest
        |> concat


functionTests : UpdateConfig String -> Test
functionTests updateConfig =
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

                        ( listbox, _ ) =
                            Listbox.focusEntry updateConfig
                                knownOption
                                Listbox.init
                                selection
                    in
                    Listbox.focusedEntry updateConfig listbox entries
                        |> Expect.equal (Just knownOption)
            , describe "after focusNextOrFirstEntry"
                [ fuzz2 Fuzz.string entriesFuzzer "on initial Listbox" <|
                    \option entries ->
                        let
                            selection =
                                []

                            actualEntries =
                                Widget.Listbox.option option :: entries

                            ( listbox, _ ) =
                                Listbox.focusNextOrFirstEntry updateConfig
                                    actualEntries
                                    Listbox.init
                                    selection
                        in
                        Listbox.focusedEntry updateConfig listbox actualEntries
                            |> Expect.equal (Just option)
                ]
            , describe "after focusPreviousOrFirstEntry"
                [ fuzz2 Fuzz.string entriesFuzzer "on initial Listbox" <|
                    \option entries ->
                        let
                            selection =
                                []

                            actualEntries =
                                Widget.Listbox.option option :: entries

                            ( listbox, _ ) =
                                Listbox.focusPreviousOrFirstEntry updateConfig
                                    actualEntries
                                    Listbox.init
                                    selection
                        in
                        Listbox.focusedEntry updateConfig listbox actualEntries
                            |> Expect.equal (Just option)
                ]
            ]
        ]


architectureTests : UpdateConfig String -> Test
architectureTests ({ behaviour } as updateConfig) =
    let
        app =
            listboxApp updateConfig
    in
    concat
        [ invariantTest "maybeKeyboardFocus" app <|
            \_ _ { listbox } ->
                expectValidFocus listbox.focus
        , invariantTest "hover" app <|
            \_ _ { listbox } ->
                expectValidOption listbox.hover
        , describe "listBlured"
            [ msgTest "keeps keyboardFocus" app (Fuzz.constant ListBlured) <|
                \before _ after ->
                    expectUnchangedFocus before after
            , msgTest "keeps mouseFocus" app (Fuzz.constant ListBlured) <|
                \before _ after ->
                    expectUnchangedHover before after
            ]
        , describe "arrowDown"
            [ msgTest "moves keyboardFocus to next option" app listArrowDownDown <|
                \before _ after ->
                    ( before, after )
                        |> Expect.all
                            [ expectNextOrFirstOptionFocused behaviour
                            , Tuple.second >> expectNoPendingFocus
                            , if behaviour.selectionFollowsFocus then
                                Tuple.second >> expectOnlyFocusSelected

                              else
                                expectUnchangedSelection
                            ]
            ]
        , describe "shiftArrowDown"
            [ msgTest "moves keyboardFocus to next option" app listShiftArrowDownDown <|
                \before _ after ->
                    ( before, after )
                        |> Expect.all
                            [ expectNextOrFirstOptionFocused behaviour
                            , Tuple.second >> expectNoPendingFocus
                            , expectSelectionToggledOfFocusedOption
                            ]
            ]
        , describe "arrowUp"
            [ msgTest "moves keyboardFocus to previous option" app listArrowUpDown <|
                \before _ after ->
                    ( before, after )
                        |> Expect.all
                            [ expectPreviousOrFirstOptionFocused behaviour
                            , Tuple.second >> expectNoPendingFocus
                            , if behaviour.selectionFollowsFocus then
                                Tuple.second >> expectOnlyFocusSelected

                              else
                                expectUnchangedSelection
                            ]
            ]
        , describe "shiftArrowUp"
            [ msgTest "moves keyboardFocus to previous option" app listShiftArrowUpDown <|
                \before _ after ->
                    ( before, after )
                        |> Expect.all
                            [ expectPreviousOrFirstOptionFocused behaviour
                            , Tuple.second >> expectNoPendingFocus
                            ]
            ]
        , describe "home"
            [ msgTest "moves keyboardFocus to first option" app listHomeDown <|
                \_ _ after ->
                    expectFirstOptionFocused after
            ]
        , describe "end"
            [ msgTest "moves keyboardFocus to last option" app listEndDown <|
                \_ _ after ->
                    expectLastOptionFocused after
            ]
        , describe "controlA"
            [ msgTest "controlA selects/deselects all options" app listControlADown <|
                \before _ after ->
                    if List.sort before.selection == List.sort options then
                        expectNothingSelected after

                    else
                        expectEverythingSelected after
            ]
        ]



---- EXPECTATIONS


expectValidFocus : Maybe String -> Expectation
expectValidFocus focus =
    case focus of
        Nothing ->
            Expect.pass

        Just option ->
            options
                |> List.any ((==) option)
                |> Expect.true ("'" ++ option ++ "' is not a valid option")


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
        |> Expect.equal (Just firstOption)


expectLastOptionFocused : Model -> Expectation
expectLastOptionFocused { listbox } =
    listbox.focus
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


expectOnlyFocusSelected : Model -> Expectation
expectOnlyFocusSelected { listbox, selection } =
    let
        focusList =
            case listbox.focus of
                Nothing ->
                    []

                Just hash ->
                    [ hash ]
    in
    selection
        |> Expect.equalLists focusList


expectUnchangedSelection : ( Model, Model ) -> Expectation
expectUnchangedSelection ( before, after ) =
    before.selection
        |> Set.fromList
        |> Expect.equalSets (Set.fromList after.selection)


expectSelectionToggledOfFocusedOption : ( Model, Model ) -> Expectation
expectSelectionToggledOfFocusedOption ( before, after ) =
    let
        beforeSelection =
            Set.fromList before.selection

        afterSelection =
            Set.fromList after.selection
    in
    case after.listbox.focus of
        Nothing ->
            Expect.fail "Expected the listbox to have a keyboardFocus"

        Just hash ->
            if List.member hash before.selection then
                Set.diff afterSelection beforeSelection
                    |> Expect.equalSets Set.empty

            else
                Set.diff beforeSelection afterSelection
                    |> Expect.equalSets Set.empty


expectNoPendingFocus : Model -> Expectation
expectNoPendingFocus { listbox } =
    listbox.pendingFocus
        |> Expect.equal Nothing


expectNextOrFirstOptionFocused : Listbox.Behaviour String -> ( Model, Model ) -> Expectation
expectNextOrFirstOptionFocused behaviour ( before, after ) =
    case ( before.listbox.focus, after.listbox.focus ) of
        ( Just focusBefore, Just focusAfter ) ->
            let
                maybeNextOption =
                    options
                        |> List.splitWhen ((==) focusBefore)
                        |> Maybe.andThen (Tuple.second >> List.drop 1 >> List.head)
            in
            case maybeNextOption of
                Nothing ->
                    if behaviour.jumpAtEnds then
                        focusAfter
                            |> Expect.equal firstOption

                    else
                        focusAfter
                            |> Expect.equal focusBefore

                Just nextOption ->
                    focusAfter
                        |> Expect.equal nextOption

        ( Nothing, Just afterKeyboardFocus ) ->
            case after.listbox.maybeLastSelectedEntry of
                Nothing ->
                    afterKeyboardFocus
                        |> Expect.equal firstOption

                Just afterLastSelectedEntry ->
                    afterKeyboardFocus
                        |> Expect.equal afterLastSelectedEntry

        ( Just focusBefore, Nothing ) ->
            case after.listbox.focus of
                Just afterKeyboardFocus ->
                    afterKeyboardFocus
                        |> Expect.equal focusBefore

                Nothing ->
                    Expect.fail "Expected the listbox to not loose its keyboardFocus"

        ( Nothing, Nothing ) ->
            Expect.fail "Expected the listbox to have a keyboardFocus"


expectPreviousOrFirstOptionFocused : Listbox.Behaviour String -> ( Model, Model ) -> Expectation
expectPreviousOrFirstOptionFocused behaviour ( before, after ) =
    case ( before.listbox.focus, after.listbox.focus ) of
        ( Just focusBefore, Just focusAfter ) ->
            let
                maybePreviousOption =
                    options
                        |> List.splitWhen ((==) focusBefore)
                        |> Maybe.andThen (Tuple.first >> List.reverse >> List.head)
            in
            case maybePreviousOption of
                Nothing ->
                    if behaviour.jumpAtEnds then
                        focusAfter
                            |> Expect.equal lastOption

                    else
                        focusAfter
                            |> Expect.equal focusBefore

                Just previousOption ->
                    focusAfter
                        |> Expect.equal previousOption

        ( Nothing, Just afterKeyboardFocus ) ->
            case after.listbox.maybeLastSelectedEntry of
                Nothing ->
                    afterKeyboardFocus
                        |> Expect.equal firstOption

                Just afterLastSelectedEntry ->
                    afterKeyboardFocus
                        |> Expect.equal afterLastSelectedEntry

        ( Just focusBefore, Nothing ) ->
            case after.listbox.focus of
                Just afterKeyboardFocus ->
                    afterKeyboardFocus
                        |> Expect.equal focusBefore

                Nothing ->
                    Expect.fail "Expected the listbox to not loose its keyboardFocus"

        ( Nothing, Nothing ) ->
            Expect.fail "Expected the listbox to have a keyboardFocus"



---- SETUP


listboxApp : UpdateConfig String -> TestedApp Model (Msg String)
listboxApp updateConfig =
    { model = ConstantModel (Model Listbox.init [] (Time.millisToPosix 0))
    , update = UpdateWithoutCmds (update updateConfig)
    , msgFuzzer = msgFuzzer
    , msgToString = Debug.toString
    , modelToString =
        \{ listbox, selection } ->
            String.join "\n"
                [ "listbox ="
                , "      { preventScroll          = "
                    ++ Debug.toString listbox.preventScroll
                    ++ ",   focus = "
                    ++ Debug.toString listbox.focus
                , "      , query                  = "
                    ++ Debug.toString listbox.query
                    ++ ", hover = "
                    ++ Debug.toString listbox.hover
                , "      , ulScrollTop            = "
                    ++ Debug.toString listbox.ulScrollTop
                    ++ ",       pendingFocus = "
                    ++ Debug.toString listbox.pendingFocus
                , "      , ulClientHeight         = "
                    ++ Debug.toString listbox.ulClientHeight
                , "      , maybeLastSelectedEntry = "
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


update : Listbox.UpdateConfig String -> Msg String -> Model -> Model
update updateConfig msg model =
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
            update updateConfig (toMsg model.now) newModel

        DomSetViewportOf _ _ _ ->
            newModel

        DomFocus targetId ->
            if targetId == Listbox.printListId id then
                update updateConfig (ListFocused id) newModel

            else
                newModel

        ScrollListToTop toMsg _ ->
            update updateConfig
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
            update updateConfig
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
            update updateConfig
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
            update updateConfig
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



---- FIXTURES


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


listShiftArrowDownDown : Fuzzer (Msg String)
listShiftArrowDownDown =
    Fuzz.constant (ListShiftArrowDownDown id)


listArrowUpDown : Fuzzer (Msg String)
listArrowUpDown =
    Fuzz.constant (ListArrowUpDown id)


listShiftArrowUpDown : Fuzzer (Msg String)
listShiftArrowUpDown =
    Fuzz.constant (ListShiftArrowUpDown id)


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
        , ( 10, listArrowUpDown )
        , ( 10, listShiftArrowUpDown )
        , ( 10, listArrowDownDown )
        , ( 10, listShiftArrowDownDown )
        , ( 1, Fuzz.constant (ListEnterDown id) )
        , ( 1, Fuzz.constant (ListSpaceDown id) )
        , ( 1, Fuzz.constant (ListShiftSpaceDown id) )
        , ( 1, listHomeDown )
        , ( 1, Fuzz.constant (ListControlShiftHomeDown id) )
        , ( 1, listEndDown )
        , ( 1, Fuzz.constant (ListControlShiftEndDown id) )
        , ( 1, listControlADown )

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
