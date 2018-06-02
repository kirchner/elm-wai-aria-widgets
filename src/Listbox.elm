module Listbox
    exposing
        ( Behaviour
        , Config
        , Event
        , HtmlAttributes
        , HtmlDetails
        , Ids
        , Listbox
        , Msg
        , TypeAhead
        , View
        , focused
        , noTypeAhead
        , onAllEntriesSelect
        , onAllEntriesUnselect
        , onEntriesSelect
        , onEntrySelect
        , onEntryUnselect
        , onEscapeDown
        , onListboxBlur
        , simpleTypeAhead
        , subscriptions
        , typeAhead
        , unfocused
        , update
        , view
        , viewLazy
        )

{-|

@docs Listbox, unfocused, view, Ids, update, Msg, subscriptions

@docs Event, onEntrySelect, onEntryUnselect, onListboxBlur, onEscapeDown

@docs focused

@docs viewLazy


# Configuration

@docs Config, Behaviour, View

@docs HtmlAttributes, HtmlDetails


## Type ahead

@docs TypeAhead, noTypeAhead, simpleTypeAhead, typeAhead

-}

{-

   Copyright 2018 Fabian Kirchner

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.

-}

import Browser
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Internal.Entries
    exposing
        ( Next(..)
        , Previous(..)
        , find
        , findNext
        , findPrevious
        , findWith
        , indexOf
        , range
        )
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Set
import Task
import Time


{-| TODO
-}
type Listbox
    = Unfocused UnfocusedData
    | Focused FocusedData
    | Empty


type alias UnfocusedData =
    { preventScroll : Bool

    -- FOCUS
    , maybeKeyboardFocus : Maybe String
    , maybeMouseFocus : Maybe String
    , maybeLastSelectedEntry : Maybe String

    -- DOM
    , ulScrollTop : Float
    , ulClientHeight : Float
    }


type alias FocusedData =
    { query : Query

    -- FOCUS
    , keyboardFocus : String
    , maybeMouseFocus : Maybe String
    , maybeLastSelectedEntry : Maybe String

    -- DOM
    , ulScrollTop : Float
    , ulClientHeight : Float
    }


type Query
    = NoQuery
    | Query Int Time.Posix String


{-| TODO
-}
unfocused : Listbox
unfocused =
    Unfocused
        { preventScroll = False
        , maybeKeyboardFocus = Nothing
        , maybeMouseFocus = Nothing
        , maybeLastSelectedEntry = Nothing
        , ulScrollTop = 0
        , ulClientHeight = 1000
        }


{-| TODO
-}
focused : String -> String -> ( Listbox, Cmd (Msg a) )
focused id keyboardFocus =
    ( Focused
        { query = NoQuery
        , keyboardFocus = keyboardFocus
        , maybeMouseFocus = Nothing
        , maybeLastSelectedEntry = Nothing
        , ulScrollTop = 0
        , ulClientHeight = 1000
        }
    , Cmd.batch
        [ Browser.scrollIntoView (printEntryId id keyboardFocus)
            |> Task.attempt (\_ -> NoOp)
        , focusList id
        ]
    )



---- CONFIG


{-| TODO
-}
type alias Config a =
    { uniqueId : a -> String
    , behaviour : Behaviour a
    , view : View a
    }


{-| TODO
-}
type alias Behaviour a =
    { jumpAtEnds : Bool
    , separateFocus : Bool
    , selectionFollowsFocus : Bool
    , handleHomeAndEnd : Bool
    , typeAhead : TypeAhead a
    }


{-| TODO
-}
type TypeAhead a
    = NoTypeAhead
    | TypeAhead Int (String -> a -> Bool)


{-| TODO
-}
noTypeAhead : TypeAhead a
noTypeAhead =
    NoTypeAhead


{-| TODO
-}
simpleTypeAhead : Int -> (a -> String) -> TypeAhead a
simpleTypeAhead timeout entryToString =
    TypeAhead timeout <|
        \query entry ->
            String.toLower (entryToString entry)
                |> String.startsWith (String.toLower query)


{-| TODO
-}
typeAhead : Int -> (String -> a -> Bool) -> TypeAhead a
typeAhead =
    TypeAhead


{-| TODO
-}
type alias View a =
    { ul : HtmlAttributes
    , li :
        { selected : Bool
        , keyboardFocused : Bool
        , mouseFocused : Bool
        , maybeQuery : Maybe String
        }
        -> a
        -> HtmlDetails
    , empty : Html Never
    }


{-| TODO
-}
type alias HtmlAttributes =
    List (Html.Attribute Never)


{-| TODO
-}
type alias HtmlDetails =
    { attributes : List (Html.Attribute Never)
    , children : List (Html Never)
    }



---- VIEW


{-| TODO
-}
type alias Ids =
    { id : String
    , labelledBy : String
    }


{-| TODO
-}
view : Config a -> Ids -> Listbox -> List a -> List a -> Html (Msg a)
view config ids listbox allEntries selection =
    let
        renderedEntries =
            { spaceAboveFirst = 0
            , droppedAboveFirst = 0
            , spaceAboveSecond = 0
            , droppedAboveSecond = 0
            , spaceBelowFirst = 0
            , droppedBelowFirst = 0
            , spaceBelowSecond = 0
            , droppedBelowSecond = 0
            , entriesAbove = []
            , visibleEntries = allEntries
            , entriesBelow = []
            }
    in
    viewHelp renderedEntries config ids listbox allEntries selection


{-| TODO
-}
viewLazy : (a -> Float) -> Config a -> Ids -> Listbox -> List a -> List a -> Html (Msg a)
viewLazy entryHeight config ids listbox allEntries selection =
    let
        renderedEntries =
            case listbox of
                Unfocused data ->
                    let
                        maybeFocusIndex =
                            case data.maybeKeyboardFocus of
                                Nothing ->
                                    Nothing

                                Just keyboardFocus ->
                                    find config.uniqueId keyboardFocus allEntries
                                        |> Maybe.map Tuple.first
                    in
                    computeRenderedEntries
                        entryHeight
                        data.ulScrollTop
                        data.ulClientHeight
                        maybeFocusIndex
                        allEntries

                Focused data ->
                    let
                        maybeFocusIndex =
                            find config.uniqueId data.keyboardFocus allEntries
                                |> Maybe.map Tuple.first
                    in
                    computeRenderedEntries
                        entryHeight
                        data.ulScrollTop
                        data.ulClientHeight
                        maybeFocusIndex
                        allEntries

                Empty ->
                    { spaceAboveFirst = 0
                    , droppedAboveFirst = 0
                    , spaceAboveSecond = 0
                    , droppedAboveSecond = 0
                    , spaceBelowFirst = 0
                    , droppedBelowFirst = 0
                    , spaceBelowSecond = 0
                    , droppedBelowSecond = 0
                    , entriesAbove = []
                    , visibleEntries = allEntries
                    , entriesBelow = []
                    }
    in
    viewHelp renderedEntries config ids listbox allEntries selection


viewHelp : RenderedEntries a -> Config a -> Ids -> Listbox -> List a -> List a -> Html (Msg a)
viewHelp renderedEntries config ids listbox allEntries selection =
    case listbox of
        Unfocused data ->
            let
                viewData =
                    { behaviour = config.behaviour
                    , id = ids.id
                    , uniqueId = config.uniqueId
                    , allEntries = allEntries
                    }
            in
            viewUnfocused
                config
                ids
                viewData
                data.maybeKeyboardFocus
                selection
                allEntries
                renderedEntries.visibleEntries
                (viewEntries
                    config
                    ids
                    data.maybeKeyboardFocus
                    data.maybeMouseFocus
                    selection
                    Nothing
                    renderedEntries
                )

        Focused data ->
            let
                maybeQuery =
                    case data.query of
                        NoQuery ->
                            Nothing

                        Query _ _ query ->
                            Just query
            in
            viewFocused config
                ids
                data.keyboardFocus
                renderedEntries.visibleEntries
                allEntries
                selection
                (viewEntries
                    config
                    ids
                    (Just data.keyboardFocus)
                    data.maybeMouseFocus
                    selection
                    maybeQuery
                    renderedEntries
                )

        Empty ->
            config.view.empty
                |> Html.map (\_ -> NoOp)


viewUnfocused :
    Config a
    -> Ids
    -> Data a
    -> Maybe String
    -> List a
    -> List a
    -> List a
    -> List (Html (Msg a))
    -> Html (Msg a)
viewUnfocused config ids data maybeKeyboardFocus selection allEntries visibleEntries =
    Html.ul
        ([ Attributes.id (printListId ids.id)
         , Attributes.attribute "role" "listbox"
         , Attributes.attribute "aria-labelledby" ids.labelledBy
         , Attributes.tabindex 0
         , Events.onMouseDown ListMouseDown
         , Events.on "scroll" <|
            Decode.map2 ListScrolled
                (Decode.at [ "target", "scrollTop" ] Decode.float)
                (Decode.at [ "target", "clientHeight" ] Decode.float)
         , Events.on "focus"
            (Decode.oneOf
                [ case selection of
                    [] ->
                        case maybeKeyboardFocus of
                            Nothing ->
                                Decode.succeed Nothing

                            Just keyboardFocus ->
                                visibleEntries
                                    |> indexOf config.uniqueId keyboardFocus
                                    |> Maybe.map
                                        (\index ->
                                            Decode.map Just (scrollDataDecoder (index + 2))
                                        )
                                    |> Maybe.withDefault (Decode.succeed Nothing)

                    firstSelection :: _ ->
                        visibleEntries
                            |> indexOf config.uniqueId (config.uniqueId firstSelection)
                            |> Maybe.map (\index -> Decode.map Just (scrollDataDecoder (index + 2)))
                            |> Maybe.withDefault (Decode.succeed Nothing)
                , Decode.succeed Nothing
                ]
                |> Decode.map (ListFocused data selection)
            )
         ]
            |> setAriaActivedescendant ids.id config.uniqueId maybeKeyboardFocus allEntries
            |> appendAttributes config.view.ul
        )


viewFocused :
    Config a
    -> Ids
    -> String
    -> List a
    -> List a
    -> List a
    -> List (Html (Msg a))
    -> Html (Msg a)
viewFocused config ids keyboardFocus visibleEntries allEntries selection =
    let
        data =
            { behaviour = config.behaviour
            , id = ids.id
            , uniqueId = config.uniqueId
            , allEntries = allEntries
            }
    in
    Html.ul
        ([ Attributes.id (printListId ids.id)
         , Attributes.attribute "role" "listbox"
         , Attributes.attribute "aria-labelledby" ids.labelledBy
         , Attributes.tabindex 0
         , Events.preventDefaultOn "keydown"
            (keyInfoDecoder
                |> Decode.andThen
                    (listKeydown data keyboardFocus visibleEntries selection)
            )
         , Events.on "scroll" <|
            Decode.map2 ListScrolled
                (Decode.at [ "target", "scrollTop" ] Decode.float)
                (Decode.at [ "target", "clientHeight" ] Decode.float)
         , Events.on "blur" (Decode.succeed ListBlured)
         ]
            |> setAriaActivedescendant ids.id config.uniqueId (Just keyboardFocus) allEntries
            |> appendAttributes config.view.ul
        )


type alias KeyInfo =
    { code : String
    , shiftDown : Bool
    , controlDown : Bool
    }


keyInfoDecoder : Decoder KeyInfo
keyInfoDecoder =
    Decode.succeed KeyInfo
        |> Decode.required "key" Decode.string
        |> Decode.required "shiftKey" Decode.bool
        |> Decode.required "ctrlKey" Decode.bool


listKeydown :
    Data a
    -> String
    -> List a
    -> List a
    -> KeyInfo
    -> Decoder ( Msg a, Bool )
listKeydown ({ id, uniqueId, behaviour, allEntries } as data) keyboardFocus visibleEntries selection { code, shiftDown, controlDown } =
    case Debug.log "code" code of
        "ArrowUp" ->
            Decode.oneOf
                [ visibleEntries
                    |> indexOf uniqueId keyboardFocus
                    |> Maybe.map (\index -> Decode.map Just (scrollDataDecoder (index + 1)))
                    |> Maybe.withDefault (Decode.succeed Nothing)
                , Decode.succeed Nothing
                ]
                |> Decode.map (ListArrowUpPressed data shiftDown selection)
                |> preventDefault

        "ArrowDown" ->
            Decode.oneOf
                [ visibleEntries
                    |> indexOf uniqueId keyboardFocus
                    |> Maybe.map (\index -> Decode.map Just (scrollDataDecoder (index + 3)))
                    |> Maybe.withDefault (Decode.succeed Nothing)
                , Decode.succeed Nothing
                ]
                |> Decode.map (ListArrowDownPressed data shiftDown selection)
                |> preventDefault

        "Enter" ->
            Decode.succeed (ListEnterPressed id uniqueId allEntries selection)
                |> preventDefault

        "Escape" ->
            Decode.succeed ListEscapePressed
                |> preventDefault

        " " ->
            if shiftDown then
                Decode.succeed (ListShiftSpacePressed id uniqueId allEntries)
                    |> preventDefault
            else
                Decode.succeed (ListSpacePressed id uniqueId allEntries selection)
                    |> preventDefault

        "Home" ->
            if behaviour.handleHomeAndEnd then
                Decode.succeed (ListHomePressed data)
                    |> preventDefault
            else
                Decode.fail "not handling that key here"

        "End" ->
            if behaviour.handleHomeAndEnd then
                Decode.succeed (ListEndPressed data)
                    |> preventDefault
            else
                Decode.fail "not handling that key here"

        "a" ->
            if controlDown then
                Decode.succeed (ListControlAPressed uniqueId allEntries selection)
                    |> preventDefault
            else
                case behaviour.typeAhead of
                    NoTypeAhead ->
                        Decode.fail "not handling that key here"

                    TypeAhead timeout matchesQuery ->
                        if String.length code == 1 then
                            ListKeyPressed data timeout matchesQuery code
                                |> Decode.succeed
                                |> preventDefault
                        else
                            Decode.fail "not handling that key here"

        _ ->
            case behaviour.typeAhead of
                NoTypeAhead ->
                    Decode.fail "not handling that key here"

                TypeAhead timeout matchesQuery ->
                    if String.length code == 1 then
                        ListKeyPressed data timeout matchesQuery code
                            |> Decode.succeed
                            |> preventDefault
                    else
                        Decode.fail "not handling that key here"


scrollDataDecoder : Int -> Decoder ScrollData
scrollDataDecoder index =
    Decode.succeed ScrollData
        |> Decode.requiredAt
            [ "target", "scrollTop" ]
            Decode.float
        |> Decode.requiredAt
            [ "target", "clientHeight" ]
            Decode.float
        |> Decode.requiredAt
            [ "target", "childNodes", String.fromInt index, "offsetTop" ]
            Decode.float
        |> Decode.requiredAt
            [ "target", "childNodes", String.fromInt index, "offsetHeight" ]
            Decode.float


viewEntries :
    Config a
    -> Ids
    -> Maybe String
    -> Maybe String
    -> List a
    -> Maybe String
    -> RenderedEntries a
    -> List (Html (Msg a))
viewEntries config ids maybeKeyboardFocus maybeMouseFocus selection maybeQuery renderedEntries =
    let
        entryConfig =
            { id = ids.id
            , li = config.view.li
            , uniqueId = config.uniqueId
            }
    in
    List.concat
        [ spacer renderedEntries.spaceAboveFirst
        , renderedEntries.entriesAbove
            |> List.map
                (\a ->
                    viewEntry entryConfig
                        config.behaviour
                        maybeQuery
                        (List.member a selection)
                        (maybeKeyboardFocus == Just (config.uniqueId a))
                        (maybeMouseFocus == Just (config.uniqueId a))
                        selection
                        a
                )
        , spacer renderedEntries.spaceAboveSecond
        , renderedEntries.visibleEntries
            |> List.map
                (\a ->
                    viewEntry entryConfig
                        config.behaviour
                        maybeQuery
                        (List.member a selection)
                        (maybeKeyboardFocus == Just (config.uniqueId a))
                        (maybeMouseFocus == Just (config.uniqueId a))
                        selection
                        a
                )
        , spacer renderedEntries.spaceBelowFirst
        , renderedEntries.entriesBelow
            |> List.map
                (\a ->
                    viewEntry entryConfig
                        config.behaviour
                        maybeQuery
                        (List.member a selection)
                        (maybeKeyboardFocus == Just (config.uniqueId a))
                        (maybeMouseFocus == Just (config.uniqueId a))
                        selection
                        a
                )
        , spacer renderedEntries.spaceBelowSecond
        ]


viewEntry :
    { id : String
    , li :
        { selected : Bool
        , keyboardFocused : Bool
        , mouseFocused : Bool
        , maybeQuery : Maybe String
        }
        -> a
        -> HtmlDetails
    , uniqueId : a -> String
    }
    -> Behaviour a
    -> Maybe String
    -> Bool
    -> Bool
    -> Bool
    -> List a
    -> a
    -> Html (Msg a)
viewEntry config behaviour maybeQuery selected keyboardFocused mouseFocused selection a =
    let
        { attributes, children } =
            config.li
                { selected = selected
                , keyboardFocused = keyboardFocused
                , mouseFocused = mouseFocused
                , maybeQuery = maybeQuery
                }
                a
    in
    Html.li
        ([ Events.onMouseEnter (EntryMouseEntered behaviour (config.uniqueId a))
         , Events.onMouseLeave (EntryMouseLeft behaviour)
         , Events.onClick (EntryClicked behaviour config.id config.uniqueId selection a)
         , Attributes.id (printEntryId config.id (config.uniqueId a))
         , Attributes.attribute "role" "option"
         ]
            |> appendAttributes attributes
        )
        (children
            |> List.map (Html.map (\_ -> NoOp))
        )


spacer : Float -> List (Html msg)
spacer height =
    [ Html.li
        (if height == 0 then
            [ Attributes.style "display" "none" ]
         else
            [ Attributes.style "height" (String.fromFloat height ++ "px") ]
        )
        []
    ]



-- VIEW HELPER


setAriaActivedescendant :
    String
    -> (a -> String)
    -> Maybe String
    -> List a
    -> List (Html.Attribute msg)
    -> List (Html.Attribute msg)
setAriaActivedescendant id uniqueId maybeKeyboardFocus entries attrs =
    case maybeKeyboardFocus of
        Nothing ->
            attrs

        Just keyboardFocus ->
            entries
                |> find uniqueId keyboardFocus
                |> Maybe.map
                    (\( _, focusedEntry ) ->
                        Attributes.attribute "aria-activedescendant"
                            (printEntryId id (uniqueId focusedEntry))
                            :: attrs
                    )
                |> Maybe.withDefault attrs



---- UPDATE


{-| TODO
-}
type Msg a
    = NoOp
      -- LIST
    | ListMouseDown
    | ListFocused (Data a) (List a) (Maybe ScrollData)
    | ListBlured
    | ListScrolled Float Float
    | ListArrowUpPressed (Data a) Bool (List a) (Maybe ScrollData)
    | ListArrowDownPressed (Data a) Bool (List a) (Maybe ScrollData)
    | ListEnterPressed String (a -> String) (List a) (List a)
    | ListEscapePressed
    | ListSpacePressed String (a -> String) (List a) (List a)
    | ListShiftSpacePressed String (a -> String) (List a)
    | ListHomePressed (Data a)
    | ListEndPressed (Data a)
    | ListControlAPressed (a -> String) (List a) (List a)
      -- QUERY
    | ListKeyPressed (Data a) Int (String -> a -> Bool) String
    | CurrentTimeReceived (Data a) Int (String -> a -> Bool) String Time.Posix
    | Tick Time.Posix
      -- ENTRY
    | EntryMouseEntered (Behaviour a) String
    | EntryMouseLeft (Behaviour a)
    | EntryClicked (Behaviour a) String (a -> String) (List a) a


type alias Data a =
    { behaviour : Behaviour a
    , id : String
    , uniqueId : a -> String
    , allEntries : List a
    }


type alias ScrollData =
    { ulScrollTop : Float
    , ulClientHeight : Float
    , liOffsetTop : Float
    , liOffsetHeight : Float
    }


{-| TODO
-}
type Event a outMsg
    = OnEntrySelect (a -> outMsg)
    | OnEntriesSelect (List a -> outMsg)
    | OnEntryUnselect (a -> outMsg)
    | OnAllEntriesSelect outMsg
    | OnAllEntriesUnselect outMsg
    | OnListboxBlur outMsg
    | OnEscapeDown outMsg


{-| TODO
-}
onEntrySelect : (a -> outMsg) -> Event a outMsg
onEntrySelect =
    OnEntrySelect


{-| TODO
-}
onEntriesSelect : (List a -> outMsg) -> Event a outMsg
onEntriesSelect =
    OnEntriesSelect


{-| TODO
-}
onEntryUnselect : (a -> outMsg) -> Event a outMsg
onEntryUnselect =
    OnEntryUnselect


{-| TODO
-}
onAllEntriesSelect : outMsg -> Event a outMsg
onAllEntriesSelect =
    OnAllEntriesSelect


{-| TODO
-}
onAllEntriesUnselect : outMsg -> Event a outMsg
onAllEntriesUnselect =
    OnAllEntriesUnselect


{-| TODO
-}
onListboxBlur : outMsg -> Event a outMsg
onListboxBlur =
    OnListboxBlur


{-| TODO
-}
onEscapeDown : outMsg -> Event a outMsg
onEscapeDown =
    OnEscapeDown


sendEntrySelected : a -> List (Event a outMsg) -> Maybe outMsg
sendEntrySelected a events =
    case events of
        [] ->
            Nothing

        (OnEntrySelect entrySelected) :: _ ->
            Just (entrySelected a)

        _ :: rest ->
            sendEntrySelected a rest


sendEntriesSelected : List (Event a outMsg) -> List a -> Maybe outMsg
sendEntriesSelected events entries =
    case events of
        [] ->
            Nothing

        (OnEntriesSelect entriesSelected) :: _ ->
            Just (entriesSelected entries)

        _ :: rest ->
            sendEntriesSelected rest entries


sendEntryUnselected : a -> List (Event a outMsg) -> Maybe outMsg
sendEntryUnselected a events =
    case events of
        [] ->
            Nothing

        (OnEntryUnselect entryUnselected) :: _ ->
            Just (entryUnselected a)

        _ :: rest ->
            sendEntryUnselected a rest


sendAllEntriesSelected : List (Event a outMsg) -> Maybe outMsg
sendAllEntriesSelected events =
    case events of
        [] ->
            Nothing

        (OnAllEntriesSelect allEntriesSelected) :: _ ->
            Just allEntriesSelected

        _ :: rest ->
            sendAllEntriesSelected rest


sendAllEntriesUnselected : List (Event a outMsg) -> Maybe outMsg
sendAllEntriesUnselected events =
    case events of
        [] ->
            Nothing

        (OnAllEntriesUnselect allEntriesUnselected) :: _ ->
            Just allEntriesUnselected

        _ :: rest ->
            sendAllEntriesUnselected rest


sendListboxBlured : List (Event a outMsg) -> Maybe outMsg
sendListboxBlured events =
    case events of
        [] ->
            Nothing

        (OnListboxBlur listboxBlured) :: _ ->
            Just listboxBlured

        _ :: rest ->
            sendListboxBlured rest


sendEscapeDown : List (Event a outMsg) -> Maybe outMsg
sendEscapeDown events =
    case events of
        [] ->
            Nothing

        (OnEscapeDown escapeDowned) :: _ ->
            Just escapeDowned

        _ :: rest ->
            sendEscapeDown rest


{-| TODO
-}
update : List (Event a outMsg) -> Listbox -> Msg a -> ( Listbox, Cmd (Msg a), Maybe outMsg )
update events listbox msg =
    case listbox of
        Unfocused unfocusedData ->
            updateUnfocused events unfocusedData msg

        Focused focusedData ->
            updateFocused events focusedData msg

        Empty ->
            ( listbox, Cmd.none, Nothing )


updateUnfocused :
    List (Event a outMsg)
    -> UnfocusedData
    -> Msg a
    -> ( Listbox, Cmd (Msg a), Maybe outMsg )
updateUnfocused events data msg =
    case msg of
        -- LIST
        ListMouseDown ->
            ( Unfocused { data | preventScroll = True }
            , Cmd.none
            , Nothing
            )

        ListFocused { behaviour, id, uniqueId, allEntries } selection maybeScrollData ->
            let
                maybeNewFocus =
                    data.maybeLastSelectedEntry
                        |> or (Maybe.map uniqueId (List.head selection))
                        |> or data.maybeKeyboardFocus
                        |> or (Maybe.map uniqueId (List.head allEntries))

                or fallback default =
                    case default of
                        Nothing ->
                            fallback

                        Just _ ->
                            default
            in
            case maybeNewFocus of
                Nothing ->
                    ( Empty
                    , Cmd.none
                    , Nothing
                    )

                Just newFocus ->
                    ( unfocusedToFocused behaviour newFocus data
                    , if data.preventScroll then
                        Cmd.none
                      else
                        adjustScrollTop id newFocus maybeScrollData
                    , if behaviour.selectionFollowsFocus then
                        case find uniqueId newFocus allEntries of
                            Nothing ->
                                Nothing

                            Just ( _, a ) ->
                                sendEntrySelected a events
                      else
                        Nothing
                    )

        ListScrolled ulScrollTop ulClientHeight ->
            ( Unfocused
                { data
                    | ulScrollTop = ulScrollTop
                    , ulClientHeight = ulClientHeight
                }
            , Cmd.none
            , Nothing
            )

        -- ENTRY
        EntryMouseEntered behaviour newFocus ->
            ( Unfocused
                { data
                    | maybeKeyboardFocus =
                        if behaviour.separateFocus then
                            data.maybeKeyboardFocus
                        else
                            Just newFocus
                    , maybeMouseFocus = Just newFocus
                }
            , Cmd.none
            , Nothing
            )

        EntryMouseLeft behaviour ->
            ( Unfocused
                { data
                    | maybeMouseFocus =
                        if behaviour.separateFocus then
                            Nothing
                        else
                            data.maybeMouseFocus
                }
            , Cmd.none
            , Nothing
            )

        EntryClicked behaviour id uniqueId selection a ->
            ( Focused
                { query = NoQuery

                -- FOCUS
                , keyboardFocus = uniqueId a
                , maybeMouseFocus = Just (uniqueId a)
                , maybeLastSelectedEntry = Just (uniqueId a)

                -- DOM
                , ulScrollTop = 0
                , ulClientHeight = 1000
                }
            , focusList id
            , toggleSelectState events selection a
            )

        _ ->
            ( Unfocused data, Cmd.none, Nothing )


updateFocused :
    List (Event a outMsg)
    -> FocusedData
    -> Msg a
    -> ( Listbox, Cmd (Msg a), Maybe outMsg )
updateFocused events data msg =
    case msg of
        -- LIST
        ListBlured ->
            ( Unfocused
                { preventScroll = False
                , maybeKeyboardFocus = Just data.keyboardFocus
                , maybeMouseFocus = data.maybeMouseFocus
                , maybeLastSelectedEntry = data.maybeLastSelectedEntry
                , ulScrollTop = data.ulScrollTop
                , ulClientHeight = data.ulClientHeight
                }
            , Cmd.none
            , sendListboxBlured events
            )

        ListScrolled ulScrollTop ulClientHeight ->
            ( Focused
                { data
                    | ulScrollTop = ulScrollTop
                    , ulClientHeight = ulClientHeight
                }
            , Cmd.none
            , Nothing
            )

        ListArrowUpPressed { behaviour, id, uniqueId, allEntries } shiftDown selection maybeScrollData ->
            case findPrevious uniqueId data.keyboardFocus allEntries of
                Just (Last lastEntry) ->
                    if behaviour.jumpAtEnds then
                        data
                            |> updateFocus behaviour uniqueId events selection shiftDown lastEntry
                            |> andDo (scrollListToBottom id)
                    else
                        ( Focused { data | query = NoQuery }
                        , Cmd.none
                        , Nothing
                        )

                Just (Previous newIndex newEntry) ->
                    data
                        |> updateFocus behaviour uniqueId events selection shiftDown newEntry
                        |> andDo (adjustScrollTop id (uniqueId newEntry) maybeScrollData)

                Nothing ->
                    ( Focused data
                    , Cmd.none
                    , Nothing
                    )

        ListArrowDownPressed { behaviour, id, uniqueId, allEntries } shiftDown selection maybeScrollData ->
            case findNext uniqueId data.keyboardFocus allEntries of
                Just (First firstEntry) ->
                    if behaviour.jumpAtEnds then
                        data
                            |> updateFocus behaviour uniqueId events selection shiftDown firstEntry
                            |> andDo (scrollListToTop id)
                    else
                        ( Focused { data | query = NoQuery }
                        , Cmd.none
                        , Nothing
                        )

                Just (Next newIndex newEntry) ->
                    data
                        |> updateFocus behaviour uniqueId events selection shiftDown newEntry
                        |> andDo (adjustScrollTop id (uniqueId newEntry) maybeScrollData)

                Nothing ->
                    ( Focused data
                    , Cmd.none
                    , Nothing
                    )

        ListEnterPressed id uniqueId allEntries selection ->
            case find uniqueId data.keyboardFocus allEntries of
                Nothing ->
                    ( Focused data
                    , Cmd.none
                    , Nothing
                    )

                Just ( _, a ) ->
                    if List.member a selection then
                        ( Focused data
                        , Cmd.none
                        , sendEntryUnselected a events
                        )
                    else
                        ( Focused { data | maybeLastSelectedEntry = Just (uniqueId a) }
                        , Cmd.none
                        , sendEntrySelected a events
                        )

        ListEscapePressed ->
            ( Focused data
            , Cmd.none
            , sendEscapeDown events
            )

        ListSpacePressed id uniqueId allEntries selection ->
            case find uniqueId data.keyboardFocus allEntries of
                Nothing ->
                    ( Focused data
                    , Cmd.none
                    , Nothing
                    )

                Just ( _, a ) ->
                    if List.member a selection then
                        ( Focused data
                        , Cmd.none
                        , sendEntryUnselected a events
                        )
                    else
                        ( Focused { data | maybeLastSelectedEntry = Just (uniqueId a) }
                        , Cmd.none
                        , sendEntrySelected a events
                        )

        ListShiftSpacePressed id uniqueId allEntries ->
            case data.maybeLastSelectedEntry of
                Nothing ->
                    ( Focused data
                    , Cmd.none
                    , Nothing
                    )

                Just lastSelectedEntry ->
                    case range uniqueId data.keyboardFocus lastSelectedEntry allEntries of
                        [] ->
                            ( Focused data
                            , Cmd.none
                            , Nothing
                            )

                        selectedEntries ->
                            ( Focused { data | maybeLastSelectedEntry = Just data.keyboardFocus }
                            , Cmd.none
                            , sendEntriesSelected events selectedEntries
                            )

        ListHomePressed { behaviour, id, uniqueId, allEntries } ->
            case List.head allEntries of
                Nothing ->
                    ( Focused data
                    , Cmd.none
                    , Nothing
                    )

                Just firstEntry ->
                    data
                        |> updateFocus behaviour uniqueId events [] False firstEntry
                        |> andDo (scrollListToTop id)

        ListEndPressed { behaviour, id, uniqueId, allEntries } ->
            case List.head (List.reverse allEntries) of
                Nothing ->
                    ( Focused data, Cmd.none, Nothing )

                Just lastEntry ->
                    data
                        |> updateFocus behaviour uniqueId events [] False lastEntry
                        |> andDo (scrollListToBottom id)

        ListControlAPressed uniqueId allEntries selection ->
            let
                allEntriesSet =
                    allEntries
                        |> List.map uniqueId
                        |> Set.fromList

                selectionSet =
                    selection
                        |> List.map uniqueId
                        |> Set.fromList
            in
            ( Focused data
            , Cmd.none
            , if Set.isEmpty (Set.diff allEntriesSet selectionSet) then
                sendAllEntriesUnselected events
              else
                sendAllEntriesSelected events
            )

        -- QUERY
        ListKeyPressed viewData timeout matchesQuery code ->
            ( Focused data
            , Time.now
                |> Task.perform
                    (CurrentTimeReceived viewData timeout matchesQuery code)
            , Nothing
            )

        CurrentTimeReceived { behaviour, id, uniqueId, allEntries } timeout matchesQuery code currentTime ->
            let
                ( newQuery, queryText ) =
                    case data.query of
                        NoQuery ->
                            ( Query timeout currentTime code, code )

                        Query _ _ query ->
                            ( Query timeout currentTime (query ++ code), query ++ code )

                newKeyboardFocus =
                    findWith matchesQuery uniqueId data.keyboardFocus queryText allEntries
            in
            case newKeyboardFocus of
                Nothing ->
                    ( Focused data, Cmd.none, Nothing )

                Just newFocus ->
                    ( Focused
                        { data
                            | query = newQuery
                            , keyboardFocus = newFocus
                            , maybeMouseFocus =
                                if behaviour.separateFocus then
                                    data.maybeMouseFocus
                                else
                                    Just newFocus
                        }
                    , Browser.scrollIntoView (printEntryId id newFocus)
                        |> Task.attempt (\_ -> NoOp)
                    , Nothing
                    )

        Tick currentTime ->
            ( case data.query of
                NoQuery ->
                    Focused data

                Query timeout time _ ->
                    if Time.posixToMillis currentTime - Time.posixToMillis time > timeout then
                        Focused { data | query = NoQuery }
                    else
                        Focused data
            , Cmd.none
            , Nothing
            )

        -- ENTRY
        EntryMouseEntered behaviour newFocus ->
            ( Focused
                { data
                    | keyboardFocus =
                        if behaviour.separateFocus then
                            data.keyboardFocus
                        else
                            newFocus
                    , maybeMouseFocus = Just newFocus
                }
            , Cmd.none
            , Nothing
            )

        EntryMouseLeft behaviour ->
            ( Focused
                { data
                    | maybeMouseFocus =
                        if behaviour.separateFocus then
                            Nothing
                        else
                            data.maybeMouseFocus
                }
            , Cmd.none
            , Nothing
            )

        EntryClicked behaviour id uniqueId selection a ->
            let
                newFocus =
                    uniqueId a
            in
            ( Focused
                { data
                    | query = NoQuery
                    , keyboardFocus = newFocus
                    , maybeMouseFocus =
                        if behaviour.separateFocus then
                            data.maybeMouseFocus
                        else
                            Just newFocus
                }
            , Cmd.none
            , if List.member a selection then
                sendEntryUnselected a events
              else
                sendEntrySelected a events
            )

        _ ->
            ( Focused data, Cmd.none, Nothing )


andDo : Cmd msg -> ( a, b ) -> ( a, Cmd msg, b )
andDo cmd ( a, b ) =
    ( a, cmd, b )


updateFocus :
    { b
        | separateFocus : Bool
        , selectionFollowsFocus : Bool
    }
    -> (a -> String)
    -> List (Event a outMsg)
    -> List a
    -> Bool
    -> a
    -> FocusedData
    -> ( Listbox, Maybe outMsg )
updateFocus behaviour uniqueId events selection shiftDown newEntry data =
    let
        newFocus =
            uniqueId newEntry
    in
    if behaviour.selectionFollowsFocus then
        ( Focused
            { data
                | query = NoQuery
                , keyboardFocus = newFocus
                , maybeMouseFocus =
                    if behaviour.separateFocus then
                        data.maybeMouseFocus
                    else
                        Just newFocus
                , maybeLastSelectedEntry = Just newFocus
            }
        , sendEntrySelected newEntry events
        )
    else if shiftDown then
        if List.member newEntry selection then
            ( Focused
                { data
                    | query = NoQuery
                    , keyboardFocus = newFocus
                    , maybeMouseFocus =
                        if behaviour.separateFocus then
                            data.maybeMouseFocus
                        else
                            Just newFocus
                    , maybeLastSelectedEntry = Nothing
                }
            , sendEntryUnselected newEntry events
            )
        else
            ( Focused
                { data
                    | query = NoQuery
                    , keyboardFocus = newFocus
                    , maybeMouseFocus =
                        if behaviour.separateFocus then
                            data.maybeMouseFocus
                        else
                            Just newFocus
                    , maybeLastSelectedEntry = Just newFocus
                }
            , sendEntrySelected newEntry events
            )
    else
        ( Focused
            { data
                | query = NoQuery
                , keyboardFocus = newFocus
                , maybeMouseFocus =
                    if behaviour.separateFocus then
                        data.maybeMouseFocus
                    else
                        Just newFocus
            }
        , Nothing
        )


unfocusedToFocused : Behaviour a -> String -> UnfocusedData -> Listbox
unfocusedToFocused behaviour keyboardFocus data =
    Focused
        { query = NoQuery
        , keyboardFocus = keyboardFocus
        , maybeMouseFocus =
            if behaviour.separateFocus then
                Nothing
            else
                Just keyboardFocus
        , maybeLastSelectedEntry = data.maybeLastSelectedEntry
        , ulScrollTop = data.ulScrollTop
        , ulClientHeight = data.ulClientHeight
        }


toggleSelectState : List (Event a outMsg) -> List a -> a -> Maybe outMsg
toggleSelectState events selection a =
    if List.member a selection then
        sendEntryUnselected a events
    else
        sendEntrySelected a events



---- SUBSCRIPTIONS


{-| TODO
-}
subscriptions : Listbox -> Sub (Msg a)
subscriptions listbox =
    case listbox of
        Unfocused _ ->
            Sub.none

        Focused data ->
            case data.query of
                NoQuery ->
                    Sub.none

                Query timeout _ _ ->
                    Time.every (toFloat (timeout // 3)) Tick

        Empty ->
            Sub.none



-- CMDS


focusList : String -> Cmd (Msg a)
focusList id =
    Browser.focus (printListId id)
        |> Task.attempt (\_ -> NoOp)


scrollListToTop : String -> Cmd (Msg a)
scrollListToTop id =
    Browser.setScrollTop (printListId id) 0
        |> Task.attempt (\_ -> NoOp)


scrollListToBottom : String -> Cmd (Msg a)
scrollListToBottom id =
    Browser.setScrollBottom (printListId id) 0
        |> Task.attempt (\_ -> NoOp)


adjustScrollTop : String -> String -> Maybe ScrollData -> Cmd (Msg a)
adjustScrollTop id entryId maybeScrollData =
    case maybeScrollData of
        Nothing ->
            Browser.scrollIntoView (printEntryId id entryId)
                |> Task.attempt (\_ -> NoOp)

        Just ({ ulScrollTop, ulClientHeight, liOffsetTop, liOffsetHeight } as scrollData) ->
            if (liOffsetTop + liOffsetHeight) > (ulScrollTop + ulClientHeight) then
                if liOffsetTop <= ulScrollTop + ulClientHeight then
                    Browser.setScrollTop (printListId id)
                        (liOffsetTop + liOffsetHeight - ulClientHeight)
                        |> Task.attempt (\_ -> NoOp)
                else
                    centerScrollTop id scrollData
            else if liOffsetTop < ulScrollTop then
                if liOffsetTop + liOffsetHeight >= ulScrollTop then
                    Browser.setScrollTop (printListId id) liOffsetTop
                        |> Task.attempt (\_ -> NoOp)
                else
                    centerScrollTop id scrollData
            else
                Cmd.none


centerScrollTop : String -> ScrollData -> Cmd (Msg a)
centerScrollTop id { ulClientHeight, liOffsetTop, liOffsetHeight } =
    Browser.setScrollTop (printListId id)
        (liOffsetTop + liOffsetHeight / 2 - ulClientHeight / 2)
        |> Task.attempt (\_ -> NoOp)



---- IDS


printListId : String -> String
printListId id =
    id ++ "__element-list"


printEntryId : String -> String -> String
printEntryId id entryId =
    id ++ "__element--" ++ entryId



-- MISC


appendAttributes :
    List (Html.Attribute Never)
    -> List (Html.Attribute (Msg a))
    -> List (Html.Attribute (Msg a))
appendAttributes neverAttrs attrs =
    neverAttrs
        |> List.map (Attributes.map (\_ -> NoOp))
        |> List.append attrs


preventDefault : Decoder msg -> Decoder ( msg, Bool )
preventDefault decoder =
    decoder
        |> Decode.map (\msg -> ( msg, True ))


allowDefault : Decoder msg -> Decoder ( msg, Bool )
allowDefault decoder =
    decoder
        |> Decode.map (\msg -> ( msg, False ))



---- COMPUTE RENDERED ENTRIES


type alias RenderedEntries a =
    { spaceAboveFirst : Float
    , droppedAboveFirst : Int
    , spaceAboveSecond : Float
    , droppedAboveSecond : Int
    , spaceBelowFirst : Float
    , droppedBelowFirst : Int
    , spaceBelowSecond : Float
    , droppedBelowSecond : Int
    , entriesAbove : List a
    , visibleEntries : List a
    , entriesBelow : List a
    }


computeRenderedEntries : (a -> Float) -> Float -> Float -> Maybe Int -> List a -> RenderedEntries a
computeRenderedEntries entryHeight ulScrollTop ulClientHeight maybeFocusIndex entries =
    let
        initialRenderedEntries =
            { spaceAboveFirst = 0
            , droppedAboveFirst = 0
            , spaceAboveSecond = 0
            , droppedAboveSecond = 0
            , spaceBelowFirst = 0
            , droppedBelowFirst = 0
            , spaceBelowSecond = 0
            , droppedBelowSecond = 0
            , entriesAbove = []
            , visibleEntries = []
            , entriesBelow = []
            }

        withoutIndex entry currentHeight data =
            let
                height =
                    entryHeight entry
            in
            if currentHeight < ulScrollTop - 200 then
                -- entry is above the rendered range
                { data
                    | spaceAboveFirst = data.spaceAboveFirst + height
                    , droppedAboveFirst = data.droppedAboveFirst + 1
                }
            else if currentHeight >= (ulScrollTop + ulClientHeight + 200) then
                { data
                    | spaceBelowFirst = data.spaceBelowFirst + height
                    , droppedBelowFirst = data.droppedBelowFirst + 1
                }
            else
                -- entry is within the rendered range
                { data | visibleEntries = entry :: data.visibleEntries }

        withIndex index currentIndex entry currentHeight data =
            let
                height =
                    entryHeight entry
            in
            if currentHeight < ulScrollTop - 200 then
                -- entry is above the rendered range
                if currentIndex < index - 1 then
                    -- entry is before focused entry
                    { data
                        | spaceAboveFirst = data.spaceAboveFirst + height
                        , droppedAboveFirst = data.droppedAboveFirst + 1
                    }
                else if currentIndex > index + 1 then
                    -- entry is after focused entry
                    { data
                        | spaceAboveSecond = data.spaceAboveSecond + height
                        , droppedAboveSecond = data.droppedAboveSecond + 1
                    }
                else
                    -- entry is focused or next to focused entry
                    { data | entriesAbove = entry :: data.entriesAbove }
            else if currentHeight > (ulScrollTop + ulClientHeight + 200) then
                -- entry is below the rendered range
                if currentIndex < index - 1 then
                    -- entry is before focused entry
                    { data
                        | spaceBelowFirst = data.spaceBelowFirst + height
                        , droppedBelowFirst = data.droppedBelowFirst + 1
                    }
                else if currentIndex > index + 1 then
                    -- entry is after focused entry
                    { data
                        | spaceBelowSecond = data.spaceBelowSecond + height
                        , droppedBelowSecond = data.droppedBelowSecond + 1
                    }
                else
                    -- entry is focused or next to focused entry
                    { data | entriesBelow = entry :: data.entriesBelow }
            else
                -- entry is within the rendered range
                { data | visibleEntries = entry :: data.visibleEntries }

        reverseLists renderedEntries =
            { renderedEntries
                | entriesAbove = List.reverse renderedEntries.entriesAbove
                , visibleEntries = List.reverse renderedEntries.visibleEntries
                , entriesBelow = List.reverse renderedEntries.entriesBelow
            }
    in
    reverseLists <|
        case maybeFocusIndex of
            Nothing ->
                entries
                    |> List.foldl
                        (\entry ( currentHeight, data ) ->
                            ( currentHeight + entryHeight entry
                            , withoutIndex entry currentHeight data
                            )
                        )
                        ( 0, initialRenderedEntries )
                    |> Tuple.second

            Just index ->
                entries
                    |> List.foldl
                        (\entry ( ( currentIndex, currentHeight ), data ) ->
                            ( ( currentIndex + 1
                              , currentHeight + entryHeight entry
                              )
                            , withIndex index currentIndex entry currentHeight data
                            )
                        )
                        ( ( 0, 0 ), initialRenderedEntries )
                    |> Tuple.second
