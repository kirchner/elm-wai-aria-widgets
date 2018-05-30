module Listbox
    exposing
        ( Behaviour
        , Config
        , Listbox
        , Msg
        , TypeAhead
        , View
        , noTypeAhead
        , simpleTypeAhead
        , subscriptions
        , typeAhead
        , unfocused
        , update
        , view
        , viewLazy
        )

{-|

@docs Listbox

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
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Task
import Time


{-| -}
type Listbox
    = Unfocused UnfocusedData
    | Focused FocusedData
    | Empty


type alias UnfocusedData =
    { maybeKeyboardFocus : Maybe String
    , maybeMouseFocus : Maybe String

    -- DOM
    , ulScrollTop : Float
    , ulClientHeight : Float
    }


type alias FocusedData =
    { query : Query

    -- FOCUS
    , keyboardFocus : String
    , maybeMouseFocus : Maybe String

    -- DOM
    , ulScrollTop : Float
    , ulClientHeight : Float
    }


type Query
    = NoQuery
    | Query Int Time.Posix String


{-| -}
unfocused : Listbox
unfocused =
    Unfocused
        { maybeKeyboardFocus = Nothing
        , maybeMouseFocus = Nothing
        , ulScrollTop = 0
        , ulClientHeight = 1000
        }



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
view : Config a -> Ids -> Listbox -> List a -> Maybe a -> Html (Msg a)
view config ids listbox allEntries maybeSelection =
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
    viewHelp renderedEntries config ids listbox allEntries maybeSelection


{-| TODO
-}
viewLazy : (a -> Float) -> Config a -> Ids -> Listbox -> List a -> Maybe a -> Html (Msg a)
viewLazy entryHeight config ids listbox allEntries maybeSelection =
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
    viewHelp renderedEntries config ids listbox allEntries maybeSelection


viewHelp : RenderedEntries a -> Config a -> Ids -> Listbox -> List a -> Maybe a -> Html (Msg a)
viewHelp renderedEntries config ids listbox allEntries maybeSelection =
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
            viewUnfocused config ids viewData data.maybeKeyboardFocus maybeSelection allEntries <|
                viewEntries
                    config
                    ids
                    data.maybeKeyboardFocus
                    data.maybeMouseFocus
                    maybeSelection
                    Nothing
                    renderedEntries

        Focused data ->
            let
                maybeQuery =
                    case data.query of
                        NoQuery ->
                            Nothing

                        Query _ _ query ->
                            Just query
            in
            viewFocused config ids data.keyboardFocus renderedEntries.visibleEntries allEntries <|
                viewEntries
                    config
                    ids
                    (Just data.keyboardFocus)
                    data.maybeMouseFocus
                    maybeSelection
                    maybeQuery
                    renderedEntries

        Empty ->
            config.view.empty
                |> Html.map (\_ -> NoOp)


viewUnfocused : Config a -> Ids -> Data a -> Maybe String -> Maybe a -> List a -> List (Html (Msg a)) -> Html (Msg a)
viewUnfocused config ids data maybeKeyboardFocus maybeSelection allEntries =
    Html.ul
        ([ Attributes.id (printListId ids.id)
         , Attributes.attribute "role" "listbox"
         , Attributes.attribute "aria-labelledby" ids.labelledBy
         , Attributes.style "position" "absolute"
         , Attributes.tabindex 0
         , Events.on "scroll" <|
            Decode.map2 ListScrolled
                (Decode.at [ "target", "scrollTop" ] Decode.float)
                (Decode.at [ "target", "clientHeight" ] Decode.float)
         , Events.on "focus" (Decode.succeed (ListFocused data maybeSelection))
         ]
            |> setAriaActivedescendant ids.id config.uniqueId maybeKeyboardFocus allEntries
            |> appendAttributes config.view.ul
        )


viewFocused : Config a -> Ids -> String -> List a -> List a -> List (Html (Msg a)) -> Html (Msg a)
viewFocused config ids keyboardFocus visibleEntries allEntries =
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
         , Attributes.style "position" "absolute"
         , Attributes.tabindex 0
         , Events.preventDefaultOn "keydown"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (listKeydown data keyboardFocus visibleEntries)
            )
         , Events.on "scroll" <|
            Decode.map2 ListScrolled
                (Decode.at [ "target", "scrollTop" ] Decode.float)
                (Decode.at [ "target", "clientHeight" ] Decode.float)
         , Events.on "blur" (Decode.succeed ListBlured)
         ]
            --|> handleKeypress data
            |> setAriaActivedescendant ids.id config.uniqueId (Just keyboardFocus) allEntries
            |> appendAttributes config.view.ul
        )


listKeydown :
    Data a
    -> String
    -> List a
    -> String
    -> Decoder ( Msg a, Bool )
listKeydown ({ id, uniqueId, behaviour, allEntries } as data) keyboardFocus visibleEntries code =
    case code of
        "ArrowUp" ->
            Decode.oneOf
                [ visibleEntries
                    |> indexOf uniqueId keyboardFocus
                    |> Maybe.map (\index -> Decode.map Just (scrollDataDecoder (index + 1)))
                    |> Maybe.withDefault (Decode.succeed Nothing)
                , Decode.succeed Nothing
                ]
                |> Decode.map (ListArrowUpPressed data)
                |> preventDefault

        "ArrowDown" ->
            Decode.oneOf
                [ visibleEntries
                    |> indexOf uniqueId keyboardFocus
                    |> Maybe.map (\index -> Decode.map Just (scrollDataDecoder (index + 3)))
                    |> Maybe.withDefault (Decode.succeed Nothing)
                , Decode.succeed Nothing
                ]
                |> Decode.map (ListArrowDownPressed data)
                |> preventDefault

        "Enter" ->
            Decode.succeed (ListEnterPressed id uniqueId allEntries)
                |> preventDefault

        " " ->
            Decode.succeed (ListSpacePressed id uniqueId allEntries)
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
    -> Maybe a
    -> Maybe String
    -> RenderedEntries a
    -> List (Html (Msg a))
viewEntries config ids maybeKeyboardFocus maybeMouseFocus maybeSelection maybeQuery renderedEntries =
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
                        (maybeSelection == Just a)
                        (maybeKeyboardFocus == Just (config.uniqueId a))
                        (maybeMouseFocus == Just (config.uniqueId a))
                        a
                )
        , spacer renderedEntries.spaceAboveSecond
        , renderedEntries.visibleEntries
            |> List.map
                (\a ->
                    viewEntry entryConfig
                        config.behaviour
                        maybeQuery
                        (maybeSelection == Just a)
                        (maybeKeyboardFocus == Just (config.uniqueId a))
                        (maybeMouseFocus == Just (config.uniqueId a))
                        a
                )
        , spacer renderedEntries.spaceBelowFirst
        , renderedEntries.entriesBelow
            |> List.map
                (\a ->
                    viewEntry entryConfig
                        config.behaviour
                        maybeQuery
                        (maybeSelection == Just a)
                        (maybeKeyboardFocus == Just (config.uniqueId a))
                        (maybeMouseFocus == Just (config.uniqueId a))
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
    -> a
    -> Html (Msg a)
viewEntry config behaviour maybeQuery selected keyboardFocused mouseFocused a =
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
         , Events.onClick (EntryClicked behaviour config.id config.uniqueId a)
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


type Msg a
    = NoOp
      -- LIST
    | ListFocused (Data a) (Maybe a)
    | ListBlured
    | ListScrolled Float Float
    | ListArrowUpPressed (Data a) (Maybe ScrollData)
    | ListArrowDownPressed (Data a) (Maybe ScrollData)
    | ListEnterPressed String (a -> String) (List a)
    | ListSpacePressed String (a -> String) (List a)
    | ListHomePressed (Data a)
    | ListEndPressed (Data a)
      -- QUERY
    | ListKeyPressed (Data a) Int (String -> a -> Bool) String
    | CurrentTimeReceived (Data a) Int (String -> a -> Bool) String Time.Posix
    | Tick Time.Posix
      -- ENTRY
    | EntryMouseEntered (Behaviour a) String
    | EntryMouseLeft (Behaviour a)
    | EntryClicked (Behaviour a) String (a -> String) a


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
update : (a -> outMsg) -> Listbox -> Msg a -> ( Listbox, Cmd (Msg a), Maybe outMsg )
update entrySelected listbox msg =
    case listbox of
        Unfocused unfocusedData ->
            updateUnfocused entrySelected unfocusedData msg

        Focused focusedData ->
            updateFocused entrySelected focusedData msg

        Empty ->
            ( listbox, Cmd.none, Nothing )


updateUnfocused : (a -> outMsg) -> UnfocusedData -> Msg a -> ( Listbox, Cmd (Msg a), Maybe outMsg )
updateUnfocused entrySelected data msg =
    case msg of
        -- LIST
        ListFocused { behaviour, id, uniqueId, allEntries } maybeSelection ->
            case maybeSelection of
                Nothing ->
                    case List.head allEntries of
                        Nothing ->
                            ( Empty
                            , Cmd.none
                            , Nothing
                            )

                        Just firstEntry ->
                            ( Focused
                                { query = NoQuery
                                , keyboardFocus = uniqueId firstEntry
                                , maybeMouseFocus =
                                    if behaviour.separateFocus then
                                        Nothing
                                    else
                                        Just (uniqueId firstEntry)
                                , ulScrollTop = 0
                                , ulClientHeight = 1000
                                }
                            , scrollListToTop id
                            , if behaviour.selectionFollowsFocus then
                                Just (entrySelected firstEntry)
                              else
                                Nothing
                            )

                Just selection ->
                    if List.member selection allEntries then
                        ( Focused
                            { query = NoQuery
                            , keyboardFocus = uniqueId selection
                            , maybeMouseFocus =
                                if behaviour.separateFocus then
                                    Nothing
                                else
                                    Just (uniqueId selection)
                            , ulScrollTop = 0
                            , ulClientHeight = 1000
                            }
                        , Browser.scrollIntoView (printEntryId id (uniqueId selection))
                            |> Task.attempt (\_ -> NoOp)
                        , Nothing
                        )
                    else
                        ( Empty, Cmd.none, Nothing )

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

        EntryClicked behaviour id uniqueId a ->
            ( Focused
                { query = NoQuery

                -- FOCUS
                , keyboardFocus = uniqueId a
                , maybeMouseFocus = Just (uniqueId a)

                -- DOM
                , ulScrollTop = 0
                , ulClientHeight = 1000
                }
            , focusList id
            , Just (entrySelected a)
            )

        _ ->
            ( Unfocused data, Cmd.none, Nothing )


updateFocused : (a -> outMsg) -> FocusedData -> Msg a -> ( Listbox, Cmd (Msg a), Maybe outMsg )
updateFocused entrySelected data msg =
    case msg of
        -- LIST
        ListBlured ->
            ( Unfocused
                { maybeKeyboardFocus = Just data.keyboardFocus
                , maybeMouseFocus = data.maybeMouseFocus
                , ulScrollTop = data.ulScrollTop
                , ulClientHeight = data.ulClientHeight
                }
            , Cmd.none
            , Nothing
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

        ListArrowUpPressed { behaviour, id, uniqueId, allEntries } maybeScrollData ->
            case findPrevious uniqueId data.keyboardFocus allEntries of
                Just (Last lastEntry) ->
                    if behaviour.jumpAtEnds then
                        ( { data | query = NoQuery }
                            |> updateKeyboardFocus behaviour (uniqueId lastEntry)
                            |> Focused
                        , scrollListToBottom id
                        , if behaviour.selectionFollowsFocus then
                            Just (entrySelected lastEntry)
                          else
                            Nothing
                        )
                    else
                        ( Focused { data | query = NoQuery }
                        , Cmd.none
                        , Nothing
                        )

                Just (Previous newIndex newEntry) ->
                    ( { data | query = NoQuery }
                        |> updateKeyboardFocus behaviour (uniqueId newEntry)
                        |> Focused
                    , case maybeScrollData of
                        Nothing ->
                            Browser.scrollIntoView (printEntryId id (uniqueId newEntry))
                                |> Task.attempt (\_ -> NoOp)

                        Just scrollData ->
                            adjustScrollTop NoOp id scrollData
                    , if behaviour.selectionFollowsFocus then
                        Just (entrySelected newEntry)
                      else
                        Nothing
                    )

                Nothing ->
                    ( Focused data
                    , Cmd.none
                    , Nothing
                    )

        ListArrowDownPressed { behaviour, id, uniqueId, allEntries } maybeScrollData ->
            case findNext uniqueId data.keyboardFocus allEntries of
                Just (First firstEntry) ->
                    if behaviour.jumpAtEnds then
                        ( { data | query = NoQuery }
                            |> updateKeyboardFocus behaviour (uniqueId firstEntry)
                            |> Focused
                        , scrollListToTop id
                        , if behaviour.selectionFollowsFocus then
                            Just (entrySelected firstEntry)
                          else
                            Nothing
                        )
                    else
                        ( Focused { data | query = NoQuery }
                        , Cmd.none
                        , Nothing
                        )

                Just (Next newIndex newEntry) ->
                    ( { data | query = NoQuery }
                        |> updateKeyboardFocus behaviour (uniqueId newEntry)
                        |> Focused
                    , case maybeScrollData of
                        Nothing ->
                            Browser.scrollIntoView (printEntryId id (uniqueId newEntry))
                                |> Task.attempt (\_ -> NoOp)

                        Just scrollData ->
                            adjustScrollTop NoOp id scrollData
                    , if behaviour.selectionFollowsFocus then
                        Just (entrySelected newEntry)
                      else
                        Nothing
                    )

                Nothing ->
                    ( Focused data
                    , Cmd.none
                    , Nothing
                    )

        ListEnterPressed id uniqueId allEntries ->
            ( Focused data
            , Cmd.none
            , case find uniqueId data.keyboardFocus allEntries of
                Nothing ->
                    Nothing

                Just ( _, a ) ->
                    Just (entrySelected a)
            )

        ListSpacePressed id uniqueId allEntries ->
            ( Focused data
            , Cmd.none
            , case find uniqueId data.keyboardFocus allEntries of
                Nothing ->
                    Nothing

                Just ( _, a ) ->
                    Just (entrySelected a)
            )

        ListHomePressed { behaviour, id, uniqueId, allEntries } ->
            case List.head allEntries of
                Nothing ->
                    ( Focused data
                    , Cmd.none
                    , Nothing
                    )

                Just firstEntry ->
                    ( { data | query = NoQuery }
                        |> updateKeyboardFocus behaviour (uniqueId firstEntry)
                        |> Focused
                    , scrollListToTop id
                    , Nothing
                    )

        ListEndPressed { behaviour, id, uniqueId, allEntries } ->
            case List.head (List.reverse allEntries) of
                Nothing ->
                    ( Focused data, Cmd.none, Nothing )

                Just firstEntry ->
                    ( { data | query = NoQuery }
                        |> updateKeyboardFocus behaviour (uniqueId firstEntry)
                        |> Focused
                    , scrollListToBottom id
                    , Nothing
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
                    ( { data | query = newQuery }
                        |> updateKeyboardFocus behaviour newFocus
                        |> Focused
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

        EntryClicked behaviour id uniqueId a ->
            ( { data | query = NoQuery }
                |> updateKeyboardFocus behaviour (uniqueId a)
                |> Focused
            , Cmd.none
            , Just (entrySelected a)
            )

        _ ->
            ( Focused data, Cmd.none, Nothing )


updateKeyboardFocus : { b | separateFocus : Bool } -> String -> FocusedData -> FocusedData
updateKeyboardFocus { separateFocus } newFocus data =
    { data
        | keyboardFocus = newFocus
        , maybeMouseFocus =
            if separateFocus then
                data.maybeMouseFocus
            else
                Just newFocus
    }



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


adjustScrollTop : msg -> String -> ScrollData -> Cmd msg
adjustScrollTop noOp id ({ ulScrollTop, ulClientHeight, liOffsetTop, liOffsetHeight } as scrollData) =
    if (liOffsetTop + liOffsetHeight) > (ulScrollTop + ulClientHeight) then
        if liOffsetTop <= ulScrollTop + ulClientHeight then
            Browser.setScrollTop (printListId id)
                (liOffsetTop + liOffsetHeight - ulClientHeight)
                |> Task.attempt (\_ -> noOp)
        else
            centerScrollTop noOp id scrollData
    else if liOffsetTop < ulScrollTop then
        if liOffsetTop + liOffsetHeight >= ulScrollTop then
            Browser.setScrollTop (printListId id) liOffsetTop
                |> Task.attempt (\_ -> noOp)
        else
            centerScrollTop noOp id scrollData
    else
        Cmd.none


centerScrollTop : msg -> String -> ScrollData -> Cmd msg
centerScrollTop noOp id { ulClientHeight, liOffsetTop, liOffsetHeight } =
    Browser.setScrollTop (printListId id)
        (liOffsetTop + liOffsetHeight / 2 - ulClientHeight / 2)
        |> Task.attempt (\_ -> noOp)



---- IDS


printListId : String -> String
printListId id =
    id ++ "__element-list"


printEntryId : String -> String -> String
printEntryId id entryId =
    id ++ "__element--" ++ entryId



---- FIND CURRENT/NEXT/PREVIOUS ENTRIES


indexOf : (a -> String) -> String -> List a -> Maybe Int
indexOf =
    indexOfHelp 0


indexOfHelp : Int -> (a -> String) -> String -> List a -> Maybe Int
indexOfHelp index uniqueId id entries =
    case entries of
        [] ->
            Nothing

        entry :: rest ->
            if uniqueId entry == id then
                Just index
            else
                indexOfHelp (index + 1) uniqueId id rest


find : (a -> String) -> String -> List a -> Maybe ( Int, a )
find =
    findHelp 0


findHelp : Int -> (a -> String) -> String -> List a -> Maybe ( Int, a )
findHelp index entryId selectedId entries =
    case entries of
        [] ->
            Nothing

        entry :: rest ->
            if entryId entry == selectedId then
                Just ( index, entry )
            else
                findHelp (index + 1) entryId selectedId rest


findWith : (String -> a -> Bool) -> (a -> String) -> String -> String -> List a -> Maybe String
findWith matchesQuery uniqueId focus query entries =
    case entries of
        [] ->
            Nothing

        entry :: rest ->
            if uniqueId entry == focus then
                let
                    id =
                        uniqueId entry
                in
                if matchesQuery query entry then
                    Just id
                else
                    proceedWith matchesQuery uniqueId id query rest
            else
                findWith matchesQuery uniqueId focus query rest


proceedWith : (String -> a -> Bool) -> (a -> String) -> String -> String -> List a -> Maybe String
proceedWith matchesQuery uniqueId id query entries =
    case entries of
        [] ->
            Just id

        next :: rest ->
            if matchesQuery query next then
                Just (uniqueId next)
            else
                proceedWith matchesQuery uniqueId id query rest


type Previous a
    = Previous Int a
    | Last a


findPrevious : (a -> String) -> String -> List a -> Maybe (Previous a)
findPrevious entryId currentId entries =
    case entries of
        [] ->
            Nothing

        first :: rest ->
            if entryId first == currentId then
                entries
                    |> List.reverse
                    |> List.head
                    |> Maybe.map Last
            else
                findPreviousHelp first 0 entryId currentId rest


findPreviousHelp : a -> Int -> (a -> String) -> String -> List a -> Maybe (Previous a)
findPreviousHelp previous index entryId currentId entries =
    case entries of
        [] ->
            Nothing

        next :: rest ->
            if entryId next == currentId then
                Just (Previous index previous)
            else
                findPreviousHelp next (index + 1) entryId currentId rest


type Next a
    = Next Int a
    | First a


findNext : (a -> String) -> String -> List a -> Maybe (Next a)
findNext entryId currentId entries =
    case entries of
        [] ->
            Nothing

        first :: rest ->
            case rest of
                [] ->
                    if entryId first == currentId then
                        Just (First first)
                    else
                        Nothing

                next :: _ ->
                    if entryId first == currentId then
                        Just (Next 1 next)
                    else
                        findNextHelp first 1 entryId currentId rest


findNextHelp : a -> Int -> (a -> String) -> String -> List a -> Maybe (Next a)
findNextHelp first index entryId currentId entries =
    case entries of
        [] ->
            Nothing

        entry :: rest ->
            case rest of
                [] ->
                    Just (First first)

                next :: _ ->
                    if entryId entry == currentId then
                        Just (Next (index + 1) next)
                    else
                        findNextHelp first (index + 1) entryId currentId rest



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
