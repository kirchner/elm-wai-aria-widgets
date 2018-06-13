module Widget.Listbox
    exposing
        ( Behaviour
        , Entry
        , Event
        , Ids
        , Listbox
        , Msg
        , ScrollData
        , TypeAhead
        , UpdateConfig
        , ViewConfig
        , Views
        , arrowDownDecoder
        , arrowUpDecoder
        , divider
        , focus
        , focusNextOrFirstEntry
        , focusPreviousOrFirstEntry
        , focused
        , focusedEntry
        , noDivider
        , noTypeAhead
        , onAllEntriesSelect
        , onAllEntriesUnselect
        , onEntriesSelect
        , onEntrySelect
        , onEntryUnselect
        , onEscapeDown
        , onListboxBlur
        , onMouseDown
        , onMouseUp
        , option
        , scrollToFocus
        , simpleTypeAhead
        , subscriptions
        , typeAhead
        , unfocused
        , update
        , updateConfig
        , view
        , viewConfig
        , viewLazy
        )

{-|

@docs Listbox, unfocused, view, Ids

@docs Entry, option, divider

@docs update, Msg, subscriptions


# Configuration

@docs UpdateConfig, updateConfig, Behaviour

@docs ViewConfig, viewConfig, Views


## Type ahead

@docs TypeAhead, noTypeAhead, simpleTypeAhead, typeAhead


# Advanced usage

@docs viewLazy

@docs focused


## Events

@docs Event

@docs onEntrySelect, onEntriesSelect, onAllEntriesSelect

@docs onEntryUnselect, onAllEntriesUnselect

@docs onListboxBlur, onEscapeDown

@docs onMouseDown, onMouseUp


## Manage keyboard focus

@docs focusedEntry, focusNextOrFirstEntry, focusPreviousOrFirstEntry

@docs focus, scrollToFocus

@docs ScrollData, arrowDownDecoder, arrowUpDecoder

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
import Internal.Entries as Internal
    exposing
        ( Entry(..)
        , Next(..)
        , Previous(..)
        , find
        , findNext
        , findPrevious
        , findWith
        , range
        )
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Set
import Task
import Time
import Widget exposing (HtmlAttributes, HtmlDetails)


{-| TODO
-}
type Listbox
    = Listbox Data


type alias Data =
    { preventScroll : Bool
    , query : Query

    -- FOCUS
    , maybeKeyboardFocus : Maybe String
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
    Listbox
        { preventScroll = False
        , query = NoQuery
        , maybeKeyboardFocus = Nothing
        , maybeMouseFocus = Nothing
        , maybeLastSelectedEntry = Nothing
        , ulScrollTop = 0
        , ulClientHeight = 1000
        }


{-| TODO
-}
focused : UpdateConfig a -> String -> a -> ( Listbox, Cmd (Msg a) )
focused (UpdateConfig uniqueId _) id a =
    ( Listbox
        { preventScroll = False
        , query = NoQuery
        , maybeKeyboardFocus = Just (uniqueId a)
        , maybeMouseFocus = Nothing
        , maybeLastSelectedEntry = Nothing
        , ulScrollTop = 0
        , ulClientHeight = 1000
        }
    , Cmd.batch
        [ Browser.scrollIntoView (printEntryId id (uniqueId a))
            |> Task.attempt (\_ -> NoOp)
        , focusList id
        ]
    )


{-| TODO
-}
type alias Entry a divider =
    Internal.Entry a divider


{-| TODO
-}
option : a -> Entry a divider
option =
    Option


{-| TODO
-}
divider : divider -> Entry a divider
divider =
    Divider



---- EXTERNAL STATE MANIPULATION


{-| TODO
-}
focus : String -> Cmd (Msg a)
focus id =
    focusList id


{-| TODO
-}
focusedEntry : UpdateConfig a -> Listbox -> List (Entry a divider) -> Maybe a
focusedEntry (UpdateConfig uniqueId _) (Listbox { maybeKeyboardFocus }) allEntries =
    maybeKeyboardFocus
        |> Maybe.andThen (find uniqueId allEntries)
        |> Maybe.map Tuple.second


{-| TODO
-}
scrollToFocus : String -> Listbox -> Maybe ScrollData -> Cmd (Msg a)
scrollToFocus id (Listbox data) maybeScrollData =
    case data.maybeKeyboardFocus of
        Nothing ->
            Cmd.none

        Just keyboardFocus ->
            adjustScrollTopNew id keyboardFocus maybeScrollData


{-| TODO
-}
focusNextOrFirstEntry :
    UpdateConfig a
    -> List (Entry a divider)
    -> Listbox
    -> ( Listbox, Maybe outMsg )
focusNextOrFirstEntry config allEntries listbox =
    let
        (UpdateConfig uniqueId behaviour) =
            config

        (Listbox data) =
            listbox
    in
    case
        data.maybeKeyboardFocus
            |> Maybe.andThen (find uniqueId allEntries)
    of
        Nothing ->
            ( Listbox
                { data
                    | maybeKeyboardFocus =
                        allEntries
                            |> Internal.firstEntry
                            |> Maybe.map uniqueId
                }
            , Nothing
            )

        Just ( _, keyboardFocus ) ->
            case findNext uniqueId allEntries (uniqueId keyboardFocus) of
                Just (First firstEntry) ->
                    if behaviour.jumpAtEnds then
                        data
                            |> updateFocus behaviour uniqueId [] [] False firstEntry
                    else
                        ( listbox, Nothing )

                Just (Next newEntry) ->
                    data
                        |> updateFocus behaviour uniqueId [] [] False newEntry

                Nothing ->
                    ( listbox, Nothing )


{-| TODO
-}
focusPreviousOrFirstEntry :
    UpdateConfig a
    -> List (Entry a divider)
    -> Listbox
    -> ( Listbox, Maybe outMsg )
focusPreviousOrFirstEntry config allEntries listbox =
    let
        (UpdateConfig uniqueId behaviour) =
            config

        (Listbox data) =
            listbox

        firstEntry entries =
            case entries of
                [] ->
                    Nothing

                (Divider _) :: rest ->
                    firstEntry rest

                (Option a) :: _ ->
                    Just a
    in
    case
        data.maybeKeyboardFocus
            |> Maybe.andThen (find uniqueId allEntries)
    of
        Nothing ->
            ( Listbox
                { data
                    | maybeKeyboardFocus =
                        allEntries
                            |> firstEntry
                            |> Maybe.map uniqueId
                }
            , Nothing
            )

        Just ( _, keyboardFocus ) ->
            case findPrevious uniqueId allEntries (uniqueId keyboardFocus) of
                Just (Last lastEntry) ->
                    if behaviour.jumpAtEnds then
                        data
                            |> updateFocus behaviour uniqueId [] [] False lastEntry
                    else
                        ( listbox, Nothing )

                Just (Previous newEntry) ->
                    data
                        |> updateFocus behaviour uniqueId [] [] False newEntry

                Nothing ->
                    ( listbox, Nothing )



---- VIEW CONFIG


{-| TODO
-}
type ViewConfig a divider
    = ViewConfig (a -> String) (Views a divider)


{-| TODO
-}
viewConfig : (a -> String) -> Views a divider -> ViewConfig a divider
viewConfig =
    ViewConfig


{-| TODO
-}
type alias Views a divider =
    { ul : HtmlAttributes
    , liOption :
        { selected : Bool
        , keyboardFocused : Bool
        , mouseFocused : Bool
        , maybeQuery : Maybe String
        }
        -> a
        -> HtmlDetails
    , liDivider : divider -> HtmlDetails
    , empty : Html Never
    , focusable : Bool
    }


{-| TODO
-}
noDivider : Never -> HtmlDetails
noDivider _ =
    { attributes = []
    , children = []
    }



---- UPDATE CONFIG


{-| TODO
-}
type UpdateConfig a
    = UpdateConfig (a -> String) (Behaviour a)


{-| TODO
-}
updateConfig : (a -> String) -> Behaviour a -> UpdateConfig a
updateConfig =
    UpdateConfig


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
        \query a ->
            String.toLower (entryToString a)
                |> String.startsWith (String.toLower query)


{-| TODO
-}
typeAhead : Int -> (String -> a -> Bool) -> TypeAhead a
typeAhead =
    TypeAhead



---- VIEW


{-| TODO
-}
type alias Ids =
    { id : String
    , labelledBy : String
    }


{-| TODO
-}
view :
    ViewConfig a divider
    -> Ids
    -> Listbox
    -> List (Entry a divider)
    -> List a
    -> Html (Msg a)
view (ViewConfig uniqueId views) ids listbox allEntries selection =
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
    viewHelp renderedEntries uniqueId views ids listbox allEntries selection


{-| TODO
-}
viewLazy :
    (a -> Float)
    -> (divider -> Float)
    -> ViewConfig a divider
    -> Ids
    -> Listbox
    -> List (Entry a divider)
    -> List a
    -> Html (Msg a)
viewLazy entryHeight dividerHeight (ViewConfig uniqueId views) ids ((Listbox data) as listbox) allEntries selection =
    let
        renderedEntries =
            computeRenderedEntries
                entryHeight
                dividerHeight
                data.ulScrollTop
                data.ulClientHeight
                maybeFocusIndex
                allEntries

        maybeFocusIndex =
            data.maybeKeyboardFocus
                |> Maybe.andThen (find uniqueId allEntries)
                |> Maybe.map Tuple.first
    in
    viewHelp renderedEntries uniqueId views ids listbox allEntries selection


viewHelp :
    RenderedEntries a divider
    -> (a -> String)
    -> Views a divider
    -> Ids
    -> Listbox
    -> List (Entry a divider)
    -> List a
    -> Html (Msg a)
viewHelp renderedEntries uniqueId views ids (Listbox data) allEntries selection =
    let
        maybeQuery =
            case data.query of
                NoQuery ->
                    Nothing

                Query _ _ query ->
                    Just query
    in
    Html.ul
        ([ Attributes.id (printListId ids.id)
         , Attributes.style "position" "relative"
         , Attributes.attribute "role" "listbox"
         , Attributes.attribute "aria-labelledby" ids.labelledBy
         , Events.preventDefaultOn "keydown"
            (keyInfoDecoder
                |> Decode.andThen
                    (listKeydown
                        uniqueId
                        ids.id
                        data.maybeKeyboardFocus
                        renderedEntries.visibleEntries
                    )
                |> preventDefault
            )
         , Events.onMouseDown ListMouseDown
         , Events.onMouseUp ListMouseUp
         , Events.on "scroll" <|
            Decode.map2 ListScrolled
                (Decode.at [ "target", "scrollTop" ] Decode.float)
                (Decode.at [ "target", "clientHeight" ] Decode.float)
         , Events.on "focus"
            (Decode.oneOf
                [ data.maybeKeyboardFocus
                    |> Maybe.map
                        (currentScrollDataDecoder [ "target" ]
                            uniqueId
                            renderedEntries.visibleEntries
                            >> Decode.map Just
                        )
                    |> Maybe.withDefault (Decode.succeed Nothing)
                , Decode.succeed Nothing
                ]
                |> Decode.map (ListFocused ids.id)
            )
         , Events.on "blur" (Decode.succeed ListBlured)
         ]
            |> setAriaActivedescendant ids.id uniqueId data.maybeKeyboardFocus allEntries
            |> setTabindex views.focusable
            |> appendAttributes views.ul
        )
        (viewEntries
            uniqueId
            views
            ids
            data.maybeKeyboardFocus
            data.maybeMouseFocus
            selection
            maybeQuery
            renderedEntries
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
    (a -> String)
    -> String
    -> Maybe String
    -> List (Entry a divider)
    -> KeyInfo
    -> Decoder (Msg a)
listKeydown uniqueId id maybeKeyboardFocus visibleEntries { code, shiftDown, controlDown } =
    case code of
        "ArrowUp" ->
            Decode.oneOf
                [ maybeKeyboardFocus
                    |> Maybe.map
                        (previousScrollDataDecoder [ "target" ] uniqueId visibleEntries)
                    |> Maybe.withDefault (Decode.succeed Nothing)
                , Decode.succeed Nothing
                ]
                |> Decode.map (ListArrowUpPressed id shiftDown)

        "ArrowDown" ->
            Decode.oneOf
                [ maybeKeyboardFocus
                    |> Maybe.map
                        (nextScrollDataDecoder [ "target" ] uniqueId visibleEntries)
                    |> Maybe.withDefault (Decode.succeed Nothing)
                , Decode.succeed Nothing
                ]
                |> Decode.map (ListArrowDownPressed id shiftDown)

        "Enter" ->
            Decode.succeed (ListEnterPressed id)

        "Escape" ->
            Decode.succeed ListEscapePressed

        " " ->
            if shiftDown then
                Decode.succeed (ListShiftSpacePressed id)
            else
                Decode.succeed (ListSpacePressed id)

        "Home" ->
            if shiftDown && controlDown then
                Decode.succeed (ListControlShiftHomePressed id)
            else
                Decode.succeed (ListHomePressed id)

        "End" ->
            if shiftDown && controlDown then
                Decode.succeed (ListControlShiftEndPressed id)
            else
                Decode.succeed (ListEndPressed id)

        "a" ->
            if controlDown then
                Decode.succeed ListControlAPressed
            else if String.length code == 1 then
                Decode.succeed (ListKeyPressed id code)
            else
                Decode.fail "not handling that key here"

        _ ->
            if String.length code == 1 then
                Decode.succeed (ListKeyPressed id code)
            else
                Decode.fail "not handling that key here"


{-| TODO
-}
arrowUpDecoder :
    ViewConfig a divider
    -> List String
    -> Listbox
    -> List (Entry a divider)
    -> Decoder (Maybe ScrollData)
arrowUpDecoder (ViewConfig uniqueId _) path (Listbox data) allEntries =
    case data.maybeKeyboardFocus of
        Nothing ->
            Decode.succeed Nothing

        Just keyboardFocus ->
            previousScrollDataDecoder path uniqueId allEntries keyboardFocus


{-| TODO
-}
arrowDownDecoder :
    ViewConfig a divider
    -> List String
    -> Listbox
    -> List (Entry a divider)
    -> Decoder (Maybe ScrollData)
arrowDownDecoder (ViewConfig uniqueId _) path (Listbox data) allEntries =
    case data.maybeKeyboardFocus of
        Nothing ->
            Decode.succeed Nothing

        Just keyboardFocus ->
            nextScrollDataDecoder path uniqueId allEntries keyboardFocus



---- SCROLL DATA


{-| TODO
-}
type alias CurrentScrollData =
    { ulScrollTop : Float
    , ulClientHeight : Float
    , ulClientTop : Float
    , liOffsetTop : Float
    , liOffsetHeight : Float
    }


currentScrollDataDecoder :
    List String
    -> (a -> String)
    -> List (Entry a divider)
    -> String
    -> Decoder CurrentScrollData
currentScrollDataDecoder path uniqueId visibleEntries currentFocus =
    case indexOfCurrentEntry 0 uniqueId visibleEntries currentFocus of
        Nothing ->
            Decode.fail "no scroll data available"

        Just index ->
            let
                requiredAt subPath =
                    Decode.requiredAt (path ++ subPath) Decode.float
            in
            Decode.succeed CurrentScrollData
                |> requiredAt [ "scrollTop" ]
                |> requiredAt [ "clientHeight" ]
                |> requiredAt [ "clientTop" ]
                |> requiredAt [ "childNodes", String.fromInt (index + 2), "offsetTop" ]
                |> requiredAt [ "childNodes", String.fromInt (index + 2), "offsetHeight" ]


indexOfCurrentEntry : Int -> (a -> String) -> List (Entry a divider) -> String -> Maybe Int
indexOfCurrentEntry currentIndex uniqueId entries id =
    case entries of
        [] ->
            Nothing

        (Divider _) :: rest ->
            indexOfCurrentEntry (currentIndex + 1) uniqueId rest id

        (Option a) :: rest ->
            if uniqueId a == id then
                Just currentIndex
            else
                indexOfCurrentEntry (currentIndex + 1) uniqueId rest id


{-| TODO
-}
type alias ScrollData =
    { ulScrollTop : Float
    , ulClientHeight : Float
    , ulClientTop : Float
    , liCurrentOffsetTop : Float
    , liCurrentOffsetHeight : Float
    , liNewOffsetTop : Float
    , liNewOffsetHeight : Float
    }


previousScrollDataDecoder :
    List String
    -> (a -> String)
    -> List (Entry a divider)
    -> String
    -> Decoder (Maybe ScrollData)
previousScrollDataDecoder path uniqueId entries currentFocus =
    case indexOfCurrentAndPreviousEntry 0 uniqueId entries currentFocus of
        Nothing ->
            Decode.succeed Nothing

        Just { currentIndex, previousIndex } ->
            let
                requiredAt subPath =
                    Decode.requiredAt (path ++ subPath) Decode.float
            in
            Decode.map Just
                (Decode.succeed ScrollData
                    |> requiredAt [ "scrollTop" ]
                    |> requiredAt [ "clientHeight" ]
                    |> requiredAt [ "clientTop" ]
                    |> requiredAt [ "childNodes", String.fromInt (currentIndex + 2), "offsetTop" ]
                    |> requiredAt [ "childNodes", String.fromInt (currentIndex + 2), "offsetHeight" ]
                    |> requiredAt [ "childNodes", String.fromInt (previousIndex + 2), "offsetTop" ]
                    |> requiredAt [ "childNodes", String.fromInt (previousIndex + 2), "offsetHeight" ]
                )


indexOfCurrentAndPreviousEntry :
    Int
    -> (a -> String)
    -> List (Entry a divider)
    -> String
    -> Maybe { currentIndex : Int, previousIndex : Int }
indexOfCurrentAndPreviousEntry currentIndex uniqueId entries id =
    case entries of
        [] ->
            Nothing

        (Divider _) :: rest ->
            indexOfCurrentAndPreviousEntry (currentIndex + 1) uniqueId rest id

        (Option first) :: rest ->
            indexOfCurrentAndPreviousEntryHelp currentIndex (currentIndex + 1) uniqueId rest id


indexOfCurrentAndPreviousEntryHelp :
    Int
    -> Int
    -> (a -> String)
    -> List (Entry a divider)
    -> String
    -> Maybe { currentIndex : Int, previousIndex : Int }
indexOfCurrentAndPreviousEntryHelp previousIndex currentIndex uniqueId entries id =
    case entries of
        [] ->
            Nothing

        (Divider _) :: rest ->
            indexOfCurrentAndPreviousEntryHelp previousIndex (currentIndex + 1) uniqueId rest id

        (Option next) :: rest ->
            if uniqueId next == id then
                Just
                    { currentIndex = currentIndex
                    , previousIndex = previousIndex
                    }
            else
                indexOfCurrentAndPreviousEntryHelp currentIndex (currentIndex + 1) uniqueId rest id


nextScrollDataDecoder :
    List String
    -> (a -> String)
    -> List (Entry a divider)
    -> String
    -> Decoder (Maybe ScrollData)
nextScrollDataDecoder path uniqueId entries currentFocus =
    case indexOfCurrentAndNextEntry 0 uniqueId entries currentFocus of
        Nothing ->
            Decode.succeed Nothing

        Just { currentIndex, nextIndex } ->
            let
                requiredAt subPath =
                    Decode.requiredAt (path ++ subPath) Decode.float
            in
            Decode.map Just
                (Decode.succeed ScrollData
                    |> requiredAt [ "scrollTop" ]
                    |> requiredAt [ "clientHeight" ]
                    |> requiredAt [ "clientTop" ]
                    |> requiredAt [ "childNodes", String.fromInt (currentIndex + 2), "offsetTop" ]
                    |> requiredAt [ "childNodes", String.fromInt (currentIndex + 2), "offsetHeight" ]
                    |> requiredAt [ "childNodes", String.fromInt (nextIndex + 2), "offsetTop" ]
                    |> requiredAt [ "childNodes", String.fromInt (nextIndex + 2), "offsetHeight" ]
                )


indexOfCurrentAndNextEntry :
    Int
    -> (a -> String)
    -> List (Entry a divider)
    -> String
    -> Maybe { currentIndex : Int, nextIndex : Int }
indexOfCurrentAndNextEntry currentIndex uniqueId entries id =
    let
        indices nextIndex =
            { currentIndex = currentIndex
            , nextIndex = nextIndex
            }
    in
    case entries of
        [] ->
            Nothing

        (Divider _) :: rest ->
            indexOfCurrentAndNextEntry (currentIndex + 1) uniqueId rest id

        (Option first) :: rest ->
            if uniqueId first == id then
                indexOfNextEntry (currentIndex + 1) rest
                    |> Maybe.map indices
            else
                indexOfCurrentAndNextEntry (currentIndex + 1) uniqueId rest id


indexOfNextEntry : Int -> List (Entry a divider) -> Maybe Int
indexOfNextEntry currentIndex entries =
    case entries of
        [] ->
            Nothing

        (Divider _) :: rest ->
            indexOfNextEntry (currentIndex + 1) rest

        (Option _) :: _ ->
            Just currentIndex



----


viewEntries :
    (a -> String)
    -> Views a divider
    -> Ids
    -> Maybe String
    -> Maybe String
    -> List a
    -> Maybe String
    -> RenderedEntries a divider
    -> List (Html (Msg a))
viewEntries uniqueId views ids maybeKeyboardFocus maybeMouseFocus selection maybeQuery renderedEntries =
    let
        entryConfig =
            { id = ids.id
            , liOption = views.liOption
            , liDivider = views.liDivider
            , uniqueId = uniqueId
            }

        maybeUniqueId e =
            case e of
                Divider _ ->
                    Nothing

                Option a ->
                    Just (uniqueId a)

        selected e s =
            case s of
                [] ->
                    False

                first :: rest ->
                    case e of
                        Divider _ ->
                            selected e rest

                        Option a ->
                            if first == a then
                                True
                            else
                                selected e rest

        viewEntryWrapper e =
            viewEntry entryConfig
                maybeQuery
                (selected e selection)
                (maybeKeyboardFocus == maybeUniqueId e)
                (maybeMouseFocus == maybeUniqueId e)
                e
    in
    List.concat
        [ spacer renderedEntries.spaceAboveFirst
        , renderedEntries.entriesAbove
            |> List.map viewEntryWrapper
        , spacer renderedEntries.spaceAboveSecond
        , renderedEntries.visibleEntries
            |> List.map viewEntryWrapper
        , spacer renderedEntries.spaceBelowFirst
        , renderedEntries.entriesBelow
            |> List.map viewEntryWrapper
        , spacer renderedEntries.spaceBelowSecond
        ]


viewEntry :
    { id : String
    , liOption :
        { selected : Bool
        , keyboardFocused : Bool
        , mouseFocused : Bool
        , maybeQuery : Maybe String
        }
        -> a
        -> HtmlDetails
    , liDivider : divider -> HtmlDetails
    , uniqueId : a -> String
    }
    -> Maybe String
    -> Bool
    -> Bool
    -> Bool
    -> Entry a divider
    -> Html (Msg a)
viewEntry config maybeQuery selected keyboardFocused mouseFocused e =
    case e of
        Option a ->
            let
                { attributes, children } =
                    config.liOption
                        { selected = selected
                        , keyboardFocused = keyboardFocused
                        , mouseFocused = mouseFocused
                        , maybeQuery = maybeQuery
                        }
                        a
            in
            Html.li
                ([ Events.onMouseEnter (EntryMouseEntered (config.uniqueId a))
                 , Events.onMouseLeave EntryMouseLeft
                 , Events.onClick (EntryClicked a)
                 , Attributes.id (printEntryId config.id (config.uniqueId a))
                 , Attributes.attribute "role" "option"
                 ]
                    |> appendAttributes attributes
                )
                (children
                    |> List.map (Html.map (\_ -> NoOp))
                )

        Divider d ->
            let
                { attributes, children } =
                    config.liDivider d
            in
            Html.li
                (appendAttributes attributes [])
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
    -> List (Entry a divider)
    -> List (Html.Attribute msg)
    -> List (Html.Attribute msg)
setAriaActivedescendant id uniqueId maybeKeyboardFocus entries attrs =
    maybeKeyboardFocus
        |> Maybe.andThen (find uniqueId entries)
        |> Maybe.map
            (\( _, a ) ->
                Attributes.attribute "aria-activedescendant"
                    (printEntryId id (uniqueId a))
                    :: attrs
            )
        |> Maybe.withDefault attrs


setTabindex : Bool -> List (Html.Attribute msg) -> List (Html.Attribute msg)
setTabindex focusable attrs =
    if focusable then
        Attributes.tabindex 0 :: attrs
    else
        attrs



---- UPDATE


{-| TODO
-}
type Msg a
    = NoOp
      -- LIST
    | ListMouseDown
    | ListMouseUp
    | ListFocused String (Maybe CurrentScrollData)
    | ListBlured
    | ListScrolled Float Float
    | ListArrowUpPressed String Bool (Maybe ScrollData)
    | ListArrowDownPressed String Bool (Maybe ScrollData)
    | ListEnterPressed String
    | ListEscapePressed
    | ListSpacePressed String
    | ListShiftSpacePressed String
    | ListHomePressed String
    | ListControlShiftHomePressed String
    | ListEndPressed String
    | ListControlShiftEndPressed String
    | ListControlAPressed
      -- QUERY
    | ListKeyPressed String String
    | CurrentTimeReceived String String Time.Posix
    | Tick Time.Posix
      -- ENTRY
    | EntryMouseEntered String
    | EntryMouseLeft
    | EntryClicked a


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
    | OnMouseDown outMsg
    | OnMouseUp outMsg


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


{-| TODO
-}
onMouseDown : outMsg -> Event a outMsg
onMouseDown =
    OnMouseDown


{-| TODO
-}
onMouseUp : outMsg -> Event a outMsg
onMouseUp =
    OnMouseUp


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


sendMouseDown : List (Event a outMsg) -> Maybe outMsg
sendMouseDown events =
    case events of
        [] ->
            Nothing

        (OnMouseDown mouseDown) :: _ ->
            Just mouseDown

        _ :: rest ->
            sendMouseDown rest


sendMouseUp : List (Event a outMsg) -> Maybe outMsg
sendMouseUp events =
    case events of
        [] ->
            Nothing

        (OnMouseUp mouseUp) :: _ ->
            Just mouseUp

        _ :: rest ->
            sendMouseUp rest


{-| TODO
-}
update :
    UpdateConfig a
    -> List (Event a outMsg)
    -> Listbox
    -> List (Entry a divider)
    -> List a
    -> Msg a
    -> ( Listbox, Cmd (Msg a), Maybe outMsg )
update (UpdateConfig uniqueId behaviour) events ((Listbox data) as listbox) allEntries selection msg =
    case msg of
        -- LIST
        ListMouseDown ->
            ( Listbox { data | preventScroll = True }
            , Cmd.none
            , sendMouseDown events
            )

        ListMouseUp ->
            ( listbox
            , Cmd.none
            , sendMouseUp events
            )

        ListFocused id maybeScrollData ->
            let
                maybeNewEntry =
                    data.maybeKeyboardFocus
                        |> or data.maybeLastSelectedEntry
                        |> Maybe.andThen (find uniqueId allEntries)
                        |> Maybe.map Tuple.second
                        |> or (List.head selection)
                        |> or (Internal.firstEntry allEntries)
            in
            case maybeNewEntry of
                Nothing ->
                    ( listbox, Cmd.none, Nothing )

                Just newEntry ->
                    updateFocus behaviour uniqueId events selection False newEntry data
                        |> andDo
                            (if data.preventScroll then
                                Cmd.none
                             else
                                adjustScrollTop id (uniqueId newEntry) maybeScrollData
                            )

        ListBlured ->
            ( Listbox
                { data
                    | preventScroll = False
                    , query = NoQuery
                }
            , Cmd.none
            , sendListboxBlured events
            )

        ListScrolled ulScrollTop ulClientHeight ->
            ( Listbox
                { data
                    | ulScrollTop = ulScrollTop
                    , ulClientHeight = ulClientHeight
                }
            , Cmd.none
            , Nothing
            )

        ListArrowUpPressed id shiftDown maybeScrollData ->
            case
                data.maybeKeyboardFocus
                    |> Maybe.andThen (findPrevious uniqueId allEntries)
            of
                Just (Last lastEntry) ->
                    if behaviour.jumpAtEnds then
                        data
                            |> updateFocus behaviour uniqueId events selection shiftDown lastEntry
                            |> andDo (scrollListToBottom id)
                    else
                        ( Listbox { data | query = NoQuery }
                        , Cmd.none
                        , Nothing
                        )

                Just (Previous newEntry) ->
                    data
                        |> updateFocus behaviour uniqueId events selection shiftDown newEntry
                        |> andDo (adjustScrollTopNew id (uniqueId newEntry) maybeScrollData)

                Nothing ->
                    ( listbox, Cmd.none, Nothing )

        ListArrowDownPressed id shiftDown maybeScrollData ->
            case
                data.maybeKeyboardFocus
                    |> Maybe.andThen (findNext uniqueId allEntries)
            of
                Just (First firstEntry) ->
                    if behaviour.jumpAtEnds then
                        data
                            |> updateFocus behaviour uniqueId events selection shiftDown firstEntry
                            |> andDo (scrollListToTop id)
                    else
                        ( Listbox { data | query = NoQuery }
                        , Cmd.none
                        , Nothing
                        )

                Just (Next newEntry) ->
                    data
                        |> updateFocus behaviour uniqueId events selection shiftDown newEntry
                        |> andDo (adjustScrollTopNew id (uniqueId newEntry) maybeScrollData)

                Nothing ->
                    ( listbox
                    , Cmd.none
                    , Nothing
                    )

        ListEnterPressed id ->
            data.maybeKeyboardFocus
                |> Maybe.andThen (find uniqueId allEntries)
                |> Maybe.map
                    (\( _, a ) ->
                        if List.member a selection then
                            ( Listbox { data | maybeLastSelectedEntry = Nothing }
                            , Cmd.none
                            , sendEntryUnselected a events
                            )
                        else
                            ( Listbox { data | maybeLastSelectedEntry = Just (uniqueId a) }
                            , Cmd.none
                            , sendEntrySelected a events
                            )
                    )
                |> Maybe.withDefault ( listbox, Cmd.none, Nothing )

        ListEscapePressed ->
            ( listbox
            , Cmd.none
            , sendEscapeDown events
            )

        ListSpacePressed id ->
            data.maybeKeyboardFocus
                |> Maybe.andThen (find uniqueId allEntries)
                |> Maybe.map
                    (\( _, a ) ->
                        if List.member a selection then
                            ( Listbox { data | maybeLastSelectedEntry = Nothing }
                            , Cmd.none
                            , sendEntryUnselected a events
                            )
                        else
                            ( Listbox { data | maybeLastSelectedEntry = Just (uniqueId a) }
                            , Cmd.none
                            , sendEntrySelected a events
                            )
                    )
                |> Maybe.withDefault ( listbox, Cmd.none, Nothing )

        ListShiftSpacePressed id ->
            case ( data.maybeKeyboardFocus, data.maybeLastSelectedEntry ) of
                ( Just keyboardFocus, Just lastSelectedEntry ) ->
                    case range uniqueId keyboardFocus lastSelectedEntry allEntries of
                        [] ->
                            ( listbox, Cmd.none, Nothing )

                        selectedEntries ->
                            ( Listbox { data | maybeLastSelectedEntry = Just keyboardFocus }
                            , Cmd.none
                            , sendEntriesSelected events selectedEntries
                            )

                _ ->
                    ( listbox, Cmd.none, Nothing )

        ListHomePressed id ->
            case Internal.firstEntry allEntries of
                Nothing ->
                    ( listbox, Cmd.none, Nothing )

                Just firstEntry ->
                    data
                        |> updateFocus behaviour uniqueId events [] False firstEntry
                        |> andDo (scrollListToTop id)

        ListControlShiftHomePressed id ->
            case Internal.firstEntry allEntries of
                Nothing ->
                    ( listbox, Cmd.none, Nothing )

                Just firstEntry ->
                    let
                        newFocus =
                            uniqueId firstEntry
                    in
                    case data.maybeKeyboardFocus of
                        Nothing ->
                            ( listbox, Cmd.none, Nothing )

                        Just keyboardFocus ->
                            case range uniqueId newFocus keyboardFocus allEntries of
                                [] ->
                                    ( listbox, Cmd.none, Nothing )

                                selectedEntries ->
                                    ( Listbox
                                        { data
                                            | maybeKeyboardFocus = Just newFocus
                                            , maybeMouseFocus =
                                                if behaviour.separateFocus then
                                                    data.maybeMouseFocus
                                                else
                                                    Just newFocus
                                            , maybeLastSelectedEntry = Just newFocus
                                        }
                                    , scrollListToTop id
                                    , sendEntriesSelected events selectedEntries
                                    )

        ListEndPressed id ->
            case Internal.lastEntry allEntries of
                Nothing ->
                    ( listbox, Cmd.none, Nothing )

                Just lastEntry ->
                    data
                        |> updateFocus behaviour uniqueId events [] False lastEntry
                        |> andDo (scrollListToBottom id)

        ListControlShiftEndPressed id ->
            case Internal.lastEntry allEntries of
                Nothing ->
                    ( listbox, Cmd.none, Nothing )

                Just lastEntry ->
                    let
                        newFocus =
                            uniqueId lastEntry
                    in
                    case data.maybeKeyboardFocus of
                        Nothing ->
                            ( listbox, Cmd.none, Nothing )

                        Just keyboardFocus ->
                            case range uniqueId newFocus keyboardFocus allEntries of
                                [] ->
                                    ( listbox, Cmd.none, Nothing )

                                selectedEntries ->
                                    ( Listbox
                                        { data
                                            | maybeKeyboardFocus = Just newFocus
                                            , maybeMouseFocus =
                                                if behaviour.separateFocus then
                                                    data.maybeMouseFocus
                                                else
                                                    Just newFocus
                                            , maybeLastSelectedEntry = Just newFocus
                                        }
                                    , scrollListToBottom id
                                    , sendEntriesSelected events selectedEntries
                                    )

        ListControlAPressed ->
            let
                allEntriesSet =
                    allEntries
                        |> List.filterMap
                            (\e ->
                                case e of
                                    Divider _ ->
                                        Nothing

                                    Option a ->
                                        Just (uniqueId a)
                            )
                        |> Set.fromList

                selectionSet =
                    selection
                        |> List.map uniqueId
                        |> Set.fromList
            in
            ( listbox
            , Cmd.none
            , if Set.isEmpty (Set.diff allEntriesSet selectionSet) then
                sendAllEntriesUnselected events
              else
                sendAllEntriesSelected events
            )

        -- QUERY
        ListKeyPressed id code ->
            case behaviour.typeAhead of
                NoTypeAhead ->
                    ( listbox, Cmd.none, Nothing )

                TypeAhead _ _ ->
                    ( listbox
                    , Time.now
                        |> Task.perform
                            (CurrentTimeReceived id code)
                    , Nothing
                    )

        CurrentTimeReceived id code currentTime ->
            case behaviour.typeAhead of
                NoTypeAhead ->
                    ( listbox, Cmd.none, Nothing )

                TypeAhead timeout matchesQuery ->
                    let
                        ( newQuery, queryText ) =
                            case data.query of
                                NoQuery ->
                                    ( Query timeout currentTime code, code )

                                Query _ _ query ->
                                    ( Query timeout currentTime (query ++ code), query ++ code )

                        newKeyboardFocus =
                            case data.maybeKeyboardFocus of
                                Nothing ->
                                    Nothing

                                Just keyboardFocus ->
                                    findWith matchesQuery uniqueId keyboardFocus queryText allEntries
                    in
                    case newKeyboardFocus of
                        Nothing ->
                            ( listbox, Cmd.none, Nothing )

                        Just newFocus ->
                            ( Listbox
                                { data
                                    | query = newQuery
                                    , maybeKeyboardFocus = Just newFocus
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
                    listbox

                Query timeout time _ ->
                    if Time.posixToMillis currentTime - Time.posixToMillis time > timeout then
                        Listbox { data | query = NoQuery }
                    else
                        listbox
            , Cmd.none
            , Nothing
            )

        -- ENTRY
        EntryMouseEntered newFocus ->
            ( Listbox
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

        EntryMouseLeft ->
            ( Listbox
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

        EntryClicked a ->
            ( Listbox
                { data
                    | query = NoQuery

                    -- FOCUS
                    , maybeKeyboardFocus = Just (uniqueId a)
                    , maybeMouseFocus = Just (uniqueId a)
                    , maybeLastSelectedEntry = Just (uniqueId a)
                }
            , Cmd.none
            , toggleSelectState events selection a
            )

        NoOp ->
            ( listbox, Cmd.none, Nothing )


andDo : Cmd msg -> ( a, b ) -> ( a, Cmd msg, b )
andDo cmd ( a, b ) =
    ( a, cmd, b )


or : Maybe a -> Maybe a -> Maybe a
or fallback default =
    case default of
        Nothing ->
            fallback

        Just _ ->
            default


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
    -> Data
    -> ( Listbox, Maybe outMsg )
updateFocus behaviour uniqueId events selection shiftDown newEntry data =
    let
        newFocus =
            uniqueId newEntry
    in
    if behaviour.selectionFollowsFocus then
        ( Listbox
            { data
                | query = NoQuery
                , maybeKeyboardFocus = Just newFocus
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
            ( Listbox
                { data
                    | query = NoQuery
                    , maybeKeyboardFocus = Just newFocus
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
            ( Listbox
                { data
                    | query = NoQuery
                    , maybeKeyboardFocus = Just newFocus
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
        ( Listbox
            { data
                | query = NoQuery
                , maybeKeyboardFocus = Just newFocus
                , maybeMouseFocus =
                    if behaviour.separateFocus then
                        data.maybeMouseFocus
                    else
                        Just newFocus
            }
        , Nothing
        )


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
subscriptions (Listbox data) =
    case data.query of
        NoQuery ->
            Sub.none

        Query timeout _ _ ->
            Time.every (toFloat (timeout // 3)) Tick



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


adjustScrollTop : String -> String -> Maybe CurrentScrollData -> Cmd (Msg a)
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


adjustScrollTopNew : String -> String -> Maybe ScrollData -> Cmd (Msg a)
adjustScrollTopNew id entryId maybeScrollData =
    case maybeScrollData of
        Nothing ->
            Browser.scrollIntoView (printEntryId id entryId)
                |> Task.attempt (\_ -> NoOp)

        Just { ulScrollTop, ulClientHeight, ulClientTop, liCurrentOffsetTop, liCurrentOffsetHeight, liNewOffsetTop, liNewOffsetHeight } ->
            if
                (liCurrentOffsetTop - ulClientTop + liCurrentOffsetHeight < ulScrollTop)
                    || (liCurrentOffsetTop > ulScrollTop + ulClientHeight)
            then
                -- current entry is not visible
                centerScrollTop id
                    { ulScrollTop = ulScrollTop
                    , ulClientHeight = ulClientHeight
                    , ulClientTop = ulClientTop
                    , liOffsetTop = liNewOffsetTop
                    , liOffsetHeight = liNewOffsetHeight
                    }
            else
            -- current entry is visible
            if
                (liNewOffsetTop - ulClientTop + liNewOffsetHeight) > (ulScrollTop + ulClientHeight)
            then
                -- lower parts of new entry are hidden
                Browser.setScrollTop (printListId id)
                    (liNewOffsetTop - ulClientTop + liNewOffsetHeight - ulClientHeight)
                    |> Task.attempt (\_ -> NoOp)
            else if liNewOffsetTop - ulClientTop < ulScrollTop then
                -- upper parts of new entry are hidden
                Browser.setScrollTop (printListId id) (liNewOffsetTop - ulClientTop)
                    |> Task.attempt (\_ -> NoOp)
            else
                Cmd.none


centerScrollTop : String -> CurrentScrollData -> Cmd (Msg a)
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


type alias RenderedEntries a divider =
    { spaceAboveFirst : Float
    , droppedAboveFirst : Int
    , spaceAboveSecond : Float
    , droppedAboveSecond : Int
    , spaceBelowFirst : Float
    , droppedBelowFirst : Int
    , spaceBelowSecond : Float
    , droppedBelowSecond : Int
    , entriesAbove : List (Entry a divider)
    , visibleEntries : List (Entry a divider)
    , entriesBelow : List (Entry a divider)
    }


computeRenderedEntries :
    (a -> Float)
    -> (divider -> Float)
    -> Float
    -> Float
    -> Maybe Int
    -> List (Entry a divider)
    -> RenderedEntries a divider
computeRenderedEntries entryHeight dividerHeight ulScrollTop ulClientHeight maybeFocusIndex entries =
    --let
    --    initialRenderedEntries =
    --        { spaceAboveFirst = 0
    --        , droppedAboveFirst = 0
    --        , spaceAboveSecond = 0
    --        , droppedAboveSecond = 0
    --        , spaceBelowFirst = 0
    --        , droppedBelowFirst = 0
    --        , spaceBelowSecond = 0
    --        , droppedBelowSecond = 0
    --        , entriesAbove = []
    --        , visibleEntries = []
    --        , entriesBelow = []
    --        }
    --    withoutIndex e currentHeight data =
    --        let
    --            height =
    --                entryHeight e
    --        in
    --        if currentHeight < ulScrollTop - 200 then
    --            -- entry is above the rendered range
    --            { data
    --                | spaceAboveFirst = data.spaceAboveFirst + height
    --                , droppedAboveFirst = data.droppedAboveFirst + 1
    --            }
    --        else if currentHeight >= (ulScrollTop + ulClientHeight + 200) then
    --            { data
    --                | spaceBelowFirst = data.spaceBelowFirst + height
    --                , droppedBelowFirst = data.droppedBelowFirst + 1
    --            }
    --        else
    --            -- entry is within the rendered range
    --            { data | visibleEntries = e :: data.visibleEntries }
    --    withIndex index currentIndex e currentHeight data =
    --        let
    --            height =
    --                entryHeight e
    --        in
    --        if currentHeight < ulScrollTop - 200 then
    --            -- entry is above the rendered range
    --            if currentIndex < index - 1 then
    --                -- entry is before focused entry
    --                { data
    --                    | spaceAboveFirst = data.spaceAboveFirst + height
    --                    , droppedAboveFirst = data.droppedAboveFirst + 1
    --                }
    --            else if currentIndex > index + 1 then
    --                -- entry is after focused entry
    --                { data
    --                    | spaceAboveSecond = data.spaceAboveSecond + height
    --                    , droppedAboveSecond = data.droppedAboveSecond + 1
    --                }
    --            else
    --                -- entry is focused or next to focused entry
    --                { data | entriesAbove = e :: data.entriesAbove }
    --        else if currentHeight > (ulScrollTop + ulClientHeight + 200) then
    --            -- entry is below the rendered range
    --            if currentIndex < index - 1 then
    --                -- entry is before focused entry
    --                { data
    --                    | spaceBelowFirst = data.spaceBelowFirst + height
    --                    , droppedBelowFirst = data.droppedBelowFirst + 1
    --                }
    --            else if currentIndex > index + 1 then
    --                -- entry is after focused entry
    --                { data
    --                    | spaceBelowSecond = data.spaceBelowSecond + height
    --                    , droppedBelowSecond = data.droppedBelowSecond + 1
    --                }
    --            else
    --                -- entry is focused or next to focused entry
    --                { data | entriesBelow = e :: data.entriesBelow }
    --        else
    --            -- entry is within the rendered range
    --            { data | visibleEntries = e :: data.visibleEntries }
    --    reverseLists renderedEntries =
    --        { renderedEntries
    --            | entriesAbove = List.reverse renderedEntries.entriesAbove
    --            , visibleEntries = List.reverse renderedEntries.visibleEntries
    --            , entriesBelow = List.reverse renderedEntries.entriesBelow
    --        }
    --in
    --reverseLists <|
    --    case maybeFocusIndex of
    --        Nothing ->
    --            entries
    --                |> List.foldl
    --                    (\e ( currentHeight, data ) ->
    --                        ( currentHeight + entryHeight e
    --                        , withoutIndex e currentHeight data
    --                        )
    --                    )
    --                    ( 0, initialRenderedEntries )
    --                |> Tuple.second
    --        Just index ->
    --            entries
    --                |> List.foldl
    --                    (\e ( ( currentIndex, currentHeight ), data ) ->
    --                        ( ( currentIndex + 1
    --                          , currentHeight + entryHeight e
    --                          )
    --                        , withIndex index currentIndex e currentHeight data
    --                        )
    --                    )
    --                    ( ( 0, 0 ), initialRenderedEntries )
    --                |> Tuple.second
    { spaceAboveFirst = 0
    , droppedAboveFirst = 0
    , spaceAboveSecond = 0
    , droppedAboveSecond = 0
    , spaceBelowFirst = 0
    , droppedBelowFirst = 0
    , spaceBelowSecond = 0
    , droppedBelowSecond = 0
    , entriesAbove = []
    , visibleEntries = entries
    , entriesBelow = []
    }
