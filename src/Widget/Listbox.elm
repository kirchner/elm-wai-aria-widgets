module Widget.Listbox
    exposing
        ( Behaviour
        , DomInfo
        , Entry
        , Listbox
        , Msg
        , Position
        , TypeAhead
        , UpdateConfig
        , ViewConfig
        , Views
        , customView
        , customViewUnique
        , divider
        , domInfo
        , focus
        , focusEntry
        , focusNextOrFirstEntry
        , focusPreviousOrFirstEntry
        , focusedEntry
        , fromFocused
        , hoveredEntry
        , init
        , noDivider
        , noTypeAhead
        , option
        , scrollIntoViewVia
        , simpleTypeAhead
        , subscriptions
        , typeAhead
        , update
        , updateConfig
        , updateUnique
        , view
        , viewConfig
        , viewLazy
        , viewUnique
        , withUnique
        )

{-|

@docs Listbox, init, view, viewUnique

@docs Entry, option, divider

@docs update, updateUnique, Msg, subscriptions


# Configuration


## Update

@docs UpdateConfig, updateConfig, Behaviour


## View

@docs ViewConfig, viewConfig, Views, noDivider


## Type ahead

@docs TypeAhead, noTypeAhead, simpleTypeAhead, typeAhead


# Advanced usage

@docs customView, customViewUnique, viewLazy


## State manipulation


### Keyboard focus

@docs focusedEntry, hoveredEntry

@docs focusEntry, focusNextOrFirstEntry, focusPreviousOrFirstEntry

@docs withUnique


## DOM Stuff


### Focus

@docs focus


### Scroll

@docs scrollIntoViewVia, DomInfo, domInfo, Position, fromFocused

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

import Browser exposing (DomError)
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
import List.Extra as List
import Set
import Task exposing (Task)
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
init : Listbox
init =
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
focus : String -> Task DomError ()
focus id =
    Browser.focus (printListId id)


{-| TODO
-}
focusedEntry : UpdateConfig a -> Listbox -> List (Entry a divider) -> Maybe a
focusedEntry (UpdateConfig uniqueId _) (Listbox { maybeKeyboardFocus }) allEntries =
    maybeKeyboardFocus
        |> Maybe.andThen (find uniqueId allEntries)
        |> Maybe.map Tuple.second


{-| TODO
-}
hoveredEntry : UpdateConfig a -> Listbox -> List (Entry a divider) -> Maybe a
hoveredEntry (UpdateConfig uniqueId _) (Listbox { maybeMouseFocus }) allEntries =
    maybeMouseFocus
        |> Maybe.andThen (find uniqueId allEntries)
        |> Maybe.map Tuple.second


{-| TODO
-}
focusEntry : UpdateConfig a -> a -> Listbox -> List a -> ( Listbox, List a )
focusEntry config newEntry (Listbox data) selection =
    let
        (UpdateConfig uniqueId behaviour) =
            config
    in
    data
        |> updateFocus behaviour uniqueId selection False newEntry


{-| TODO
-}
focusNextOrFirstEntry :
    UpdateConfig a
    -> List (Entry a divider)
    -> Listbox
    -> List a
    -> ( Listbox, List a )
focusNextOrFirstEntry config allEntries listbox selection =
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
            , selection
            )

        Just ( _, keyboardFocus ) ->
            case findNext uniqueId allEntries (uniqueId keyboardFocus) of
                Just (First firstEntry) ->
                    if behaviour.jumpAtEnds then
                        data
                            |> updateFocus behaviour uniqueId selection False firstEntry
                    else
                        ( listbox, selection )

                Just (Next newEntry) ->
                    data
                        |> updateFocus behaviour uniqueId selection False newEntry

                Nothing ->
                    ( listbox, selection )


{-| TODO
-}
focusPreviousOrFirstEntry :
    UpdateConfig a
    -> List (Entry a divider)
    -> Listbox
    -> List a
    -> ( Listbox, List a )
focusPreviousOrFirstEntry config allEntries listbox selection =
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
            , selection
            )

        Just ( _, keyboardFocus ) ->
            case findPrevious uniqueId allEntries (uniqueId keyboardFocus) of
                Just (Last lastEntry) ->
                    if behaviour.jumpAtEnds then
                        data
                            |> updateFocus behaviour uniqueId selection False lastEntry
                    else
                        ( listbox, selection )

                Just (Previous newEntry) ->
                    data
                        |> updateFocus behaviour uniqueId selection False newEntry

                Nothing ->
                    ( listbox, selection )



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
viewUnique :
    ViewConfig a divider
    ->
        { id : String
        , labelledBy : String
        , lift : Msg a -> msg
        }
    -> List (Entry a divider)
    -> Listbox
    -> Maybe a
    -> Html msg
viewUnique config cfg entries listbox selection =
    view config cfg entries listbox (maybeToList selection)


{-| TODO

    view model =
        Html.div []
            [ Listbox.view viewConfig
                { id = "listbox"
                , labelledBy = "label"
                , lift = ListboxMsg
                }
                model.listbox
                entries
                model.selection
            ]

-}
view :
    ViewConfig a divider
    ->
        { id : String
        , labelledBy : String
        , lift : Msg a -> msg
        }
    -> List (Entry a divider)
    -> Listbox
    -> List a
    -> Html msg
view config { id, labelledBy, lift } =
    customView config
        { id = id
        , labelledBy = labelledBy
        , lift = lift
        , onKeyPress = Decode.fail "not handling this event here"
        , onMouseDown = Decode.fail "not handling this event here"
        , onMouseUp = Decode.fail "not handling this event here"
        , onBlur = Decode.fail "not handling this event here"
        }


{-| TODO
-}
customViewUnique :
    ViewConfig a divider
    ->
        { id : String
        , labelledBy : String
        , lift : Msg a -> msg
        , onKeyPress : Decoder msg
        , onMouseDown : Decoder msg
        , onMouseUp : Decoder msg
        , onBlur : Decoder msg
        }
    -> List (Entry a divider)
    -> Listbox
    -> Maybe a
    -> Html msg
customViewUnique config cfg allEntries listbox selection =
    customView config cfg allEntries listbox (maybeToList selection)


{-| TODO

    viewExample model =
        Html.div []
            [ Listbox.customView viewConfig
                { id = "listbox"
                , labelledBy = "label"
                , lift = ListboxMsg
                , onKeyPress =
                    Decode.field "key" Decode.string
                        |> Decode.andThen
                            (\code ->
                                case code of
                                    "ArrowUp" ->
                                        Decode.succeed ArrowUpPressed

                                    _ ->
                                        Decode.fail "not handling that key here"
                            )
                , onMouseDown = Decode.fail ""
                , onMouseUp = Decode.fail ""
                , onBlur = Decode.fail ""
                }
                model.listbox
                entries
                model.selection
            ]

-}
customView :
    ViewConfig a divider
    ->
        { id : String
        , labelledBy : String
        , lift : Msg a -> msg
        , onKeyPress : Decoder msg
        , onMouseDown : Decoder msg
        , onMouseUp : Decoder msg
        , onBlur : Decoder msg
        }
    -> List (Entry a divider)
    -> Listbox
    -> List a
    -> Html msg
customView (ViewConfig uniqueId views) cfg allEntries listbox selection =
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
    viewHelp renderedEntries uniqueId views cfg listbox allEntries selection


{-| TODO
-}
viewLazy :
    (a -> Float)
    -> (divider -> Float)
    -> ViewConfig a divider
    ->
        { id : String
        , labelledBy : String
        , lift : Msg a -> msg
        , onKeyPress : Decoder msg
        , onMouseDown : Decoder msg
        , onMouseUp : Decoder msg
        , onBlur : Decoder msg
        }
    -> List (Entry a divider)
    -> Listbox
    -> List a
    -> Html msg
viewLazy entryHeight dividerHeight (ViewConfig uniqueId views) cfg allEntries listbox selection =
    let
        (Listbox { ulScrollTop, ulClientHeight, maybeKeyboardFocus }) =
            listbox

        renderedEntries =
            computeRenderedEntries
                entryHeight
                dividerHeight
                ulScrollTop
                ulClientHeight
                maybeFocusIndex
                allEntries

        maybeFocusIndex =
            maybeKeyboardFocus
                |> Maybe.andThen (find uniqueId allEntries)
                |> Maybe.map Tuple.first
    in
    viewHelp renderedEntries uniqueId views cfg listbox allEntries selection


viewHelp :
    RenderedEntries a divider
    -> (a -> String)
    -> Views a divider
    ->
        { id : String
        , labelledBy : String
        , lift : Msg a -> msg
        , onKeyPress : Decoder msg
        , onMouseDown : Decoder msg
        , onMouseUp : Decoder msg
        , onBlur : Decoder msg
        }
    -> Listbox
    -> List (Entry a divider)
    -> List a
    -> Html msg
viewHelp renderedEntries uniqueId views cfg (Listbox data) allEntries selection =
    let
        maybeQuery =
            case data.query of
                NoQuery ->
                    Nothing

                Query _ _ query ->
                    Just query
    in
    Html.ul
        ([ Attributes.id (printListId cfg.id)
         , Attributes.style "position" "relative"
         , Attributes.attribute "role" "listbox"
         , Attributes.attribute "aria-labelledby" cfg.labelledBy
         , Events.preventDefaultOn "keypress"
            (Decode.oneOf
                [ cfg.onKeyPress
                , keyInfoDecoder
                    |> Decode.andThen
                        (listKeyPress
                            uniqueId
                            cfg.id
                            data.maybeKeyboardFocus
                            renderedEntries.visibleEntries
                        )
                    |> Decode.map cfg.lift
                ]
                |> preventDefault
            )
         , Events.on "mousedown" <|
            Decode.oneOf
                [ cfg.onMouseDown
                , Decode.succeed (cfg.lift ListMouseDown)
                ]
         , Events.on "mouseup" <|
            Decode.oneOf
                [ cfg.onMouseUp
                , Decode.succeed (cfg.lift ListMouseUp)
                ]
         , Events.on "scroll" <|
            Decode.map cfg.lift <|
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
                |> Decode.map (ListFocused cfg.id)
                |> Decode.map cfg.lift
            )
         , Events.on "blur" <|
            Decode.oneOf
                [ cfg.onBlur
                , Decode.succeed (cfg.lift ListBlured)
                ]
         ]
            |> setAriaActivedescendant cfg.id uniqueId data.maybeKeyboardFocus allEntries
            |> setTabindex views.focusable
            |> appendAttributes cfg.lift views.ul
        )
        (viewEntries
            uniqueId
            views
            cfg
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


listKeyPress :
    (a -> String)
    -> String
    -> Maybe String
    -> List (Entry a divider)
    -> KeyInfo
    -> Decoder (Msg a)
listKeyPress uniqueId id maybeKeyboardFocus visibleEntries { code, shiftDown, controlDown } =
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



---- DOM INFO


{-| TODO
-}
type DomInfo
    = NoDomInfo
    | DomInfo DomInfoData


type alias DomInfoData =
    { ulScrollTop : Float
    , ulClientHeight : Float
    , ulClientTop : Float
    , liOffsetTop : Float
    , liOffsetHeight : Float
    }


{-| TODO
-}
type Position a divider
    = FromFocused Int (a -> String) (Maybe String) (List (Entry a divider))


{-| TODO
-}
fromFocused : ViewConfig a divider -> List (Entry a divider) -> Listbox -> Int -> Position a divider
fromFocused (ViewConfig uniqueId _) entries (Listbox { maybeKeyboardFocus }) distance =
    FromFocused distance uniqueId maybeKeyboardFocus entries


{-| TODO
-}
domInfo : List String -> Position a divider -> Decoder DomInfo
domInfo path position =
    case position of
        FromFocused distance uniqueId maybeKeyboardFocus entries ->
            case Maybe.andThen (indexOfCurrentEntry 0 uniqueId entries) maybeKeyboardFocus of
                Nothing ->
                    Decode.succeed NoDomInfo

                Just index ->
                    let
                        requiredAt subPath =
                            Decode.requiredAt (path ++ subPath) Decode.float

                        actualIndex =
                            String.fromInt (index + distance + 2)
                    in
                    Decode.map DomInfo
                        (Decode.succeed DomInfoData
                            |> requiredAt [ "scrollTop" ]
                            |> requiredAt [ "clientHeight" ]
                            |> requiredAt [ "clientTop" ]
                            |> requiredAt [ "childNodes", actualIndex, "offsetTop" ]
                            |> requiredAt [ "childNodes", actualIndex, "offsetHeight" ]
                        )


{-| TODO
-}
scrollIntoViewVia : DomInfo -> String -> Listbox -> Task DomError ()
scrollIntoViewVia info id (Listbox data) =
    case info of
        NoDomInfo ->
            Task.succeed ()

        DomInfo { ulScrollTop, ulClientHeight, ulClientTop, liOffsetTop, liOffsetHeight } ->
            if (liOffsetTop - ulClientTop + liOffsetHeight) > (ulScrollTop + ulClientHeight) then
                -- lower parts of new entry are hidden
                Browser.setScrollTop (printListId id)
                    (liOffsetTop - ulClientTop + liOffsetHeight - ulClientHeight)
            else if liOffsetTop - ulClientTop < ulScrollTop then
                -- upper parts of new entry are hidden
                Browser.setScrollTop (printListId id) (liOffsetTop - ulClientTop)
            else
                Task.succeed ()



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
    ->
        { id : String
        , labelledBy : String
        , lift : Msg a -> msg
        , onKeyPress : Decoder msg
        , onMouseDown : Decoder msg
        , onMouseUp : Decoder msg
        , onBlur : Decoder msg
        }
    -> Maybe String
    -> Maybe String
    -> List a
    -> Maybe String
    -> RenderedEntries a divider
    -> List (Html msg)
viewEntries uniqueId views cfg maybeKeyboardFocus maybeMouseFocus selection maybeQuery renderedEntries =
    let
        entryConfig =
            { id = cfg.id
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
                cfg.lift
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
    -> (Msg a -> msg)
    -> Maybe String
    -> Bool
    -> Bool
    -> Bool
    -> Entry a divider
    -> Html msg
viewEntry config lift maybeQuery selected keyboardFocused mouseFocused e =
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

                setAriaSelected attrs =
                    if selected then
                        Attributes.attribute "aria-selected" "true" :: attrs
                    else
                        attrs
            in
            Html.li
                ([ Events.onMouseEnter (lift (EntryMouseEntered (config.uniqueId a)))
                 , Events.onMouseLeave (lift EntryMouseLeft)
                 , Events.onClick (lift (EntryClicked a))
                 , Attributes.id (printEntryId config.id (config.uniqueId a))
                 , Attributes.attribute "role" "option"
                 ]
                    |> setAriaSelected
                    |> appendAttributes lift attributes
                )
                (children
                    |> List.map (Html.map (\_ -> lift NoOp))
                )

        Divider d ->
            let
                { attributes, children } =
                    config.liDivider d
            in
            Html.li
                (appendAttributes lift attributes [])
                (children
                    |> List.map (Html.map (\_ -> lift NoOp))
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
withUnique : Maybe a -> (List a -> ( Listbox, List a )) -> ( Listbox, Maybe a )
withUnique selection func =
    Tuple.mapSecond listToMaybe (func (maybeToList selection))


maybeToList : Maybe a -> List a
maybeToList maybeA =
    case maybeA of
        Nothing ->
            []

        Just a ->
            [ a ]


listToMaybe : List a -> Maybe a
listToMaybe listA =
    case listA of
        [] ->
            Nothing

        a :: _ ->
            Just a


{-| TODO
-}
updateUnique :
    UpdateConfig a
    -> List (Entry a divider)
    -> Msg a
    -> Listbox
    -> Maybe a
    -> ( Listbox, Cmd (Msg a), Maybe a )
updateUnique config allEntries msg listbox selection =
    let
        ( newListbox, cmd, newSelection ) =
            update config allEntries msg listbox <|
                maybeToList selection
    in
    ( newListbox, cmd, listToMaybe newSelection )


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

    update msg model =
        case msg of
            ListboxMsg listboxMsg ->
                let
                    ( newListbox, listboxCmd, newSelection ) =
                        Listbox.update updateConfig
                            model.listbox
                            entries
                            model.selection
                            listboxMsg
                in
                ( { model
                    | listbox = newListbox
                    , selection = newSelection
                  }
                , Cmd.map ListboxMsg listboxCmd
                )

-}
update :
    UpdateConfig a
    -> List (Entry a divider)
    -> Msg a
    -> Listbox
    -> List a
    -> ( Listbox, Cmd (Msg a), List a )
update (UpdateConfig uniqueId behaviour) allEntries msg ((Listbox data) as listbox) selection =
    case msg of
        -- LIST
        ListMouseDown ->
            ( Listbox { data | preventScroll = True }
            , Cmd.none
            , selection
            )

        ListMouseUp ->
            ( Listbox { data | preventScroll = False }
            , Cmd.none
            , selection
            )

        ListFocused id maybeScrollData ->
            if data.preventScroll then
                ( listbox, Cmd.none, selection )
            else
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
                        ( listbox, Cmd.none, selection )

                    Just newEntry ->
                        updateFocus behaviour uniqueId selection False newEntry data
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
            , selection
            )

        ListScrolled ulScrollTop ulClientHeight ->
            ( Listbox
                { data
                    | ulScrollTop = ulScrollTop
                    , ulClientHeight = ulClientHeight
                }
            , Cmd.none
            , selection
            )

        ListArrowUpPressed id shiftDown maybeScrollData ->
            case
                data.maybeKeyboardFocus
                    |> Maybe.andThen (findPrevious uniqueId allEntries)
            of
                Just (Last lastEntry) ->
                    if behaviour.jumpAtEnds then
                        data
                            |> updateFocus behaviour uniqueId selection shiftDown lastEntry
                            |> andDo (scrollListToBottom id)
                    else
                        ( Listbox { data | query = NoQuery }
                        , Cmd.none
                        , selection
                        )

                Just (Previous newEntry) ->
                    data
                        |> updateFocus behaviour uniqueId selection shiftDown newEntry
                        |> andDo (adjustScrollTopNew id (uniqueId newEntry) maybeScrollData)

                Nothing ->
                    ( Listbox { data | query = NoQuery }
                    , Cmd.none
                    , selection
                    )

        ListArrowDownPressed id shiftDown maybeScrollData ->
            case
                data.maybeKeyboardFocus
                    |> Maybe.andThen (findNext uniqueId allEntries)
            of
                Just (First firstEntry) ->
                    if behaviour.jumpAtEnds then
                        data
                            |> updateFocus behaviour uniqueId selection shiftDown firstEntry
                            |> andDo (scrollListToTop id)
                    else
                        ( Listbox { data | query = NoQuery }
                        , Cmd.none
                        , selection
                        )

                Just (Next newEntry) ->
                    data
                        |> updateFocus behaviour uniqueId selection shiftDown newEntry
                        |> andDo (adjustScrollTopNew id (uniqueId newEntry) maybeScrollData)

                Nothing ->
                    ( Listbox { data | query = NoQuery }
                    , Cmd.none
                    , selection
                    )

        ListEnterPressed id ->
            data.maybeKeyboardFocus
                |> Maybe.andThen (find uniqueId allEntries)
                |> Maybe.map
                    (\( _, a ) ->
                        if List.member a selection then
                            ( Listbox { data | maybeLastSelectedEntry = Nothing }
                            , Cmd.none
                            , List.filter (\b -> a /= b) selection
                            )
                        else
                            ( Listbox { data | maybeLastSelectedEntry = Just (uniqueId a) }
                            , Cmd.none
                            , a :: selection
                            )
                    )
                |> Maybe.withDefault ( listbox, Cmd.none, selection )

        ListSpacePressed id ->
            data.maybeKeyboardFocus
                |> Maybe.andThen (find uniqueId allEntries)
                |> Maybe.map
                    (\( _, a ) ->
                        if List.member a selection then
                            ( Listbox { data | maybeLastSelectedEntry = Nothing }
                            , Cmd.none
                            , List.filter (\b -> a /= b) selection
                            )
                        else
                            ( Listbox { data | maybeLastSelectedEntry = Just (uniqueId a) }
                            , Cmd.none
                            , a :: selection
                            )
                    )
                |> Maybe.withDefault ( listbox, Cmd.none, selection )

        ListShiftSpacePressed id ->
            case ( data.maybeKeyboardFocus, data.maybeLastSelectedEntry ) of
                ( Just keyboardFocus, Just lastSelectedEntry ) ->
                    case range uniqueId keyboardFocus lastSelectedEntry allEntries of
                        [] ->
                            ( listbox, Cmd.none, selection )

                        selectedEntries ->
                            ( Listbox { data | maybeLastSelectedEntry = Just keyboardFocus }
                            , Cmd.none
                            , List.uniqueBy uniqueId (selectedEntries ++ selection)
                            )

                _ ->
                    ( listbox, Cmd.none, selection )

        ListHomePressed id ->
            case Internal.firstEntry allEntries of
                Nothing ->
                    ( listbox, Cmd.none, selection )

                Just firstEntry ->
                    data
                        |> updateFocus behaviour uniqueId [] False firstEntry
                        |> andDo (scrollListToTop id)

        ListControlShiftHomePressed id ->
            case Internal.firstEntry allEntries of
                Nothing ->
                    ( listbox, Cmd.none, selection )

                Just firstEntry ->
                    let
                        newFocus =
                            uniqueId firstEntry
                    in
                    case data.maybeKeyboardFocus of
                        Nothing ->
                            ( listbox, Cmd.none, selection )

                        Just keyboardFocus ->
                            case range uniqueId newFocus keyboardFocus allEntries of
                                [] ->
                                    ( listbox, Cmd.none, selection )

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
                                    , List.uniqueBy uniqueId (selectedEntries ++ selection)
                                    )

        ListEndPressed id ->
            case Internal.lastEntry allEntries of
                Nothing ->
                    ( listbox, Cmd.none, selection )

                Just lastEntry ->
                    data
                        |> updateFocus behaviour uniqueId [] False lastEntry
                        |> andDo (scrollListToBottom id)

        ListControlShiftEndPressed id ->
            case Internal.lastEntry allEntries of
                Nothing ->
                    ( listbox, Cmd.none, selection )

                Just lastEntry ->
                    let
                        newFocus =
                            uniqueId lastEntry
                    in
                    case data.maybeKeyboardFocus of
                        Nothing ->
                            ( listbox, Cmd.none, selection )

                        Just keyboardFocus ->
                            case range uniqueId newFocus keyboardFocus allEntries of
                                [] ->
                                    ( listbox, Cmd.none, selection )

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
                                    , List.uniqueBy uniqueId (selectedEntries ++ selection)
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

                allEntriesList =
                    allEntries
                        |> List.filterMap
                            (\e ->
                                case e of
                                    Divider _ ->
                                        Nothing

                                    Option a ->
                                        Just a
                            )

                selectionSet =
                    selection
                        |> List.map uniqueId
                        |> Set.fromList
            in
            ( listbox
            , Cmd.none
            , if Set.isEmpty (Set.diff allEntriesSet selectionSet) then
                []
              else
                allEntriesList
            )

        -- QUERY
        ListKeyPressed id code ->
            case behaviour.typeAhead of
                NoTypeAhead ->
                    ( listbox, Cmd.none, selection )

                TypeAhead _ _ ->
                    ( listbox
                    , Time.now
                        |> Task.perform
                            (CurrentTimeReceived id code)
                    , selection
                    )

        CurrentTimeReceived id code currentTime ->
            case behaviour.typeAhead of
                NoTypeAhead ->
                    ( listbox, Cmd.none, selection )

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
                            ( listbox, Cmd.none, selection )

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
                            , selection
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
            , selection
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
            , selection
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
            , selection
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
            , if List.member a selection then
                List.filter (\b -> a /= b) selection
              else
                a :: selection
            )

        NoOp ->
            ( listbox, Cmd.none, selection )


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
    -> List a
    -> Bool
    -> a
    -> Data
    -> ( Listbox, List a )
updateFocus behaviour uniqueId selection shiftDown newEntry data =
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
        , newEntry :: selection
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
            , List.remove newEntry selection
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
            , newEntry :: selection
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
        , selection
        )



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
    (Msg a -> msg)
    -> List (Html.Attribute Never)
    -> List (Html.Attribute msg)
    -> List (Html.Attribute msg)
appendAttributes lift neverAttrs attrs =
    neverAttrs
        |> List.map (Attributes.map (\_ -> lift NoOp))
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
