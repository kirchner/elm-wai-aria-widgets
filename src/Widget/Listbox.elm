module Widget.Listbox
    exposing
        ( Behaviour
        , Entry
        , Listbox
        , Msg
        , TypeAhead
        , UpdateConfig
        , ViewConfig
        , Views
        , customView
        , customViewUnique
        , divider
        , focus
        , focusEntry
        , focusNextOrFirstEntry
        , focusPreviousOrFirstEntry
        , focusedEntry
        , hoveredEntry
        , init
        , noDivider
        , noTypeAhead
        , option
        , scrollToFocus
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

@docs focus

@docs scrollToFocus

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

import Browser.Dom as Dom
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
    , maybePendingKeyboardFocus : Maybe String
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
        , maybePendingKeyboardFocus = Nothing
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
focus : String -> Task Dom.Error ()
focus id =
    Dom.focus (printListId id)


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
        |> Tuple.mapFirst
            (\(Listbox newData) ->
                Listbox
                    { newData
                        | maybeKeyboardFocus = newData.maybePendingKeyboardFocus
                        , maybePendingKeyboardFocus = Nothing
                    }
            )


{-| TODO
-}
scrollToFocus : String -> Listbox -> Cmd (Msg a)
scrollToFocus id (Listbox data) =
    case data.maybeKeyboardFocus of
        Nothing ->
            Cmd.none

        Just focusId ->
            adjustScrollTop id focusId


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
        , onKeyDown = Decode.fail "not handling this event here"
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
        , onKeyDown : Decoder msg
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
                , onKeyDown =
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
        , onKeyDown : Decoder msg
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
        , onKeyDown : Decoder msg
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
        , onKeyDown : Decoder msg
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
         , Attributes.attribute "role" "listbox"
         , Attributes.attribute "aria-labelledby" cfg.labelledBy
         , Events.preventDefaultOn "keydown"
            (Decode.oneOf
                [ cfg.onKeyDown
                , keyInfoDecoder
                    |> Decode.andThen (listKeyPress cfg.id)
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
         , Events.on "focus" <|
            Decode.map cfg.lift <|
                Decode.succeed (ListFocused cfg.id)
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


listKeyPress : String -> KeyInfo -> Decoder (Msg a)
listKeyPress id { code, shiftDown, controlDown } =
    case code of
        "ArrowUp" ->
            Decode.succeed (ListArrowUpPressed id shiftDown)

        "ArrowDown" ->
            Decode.succeed (ListArrowDownPressed id shiftDown)

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


viewEntries :
    (a -> String)
    -> Views a divider
    ->
        { id : String
        , labelledBy : String
        , lift : Msg a -> msg
        , onKeyDown : Decoder msg
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
withUnique :
    Maybe a
    -> (List a -> ( Listbox, List a ))
    -> ( Listbox, Maybe a )
withUnique selection func =
    let
        ( listbox, list ) =
            func (maybeToList selection)
    in
    ( listbox, listToMaybe list )


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
    | ListFocused String
    | ListBlured
    | ListArrowUpPressed String Bool
    | ListArrowDownPressed String Bool
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
      -- SCROLLING
    | InitialEntryDomElementReceived String Dom.Viewport Dom.Element Dom.Element
    | EntryDomElementReceived String String Dom.Viewport Dom.Element Dom.Element Dom.Element
    | ListViewportReceived Direction String Dom.Viewport


type Direction
    = Top
    | Bottom


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

        ListFocused id ->
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
                            |> Maybe.andThen (uniqueId >> find uniqueId allEntries)
                            |> Maybe.map Tuple.second
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
                                    adjustScrollTop id (uniqueId newEntry)
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

        ListArrowUpPressed id shiftDown ->
            case data.maybePendingKeyboardFocus of
                Just _ ->
                    ( listbox, Cmd.none, selection )

                Nothing ->
                    case data.maybeKeyboardFocus of
                        Nothing ->
                            ( Listbox { data | query = NoQuery }
                            , Cmd.none
                            , selection
                            )

                        Just currentFocusId ->
                            case findPrevious uniqueId allEntries currentFocusId of
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
                                        |> andDo (adjustScrollTopNew id (uniqueId newEntry) currentFocusId)

                                Nothing ->
                                    ( Listbox { data | query = NoQuery }
                                    , Cmd.none
                                    , selection
                                    )

        ListArrowDownPressed id shiftDown ->
            case data.maybePendingKeyboardFocus of
                Just _ ->
                    ( listbox, Cmd.none, selection )

                Nothing ->
                    case data.maybeKeyboardFocus of
                        Nothing ->
                            ( Listbox { data | query = NoQuery }
                            , Cmd.none
                            , selection
                            )

                        Just currentFocusId ->
                            case findNext uniqueId allEntries currentFocusId of
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
                                        |> andDo (adjustScrollTopNew id (uniqueId newEntry) currentFocusId)

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
                            , adjustScrollTop id newFocus
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

        -- SCROLLING
        InitialEntryDomElementReceived id { viewport } list li ->
            let
                minimalGap =
                    30

                liY =
                    li.element.y - list.element.y + viewport.y

                liHeight =
                    li.element.height

                entryHidden =
                    (liY + liHeight - minimalGap < viewport.y)
                        || (liY + minimalGap > viewport.y + viewport.height)

                centerEntry =
                    Task.attempt (\_ -> NoOp) <|
                        Dom.setViewportOf (printListId id) viewport.x <|
                            (liY + liHeight / 2 - viewport.height / 2)
            in
            ( Listbox
                { data
                    | maybeKeyboardFocus =
                        data.maybePendingKeyboardFocus
                            |> or data.maybeKeyboardFocus
                    , maybePendingKeyboardFocus = Nothing
                }
            , if entryHidden then
                centerEntry
              else
                Cmd.none
            , selection
            )

        EntryDomElementReceived entryId id { viewport } list li previousLi ->
            let
                minimalGap =
                    30

                initialGap =
                    200

                -- MEASUREMENTS
                liY =
                    li.element.y - list.element.y + viewport.y

                liHeight =
                    li.element.height

                previousLiY =
                    previousLi.element.y - list.element.y + viewport.y

                previousLiHeight =
                    previousLi.element.height

                -- CONDITIONS
                previousEntryHidden =
                    (previousLiY + previousLiHeight < viewport.y)
                        || (previousLiY > viewport.y + viewport.height)

                newEntryTooLow =
                    liY + liHeight + minimalGap > viewport.y + viewport.height

                newEntryTooHigh =
                    liY - minimalGap < viewport.y

                -- ACTIONS
                centerNewEntry =
                    Task.attempt (\_ -> NoOp) <|
                        Dom.setViewportOf (printListId id) viewport.x <|
                            (liY + liHeight / 2 - viewport.height / 2)

                scrollDownToNewEntry =
                    Task.attempt (\_ -> NoOp) <|
                        Dom.setViewportOf (printListId id) viewport.x <|
                            (liY + liHeight - viewport.height + initialGap)

                scrollUpToNewEntry =
                    Task.attempt (\_ -> NoOp) <|
                        Dom.setViewportOf (printListId id) viewport.x <|
                            (liY - initialGap)
            in
            ( Listbox
                { data
                    | maybeKeyboardFocus = data.maybePendingKeyboardFocus
                    , maybePendingKeyboardFocus = Nothing
                }
            , if previousEntryHidden then
                centerNewEntry
              else if newEntryTooLow then
                scrollDownToNewEntry
              else if newEntryTooHigh then
                scrollUpToNewEntry
              else
                Cmd.none
            , selection
            )

        ListViewportReceived direction id list ->
            ( Listbox
                { data
                    | maybeKeyboardFocus = data.maybePendingKeyboardFocus
                    , maybePendingKeyboardFocus = Nothing
                }
            , case direction of
                Top ->
                    Task.attempt (\_ -> NoOp) <|
                        Dom.setViewportOf (printListId id) list.viewport.x 0

                Bottom ->
                    Task.attempt (\_ -> NoOp) <|
                        Dom.setViewportOf (printListId id) list.viewport.x list.scene.height
            , selection
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
                , maybePendingKeyboardFocus = Just newFocus
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
                    , maybePendingKeyboardFocus = Just newFocus
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
                    , maybePendingKeyboardFocus = Just newFocus
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
                , maybePendingKeyboardFocus = Just newFocus
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
    Dom.focus (printListId id)
        |> Task.attempt (\_ -> NoOp)


scrollListToTop : String -> Cmd (Msg a)
scrollListToTop id =
    Task.succeed (ListViewportReceived Top id)
        |> and (Dom.getViewportOf (printListId id))
        |> Task.attempt
            (\result ->
                case result of
                    Err _ ->
                        NoOp

                    Ok msg ->
                        msg
            )


scrollListToBottom : String -> Cmd (Msg a)
scrollListToBottom id =
    Task.succeed (ListViewportReceived Bottom id)
        |> and (Dom.getViewportOf (printListId id))
        |> Task.attempt
            (\result ->
                case result of
                    Err _ ->
                        NoOp

                    Ok msg ->
                        msg
            )


adjustScrollTop : String -> String -> Cmd (Msg a)
adjustScrollTop id entryId =
    Task.succeed (InitialEntryDomElementReceived id)
        |> and (Dom.getViewportOf (printListId id))
        |> and (Dom.getElement (printListId id))
        |> and (Dom.getElement (printEntryId id entryId))
        |> Task.attempt
            (\result ->
                case result of
                    Err _ ->
                        NoOp

                    Ok msg ->
                        msg
            )


adjustScrollTopNew : String -> String -> String -> Cmd (Msg a)
adjustScrollTopNew id entryId previousEntryId =
    Task.succeed (EntryDomElementReceived entryId id)
        |> and (Dom.getViewportOf (printListId id))
        |> and (Dom.getElement (printListId id))
        |> and (Dom.getElement (printEntryId id entryId))
        |> and (Dom.getElement (printEntryId id previousEntryId))
        |> Task.attempt
            (\result ->
                case result of
                    Err _ ->
                        NoOp

                    Ok msg ->
                        msg
            )


and task previousTask =
    Task.map2 apply previousTask task


apply f a =
    f a



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
