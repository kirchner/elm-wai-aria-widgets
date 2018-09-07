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
        , viewUnique
        , withUnique
        )

{-| Implementation of the [listbox
widget](https://www.w3.org/TR/wai-aria-practices-1.1/#Listbox):

> A listbox widget presents a list of options and allows a user to select one
> or more of them.

TODO: link to ellie example

Take a look at the documentation of `Behaviour` for the default keyboard
interactions this widget offers.

@docs Listbox, init, view, viewUnique

@docs Entry, option, divider

@docs update, updateUnique, Msg, subscriptions


# Configuration


## Update

@docs UpdateConfig, updateConfig, Behaviour


## View

@docs ViewConfig, viewConfig, Views, noDivider


## Type-ahead

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
import Internal.KeyInfo as KeyInfo exposing (KeyInfo)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import List.Extra as List
import Set
import Task exposing (Task)
import Time
import Widget exposing (HtmlAttributes, HtmlDetails)


{-| Tracks the keyboard and mouse focus as well as the current query. The full
list of entries and the currently selected option(s) live in your own model.
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


{-| An initial listbox with on option focused.
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


{-| When updating or viewing a listbox you have to provide a list of entries.
These can be selectable options or non-selectable dividers.
-}
type alias Entry a divider =
    Internal.Entry a divider


{-| Create a selectable option.
-}
option : a -> Entry a divider
option =
    Option


{-| Create a non-selectable divider.
-}
divider : divider -> Entry a divider
divider =
    Divider



---- EXTERNAL STATE MANIPULATION


{-| A task to give the listbox focus. The first argument must match the `id`
used in the `view` function!
-}
focus : String -> Task Dom.Error ()
focus id =
    Dom.focus (printListId id)


{-| Returns the option which currently has keyboard focus.
-}
focusedEntry : UpdateConfig a -> Listbox -> List (Entry a divider) -> Maybe a
focusedEntry (UpdateConfig uniqueId _) (Listbox { maybeKeyboardFocus }) allEntries =
    maybeKeyboardFocus
        |> Maybe.andThen (find uniqueId allEntries)
        |> Maybe.map Tuple.second


{-| Returns the option which currently has mouse focus.
-}
hoveredEntry : UpdateConfig a -> Listbox -> List (Entry a divider) -> Maybe a
hoveredEntry (UpdateConfig uniqueId _) (Listbox { maybeMouseFocus }) allEntries =
    maybeMouseFocus
        |> Maybe.andThen (find uniqueId allEntries)
        |> Maybe.map Tuple.second


{-| Sets the keyboard focus to the provided options.

**Note**: This will not adjust the scroll position of the listbox, so you might
want to apply `scrollToFocus` afterwards.

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


{-| Sets the keyboard focus to the next option. If `jumpAtEnds` is true and the
focus is already on the last option, the first option is selected.

**Note**: This will not adjust the scroll position of the listbox, so you might
want to apply `scrollToFocus` afterwards.

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


{-| Sets the keyboard focus to the previous option. If `jumpAtEnds` is true and the
focus is already on the first option, the first option is selected.

**Note**: This will not adjust the scroll position of the listbox, so you might
want to apply `scrollToFocus` afterwards.

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


{-| A command adjusting the scroll position of the listbox such that the
current keyboard focus is visible.
-}
scrollToFocus : String -> Listbox -> Cmd (Msg a)
scrollToFocus id (Listbox data) =
    case data.maybeKeyboardFocus of
        Nothing ->
            Cmd.none

        Just focusId ->
            adjustScrollTop id focusId


{-| You can wrap `focusEntry`, `focusNextOrFirstEntry` and
`focusPreviousOrFirstEntry` with this function if you have a listbox where the
user can only select at most one option.
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



---- VIEW CONFIG


{-| -}
type ViewConfig a divider
    = ViewConfig (a -> String) (Views a divider)


{-| Generate a `ViewConfig` by providing a hash function for the entries and
a `Views` record, which holds all the styling information. You usually do
**not** want to store this inside your model.
-}
viewConfig : (a -> String) -> Views a divider -> ViewConfig a divider
viewConfig =
    ViewConfig


{-| ** Available view customizations **

This is the second argument to `viewConfig`. You can customize the styling with
the following fields:

  - **ul**: A list of html attributes applied to the outer listbox.

  - **liOption**: A function return `HtmlDetails` for each option in your
    entries list. It gets the actual option value `a` and flags telling you if
    this option is currently `selected` or has focus (`keyboardFocus` and
    `mouseFocus`). If the user typed in a query, you get this via the
    `maybeQuery` field.

  - **liDivider**: This lets you style the divider list entries. It gets the
    actual `divider` entry and returns `HtmlDetails`.

  - **empty**: What should be rendered when the listbox is empty?

  - **focusable**: Should the listbox be focusable?

The DOM structure of a listbox will be something like this:

    listbox =
        Html.ul
            [ ... ] -- ul attributes
            [ li
                [ ... ] -- liDivider attributes
                [ ... ] -- liDivider children
            , li
                [ ... ] -- liOption attributes
                [ ... ] -- liOption children
            , ...
            , li
                [ ... ] -- liOption attributes
                [ ... ] -- liOption children
            ]

Provided you have specified some CSS classes, a view configuration could look
like this:

    views : Views String Never
    views =
        { ul = [ Html.Attributes.class "listbox__container" ]
        , liOption =
            \{ selected, keyboardFocused } option ->
                { attributes =
                    [ Html.Attributes.class "listbox__option"
                    , Html.Attributes.classList
                        [ ( "listbox__option--selected", selected )
                        , ( "listbox__option--keyboardFocused", keyboardFocused )
                        ]
                    ]
                , children =
                    [ Html.text option ]
                }
        , liDivider = noDivider
        , empty = Html.text ""
        , focusable = True
        }

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


{-| Helper function which can be used for the `liDivider` field in your view
customizations if you do not have any dividers in your listbox.
-}
noDivider : Never -> HtmlDetails
noDivider _ =
    { attributes = []
    , children = []
    }



---- UPDATE CONFIG


{-| -}
type UpdateConfig a
    = UpdateConfig (a -> String) (Behaviour a)


{-| Generate an `UpdateConfig` by providing a hash function for the entries and
a `Behaviour` record.
-}
updateConfig : (a -> String) -> Behaviour a -> UpdateConfig a
updateConfig =
    UpdateConfig


{-| ** Available behaviour customizations **

You can customize the behaviour of the listbox with the following options:

  - **jumpAtEnds**: Should the keyboard focus jump to the other end of the list
    when pressing `ArrowUp` while focusing the first option (or `ArrowDown` while
    focusing the last).

  - **separateFocus**: Whether the mouse focus and the keyboard focus can be
    different.

  - **selectionFollowsFocus**: Do we automatically add the entry gaining
    keyboard focus to the selection?

  - **handleHomeAndEnd**: Should we handle the `Home` and `End` keys (to jump
    to the top or bottom of the list)?

  - **typeAhead**: Make it possible to jump to options by typing in a query.
    Take a look at `TypeAhead` for more information.

  - **minimalGap**: If the distance (in px) of the option having the keyboard
    focus to the borders of the listbox scene is smaller then this value, the
    listbox will adjust its scroll position so that this distance is at least
    `initialGap`.

  - **initialGap**: The minimal distance (in px) of the option having the
    keyboard focus to the borders of the listbox scene after the scroll position
    has been adjusted.

A behaviour configuration could look something like this:

    behaviour : Behaviour String
    behaviour =
        { jumpAtEnds = True
        , separateFocus = True
        , selectionFollowsFocus = False
        , handleHomeAndEnd = True
        , typeAhead = simpleTypeAhead 300 identity
        , minimalGap = 30
        , initialGap = 200
        }

The listbox will behave as explained in the [WAI-ARIA Authoring Practices
1.1](https://www.w3.org/TR/wai-aria-practices-1.1/#Listbox) under `Keyboard
Interaction`. Note that you get the "recommended selection model" if you
choose `selectionFollowsFocus = False`, and the "alternative selection model"
for `selectionFollowsFocus = True`.

-}
type alias Behaviour a =
    { jumpAtEnds : Bool
    , separateFocus : Bool
    , selectionFollowsFocus : Bool
    , handleHomeAndEnd : Bool
    , typeAhead : TypeAhead a
    , minimalGap : Float
    , initialGap : Float
    }


{-| -}
type TypeAhead a
    = NoTypeAhead
    | TypeAhead Int (String -> a -> Bool)


{-| Use this inside `Behaviour` if you do not want to activate the type-ahead
functionality.
-}
noTypeAhead : TypeAhead a
noTypeAhead =
    NoTypeAhead


{-| Activate the type-ahead functionality. When the user types in a search
query. The second argument -- `a -> String` -- should be a reasonable
stringification of the options. It is used to check whether an option starts
with this query or not. The listbox will then move the keyboard focus forward
to the next matching option. The first argument is the timeout (in
milliseconds) after which the query is reseted.
-}
simpleTypeAhead : Int -> (a -> String) -> TypeAhead a
simpleTypeAhead timeout entryToString =
    TypeAhead timeout <|
        \query a ->
            String.toLower (entryToString a)
                |> String.startsWith (String.toLower query)


{-| This works like `simpleTypeAhead` but gives you you more flexibility when
customizing the matching condition. The first argument is the timeout. The
second argument is a function which gets the current query and an option,
returning if the query matches this option.
-}
typeAhead : Int -> (String -> a -> Bool) -> TypeAhead a
typeAhead =
    TypeAhead



---- VIEW


{-| Take a list of all entries and a list of selected options and display it as
a listbox. You have to provide a `ViewConfig` for the styling and the following
information:

  - **id**: The unique id of the listbox.

  - **labelledBy**: The unique id of a label element describing the content of
    the listbox.

  - **lift**: Your message type constructor wrapping the listbox `Msg`'s.

For example:

    view : Listbox -> List String -> Html Msg
    view model selection =
        Html.div []
            [ Listbox.view viewConfig
                { id = "fruits-listbox"
                , labelledBy = "fruits"
                , lift = ListboxMsg
                }
                listbox
                fruits
                selection
            ]

    fruits : List (Entry String divider)
    fruits =
        List.map Listbox.option
            [ "Apple", "Banana", "Cherry", "Durian", "Elderberries" ]

    type Msg
        = ListboxMsg Listbox.Msg

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


{-| Use this instead of `view` if the user can only select **at most one**
entry in the listbox. The only difference between the type signature of this
function and the one of `view` is that the last argument is a `Maybe a` instead
of a `List a`.
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


{-| Use this instead of `view` if you need to attach your own event handlers.
You can provide the following event decoders:

  - **onKeyDown**: Handle `keydown` events on the listbox.

  - **onMouseDown**: Handle `mousedown` events on the listbox.

  - **onMouseUp**: Handle `mouseup` events on the listbox.

  - **onBlur**: Handle `blur` events on the listbox.

If you provide a failing decoder the event will be handled by the listbox
itself.

For example, to handle the `ArrowUp` key yourself, you could do something like
this:

    view : Listbox -> List String -> Html Msg
    view listbox selection =
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
                , onMouseDown = Decode.fail "not handling that key here"
                , onMouseUp = Decode.fail "not handling that key here"
                , onBlur = Decode.fail "not handling that key here"
                }
                listbox
                entries
                selection
            ]

    type Msg
        = ListboxMsg Listbox.Msg
        | ArrowUpPressed

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


{-| Use this instead of `viewUnique` if you need to attach your own event
handlers. Take a look at `customView` for more details.
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
                , KeyInfo.decoder
                    |> Decode.andThen
                        (listKeyPress cfg.id
                            >> Maybe.map (Decode.succeed << cfg.lift)
                            >> Maybe.withDefault (Decode.fail "not handling that key combination")
                        )
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


listKeyPress : String -> KeyInfo -> Maybe (Msg a)
listKeyPress id { code, altDown, controlDown, metaDown, shiftDown } =
    case code of
        "ArrowUp" ->
            if not altDown && not controlDown && not metaDown then
                Just (ListArrowUpPressed id shiftDown)
            else
                Nothing

        "ArrowDown" ->
            if not altDown && not controlDown && not metaDown then
                Just (ListArrowDownPressed id shiftDown)
            else
                Nothing

        "Enter" ->
            if not altDown && not controlDown && not metaDown && not shiftDown then
                Just (ListEnterPressed id)
            else
                Nothing

        " " ->
            if not altDown && not controlDown && not metaDown && shiftDown then
                Just (ListShiftSpacePressed id)
            else if not altDown && not controlDown && not metaDown && not shiftDown then
                Just (ListSpacePressed id)
            else
                Nothing

        "Home" ->
            if not altDown && controlDown && not metaDown && shiftDown then
                Just (ListControlShiftHomePressed id)
            else if not altDown && not controlDown && not metaDown && not shiftDown then
                Just (ListHomePressed id)
            else
                Nothing

        "End" ->
            if not altDown && controlDown && not metaDown && shiftDown then
                Just (ListControlShiftEndPressed id)
            else if not altDown && not controlDown && not metaDown && not shiftDown then
                Just (ListEndPressed id)
            else
                Nothing

        "a" ->
            if not altDown && controlDown && not metaDown && not shiftDown then
                Just ListControlAPressed
            else if
                not altDown
                    && not controlDown
                    && not metaDown
                    && not shiftDown
                    && (String.length code == 1)
            then
                Just (ListKeyPressed id code)
            else
                Nothing

        _ ->
            if
                not altDown
                    && not controlDown
                    && not metaDown
                    && not shiftDown
                    && (String.length code == 1)
            then
                Just (ListKeyPressed id code)
            else
                Nothing


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


{-| Use this function instead of `update` if the user can only select **at most
one** entry in the listbox. The only difference between the type signature of
this function and the one of `update` is that the last argument is a `Maybe
a` instead of a `List a`.
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


{-| The listbox's message type.
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


{-| Use this function to update the listbox state. You have to provide the same
entries and selection as to your view function.

For example:

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

In a more sofisticated example, the entries could be dynamic, as well. (For
example, loaded via an HTTP request.)

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
                liY =
                    li.element.y - list.element.y + viewport.y

                liHeight =
                    li.element.height

                entryHidden =
                    (liY + liHeight - behaviour.minimalGap < viewport.y)
                        || (liY + behaviour.minimalGap > viewport.y + viewport.height)

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
                    liY + liHeight + behaviour.minimalGap > viewport.y + viewport.height

                newEntryTooHigh =
                    liY - behaviour.minimalGap < viewport.y

                -- ACTIONS
                centerNewEntry =
                    Task.attempt (\_ -> NoOp) <|
                        Dom.setViewportOf (printListId id) viewport.x <|
                            (liY + liHeight / 2 - viewport.height / 2)

                scrollDownToNewEntry =
                    Task.attempt (\_ -> NoOp) <|
                        Dom.setViewportOf (printListId id) viewport.x <|
                            (liY + liHeight - viewport.height + behaviour.initialGap)

                scrollUpToNewEntry =
                    Task.attempt (\_ -> NoOp) <|
                        Dom.setViewportOf (printListId id) viewport.x <|
                            (liY - behaviour.initialGap)
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


{-| Do not forget to add this to your subscriptions:

    subscriptions model =
        Sub.map ListboxMsg (Listbox.subscriptions model.listbox)

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
