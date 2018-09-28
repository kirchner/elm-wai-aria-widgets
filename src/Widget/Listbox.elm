module Widget.Listbox exposing
    ( Listbox(..), init, view
    , Entry, option, divider
    , update, Msg(..), subscriptions
    , UpdateConfig, updateConfig, Behaviour
    , ViewConfig, viewConfig, Views, noDivider
    , TypeAhead, noTypeAhead, simpleTypeAhead, typeAhead
    , customView
    , focusedEntry, hoveredEntry
    , focusEntry, focusNextOrFirstEntry, focusPreviousOrFirstEntry
    , focus
    , scrollToFocus
    )

{-| Implementation of the [listbox
widget](https://www.w3.org/TR/wai-aria-practices-1.1/#Listbox):

> A listbox widget presents a list of options and allows a user to select one
> or more of them.

TODO: link to ellie example

Take a look at the documentation of `Behaviour` for the default keyboard
interactions this widget offers.

@docs Listbox, init, view

@docs Entry, option, divider

@docs update, Msg, subscriptions


# Configuration


## Update

@docs UpdateConfig, updateConfig, Behaviour


## View

@docs ViewConfig, viewConfig, Views, noDivider


## Type-ahead

@docs TypeAhead, noTypeAhead, simpleTypeAhead, typeAhead


# Advanced usage

@docs customView


## State manipulation


### Keyboard focus

@docs focusedEntry, hoveredEntry

@docs focusEntry, focusNextOrFirstEntry, focusPreviousOrFirstEntry


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
import Internal.KeyInfo as KeyInfo exposing (KeyInfo)
import Internal.Listbox as Internal
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import List.Extra as List
import Set
import Task exposing (Task)
import Time exposing (Posix)
import Widget exposing (HtmlAttributes, HtmlDetails)


{-| Tracks the keyboard and mouse focus as well as the current query. The full
list of entries and the currently selected option(s) live in your own model.
-}
type Listbox
    = Listbox Internal.Listbox


{-| An initial listbox with no option focused.
-}
init : Listbox
init =
    Listbox Internal.init


{-| When updating or viewing a listbox you have to provide a list of entries.
These can be selectable options or non-selectable dividers.
-}
type alias Entry a divider =
    Internal.Entry a divider


{-| Create a selectable option.
-}
option : a -> Entry a divider
option =
    Internal.Option


{-| Create a non-selectable divider.
-}
divider : divider -> Entry a divider
divider =
    Internal.Divider



---- EXTERNAL STATE MANIPULATION


{-| A task to give the listbox focus. The first argument must match the `id`
used in the `view` function!
-}
focus : String -> Task Dom.Error ()
focus id =
    Internal.focus id


{-| Returns the option which currently has keyboard focus.
-}
focusedEntry : UpdateConfig a -> Listbox -> List (Entry a divider) -> Maybe a
focusedEntry (UpdateConfig config) (Listbox listbox) =
    Internal.focusedEntry config listbox


{-| Returns the option which currently has mouse focus.
-}
hoveredEntry : UpdateConfig a -> Listbox -> List (Entry a divider) -> Maybe a
hoveredEntry (UpdateConfig config) (Listbox listbox) =
    Internal.hoveredEntry config listbox


{-| Sets the keyboard focus to the provided options.

**Note**: This will not adjust the scroll position of the listbox, so you might
want to apply `scrollToFocus` afterwards.

-}
focusEntry : UpdateConfig a -> a -> Listbox -> List a -> ( Listbox, List a )
focusEntry (UpdateConfig config) newEntry (Listbox listbox) selection =
    Internal.focusEntry config newEntry listbox selection
        |> Tuple.mapFirst Listbox


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
focusNextOrFirstEntry (UpdateConfig config) allEntries (Listbox listbox) selection =
    Internal.focusNextOrFirstEntry config allEntries listbox selection
        |> Tuple.mapFirst Listbox


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
focusPreviousOrFirstEntry (UpdateConfig config) allEntries (Listbox listbox) selection =
    Internal.focusPreviousOrFirstEntry config allEntries listbox selection
        |> Tuple.mapFirst Listbox


{-| A command adjusting the scroll position of the listbox such that the
current keyboard focus is visible.
-}
scrollToFocus : String -> Listbox -> Cmd (Msg a)
scrollToFocus id (Listbox listbox) =
    perform (Internal.scrollToFocus id listbox)



---- VIEW CONFIG


{-| -}
type ViewConfig a divider
    = ViewConfig (Internal.ViewConfig a divider)


{-| Generate a `ViewConfig` by providing a hash function for the entries and
a `Views` record, which holds all the styling information. You usually do
**not** want to store this inside your model.
-}
viewConfig : (a -> String) -> Views a divider -> ViewConfig a divider
viewConfig uniqueId views =
    ViewConfig
        { uniqueId = uniqueId
        , views = views
        }


{-| \*\* Available view customizations \*\*

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
    Internal.Views a divider


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
    = UpdateConfig (Internal.UpdateConfig a)


{-| Generate an `UpdateConfig` by providing a hash function for the entries and
a `Behaviour` record.
-}
updateConfig : (a -> String) -> Behaviour a -> UpdateConfig a
updateConfig uniqueId behaviour =
    UpdateConfig
        { uniqueId = uniqueId
        , behaviour = behaviour
        }


{-| \*\* Available behaviour customizations \*\*

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
    Internal.Behaviour a


{-| -}
type alias TypeAhead a =
    Internal.TypeAhead a


{-| Use this inside `Behaviour` if you do not want to activate the type-ahead
functionality.
-}
noTypeAhead : TypeAhead a
noTypeAhead =
    Internal.noTypeAhead


{-| Activate the type-ahead functionality. When the user types in a search
query. The second argument -- `a -> String` -- should be a reasonable
stringification of the options. It is used to check whether an option starts
with this query or not. The listbox will then move the keyboard focus forward
to the next matching option. The first argument is the timeout (in
milliseconds) after which the query is reseted.
-}
simpleTypeAhead : Int -> (a -> String) -> TypeAhead a
simpleTypeAhead =
    Internal.simpleTypeAhead


{-| This works like `simpleTypeAhead` but gives you you more flexibility when
customizing the matching condition. The first argument is the timeout. The
second argument is a function which gets the current query and an option,
returning if the query matches this option.
-}
typeAhead : Int -> (String -> a -> Bool) -> TypeAhead a
typeAhead =
    Internal.typeAhead



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
    view listbox selection =
        Html.div []
            [ Listbox.view viewConfig
                { id = "fruits-listbox"
                , labelledBy = "fruits"
                , lift = ListboxMsg
                }
                fruits
                listbox
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
view (ViewConfig config) { id, labelledBy, lift } allEntries (Listbox listbox) selection =
    let
        internalCfg =
            { id = id
            , labelledBy = labelledBy
            , lift = Msg >> lift
            , onKeyDown = Decode.fail "not handling this event here"
            , onMouseDown = Decode.fail "not handling this event here"
            , onMouseUp = Decode.fail "not handling this event here"
            , onBlur = Decode.fail "not handling this event here"
            }
    in
    Internal.view config internalCfg allEntries listbox selection


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
                entries
                listbox
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
customView (ViewConfig config) cfg allEntries (Listbox listbox) selection =
    let
        internalCfg =
            { id = cfg.id
            , labelledBy = cfg.labelledBy
            , lift = Msg >> cfg.lift
            , onKeyDown = cfg.onKeyDown
            , onMouseDown = cfg.onMouseDown
            , onMouseUp = cfg.onMouseUp
            , onBlur = cfg.onBlur
            }
    in
    Internal.view config internalCfg allEntries listbox selection


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
viewLazy entryHeight dividerHeight (ViewConfig config) cfg allEntries (Listbox listbox) selection =
    let
        internalCfg =
            { id = cfg.id
            , labelledBy = cfg.labelledBy
            , lift = Msg >> cfg.lift
            , onKeyDown = cfg.onKeyDown
            , onMouseDown = cfg.onMouseDown
            , onMouseUp = cfg.onMouseUp
            , onBlur = cfg.onBlur
            }
    in
    Internal.viewLazy entryHeight dividerHeight config internalCfg allEntries listbox selection



---- UPDATE


{-| The listbox's message type.
-}
type Msg a
    = Msg (Internal.Msg a)


{-| Use this function to update the listbox state. You have to provide the same
entries and selection as to your view function.

For example:

    update msg model =
        case msg of
            ListboxMsg listboxMsg ->
                let
                    ( newListbox, listboxCmd, newSelection ) =
                        Listbox.update updateConfig
                            entries
                            listboxMsg
                            model.listbox
                            model.selection
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
update (UpdateConfig config) entries (Msg msg) (Listbox listbox) selection =
    let
        ( newListbox, effect, newSelection ) =
            Internal.update config entries msg listbox selection
    in
    ( Listbox newListbox, perform effect, newSelection )


perform : Internal.Effect a -> Cmd (Msg a)
perform effect =
    Cmd.map Msg <|
        case effect of
            Internal.CmdNone ->
                Cmd.none

            Internal.TimeNow toMsg ->
                Task.perform toMsg Time.now

            Internal.DomSetViewportOf id x y ->
                Task.attempt (\_ -> Internal.NoOp) <|
                    Dom.setViewportOf id x y

            Internal.DomFocus id ->
                Task.attempt (\_ -> Internal.NoOp) <|
                    Dom.focus id

            Internal.ScrollListToTop toMsg id ->
                Task.succeed toMsg
                    |> and (Dom.getViewportOf (Internal.printListId id))
                    |> Task.attempt
                        (\result ->
                            case result of
                                Err _ ->
                                    Internal.NoOp

                                Ok msg ->
                                    msg
                        )

            Internal.ScrollListToBottom toMsg id ->
                Task.succeed toMsg
                    |> and (Dom.getViewportOf (Internal.printListId id))
                    |> Task.attempt
                        (\result ->
                            case result of
                                Err _ ->
                                    Internal.NoOp

                                Ok msg ->
                                    msg
                        )

            Internal.AdjustScrollTop toMsg id entryId ->
                Task.map3 Internal.InitialEntryDomData
                    (Dom.getViewportOf (Internal.printListId id))
                    (Dom.getElement (Internal.printListId id))
                    (Dom.getElement (Internal.printEntryId id entryId))
                    |> Task.attempt
                        (\result ->
                            case result of
                                Err _ ->
                                    Internal.NoOp

                                Ok initialEntryDomData ->
                                    toMsg initialEntryDomData
                        )

            Internal.AdjustScrollTopNew toMsg id entryId previousEntryId ->
                Task.map4 Internal.EntryDomData
                    (Dom.getViewportOf (Internal.printListId id))
                    (Dom.getElement (Internal.printListId id))
                    (Dom.getElement (Internal.printEntryId id entryId))
                    (Dom.getElement (Internal.printEntryId id previousEntryId))
                    |> Task.attempt
                        (\result ->
                            case result of
                                Err _ ->
                                    Internal.NoOp

                                Ok entryDomData ->
                                    toMsg entryDomData
                        )


and : Task x a -> Task x (a -> b) -> Task x b
and task previousTask =
    Task.map2 apply previousTask task


apply : (a -> b) -> a -> b
apply f a =
    f a



---- SUBSCRIPTIONS


{-| Do not forget to add this to your subscriptions:

    subscriptions model =
        Sub.map ListboxMsg (Listbox.subscriptions model.listbox)

-}
subscriptions : Listbox -> Sub (Msg a)
subscriptions (Listbox listbox) =
    Sub.map Msg (Internal.subscriptions listbox)
