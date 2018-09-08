module Widget.Listbox.Dropdown
    exposing
        ( Behaviour
        , Dropdown
        , Msg
        , UpdateConfig
        , ViewConfig
        , Views
        , init
        , subscriptions
        , update
        , updateConfig
        , view
        , viewConfig
        )

{-| This is a collapsible dropdown version of `Widget.Listbox`. The behaviour
is based on the [Collapsible Dropdown Listbox
Example](https://www.w3.org/TR/wai-aria-practices-1.1/examples/listbox/listbox-collapsible.html).

TODO: add ellie example

@docs Dropdown, init, view, update, Msg, subscriptions


# Configuration

@docs UpdateConfig, updateConfig, Behaviour

@docs ViewConfig, viewConfig, Views

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
import Json.Decode as Decode exposing (Decoder)
import Task
import Widget exposing (HtmlAttributes, HtmlDetails)
import Widget.Listbox as Listbox exposing (Entry, Listbox, TypeAhead)
import Widget.Listbox.Unique as ListboxUnique


{-| Tracks the keyboard and mouse focus as well as the current query and
whether the dropdown is open or closed. The full list of entries and the
currently selected option(s) live in your own model.
-}
type Dropdown
    = Dropdown Data


type alias Data =
    { open : Bool
    , preventBlur : Bool
    , listbox : Listbox
    , pendingFocusListbox : Maybe String
    }


{-| An initial dropdown with no option focused, and which is closed.
-}
init : Dropdown
init =
    Dropdown
        { open = False
        , preventBlur = False
        , listbox = Listbox.init
        , pendingFocusListbox = Nothing
        }



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

  - **container**: A list of html attributes applied to the container div which
    holds the button and the listbox.

  - **button**: A function which returns `HtmlDetails` for the button which
    shows the current selection and toggles the visibility of the listbox. The
    function gets as arguments the current selection and whether the listbox is
    visible or not.

  - **ul**: A list of html attributes applied to the outer listbox.

  - **liOption**: A function which returns `HtmlDetails` for each option in
    your entries list. It gets the actual option value `a` and flags telling you
    if this option is currently `selected` or has focus (`keyboardFocus` and
    `mouseFocus`). If the user typed in a query, you get this via the
    `maybeQuery` field.

  - **liDivider**: This lets you style the divider list entries. It gets the
    actual `divider` entry and returns `HtmlDetails`.

The DOM structure of a dropdown will be something like this:

    listbox =
        Html.div
            [ ... ] -- container attributes
            [ Html.button
                [ ... ] -- button attributes
            , Html.ul
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
            ]

Provided you have specified some CSS classes, a view configuration could look
like this:

    views : Views String Never
    views =
        { container = [ Html.Attributes.class "dropdown__container" ]
        , button =
            \{ maybeSelection } ->
                { attributes = [ Html.Attributes.class "dropdown__button" ]
                , children =
                    [ Html.text (Maybe.withDefault "Make a selection.." maybeSelection) ]
                }
        , ul = [ Html.Attributes.class "listbox__container" ]
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
        }

-}
type alias Views a divider =
    { container : HtmlAttributes
    , button :
        { maybeSelection : Maybe a
        , open : Bool
        }
        -> HtmlDetails
    , ul : HtmlAttributes
    , liOption :
        { selected : Bool
        , keyboardFocused : Bool
        , mouseFocused : Bool
        , maybeQuery : Maybe String
        }
        -> a
        -> HtmlDetails
    , liDivider : divider -> HtmlDetails
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

You can customize the behaviour of the dropdown with the following options:

  - **jumpAtEnds**: Should the keyboard focus jump to the other end of the list
    when pressing `ArrowUp` while focusing the first option (or `ArrowDown` while
    focusing the last).

  - **closeAfterMouseSelection**: Should the dropdown be hidden after the user
    selected an option with the mouse?

  - **separateFocus**: Whether the mouse focus and the keyboard focus can be
    different.

  - **selectionFollowsFocus**: Do we automatically select the entry gaining
    keyboard focus?

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
        , closeAfterMouseSelection = False
        , separateFocus = True
        , selectionFollowsFocus = False
        , handleHomeAndEnd = True
        , typeAhead = simpleTypeAhead 300 identity
        , minimalGap = 30
        , initialGap = 200
        }

The dropdown will behave as explained in the [WAI-ARIA Authoring Practices
1.1](https://www.w3.org/TR/wai-aria-practices-1.1/examples/listbox/listbox-collapsible.html)
under `Keyboard Support`.

-}
type alias Behaviour a =
    { jumpAtEnds : Bool
    , closeAfterMouseSelection : Bool
    , separateFocus : Bool
    , selectionFollowsFocus : Bool
    , handleHomeAndEnd : Bool
    , typeAhead : TypeAhead a
    , minimalGap : Float
    , initialGap : Float
    }



---- VIEW


{-| Take a list of all entries and a list of selected options and display it as
a dropdown. You have to provide a `ViewConfig` for the styling and the following
information:

  - **id**: The unique id of the dropdown.

  - **labelledBy**: The unique id of a label element describing the content of
    the dropdown.

For example:

    view : Dropdown -> Maybe String -> Html Msg
    view dropdown selection =
        Html.div []
            [ Listbox.Dropdown.view viewConfig
                { id = "fruits-dropdown"
                , labelledBy = "fruits"
                }
                fruits
                dropdown
                selection
            ]

    fruits : List (Entry String divider)
    fruits =
        List.map Listbox.option
            [ "Apple", "Banana", "Cherry", "Durian", "Elderberries" ]

    type Msg
        = DropdownMsg Listbox.Dropdown.Msg

-}
view :
    ViewConfig a divider
    ->
        { id : String
        , labelledBy : String
        }
    -> List (Entry a divider)
    -> Dropdown
    -> Maybe a
    -> Html (Msg a)
view (ViewConfig uniqueId views) ids allEntries (Dropdown data) maybeSelection =
    let
        buttonHtmlDetails =
            views.button
                { maybeSelection = maybeSelection
                , open = True
                }

        listboxConfig =
            Listbox.viewConfig uniqueId
                { ul =
                    if data.open then
                        Attributes.style "position" "absolute" :: views.ul
                    else
                        Attributes.style "display" "none"
                            :: Attributes.style "position" "absolute"
                            :: views.ul
                , liOption = views.liOption
                , liDivider = views.liDivider
                , empty = Html.text ""
                , focusable = True
                }
    in
    Html.div
        (appendAttributes views.container
            [ Events.onMouseDown ListboxMousePressed
            , Events.onMouseUp (ListboxMouseReleased ids.id)
            ]
        )
        [ viewButton ids.id buttonHtmlDetails ids.labelledBy maybeSelection True
        , ListboxUnique.customView listboxConfig
            { id = printListboxId ids.id
            , labelledBy = ids.labelledBy
            , lift = ListboxMsg (Just ids.id)
            , onKeyDown =
                KeyInfo.decoder
                    |> Decode.andThen
                        (\{ code, altDown, controlDown, metaDown, shiftDown } ->
                            case code of
                                "Escape" ->
                                    if
                                        not altDown
                                            && not controlDown
                                            && not metaDown
                                            && not shiftDown
                                    then
                                        Decode.succeed (ListboxEscapePressed ids.id)
                                    else
                                        Decode.fail "not handling that key combination"

                                "Enter" ->
                                    if
                                        not altDown
                                            && not controlDown
                                            && not metaDown
                                            && not shiftDown
                                    then
                                        Decode.succeed (ListboxEnterPressed ids.id)
                                    else
                                        Decode.fail "not handling that key combination"

                                _ ->
                                    Decode.fail "not handling that key combination"
                        )
            , onMouseDown = Decode.fail "not handling this event here"
            , onMouseUp = Decode.fail "not handling this event here"
            , onBlur = Decode.succeed (ListboxBlured ids.id)
            }
            allEntries
            data.listbox
            maybeSelection
        ]


viewClosed : String -> HtmlAttributes -> HtmlDetails -> String -> Maybe a -> Html (Msg a)
viewClosed id containerHtmlAttributes buttonHtmlDetails labelledBy selection =
    Html.div
        (appendAttributes containerHtmlAttributes [])
        [ viewButton id buttonHtmlDetails labelledBy selection False ]


viewButton : String -> HtmlDetails -> String -> Maybe a -> Bool -> Html (Msg a)
viewButton id { attributes, children } labelledBy selection open =
    Html.button
        ([ Attributes.id (printButtonId id)
         , Attributes.type_ "button"
         , Attributes.attribute "aria-haspopup" "listbox"
         , Attributes.attribute "aria-labelledby"
            (printButtonId id ++ " " ++ labelledBy)
         , Attributes.style "position" "relative"
         , Attributes.tabindex 0
         , Events.onClick (ButtonClicked id)
         , Events.on "keydown"
            (Decode.field "key" Decode.string
                |> Decode.andThen (buttonKeyDown id)
            )
         ]
            |> setAriaExpanded open
            |> appendAttributes attributes
        )
        (List.map (Html.map (\_ -> NoOp)) children)


buttonKeyDown : String -> String -> Decoder (Msg a)
buttonKeyDown id code =
    case code of
        "ArrowUp" ->
            Decode.succeed (ButtonArrowUpPressed id)

        "ArrowDown" ->
            Decode.succeed (ButtonArrowDownPressed id)

        _ ->
            Decode.fail "not handling that key here"



-- VIEW HELPER


setAriaExpanded : Bool -> List (Html.Attribute msg) -> List (Html.Attribute msg)
setAriaExpanded isOpen attrs =
    if isOpen then
        Attributes.attribute "aria-expanded" "true" :: attrs
    else
        attrs



-- IDS


printButtonId : String -> String
printButtonId id =
    id ++ "__button"


printListboxId : String -> String
printListboxId id =
    id ++ "-listbox"



---- UPDATE


{-| The dropdown's message type.
-}
type Msg a
    = NoOp
    | NextAnimationFrame
      -- BUTTON
    | ButtonClicked String
    | ButtonArrowUpPressed String
    | ButtonArrowDownPressed String
      -- LISTBOX
    | ListboxMsg (Maybe String) (Listbox.Msg a)
    | ListboxEscapePressed String
    | ListboxEnterPressed String
    | ListboxBlured String
    | ListboxMousePressed
    | ListboxMouseReleased String


{-| Use this function to update the dropdown state. You have to provide the
same entries and selection as to your view function.

For example:

    update msg model =
        case msg of
            DropdownMsg dropdownMsg ->
                let
                    ( newDropdown, dropdownCmd, newSelection ) =
                        Listbox.Dropdown.update updateConfig
                            entries
                            model.dropdown
                            model.selection
                            dropdownMsg
                in
                ( { model
                    | dropdown = newDropdown
                    , selection = newSelection
                  }
                , Cmd.map DropdownMsg dropdownCmd
                )

In a more sofisticated example, the entries could be dynamic, as well. (For
example, loaded via an HTTP request.)

-}
update :
    UpdateConfig a
    -> List (Entry a divider)
    -> Msg a
    -> Dropdown
    -> Maybe a
    -> ( Dropdown, Cmd (Msg a), Maybe a )
update (UpdateConfig uniqueId behaviour) allEntries msg dropdown maybeSelection =
    let
        (Dropdown data) =
            dropdown

        listboxConfig =
            Listbox.updateConfig uniqueId
                { jumpAtEnds = behaviour.jumpAtEnds
                , separateFocus = behaviour.separateFocus
                , selectionFollowsFocus = behaviour.selectionFollowsFocus
                , handleHomeAndEnd = behaviour.handleHomeAndEnd
                , typeAhead = behaviour.typeAhead
                , minimalGap = behaviour.minimalGap
                , initialGap = behaviour.initialGap
                }
    in
    case msg of
        NoOp ->
            ( dropdown, Cmd.none, maybeSelection )

        NextAnimationFrame ->
            case data.pendingFocusListbox of
                Nothing ->
                    ( dropdown, Cmd.none, maybeSelection )

                Just id ->
                    ( Dropdown { data | pendingFocusListbox = Nothing }
                    , Task.attempt (\_ -> NoOp) <|
                        Listbox.focus (printListboxId id)
                    , maybeSelection
                    )

        -- BUTTON
        ButtonClicked id ->
            ( Dropdown { data | open = True }
            , Task.attempt (\_ -> NoOp) <|
                Listbox.focus (printListboxId id)
            , maybeSelection
            )

        ButtonArrowUpPressed id ->
            let
                ( newListbox, newSelection ) =
                    ListboxUnique.focusPreviousOrFirstEntry listboxConfig
                        allEntries
                        data.listbox
                        maybeSelection
            in
            ( Dropdown
                { data
                    | open = True
                    , listbox = newListbox
                    , pendingFocusListbox =
                        if data.open then
                            Nothing
                        else
                            Just id
                }
            , Task.attempt (\_ -> NoOp) <|
                Listbox.focus (printListboxId id)
            , newSelection
            )

        ButtonArrowDownPressed id ->
            let
                ( newListbox, newSelection ) =
                    ListboxUnique.focusNextOrFirstEntry listboxConfig
                        allEntries
                        data.listbox
                        maybeSelection
            in
            ( Dropdown
                { data
                    | open = True
                    , listbox = newListbox
                    , pendingFocusListbox =
                        if data.open then
                            Nothing
                        else
                            Just id
                }
            , Task.attempt (\_ -> NoOp) <|
                Listbox.focus (printListboxId id)
            , newSelection
            )

        ListboxMsg maybeId listboxMsg ->
            let
                ( newListbox, listboxCmd, newSelection ) =
                    ListboxUnique.update listboxConfig
                        allEntries
                        listboxMsg
                        data.listbox
                        maybeSelection
            in
            ( Dropdown { data | listbox = newListbox }
            , Cmd.map (ListboxMsg maybeId) listboxCmd
            , newSelection
            )

        ListboxEscapePressed id ->
            ( Dropdown { data | open = False }
            , focusButton id
            , maybeSelection
            )

        ListboxEnterPressed id ->
            if data.open then
                ( Dropdown { data | open = False }
                , focusButton id
                , Listbox.focusedEntry listboxConfig data.listbox allEntries
                )
            else
                ( dropdown, Cmd.none, maybeSelection )

        ListboxBlured id ->
            if data.preventBlur then
                ( dropdown, Cmd.none, maybeSelection )
            else
                ( Dropdown { data | open = False }
                , focusButton id
                , maybeSelection
                )

        ListboxMousePressed ->
            ( Dropdown { data | preventBlur = True }
            , Cmd.none
            , maybeSelection
            )

        ListboxMouseReleased id ->
            if behaviour.closeAfterMouseSelection then
                ( Dropdown { data | open = False, preventBlur = False }
                , focusButton id
                , maybeSelection
                )
            else
                ( Dropdown { data | preventBlur = False }
                , Cmd.none
                , maybeSelection
                )



-- CMDS


focusButton : String -> Cmd (Msg a)
focusButton id =
    Dom.focus (printButtonId id)
        |> Task.attempt (\_ -> NoOp)



---- SUBSCRIPTIONS


{-| Do not forget to add this to your subscriptions:

    subscriptions model =
        Sub.map DropdownMsg (Listbox.Dropdown.subscriptions model.dropdown)

-}
subscriptions : Dropdown -> Sub (Msg a)
subscriptions (Dropdown data) =
    Sub.batch
        [ if data.open then
            Sub.map (ListboxMsg Nothing) (Listbox.subscriptions data.listbox)
          else
            Sub.none
        ]



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
