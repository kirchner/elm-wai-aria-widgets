module Internal.Listbox exposing
    ( Behaviour
    , Effect(..)
    , Entry(..)
    , EntryDomData
    , Focus(..)
    , InitialEntryDomData
    , Listbox
    , Msg(..)
    , Query(..)
    , TypeAhead(..)
    , UpdateConfig
    , ViewConfig
    , Views
    , focusEntry
    , focusFirstEntry
    , focusNextOrFirstEntry
    , focusPreviousOrFirstEntry
    , focusedEntry
    , hoveredEntry
    , init
    , printEntryId
    , printListId
    , scrollListToBottom
    , scrollListToTop
    , scrollToFocus
    , subscriptions
    , update
    , view
    , viewLazy
    )

import Browser.Dom as Dom
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Internal.KeyInfo as KeyInfo exposing (KeyInfo)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import List.Extra as List
import Set
import Task exposing (Task)
import Time exposing (Posix)
import Widget exposing (HtmlAttributes, HtmlDetails)



---- MODEL


type alias Listbox =
    { preventScroll : Bool
    , query : Query

    -- FOCUS
    , focus : Focus
    , maybeMouseFocus : Maybe String
    , maybeLastSelectedEntry : Maybe String

    -- DOM
    , ulScrollTop : Float
    , ulClientHeight : Float
    }



-- QUERY


type Query
    = NoQuery
    | Query Int Time.Posix String



-- FOCUS


type Focus
    = NoFocus
    | Focus String
    | Pending
        { current : String
        , next : String
        }


schedule : String -> Focus -> Focus
schedule next f =
    case f of
        NoFocus ->
            Focus next

        Focus current ->
            Pending
                { current = current
                , next = next
                }

        Pending data ->
            Pending { data | next = next }


setFocus : Focus -> Focus
setFocus f =
    case f of
        NoFocus ->
            f

        Focus _ ->
            f

        Pending { next } ->
            Focus next


isFocused : Focus -> String -> Bool
isFocused f id =
    case f of
        NoFocus ->
            False

        Focus current ->
            id == current

        Pending { current } ->
            id == current


findFocus : (a -> String) -> List (Entry a divider) -> Focus -> Maybe a
findFocus uniqueId allEntries f =
    case f of
        NoFocus ->
            Nothing

        Focus current ->
            Maybe.map Tuple.second (find uniqueId allEntries current)

        Pending { next } ->
            Maybe.map Tuple.second (find uniqueId allEntries next)



---- INIT


init : Listbox
init =
    { preventScroll = False
    , query = NoQuery
    , focus = NoFocus
    , maybeMouseFocus = Nothing
    , maybeLastSelectedEntry = Nothing
    , ulScrollTop = 0
    , ulClientHeight = 1000
    }



---- ENTRY


type Entry a divider
    = Option a
    | Divider divider



---- EXTERNAL STATE MANIPULATION


focusedEntry : UpdateConfig a -> Listbox -> List (Entry a divider) -> Maybe a
focusedEntry { uniqueId } listbox allEntries =
    findFocus uniqueId allEntries listbox.focus


hoveredEntry : UpdateConfig a -> Listbox -> List (Entry a divider) -> Maybe a
hoveredEntry { uniqueId } { maybeMouseFocus } allEntries =
    maybeMouseFocus
        |> Maybe.andThen (find uniqueId allEntries)
        |> Maybe.map Tuple.second


focusEntry : UpdateConfig a -> a -> Listbox -> List a -> ( Listbox, List a )
focusEntry { uniqueId, behaviour } newEntry listbox selection =
    listbox
        |> updateFocus behaviour uniqueId selection False newEntry
        |> Tuple.mapFirst focusPendingKeyboardFocus


focusNextOrFirstEntry :
    UpdateConfig a
    -> List (Entry a divider)
    -> Listbox
    -> List a
    -> ( Listbox, List a )
focusNextOrFirstEntry ({ uniqueId, behaviour } as config) allEntries listbox selection =
    case focusedEntry config listbox allEntries of
        Nothing ->
            ( { listbox
                | focus =
                    case Maybe.map uniqueId (firstEntry allEntries) of
                        Nothing ->
                            listbox.focus

                        Just id ->
                            schedule id listbox.focus
              }
            , selection
            )

        Just keyboardFocus ->
            case findNext uniqueId allEntries (uniqueId keyboardFocus) of
                Just (First entry) ->
                    if behaviour.jumpAtEnds then
                        listbox
                            |> updateFocus behaviour uniqueId selection False entry
                            |> Tuple.mapFirst focusPendingKeyboardFocus

                    else
                        ( listbox, selection )

                Just (Next newEntry) ->
                    listbox
                        |> updateFocus behaviour uniqueId selection False newEntry
                        |> Tuple.mapFirst focusPendingKeyboardFocus

                Nothing ->
                    ( listbox, selection )


focusPreviousOrFirstEntry :
    UpdateConfig a
    -> List (Entry a divider)
    -> Listbox
    -> List a
    -> ( Listbox, List a )
focusPreviousOrFirstEntry ({ uniqueId, behaviour } as config) allEntries listbox selection =
    case focusedEntry config listbox allEntries of
        Nothing ->
            ( { listbox
                | focus =
                    case Maybe.map uniqueId (firstEntry allEntries) of
                        Nothing ->
                            listbox.focus

                        Just id ->
                            schedule id listbox.focus
              }
            , selection
            )

        Just keyboardFocus ->
            case findPrevious uniqueId allEntries (uniqueId keyboardFocus) of
                Just (Last entry) ->
                    if behaviour.jumpAtEnds then
                        listbox
                            |> updateFocus behaviour uniqueId selection False entry
                            |> Tuple.mapFirst focusPendingKeyboardFocus

                    else
                        ( listbox, selection )

                Just (Previous newEntry) ->
                    listbox
                        |> updateFocus behaviour uniqueId selection False newEntry
                        |> Tuple.mapFirst focusPendingKeyboardFocus

                Nothing ->
                    ( listbox, selection )


scrollToFocus : String -> Listbox -> Effect a
scrollToFocus id listbox =
    case listbox.focus of
        NoFocus ->
            CmdNone

        Focus current ->
            adjustScrollTop id current

        Pending { current } ->
            adjustScrollTop id current



---- VIEW CONFIG


type alias ViewConfig a divider =
    { uniqueId : a -> String
    , views : Views a divider
    }


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



---- UPDATE CONFIG


type alias UpdateConfig a =
    { uniqueId : a -> String
    , behaviour : Behaviour a
    }


type alias Behaviour a =
    { jumpAtEnds : Bool
    , separateFocus : Bool
    , selectionFollowsFocus : Bool
    , handleHomeAndEnd : Bool
    , typeAhead : TypeAhead a
    , minimalGap : Float
    , initialGap : Float
    }


type TypeAhead a
    = NoTypeAhead
    | TypeAhead Int (String -> a -> Bool)



---- VIEW


type alias Customization a msg =
    { id : String
    , labelledBy : String
    , lift : Msg a -> msg
    , onKeyDown : Decoder msg
    , onMouseDown : Decoder msg
    , onMouseUp : Decoder msg
    , onBlur : Decoder msg
    }


view :
    ViewConfig a divider
    -> Customization a msg
    -> List (Entry a divider)
    -> Listbox
    -> List a
    -> Html msg
view { uniqueId, views } cfg allEntries listbox selection =
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


viewLazy :
    (a -> Float)
    -> (divider -> Float)
    -> ViewConfig a divider
    -> Customization a msg
    -> List (Entry a divider)
    -> Listbox
    -> List a
    -> Html msg
viewLazy entryHeight dividerHeight { uniqueId, views } cfg allEntries listbox selection =
    let
        { ulScrollTop, ulClientHeight } =
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
            case listbox.focus of
                NoFocus ->
                    Nothing

                Focus current ->
                    find uniqueId allEntries current
                        |> Maybe.map Tuple.first

                Pending { current } ->
                    find uniqueId allEntries current
                        |> Maybe.map Tuple.first
    in
    viewHelp renderedEntries uniqueId views cfg listbox allEntries selection


viewHelp :
    RenderedEntries a divider
    -> (a -> String)
    -> Views a divider
    -> Customization a msg
    -> Listbox
    -> List (Entry a divider)
    -> List a
    -> Html msg
viewHelp renderedEntries uniqueId views cfg data allEntries selection =
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
                , Decode.andThen (listKeyPress cfg.id >> Decode.map cfg.lift)
                    KeyInfo.decoder
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
            Decode.succeed (cfg.lift (ListFocused cfg.id))
         , Events.on "blur" <|
            Decode.oneOf
                [ cfg.onBlur
                , Decode.succeed (cfg.lift ListBlured)
                ]
         ]
            |> setAriaActivedescendant cfg.id uniqueId data.focus allEntries
            |> setTabindex views.focusable
            |> appendAttributes cfg.lift views.ul
        )
        (viewEntries
            uniqueId
            views
            cfg
            data.focus
            data.maybeMouseFocus
            selection
            maybeQuery
            renderedEntries
        )


listKeyPress : String -> KeyInfo -> Decoder (Msg a)
listKeyPress id { code, altDown, controlDown, metaDown, shiftDown } =
    let
        noModifierDown =
            not altDown && not controlDown && not metaDown && not shiftDown

        onlyShiftDown =
            not altDown && not controlDown && not metaDown && shiftDown

        onlyControlDown =
            not altDown && controlDown && not metaDown && not shiftDown

        notHandlingThatKey =
            Decode.fail "not handling that key combination"
    in
    case code of
        "ArrowUp" ->
            if noModifierDown then
                Decode.succeed (ListArrowUpDown id)

            else if onlyShiftDown then
                Decode.succeed (ListShiftArrowUpDown id)

            else
                notHandlingThatKey

        "ArrowDown" ->
            if noModifierDown then
                Decode.succeed (ListArrowDownDown id)

            else if onlyShiftDown then
                Decode.succeed (ListShiftArrowDownDown id)

            else
                notHandlingThatKey

        "Enter" ->
            if noModifierDown then
                Decode.succeed (ListEnterDown id)

            else
                notHandlingThatKey

        " " ->
            if onlyShiftDown then
                Decode.succeed (ListShiftSpaceDown id)

            else if noModifierDown then
                Decode.succeed (ListSpaceDown id)

            else
                notHandlingThatKey

        "Home" ->
            if onlyShiftDown then
                Decode.succeed (ListControlShiftHomeDown id)

            else if noModifierDown then
                Decode.succeed (ListHomeDown id)

            else
                notHandlingThatKey

        "End" ->
            if onlyShiftDown then
                Decode.succeed (ListControlShiftEndDown id)

            else if noModifierDown then
                Decode.succeed (ListEndDown id)

            else
                notHandlingThatKey

        "a" ->
            if onlyControlDown then
                Decode.succeed ListControlADown

            else if noModifierDown && (String.length code == 1) then
                Decode.succeed (ListKeyDown id code)

            else
                notHandlingThatKey

        _ ->
            if noModifierDown && (String.length code == 1) then
                Decode.succeed (ListKeyDown id code)

            else
                notHandlingThatKey


viewEntries :
    (a -> String)
    -> Views a divider
    -> Customization a msg
    -> Focus
    -> Maybe String
    -> List a
    -> Maybe String
    -> RenderedEntries a divider
    -> List (Html msg)
viewEntries uniqueId views cfg f maybeMouseFocus selection maybeQuery renderedEntries =
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
                (maybeUniqueId e
                    |> Maybe.map (isFocused f)
                    |> Maybe.withDefault False
                )
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
    -> Focus
    -> List (Entry a divider)
    -> List (Html.Attribute msg)
    -> List (Html.Attribute msg)
setAriaActivedescendant id uniqueId f entries attrs =
    findFocus uniqueId entries f
        |> Maybe.map
            (\a ->
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



---- UPDATE


type Msg a
    = NoOp
      -- LIST
    | ListMouseDown
    | ListMouseUp
    | ListFocused String
    | ListBlured
    | ListArrowUpDown String
    | ListShiftArrowUpDown String
    | ListArrowDownDown String
    | ListShiftArrowDownDown String
    | ListEnterDown String
    | ListSpaceDown String
    | ListShiftSpaceDown String
    | ListHomeDown String
    | ListControlShiftHomeDown String
    | ListEndDown String
    | ListControlShiftEndDown String
    | ListControlADown
      -- QUERY
    | ListKeyDown String String
    | CurrentTimeReceived String String Time.Posix
    | Tick Time.Posix
      -- ENTRY
    | EntryMouseEntered String
    | EntryMouseLeft
    | EntryClicked a
      -- SCROLLING
    | InitialEntryDomElementReceived String InitialEntryDomData
    | EntryDomElementReceived String String EntryDomData
    | ListViewportReceived Direction String Dom.Viewport


type alias InitialEntryDomData =
    { viewportList : Dom.Viewport
    , elementList : Dom.Element
    , elementLi : Dom.Element
    }


type alias EntryDomData =
    { viewportList : Dom.Viewport
    , elementList : Dom.Element
    , elementLi : Dom.Element
    , elementPreviousLi : Dom.Element
    }


type Direction
    = Top
    | Bottom


type Effect a
    = CmdNone
    | TimeNow (Posix -> Msg a)
    | DomSetViewportOf String Float Float
    | DomFocus String
      -- CUSTOM
    | ScrollListToTop (Dom.Viewport -> Msg a) String
    | ScrollListToBottom (Dom.Viewport -> Msg a) String
    | AdjustScrollTop (InitialEntryDomData -> Msg a) String String
    | AdjustScrollTopNew (EntryDomData -> Msg a) String String String


update :
    UpdateConfig a
    -> List (Entry a divider)
    -> Msg a
    -> Listbox
    -> List a
    -> ( Listbox, Effect a, List a )
update ({ uniqueId, behaviour } as config) allEntries msg listbox selection =
    let
        -- LISTBOX
        to newListbox =
            ( newListbox
            , CmdNone
            , selection
            )

        stay =
            ( listbox
            , CmdNone
            , selection
            )

        -- EFFECT
        withEffect effect ( newListbox, _, newSelection ) =
            ( newListbox, effect, selection )

        -- SELECTION
        select listA ( newListbox, effect, newSelection ) =
            ( newListbox
            , effect
            , List.uniqueBy uniqueId (listA ++ newSelection)
            )

        unselect a ( newListbox, effect, newSelection ) =
            ( newListbox
            , effect
            , List.filter (\b -> a /= b) newSelection
            )

        toggle a ( newListbox, effect, newSelection ) =
            ( newListbox
            , effect
            , if List.member a newSelection then
                List.filter (\b -> a /= b) newSelection

              else
                List.uniqueBy uniqueId (a :: newSelection)
            )
    in
    case msg of
        -- LIST
        ListMouseDown ->
            to { listbox | preventScroll = True }

        ListMouseUp ->
            to { listbox | preventScroll = False }

        ListFocused id ->
            if listbox.preventScroll then
                stay

            else
                focusFirstEntry id config allEntries listbox selection

        ListBlured ->
            to
                { listbox
                    | preventScroll = False
                    , query = NoQuery
                }

        ListArrowUpDown id ->
            case listbox.focus of
                NoFocus ->
                    focusFirstEntry id config allEntries listbox selection

                Focus current ->
                    focusPreviousEntry id config allEntries listbox selection False current

                Pending _ ->
                    stay

        ListShiftArrowUpDown id ->
            case listbox.focus of
                NoFocus ->
                    focusFirstEntry id config allEntries listbox selection

                Focus current ->
                    focusPreviousEntry id config allEntries listbox selection True current

                Pending _ ->
                    stay

        ListArrowDownDown id ->
            case listbox.focus of
                NoFocus ->
                    focusFirstEntry id config allEntries listbox selection

                Focus current ->
                    focusNextEntry id config allEntries listbox selection False current

                Pending _ ->
                    stay

        ListShiftArrowDownDown id ->
            case listbox.focus of
                NoFocus ->
                    focusFirstEntry id config allEntries listbox selection

                Focus current ->
                    focusNextEntry id config allEntries listbox selection True current

                Pending _ ->
                    stay

        ListEnterDown id ->
            stay
                |> (case focusedEntry config listbox allEntries of
                        Nothing ->
                            identity

                        Just a ->
                            toggle a
                   )

        ListSpaceDown id ->
            stay
                |> (case focusedEntry config listbox allEntries of
                        Nothing ->
                            identity

                        Just a ->
                            toggle a
                   )

        ListShiftSpaceDown id ->
            case ( listbox.focus, listbox.maybeLastSelectedEntry ) of
                ( Focus keyboardFocus, Just lastSelectedEntry ) ->
                    case range uniqueId keyboardFocus lastSelectedEntry allEntries of
                        [] ->
                            stay

                        selectedEntries ->
                            to { listbox | maybeLastSelectedEntry = Just keyboardFocus }
                                |> select selectedEntries

                ( Pending { next }, Just lastSelectedEntry ) ->
                    case range uniqueId next lastSelectedEntry allEntries of
                        [] ->
                            stay

                        selectedEntries ->
                            to { listbox | maybeLastSelectedEntry = Just next }
                                |> select selectedEntries

                _ ->
                    stay

        ListHomeDown id ->
            case firstEntry allEntries of
                Nothing ->
                    stay

                Just entry ->
                    listbox
                        |> updateFocus behaviour uniqueId selection False entry
                        |> andDo (scrollListToTop id)

        ListControlShiftHomeDown id ->
            case Maybe.map uniqueId (firstEntry allEntries) of
                Nothing ->
                    stay

                Just newFocus ->
                    case listbox.focus of
                        NoFocus ->
                            stay

                        Focus keyboardFocus ->
                            case range uniqueId newFocus keyboardFocus allEntries of
                                [] ->
                                    stay

                                selectedEntries ->
                                    to
                                        { listbox
                                            | focus = schedule newFocus listbox.focus
                                            , maybeMouseFocus =
                                                if behaviour.separateFocus then
                                                    listbox.maybeMouseFocus

                                                else
                                                    Just newFocus
                                            , maybeLastSelectedEntry = Just newFocus
                                        }
                                        |> select selectedEntries
                                        |> withEffect (scrollListToTop id)

                        Pending { next } ->
                            case range uniqueId newFocus next allEntries of
                                [] ->
                                    stay

                                selectedEntries ->
                                    to
                                        { listbox
                                            | focus = schedule newFocus listbox.focus
                                            , maybeMouseFocus =
                                                if behaviour.separateFocus then
                                                    listbox.maybeMouseFocus

                                                else
                                                    Just newFocus
                                            , maybeLastSelectedEntry = Just newFocus
                                        }
                                        |> select selectedEntries
                                        |> withEffect (scrollListToTop id)

        ListEndDown id ->
            case lastEntry allEntries of
                Nothing ->
                    stay

                Just entry ->
                    listbox
                        |> updateFocus behaviour uniqueId selection False entry
                        |> andDo (scrollListToBottom id)

        ListControlShiftEndDown id ->
            case Maybe.map uniqueId (firstEntry allEntries) of
                Nothing ->
                    stay

                Just newFocus ->
                    case listbox.focus of
                        NoFocus ->
                            stay

                        Focus keyboardFocus ->
                            case range uniqueId newFocus keyboardFocus allEntries of
                                [] ->
                                    stay

                                selectedEntries ->
                                    to
                                        { listbox
                                            | focus = schedule newFocus listbox.focus
                                            , maybeMouseFocus =
                                                if behaviour.separateFocus then
                                                    listbox.maybeMouseFocus

                                                else
                                                    Just newFocus
                                            , maybeLastSelectedEntry = Just newFocus
                                        }
                                        |> select selectedEntries
                                        |> withEffect (scrollListToTop id)

                        Pending { next } ->
                            case range uniqueId newFocus next allEntries of
                                [] ->
                                    stay

                                selectedEntries ->
                                    to
                                        { listbox
                                            | focus = schedule newFocus listbox.focus
                                            , maybeMouseFocus =
                                                if behaviour.separateFocus then
                                                    listbox.maybeMouseFocus

                                                else
                                                    Just newFocus
                                            , maybeLastSelectedEntry = Just newFocus
                                        }
                                        |> select selectedEntries
                                        |> withEffect (scrollListToTop id)

        ListControlADown ->
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
            , CmdNone
            , if Set.isEmpty (Set.diff allEntriesSet selectionSet) then
                []

              else
                allEntriesList
            )

        -- QUERY
        ListKeyDown id key ->
            case behaviour.typeAhead of
                NoTypeAhead ->
                    stay

                TypeAhead _ _ ->
                    stay
                        |> withEffect (TimeNow (CurrentTimeReceived id key))

        CurrentTimeReceived id key currentTime ->
            case behaviour.typeAhead of
                NoTypeAhead ->
                    stay

                TypeAhead timeout matchesQuery ->
                    let
                        ( newQuery, queryText ) =
                            case listbox.query of
                                NoQuery ->
                                    ( Query timeout currentTime key, key )

                                Query _ _ query ->
                                    ( Query timeout currentTime (query ++ key), query ++ key )

                        newKeyboardFocus =
                            case listbox.focus of
                                NoFocus ->
                                    Nothing

                                Focus current ->
                                    findWith matchesQuery uniqueId current queryText allEntries

                                Pending { current } ->
                                    findWith matchesQuery uniqueId current queryText allEntries
                    in
                    case newKeyboardFocus of
                        Nothing ->
                            stay

                        Just newFocus ->
                            to
                                { listbox
                                    | query = newQuery
                                    , focus = Focus newFocus
                                    , maybeMouseFocus =
                                        if behaviour.separateFocus then
                                            listbox.maybeMouseFocus

                                        else
                                            Just newFocus
                                }
                                |> withEffect (adjustScrollTop id newFocus)

        Tick currentTime ->
            case listbox.query of
                NoQuery ->
                    stay

                Query timeout time _ ->
                    if
                        (Time.posixToMillis currentTime - Time.posixToMillis time)
                            > timeout
                    then
                        to { listbox | query = NoQuery }

                    else
                        stay

        -- ENTRY
        EntryMouseEntered newFocus ->
            to
                { listbox
                    | focus =
                        if behaviour.separateFocus then
                            listbox.focus

                        else
                            Focus newFocus
                    , maybeMouseFocus = Just newFocus
                }

        EntryMouseLeft ->
            to
                { listbox
                    | maybeMouseFocus =
                        if behaviour.separateFocus then
                            Nothing

                        else
                            listbox.maybeMouseFocus
                }

        EntryClicked a ->
            to
                { listbox
                    | query = NoQuery

                    -- FOCUS
                    , focus = Focus (uniqueId a)
                    , maybeMouseFocus = Just (uniqueId a)
                    , maybeLastSelectedEntry = Just (uniqueId a)
                }
                |> toggle a

        -- SCROLLING
        InitialEntryDomElementReceived id { viewportList, elementList, elementLi } ->
            let
                { viewport } =
                    viewportList

                liY =
                    elementLi.element.y - elementList.element.y + viewport.y

                liHeight =
                    elementLi.element.height

                entryHidden =
                    (liY + liHeight - behaviour.minimalGap < viewport.y)
                        || (liY + behaviour.minimalGap > viewport.y + viewport.height)

                centerEntry =
                    DomSetViewportOf (printListId id) viewport.x <|
                        (liY + liHeight / 2 - viewport.height / 2)

                newListbox =
                    { listbox | focus = setFocus listbox.focus }
            in
            if entryHidden then
                to newListbox
                    |> withEffect centerEntry

            else
                to newListbox

        EntryDomElementReceived entryId id entryDomData ->
            let
                viewport =
                    entryDomData.viewportList.viewport

                list =
                    entryDomData.elementList

                li =
                    entryDomData.elementLi

                previousLi =
                    entryDomData.elementPreviousLi

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
                    DomSetViewportOf (printListId id) viewport.x <|
                        (liY + liHeight / 2 - viewport.height / 2)

                scrollDownToNewEntry =
                    DomSetViewportOf (printListId id) viewport.x <|
                        (liY + liHeight - viewport.height + behaviour.initialGap)

                scrollUpToNewEntry =
                    DomSetViewportOf (printListId id) viewport.x <|
                        (liY - behaviour.initialGap)

                newListbox =
                    { listbox | focus = setFocus listbox.focus }
            in
            if previousEntryHidden then
                to newListbox
                    |> withEffect centerNewEntry

            else if newEntryTooLow then
                to newListbox
                    |> withEffect scrollDownToNewEntry

            else if newEntryTooHigh then
                to newListbox
                    |> withEffect scrollUpToNewEntry

            else
                to newListbox

        ListViewportReceived direction id list ->
            let
                newListbox =
                    { listbox | focus = setFocus listbox.focus }
            in
            case direction of
                Top ->
                    to newListbox
                        |> withEffect
                            (DomSetViewportOf (printListId id) list.viewport.x 0)

                Bottom ->
                    to newListbox
                        |> withEffect
                            (DomSetViewportOf (printListId id) list.viewport.x list.scene.height)

        NoOp ->
            stay


focusFirstEntry :
    String
    -> UpdateConfig a
    -> List (Entry a divider)
    -> Listbox
    -> List a
    -> ( Listbox, Effect a, List a )
focusFirstEntry id { uniqueId, behaviour } allEntries listbox selection =
    let
        maybeNewEntry =
            listbox.maybeLastSelectedEntry
                |> Maybe.andThen (find uniqueId allEntries)
                |> Maybe.map Tuple.second
                |> or (List.head selection)
                |> Maybe.andThen (uniqueId >> find uniqueId allEntries)
                |> Maybe.map Tuple.second
                |> or (firstEntry allEntries)

        resetQuery ( newListbox, effect, newSelection ) =
            ( { newListbox | query = NoQuery }
            , effect
            , newSelection
            )
    in
    case maybeNewEntry of
        Nothing ->
            ( { listbox | query = NoQuery }
            , CmdNone
            , selection
            )

        Just newEntry ->
            updateFocus behaviour uniqueId selection False newEntry listbox
                |> (if listbox.preventScroll then
                        Tuple.mapFirst focusPendingKeyboardFocus
                            >> andDo CmdNone

                    else
                        andDo (adjustScrollTop id (uniqueId newEntry))
                   )
                |> resetQuery


focusPreviousEntry id { uniqueId, behaviour } allEntries listbox selection toggle currentFocusId =
    case findPrevious uniqueId allEntries currentFocusId of
        Just (Last entry) ->
            if behaviour.jumpAtEnds then
                listbox
                    |> updateFocus behaviour uniqueId selection toggle entry
                    |> andDo (scrollListToBottom id)

            else
                ( { listbox | query = NoQuery }
                , CmdNone
                , selection
                )

        Just (Previous newEntry) ->
            listbox
                |> updateFocus behaviour uniqueId selection toggle newEntry
                |> andDo (adjustScrollTopNew id (uniqueId newEntry) currentFocusId)

        Nothing ->
            ( { listbox | query = NoQuery }
            , CmdNone
            , selection
            )


focusNextEntry id { uniqueId, behaviour } allEntries listbox selection toggle currentFocusId =
    case findNext uniqueId allEntries currentFocusId of
        Just (First entry) ->
            if behaviour.jumpAtEnds then
                listbox
                    |> updateFocus behaviour uniqueId selection toggle entry
                    |> andDo (scrollListToTop id)

            else
                ( { listbox | query = NoQuery }
                , CmdNone
                , selection
                )

        Just (Next newEntry) ->
            listbox
                |> updateFocus behaviour uniqueId selection toggle newEntry
                |> andDo (adjustScrollTopNew id (uniqueId newEntry) currentFocusId)

        Nothing ->
            ( { listbox | query = NoQuery }
            , CmdNone
            , selection
            )


focusPendingKeyboardFocus : Listbox -> Listbox
focusPendingKeyboardFocus listbox =
    { listbox | focus = setFocus listbox.focus }


updateFocus :
    { b
        | separateFocus : Bool
        , selectionFollowsFocus : Bool
    }
    -> (a -> String)
    -> List a
    -> Bool
    -> a
    -> Listbox
    -> ( Listbox, List a )
updateFocus behaviour uniqueId selection shiftDown newEntry listbox =
    let
        newFocus =
            uniqueId newEntry
    in
    if behaviour.selectionFollowsFocus then
        ( { listbox
            | query = NoQuery
            , focus = schedule newFocus listbox.focus
            , maybeMouseFocus =
                if behaviour.separateFocus then
                    listbox.maybeMouseFocus

                else
                    Just newFocus
            , maybeLastSelectedEntry = Just newFocus
          }
        , newEntry :: selection
        )

    else if shiftDown then
        if List.member newEntry selection then
            ( { listbox
                | query = NoQuery
                , focus = schedule newFocus listbox.focus
                , maybeMouseFocus =
                    if behaviour.separateFocus then
                        listbox.maybeMouseFocus

                    else
                        Just newFocus
                , maybeLastSelectedEntry = Nothing
              }
            , List.remove newEntry selection
            )

        else
            ( { listbox
                | query = NoQuery
                , focus = schedule newFocus listbox.focus
                , maybeMouseFocus =
                    if behaviour.separateFocus then
                        listbox.maybeMouseFocus

                    else
                        Just newFocus
                , maybeLastSelectedEntry = Just newFocus
              }
            , newEntry :: selection
            )

    else
        ( { listbox
            | query = NoQuery
            , focus = schedule newFocus listbox.focus
            , maybeMouseFocus =
                if behaviour.separateFocus then
                    listbox.maybeMouseFocus

                else
                    Just newFocus
          }
        , selection
        )



-- HELPER


andDo : effect -> ( a, b ) -> ( a, effect, b )
andDo effect ( a, b ) =
    ( a, effect, b )


or : Maybe a -> Maybe a -> Maybe a
or fallback default =
    case default of
        Nothing ->
            fallback

        Just _ ->
            default



-- EFFECTS


focusList : String -> Effect a
focusList id =
    DomFocus (printListId id)


scrollListToTop : String -> Effect a
scrollListToTop id =
    ScrollListToTop (ListViewportReceived Top id) id


scrollListToBottom : String -> Effect a
scrollListToBottom id =
    ScrollListToBottom (ListViewportReceived Bottom id) id


adjustScrollTop : String -> String -> Effect a
adjustScrollTop id =
    AdjustScrollTop (InitialEntryDomElementReceived id) id


adjustScrollTopNew : String -> String -> String -> Effect a
adjustScrollTopNew id entryId =
    AdjustScrollTopNew (EntryDomElementReceived entryId id) id entryId



---- SUBSCRIPTIONS


subscriptions : Listbox -> Sub (Msg a)
subscriptions listbox =
    case listbox.query of
        NoQuery ->
            Sub.none

        Query timeout _ _ ->
            Time.every (toFloat (timeout // 3)) Tick



---- IDS


printListId : String -> String
printListId id =
    id ++ "__element-list"


printEntryId : String -> String -> String
printEntryId id entryId =
    id ++ "__element--" ++ entryId



--- FIND


find : (a -> String) -> List (Entry a divider) -> String -> Maybe ( Int, a )
find =
    findHelp 0


findHelp :
    Int
    -> (a -> String)
    -> List (Entry a divider)
    -> String
    -> Maybe ( Int, a )
findHelp index uniqueId entries selectedId =
    case entries of
        [] ->
            Nothing

        (Divider _) :: rest ->
            findHelp (index + 1) uniqueId rest selectedId

        (Option entry) :: rest ->
            if uniqueId entry == selectedId then
                Just ( index, entry )

            else
                findHelp (index + 1) uniqueId rest selectedId


findWith :
    (String -> a -> Bool)
    -> (a -> String)
    -> String
    -> String
    -> List (Entry a divider)
    -> Maybe String
findWith matchesQuery uniqueId id query entries =
    case entries of
        [] ->
            Nothing

        (Divider _) :: rest ->
            findWith matchesQuery uniqueId id query rest

        (Option a) :: rest ->
            if uniqueId a == id then
                if matchesQuery query a then
                    Just id

                else
                    proceedWith matchesQuery uniqueId id query rest

            else
                findWith matchesQuery uniqueId id query rest


proceedWith :
    (String -> a -> Bool)
    -> (a -> String)
    -> String
    -> String
    -> List (Entry a divider)
    -> Maybe String
proceedWith matchesQuery uniqueId id query entries =
    case entries of
        [] ->
            Just id

        (Divider _) :: rest ->
            proceedWith matchesQuery uniqueId id query rest

        (Option a) :: rest ->
            if matchesQuery query a then
                Just (uniqueId a)

            else
                proceedWith matchesQuery uniqueId id query rest


lastEntry : List (Entry a divider) -> Maybe a
lastEntry entries =
    firstEntry (List.reverse entries)


firstEntry : List (Entry a divider) -> Maybe a
firstEntry entries =
    case entries of
        [] ->
            Nothing

        (Divider _) :: rest ->
            firstEntry rest

        (Option a) :: _ ->
            Just a



---- PREVIOUS


type Previous a
    = Previous a
    | Last a


findPrevious :
    (a -> String)
    -> List (Entry a divider)
    -> String
    -> Maybe (Previous a)
findPrevious uniqueId entries currentId =
    case entries of
        [] ->
            Nothing

        (Divider _) :: rest ->
            findPrevious uniqueId rest currentId

        (Option first) :: rest ->
            if uniqueId first == currentId then
                entries
                    |> lastEntry
                    |> Maybe.map Last

            else
                findPreviousHelp first uniqueId rest currentId


findPreviousHelp :
    a
    -> (a -> String)
    -> List (Entry a divider)
    -> String
    -> Maybe (Previous a)
findPreviousHelp previous uniqueId entries currentId =
    case entries of
        [] ->
            Nothing

        (Divider _) :: rest ->
            findPreviousHelp previous uniqueId rest currentId

        (Option first) :: rest ->
            if uniqueId first == currentId then
                Just (Previous previous)

            else
                findPreviousHelp first uniqueId rest currentId



---- NEXT


type Next a
    = Next a
    | First a


findNext :
    (a -> String)
    -> List (Entry a divider)
    -> String
    -> Maybe (Next a)
findNext uniqueId entries currentId =
    case entries of
        [] ->
            Nothing

        (Divider _) :: rest ->
            findNext uniqueId rest currentId

        (Option first) :: rest ->
            if uniqueId first == currentId then
                firstEntry rest
                    |> Maybe.map Next

            else
                Just (findNextHelp first uniqueId rest currentId)


findNextHelp :
    a
    -> (a -> String)
    -> List (Entry a divider)
    -> String
    -> Next a
findNextHelp first uniqueId entries currentId =
    case entries of
        [] ->
            First first

        (Divider _) :: rest ->
            findNextHelp first uniqueId rest currentId

        (Option a) :: rest ->
            if uniqueId a == currentId then
                firstEntry rest
                    |> Maybe.map Next
                    |> Maybe.withDefault (First first)

            else
                findNextHelp first uniqueId rest currentId



---- RANGE


range : (a -> String) -> String -> String -> List (Entry a divider) -> List a
range uniqueId start end entries =
    case entries of
        [] ->
            []

        (Divider _) :: rest ->
            range uniqueId start end rest

        (Option a) :: rest ->
            if uniqueId a == start then
                rangeHelp uniqueId [ a ] end rest

            else if uniqueId a == end then
                rangeHelp uniqueId [ a ] start rest

            else
                range uniqueId start end rest


rangeHelp : (a -> String) -> List a -> String -> List (Entry a divider) -> List a
rangeHelp uniqueId collected end entries =
    case entries of
        [] ->
            []

        (Divider _) :: rest ->
            rangeHelp uniqueId collected end rest

        (Option a) :: rest ->
            if uniqueId a == end then
                a :: collected

            else
                rangeHelp uniqueId (a :: collected) end rest



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
