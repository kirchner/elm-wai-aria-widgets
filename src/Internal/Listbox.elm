module Internal.Listbox exposing
    ( Behaviour
    , Effect(..)
    , Entry(..)
    , EntryDomData
    , InitialEntryDomData
    , Listbox
    , Msg(..)
    , Query(..)
    , TypeAhead(..)
    , UpdateConfig
    , ViewConfig
    , Views
    , focusEntry
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

import Accessibility.Aria as Aria
import Accessibility.Role as Role
import Accessibility.Widget as Widget
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
    , focus : Maybe String
    , pendingFocus : Maybe PendingFocus
    , hover : Maybe String
    , maybeLastSelectedEntry : Maybe String

    -- DOM
    , ulScrollTop : Float
    , ulClientHeight : Float
    }


type alias PendingFocus =
    { hash : String
    , shiftDown : Bool
    }


type Query
    = NoQuery
    | Query Int Time.Posix String



---- INIT


init : Listbox
init =
    { preventScroll = False
    , query = NoQuery
    , focus = Nothing
    , pendingFocus = Nothing
    , hover = Nothing
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
    Maybe.andThen (find uniqueId allEntries) listbox.focus


hoveredEntry : UpdateConfig a -> Listbox -> List (Entry a divider) -> Maybe a
hoveredEntry { uniqueId } { hover } allEntries =
    Maybe.andThen (find uniqueId allEntries) hover


focusEntry : UpdateConfig a -> a -> Listbox -> List a -> ( Listbox, List a )
focusEntry { uniqueId, behaviour } a listbox selection =
    ( { listbox
        | query = NoQuery
        , focus = Just (uniqueId a)
      }
    , if behaviour.selectionFollowsFocus then
        List.uniqueBy uniqueId (a :: selection)

      else
        selection
    )


focusNextOrFirstEntry :
    UpdateConfig a
    -> List (Entry a divider)
    -> Listbox
    -> List a
    -> ( Listbox, List a )
focusNextOrFirstEntry config allEntries listbox selection =
    let
        { uniqueId, behaviour } =
            config

        maybeA =
            case listbox.focus of
                Nothing ->
                    firstEntry allEntries

                Just hash ->
                    case findNext uniqueId allEntries hash of
                        Just (First a) ->
                            if behaviour.jumpAtEnds then
                                Just a

                            else
                                Nothing

                        Just (Next a) ->
                            Just a

                        Nothing ->
                            Nothing
    in
    case maybeA of
        Nothing ->
            ( listbox, selection )

        Just a ->
            let
                newListbox =
                    { listbox | focus = Just (uniqueId a) }
            in
            if behaviour.selectionFollowsFocus then
                ( newListbox
                , List.uniqueBy uniqueId (a :: selection)
                )

            else
                ( newListbox
                , selection
                )


focusPreviousOrFirstEntry :
    UpdateConfig a
    -> List (Entry a divider)
    -> Listbox
    -> List a
    -> ( Listbox, List a )
focusPreviousOrFirstEntry config allEntries listbox selection =
    let
        { uniqueId, behaviour } =
            config

        maybeA =
            case listbox.focus of
                Nothing ->
                    firstEntry allEntries

                Just hash ->
                    case findPrevious uniqueId allEntries hash of
                        Just (Last a) ->
                            if behaviour.jumpAtEnds then
                                Just a

                            else
                                Nothing

                        Just (Previous a) ->
                            Just a

                        Nothing ->
                            Nothing
    in
    case maybeA of
        Nothing ->
            ( listbox, selection )

        Just a ->
            let
                newListbox =
                    { listbox | focus = Just (uniqueId a) }
            in
            if behaviour.selectionFollowsFocus then
                ( newListbox
                , List.uniqueBy uniqueId (a :: selection)
                )

            else
                ( newListbox
                , selection
                )


scrollToFocus : String -> Listbox -> Effect a
scrollToFocus id listbox =
    case listbox.focus of
        Nothing ->
            CmdNone

        Just hash ->
            adjustScrollTop id hash



---- VIEW CONFIG


type alias ViewConfig a divider =
    { uniqueId : a -> String
    , views : Views a divider
    }


type alias Views a divider =
    { ul : HtmlAttributes
    , liOption :
        { selected : Bool
        , focused : Bool
        , hovered : Bool
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
view { uniqueId, views } customization allEntries listbox selection =
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
    viewHelp renderedEntries uniqueId views customization allEntries listbox selection


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
            Maybe.andThen (indexOf uniqueId allEntries) listbox.focus
    in
    viewHelp renderedEntries uniqueId views cfg allEntries listbox selection


viewHelp :
    RenderedEntries a divider
    -> (a -> String)
    -> Views a divider
    -> Customization a msg
    -> List (Entry a divider)
    -> Listbox
    -> List a
    -> Html msg
viewHelp renderedEntries uniqueId views customization allEntries listbox selection =
    let
        { id, lift, labelledBy, onKeyDown, onMouseUp, onMouseDown, onBlur } =
            customization

        viewEntries =
            List.map (viewEntry uniqueId views customization listbox selection)
    in
    Html.ul
        ([ Attributes.id (printListId id)
         , Role.listBox
         , Aria.labelledBy labelledBy
         , Events.preventDefaultOn "keydown" <|
            preventDefault <|
                Decode.oneOf
                    [ onKeyDown
                    , Decode.andThen
                        (listKeyPress id >> Decode.map lift)
                        KeyInfo.decoder
                    ]
         , Events.on "mousedown" <|
            Decode.oneOf [ onMouseDown, Decode.succeed (lift ListMouseDown) ]
         , Events.on "mouseup" <|
            Decode.oneOf [ onMouseUp, Decode.succeed (lift ListMouseUp) ]
         , Events.on "focus" <|
            Decode.succeed (lift (ListFocused id))
         , Events.on "blur" <|
            Decode.oneOf [ onBlur, Decode.succeed (lift ListBlured) ]
         ]
            |> setAriaActivedescendant id uniqueId listbox.focus allEntries
            |> setTabindex views.focusable
            |> appendAttributes lift views.ul
        )
        (List.concat
            [ spacer renderedEntries.spaceAboveFirst
            , viewEntries renderedEntries.entriesAbove
            , spacer renderedEntries.spaceAboveSecond
            , viewEntries renderedEntries.visibleEntries
            , spacer renderedEntries.spaceBelowFirst
            , viewEntries renderedEntries.entriesBelow
            , spacer renderedEntries.spaceBelowSecond
            ]
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
            if not altDown && controlDown && not metaDown && shiftDown then
                Decode.succeed (ListControlShiftHomeDown id)

            else if noModifierDown then
                Decode.succeed (ListHomeDown id)

            else
                notHandlingThatKey

        "End" ->
            if not altDown && controlDown && not metaDown && shiftDown then
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


viewEntry :
    (a -> String)
    -> Views a divider
    -> Customization a msg
    -> Listbox
    -> List a
    -> Entry a divider
    -> Html msg
viewEntry uniqueId config { id, lift } listbox selection entry =
    let
        mapNever =
            List.map (Html.map (\_ -> lift NoOp))
    in
    case entry of
        Option option ->
            let
                hash =
                    uniqueId option

                selected =
                    List.any ((==) option) selection

                { attributes, children } =
                    config.liOption
                        { selected = selected
                        , focused = listbox.focus == Just hash
                        , hovered = listbox.hover == Just hash
                        , maybeQuery =
                            case listbox.query of
                                NoQuery ->
                                    Nothing

                                Query _ _ query ->
                                    Just query
                        }
                        option

                setAriaSelected attrs =
                    if selected then
                        Widget.selected True :: attrs

                    else
                        attrs
            in
            Html.li
                ([ Events.onMouseEnter (lift (EntryMouseEntered hash))
                 , Events.onMouseLeave (lift EntryMouseLeft)
                 , Events.onClick (lift (EntryClicked option))
                 , Attributes.id (printEntryId id hash)
                 , Role.option
                 ]
                    |> setAriaSelected
                    |> appendAttributes lift attributes
                )
                (mapNever children)

        Divider d ->
            let
                { attributes, children } =
                    config.liDivider d
            in
            Html.li
                (appendAttributes lift attributes [])
                (mapNever children)


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
setAriaActivedescendant id uniqueId focus entries attrs =
    focus
        |> Maybe.andThen (find uniqueId entries)
        |> Maybe.map
            (\a ->
                Aria.activeDescendant (printEntryId id (uniqueId a))
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
        unchanged =
            ( listbox
            , CmdNone
            , selection
            )

        fromModel newListbox =
            ( newListbox
            , CmdNone
            , selection
            )

        withEffect effect ( newListbox, _, newSelection ) =
            ( newListbox, effect, newSelection )

        withSelection newSelection ( newListbox, effect, _ ) =
            ( newListbox, effect, newSelection )

        -- SELECTION
        select a listA ( newListbox, effect, newSelection ) =
            { newListbox | maybeLastSelectedEntry = Just (uniqueId a) }
                |> fromModel
                |> withSelection (List.uniqueBy uniqueId (a :: listA ++ newSelection))

        unselect a ( newListbox, effect, newSelection ) =
            newListbox
                |> fromModel
                |> withSelection (List.filter (\b -> a /= b) newSelection)

        toggle a ( newListbox, effect, newSelection ) =
            if List.member a newSelection then
                newListbox
                    |> fromModel
                    |> withSelection (List.filter (\b -> a /= b) newSelection)

            else
                { newListbox | maybeLastSelectedEntry = Just (uniqueId a) }
                    |> fromModel
                    |> withSelection (List.uniqueBy uniqueId (a :: newSelection))

        -- FOCUS
        initFocus id =
            let
                maybeA =
                    listbox.focus
                        |> or listbox.maybeLastSelectedEntry
                        |> Maybe.andThen (find uniqueId allEntries)
                        |> or (List.head selection)
                        |> Maybe.andThen (uniqueId >> find uniqueId allEntries)
                        |> or (firstEntry allEntries)
            in
            case maybeA of
                Nothing ->
                    fromModel { listbox | query = NoQuery }

                Just a ->
                    let
                        hash =
                            uniqueId a

                        newListbox =
                            { listbox
                                | query = NoQuery
                                , focus = Just hash
                            }
                    in
                    if behaviour.selectionFollowsFocus then
                        newListbox
                            |> fromModel
                            |> withEffect (adjustScrollTop id hash)
                            |> withSelection (List.uniqueBy uniqueId (a :: selection))

                    else
                        newListbox
                            |> fromModel
                            |> withEffect (adjustScrollTop id hash)

        scheduleFocusPrevious id shiftDown current =
            case findPrevious uniqueId allEntries current of
                Just (Last a) ->
                    if behaviour.jumpAtEnds then
                        { listbox
                            | query = NoQuery
                            , pendingFocus =
                                Just (PendingFocus (uniqueId a) shiftDown)
                        }
                            |> fromModel
                            |> withEffect (scrollListToBottom id)

                    else
                        fromModel { listbox | query = NoQuery }

                Just (Previous a) ->
                    let
                        hash =
                            uniqueId a
                    in
                    { listbox
                        | query = NoQuery
                        , pendingFocus = Just (PendingFocus hash shiftDown)
                    }
                        |> fromModel
                        |> withEffect (adjustScrollTopNew id hash current)

                Nothing ->
                    fromModel { listbox | query = NoQuery }

        scheduleFocusNext id shiftDown current =
            case findNext uniqueId allEntries current of
                Just (First a) ->
                    if behaviour.jumpAtEnds then
                        { listbox
                            | query = NoQuery
                            , pendingFocus =
                                Just (PendingFocus (uniqueId a) shiftDown)
                        }
                            |> fromModel
                            |> withEffect (scrollListToTop id)

                    else
                        fromModel { listbox | query = NoQuery }

                Just (Next a) ->
                    let
                        hash =
                            uniqueId a
                    in
                    { listbox
                        | query = NoQuery
                        , pendingFocus = Just (PendingFocus hash shiftDown)
                    }
                        |> fromModel
                        |> withEffect (adjustScrollTopNew id hash current)

                Nothing ->
                    fromModel { listbox | query = NoQuery }

        focusScheduledFocus =
            case listbox.pendingFocus of
                Nothing ->
                    unchanged

                Just { hash, shiftDown } ->
                    case find uniqueId allEntries hash of
                        Nothing ->
                            unchanged

                        Just a ->
                            let
                                newListbox =
                                    { listbox
                                        | focus = Just hash
                                        , pendingFocus = Nothing
                                    }
                            in
                            if behaviour.selectionFollowsFocus then
                                newListbox
                                    |> fromModel
                                    |> select a []

                            else if shiftDown then
                                newListbox
                                    |> fromModel
                                    |> toggle a

                            else
                                fromModel newListbox
    in
    case msg of
        NoOp ->
            unchanged

        -- LIST
        ListMouseDown ->
            fromModel { listbox | preventScroll = True }

        ListMouseUp ->
            fromModel { listbox | preventScroll = False }

        ListFocused id ->
            if listbox.preventScroll then
                unchanged

            else
                initFocus id

        ListBlured ->
            fromModel
                { listbox
                    | query = NoQuery
                    , preventScroll = False
                }

        ListArrowUpDown id ->
            case listbox.focus of
                Nothing ->
                    initFocus id

                Just hash ->
                    if listbox.pendingFocus /= Nothing then
                        unchanged

                    else
                        scheduleFocusPrevious id False hash

        ListShiftArrowUpDown id ->
            case listbox.focus of
                Nothing ->
                    initFocus id

                Just hash ->
                    if listbox.pendingFocus /= Nothing then
                        unchanged

                    else
                        scheduleFocusPrevious id True hash

        ListArrowDownDown id ->
            case listbox.focus of
                Nothing ->
                    initFocus id

                Just hash ->
                    if listbox.pendingFocus /= Nothing then
                        unchanged

                    else
                        scheduleFocusNext id False hash

        ListShiftArrowDownDown id ->
            case listbox.focus of
                Nothing ->
                    initFocus id

                Just hash ->
                    if listbox.pendingFocus /= Nothing then
                        unchanged

                    else
                        scheduleFocusNext id True hash

        ListEnterDown id ->
            case focusedEntry config listbox allEntries of
                Nothing ->
                    unchanged

                Just a ->
                    unchanged
                        |> toggle a

        ListSpaceDown id ->
            case focusedEntry config listbox allEntries of
                Nothing ->
                    unchanged

                Just a ->
                    unchanged
                        |> toggle a

        ListShiftSpaceDown id ->
            let
                selected =
                    Maybe.map2 (range uniqueId allEntries)
                        listbox.focus
                        listbox.maybeLastSelectedEntry
                        |> Maybe.withDefault []
            in
            case selected of
                [] ->
                    unchanged

                a :: listA ->
                    unchanged
                        |> select a listA

        ListHomeDown id ->
            case firstEntry allEntries of
                Nothing ->
                    unchanged

                Just a ->
                    { listbox
                        | query = NoQuery
                        , pendingFocus = Just (PendingFocus (uniqueId a) False)
                    }
                        |> fromModel
                        |> withEffect (scrollListToTop id)

        ListControlShiftHomeDown id ->
            case Maybe.map uniqueId (firstEntry allEntries) of
                Nothing ->
                    unchanged

                Just hash ->
                    let
                        selected =
                            listbox.focus
                                |> Maybe.map (range uniqueId allEntries hash)
                                |> Maybe.withDefault []
                    in
                    case selected of
                        [] ->
                            unchanged

                        a :: listA ->
                            { listbox
                                | focus = Just hash
                                , hover =
                                    if behaviour.separateFocus then
                                        listbox.hover

                                    else
                                        Just hash
                            }
                                |> fromModel
                                |> select a listA
                                |> withEffect (scrollListToTop id)

        ListEndDown id ->
            case lastEntry allEntries of
                Nothing ->
                    unchanged

                Just a ->
                    { listbox
                        | query = NoQuery
                        , pendingFocus = Just (PendingFocus (uniqueId a) False)
                    }
                        |> fromModel
                        |> withEffect (scrollListToBottom id)

        ListControlShiftEndDown id ->
            case Maybe.map uniqueId (lastEntry allEntries) of
                Nothing ->
                    unchanged

                Just hash ->
                    let
                        selected =
                            listbox.focus
                                |> Maybe.map (range uniqueId allEntries hash)
                                |> Maybe.withDefault []
                    in
                    case selected of
                        [] ->
                            unchanged

                        a :: listA ->
                            { listbox
                                | focus = Just hash
                                , hover =
                                    if behaviour.separateFocus then
                                        listbox.hover

                                    else
                                        Just hash
                            }
                                |> fromModel
                                |> select a listA
                                |> withEffect (scrollListToBottom id)

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
            if Set.isEmpty (Set.diff allEntriesSet selectionSet) then
                unchanged
                    |> withSelection []

            else
                unchanged
                    |> withSelection allEntriesList

        -- QUERY
        ListKeyDown id key ->
            case behaviour.typeAhead of
                NoTypeAhead ->
                    unchanged

                TypeAhead _ _ ->
                    unchanged
                        |> withEffect (TimeNow (CurrentTimeReceived id key))

        CurrentTimeReceived id key currentTime ->
            case behaviour.typeAhead of
                NoTypeAhead ->
                    unchanged

                TypeAhead timeout matchesQuery ->
                    let
                        ( newQuery, queryText ) =
                            case listbox.query of
                                NoQuery ->
                                    ( Query timeout currentTime key, key )

                                Query _ _ query ->
                                    ( Query timeout currentTime (query ++ key), query ++ key )

                        maybeHash =
                            Maybe.andThen
                                (findWith matchesQuery uniqueId queryText allEntries)
                                listbox.focus
                    in
                    case maybeHash of
                        Nothing ->
                            unchanged

                        Just hash ->
                            { listbox
                                | query = newQuery
                                , focus = Just hash
                                , hover =
                                    if behaviour.separateFocus then
                                        listbox.hover

                                    else
                                        Just hash
                            }
                                |> fromModel
                                |> withEffect (adjustScrollTop id hash)

        Tick currentTime ->
            case listbox.query of
                NoQuery ->
                    unchanged

                Query timeout time _ ->
                    if
                        (Time.posixToMillis currentTime - Time.posixToMillis time)
                            > timeout
                    then
                        fromModel { listbox | query = NoQuery }

                    else
                        unchanged

        -- ENTRY
        EntryMouseEntered newFocus ->
            fromModel
                { listbox
                    | focus =
                        if behaviour.separateFocus then
                            listbox.focus

                        else
                            Just newFocus
                    , hover = Just newFocus
                }

        EntryMouseLeft ->
            fromModel
                { listbox
                    | hover =
                        if behaviour.separateFocus then
                            Nothing

                        else
                            listbox.hover
                }

        EntryClicked a ->
            let
                hash =
                    uniqueId a
            in
            { listbox
                | query = NoQuery
                , focus = Just hash
                , hover = Just hash
            }
                |> fromModel
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
            in
            if entryHidden then
                focusScheduledFocus
                    |> withEffect centerEntry

            else
                focusScheduledFocus

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

                -- EFFECT
                centerNewEntry =
                    DomSetViewportOf (printListId id) viewport.x <|
                        (liY + liHeight / 2 - viewport.height / 2)

                scrollDownToNewEntry =
                    DomSetViewportOf (printListId id) viewport.x <|
                        (liY + liHeight - viewport.height + behaviour.initialGap)

                scrollUpToNewEntry =
                    DomSetViewportOf (printListId id) viewport.x <|
                        (liY - behaviour.initialGap)

                effect =
                    if previousEntryHidden then
                        centerNewEntry

                    else if newEntryTooLow then
                        scrollDownToNewEntry

                    else if newEntryTooHigh then
                        scrollUpToNewEntry

                    else
                        CmdNone
            in
            focusScheduledFocus
                |> withEffect effect

        ListViewportReceived direction id list ->
            let
                effect =
                    case direction of
                        Top ->
                            DomSetViewportOf (printListId id)
                                list.viewport.x
                                0

                        Bottom ->
                            DomSetViewportOf (printListId id)
                                list.viewport.x
                                list.scene.height
            in
            focusScheduledFocus
                |> withEffect effect


focusPendingKeyboardFocus : Listbox -> Listbox
focusPendingKeyboardFocus listbox =
    case listbox.pendingFocus of
        Nothing ->
            listbox

        Just { hash } ->
            { listbox | focus = Just hash }



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


indexOf : (a -> String) -> List (Entry a divider) -> String -> Maybe Int
indexOf uniqueId entries selectedId =
    findHelp 0 uniqueId entries selectedId
        |> Maybe.map Tuple.first


find : (a -> String) -> List (Entry a divider) -> String -> Maybe a
find uniqueId entries selectedId =
    findHelp 0 uniqueId entries selectedId
        |> Maybe.map Tuple.second


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
    -> List (Entry a divider)
    -> String
    -> Maybe String
findWith matchesQuery uniqueId query entries id =
    case entries of
        [] ->
            Nothing

        (Divider _) :: rest ->
            findWith matchesQuery uniqueId query rest id

        (Option a) :: rest ->
            if uniqueId a == id then
                if matchesQuery query a then
                    Just id

                else
                    proceedWith matchesQuery uniqueId id query rest

            else
                findWith matchesQuery uniqueId query rest id


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


findNext : (a -> String) -> List (Entry a divider) -> String -> Maybe (Next a)
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


range : (a -> String) -> List (Entry a divider) -> String -> String -> List a
range uniqueId entries end start =
    case entries of
        [] ->
            []

        (Divider _) :: rest ->
            range uniqueId rest start end

        (Option a) :: rest ->
            if uniqueId a == start then
                rangeHelp uniqueId [ a ] end rest

            else if uniqueId a == end then
                List.reverse (rangeHelp uniqueId [ a ] start rest)

            else
                range uniqueId rest start end


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
