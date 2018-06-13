module Rearrangable.SingleSelect
    exposing
        ( Model
        , Msg
        , init
        , subscriptions
        , update
        , view
        )

import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Json.Decode as Decode
import Task
import Widget.Listbox as Listbox exposing (Listbox)


type alias Model =
    { importantFeaturesListbox : Listbox
    , importantFeatures : List String
    , selectedImportantFeature : Maybe String
    , unimportantFeaturesListbox : Listbox
    , unimportantFeatures : List String
    , selectedUnimportantFeature : Maybe String
    }


init : Model
init =
    { importantFeaturesListbox = Listbox.init
    , importantFeatures =
        [ "Proximity of public K-12 schools"
        , "Proximity of child-friendly parks"
        , "Proximity of grocery shopping"
        , "Proximity of fast food"
        , "Proximity of fine dining"
        , "Neighborhood walkability"
        , "Availability of public transit"
        , "Proximity of hospital and medical services"
        , "Level of traffic noise"
        , "Access to major highways"
        ]
    , selectedImportantFeature = Nothing
    , unimportantFeaturesListbox = Listbox.init
    , unimportantFeatures = []
    , selectedUnimportantFeature = Nothing
    }



---- UPDATE


type Msg
    = NoOp
    | ImportantFeaturesListboxMsg (Listbox.Msg String)
    | ImportantFeaturesUpPressed Listbox.DomInfo
    | ImportantFeaturesDownPressed Listbox.DomInfo
    | ImportantFeaturesNotImportantClicked
    | UnimportantFeaturesListboxMsg (Listbox.Msg String)
    | UnimportantFeaturesImportantClicked


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ImportantFeaturesListboxMsg listboxMsg ->
            let
                ( newListbox, listboxCmd, maybeOutMsg ) =
                    Listbox.update listboxUpdateConfig
                        [ Listbox.onEntrySelect identity ]
                        model.importantFeaturesListbox
                        (List.map Listbox.option model.importantFeatures)
                        (maybeToList model.selectedImportantFeature)
                        listboxMsg
            in
            ( { model
                | importantFeaturesListbox = newListbox
                , selectedImportantFeature =
                    case maybeOutMsg of
                        Just feature ->
                            Just feature

                        _ ->
                            model.selectedImportantFeature
              }
            , Cmd.map ImportantFeaturesListboxMsg listboxCmd
            )

        ImportantFeaturesUpPressed domInfo ->
            case model.selectedImportantFeature of
                Nothing ->
                    ( model, Cmd.none )

                Just feature ->
                    let
                        moveUp features =
                            case features of
                                current :: next :: rest ->
                                    if next == feature then
                                        next :: current :: moveUp rest
                                    else
                                        current :: moveUp (next :: rest)

                                _ ->
                                    features
                    in
                    ( { model | importantFeatures = moveUp model.importantFeatures }
                    , Task.attempt (\_ -> NoOp) <|
                        Listbox.scrollIntoViewVia domInfo
                            "important-features-listbox"
                            model.importantFeaturesListbox
                    )

        ImportantFeaturesDownPressed domInfo ->
            case model.selectedImportantFeature of
                Nothing ->
                    ( model, Cmd.none )

                Just feature ->
                    let
                        moveDown features =
                            case features of
                                current :: next :: rest ->
                                    if current == feature then
                                        next :: current :: moveDown rest
                                    else
                                        current :: moveDown (next :: rest)

                                _ ->
                                    features
                    in
                    ( { model | importantFeatures = moveDown model.importantFeatures }
                    , Task.attempt (\_ -> NoOp) <|
                        Listbox.scrollIntoViewVia domInfo
                            "important-features-listbox"
                            model.importantFeaturesListbox
                    )

        ImportantFeaturesNotImportantClicked ->
            case model.selectedImportantFeature of
                Nothing ->
                    ( model, Cmd.none )

                Just feature ->
                    ( { model
                        | selectedImportantFeature = Nothing
                        , importantFeatures =
                            List.filter (\f -> f /= feature) model.importantFeatures
                        , unimportantFeatures = feature :: model.unimportantFeatures
                      }
                    , Cmd.none
                    )

        UnimportantFeaturesListboxMsg listboxMsg ->
            let
                ( newListbox, listboxCmd, maybeOutMsg ) =
                    Listbox.update listboxUpdateConfig
                        [ Listbox.onEntrySelect identity ]
                        model.unimportantFeaturesListbox
                        (List.map Listbox.option model.unimportantFeatures)
                        (maybeToList model.selectedUnimportantFeature)
                        listboxMsg
            in
            ( { model
                | unimportantFeaturesListbox = newListbox
                , selectedUnimportantFeature =
                    case maybeOutMsg of
                        Just feature ->
                            Just feature

                        _ ->
                            model.selectedUnimportantFeature
              }
            , Cmd.map UnimportantFeaturesListboxMsg listboxCmd
            )

        UnimportantFeaturesImportantClicked ->
            case model.selectedUnimportantFeature of
                Nothing ->
                    ( model, Cmd.none )

                Just feature ->
                    ( { model
                        | selectedUnimportantFeature = Nothing
                        , unimportantFeatures =
                            List.filter (\f -> f /= feature) model.unimportantFeatures
                        , importantFeatures = feature :: model.importantFeatures
                      }
                    , Cmd.none
                    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map ImportantFeaturesListboxMsg
            (Listbox.subscriptions model.importantFeaturesListbox)
        , Sub.map UnimportantFeaturesListboxMsg
            (Listbox.subscriptions model.unimportantFeaturesListbox)
        ]



---- VIEW


view : Model -> Html Msg
view model =
    let
        firstSelected =
            List.head model.importantFeatures == model.selectedImportantFeature

        lastSelected =
            List.head (List.reverse model.importantFeatures)
                == model.selectedImportantFeature

        aboveFocused =
            Listbox.fromFocused -1
                listboxViewConfig
                model.importantFeaturesListbox
                entries

        belowFocused =
            Listbox.fromFocused 1
                listboxViewConfig
                model.importantFeaturesListbox
                entries

        entries =
            List.map Listbox.option model.importantFeatures

        pathToListbox =
            [ "target"
            , "parentElement"
            , "parentElement"
            , "previousSibling"
            , "childNodes"
            , "1"
            , "firstChild"
            ]
    in
    Html.div
        [ Attributes.class "columns" ]
        [ Html.div
            [ Attributes.class "column" ]
            [ Html.div
                [ Attributes.class "field" ]
                [ Html.label
                    [ Attributes.class "label"
                    , Attributes.id "important-features-listbox-label"
                    ]
                    [ Html.text "Important features:" ]
                , Html.div
                    [ Attributes.class "control" ]
                    [ Listbox.view
                        listboxViewConfig
                        { id = "important-features-listbox"
                        , labelledBy = "important-features-listbox-label"
                        }
                        model.importantFeaturesListbox
                        entries
                        (maybeToList model.selectedImportantFeature)
                        |> Html.map ImportantFeaturesListboxMsg
                    ]
                ]
            , Html.div
                [ Attributes.class "field"
                , Attributes.class "is-grouped"
                ]
                [ Html.div
                    [ Attributes.class "control" ]
                    [ Html.button
                        [ Attributes.class "button"
                        , Attributes.disabled
                            (model.selectedImportantFeature == Nothing || firstSelected)
                        , Events.on "click" <|
                            Decode.map ImportantFeaturesUpPressed <|
                                Listbox.domInfoOf aboveFocused pathToListbox
                        ]
                        [ Html.text "Up" ]
                    ]
                , Html.div
                    [ Attributes.class "control" ]
                    [ Html.button
                        [ Attributes.class "button"
                        , Attributes.disabled
                            (model.selectedImportantFeature == Nothing || lastSelected)
                        , Events.on "click" <|
                            Decode.map ImportantFeaturesDownPressed <|
                                Listbox.domInfoOf belowFocused pathToListbox
                        ]
                        [ Html.text "Down" ]
                    ]
                , Html.div
                    [ Attributes.class "control" ]
                    [ Html.button
                        [ Attributes.class "button"
                        , Attributes.disabled (model.selectedImportantFeature == Nothing)
                        , Events.onClick ImportantFeaturesNotImportantClicked
                        ]
                        [ Html.span []
                            [ Html.text "Not Important" ]
                        , Html.span
                            [ Attributes.class "icon"
                            , Attributes.class "is-small"
                            ]
                            [ Html.i
                                [ Attributes.class "fas"
                                , Attributes.class "fa-arrow-right"
                                ]
                                []
                            ]
                        ]
                    ]
                ]
            ]
        , Html.div
            [ Attributes.class "column" ]
            [ Html.div
                [ Attributes.class "field" ]
                [ Html.label
                    [ Attributes.class "label"
                    , Attributes.id "unimportant-features-listbox-label"
                    ]
                    [ Html.text "Unimportant features:" ]
                , Html.div
                    [ Attributes.class "control" ]
                    [ Listbox.view listboxViewConfig
                        { id = "unimportant-features-listbox"
                        , labelledBy = "unimportant-features-listbox-label"
                        }
                        model.unimportantFeaturesListbox
                        (List.map Listbox.option model.unimportantFeatures)
                        (maybeToList model.selectedUnimportantFeature)
                        |> Html.map UnimportantFeaturesListboxMsg
                    ]
                ]
            , Html.div
                [ Attributes.class "field"
                , Attributes.class "is-grouped"
                ]
                [ Html.div
                    [ Attributes.class "control" ]
                    [ Html.button
                        [ Attributes.class "button"
                        , Attributes.disabled (model.selectedUnimportantFeature == Nothing)
                        , Events.onClick UnimportantFeaturesImportantClicked
                        ]
                        [ Html.span
                            [ Attributes.class "icon"
                            , Attributes.class "is-small"
                            ]
                            [ Html.i
                                [ Attributes.class "fas"
                                , Attributes.class "fa-arrow-left"
                                ]
                                []
                            ]
                        , Html.span []
                            [ Html.text "Important" ]
                        ]
                    ]
                ]
            ]
        ]



---- CONFIGURATION


listboxUpdateConfig : Listbox.UpdateConfig String
listboxUpdateConfig =
    Listbox.updateConfig identity
        { jumpAtEnds = False
        , separateFocus = True
        , selectionFollowsFocus = True
        , handleHomeAndEnd = True
        , typeAhead = Listbox.noTypeAhead
        }


listboxViewConfig : Listbox.ViewConfig String Never
listboxViewConfig =
    Listbox.viewConfig identity
        { ul = [ Attributes.class "list" ]
        , liOption =
            \{ keyboardFocused, mouseFocused, maybeQuery } name ->
                { attributes =
                    [ Attributes.class "entry"
                    , Attributes.classList
                        [ ( "entry--keyboard-focused", keyboardFocused )
                        , ( "entry--mouse-focused", mouseFocused )
                        ]
                    ]
                , children = [ Html.text name ]
                }
        , liDivider = Listbox.noDivider
        , empty = Html.div [] [ Html.text "this list is empty" ]
        , focusable = True
        }



---- HELPER


maybeToList : Maybe a -> List a
maybeToList maybeA =
    case maybeA of
        Nothing ->
            []

        Just a ->
            [ a ]
