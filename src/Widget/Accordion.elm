module Widget.Accordion
    exposing
        ( Accordion
        , PanelState(..)
        , Section
        , ViewConfig
        , Views
        , init
        , section
        , view
        , viewConfig
        )

{-|

    TODO

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

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Widget exposing (HtmlAttributes, HtmlDetails)


{-| TODO
-}
type Accordion
    = Accordion (Dict String PanelState)


{-| TODO
-}
init : Accordion
init =
    Accordion Dict.empty


{-| TODO
-}
type PanelState
    = Collapsed
    | Expanded


{-| TODO
-}
type Section msg
    = Section PanelState (SectionData msg)


type alias SectionData msg =
    { id : String
    , header : PanelState -> HtmlDetails
    , panel : List (Html msg)
    }


{-| TODO
-}
section :
    PanelState
    ->
        { id : String
        , header : PanelState -> HtmlDetails
        , panel : List (Html msg)
        }
    -> Section msg
section =
    Section



---- CONFIG


{-| TODO
-}
type ViewConfig
    = ViewConfig Views


{-| TODO
-}
viewConfig : Views -> ViewConfig
viewConfig =
    ViewConfig


{-| TODO
-}
type alias Views =
    { dl : HtmlAttributes
    , dt : HtmlAttributes
    , dd : HtmlAttributes
    }



---- VIEW


{-| TODO
-}
view : ViewConfig -> (Accordion -> msg) -> String -> Accordion -> List (Section msg) -> Html msg
view (ViewConfig views) lift id (Accordion panelStates) sections =
    let
        noOp =
            lift (Accordion panelStates)
    in
    Html.dl
        ([ Attributes.id id
         , Attributes.attribute "role" "presentation"
         ]
            |> appendAttributes noOp views.dl
        )
        (sections
            |> List.map (viewSection views lift id panelStates)
            |> List.concat
        )


viewSection :
    Views
    -> (Accordion -> msg)
    -> String
    -> Dict String PanelState
    -> Section msg
    -> List (Html msg)
viewSection views lift id panelStates (Section initialState data) =
    let
        buttonId =
            id ++ "--" ++ data.id ++ "__accordion-header"

        panelId =
            id ++ "--" ++ data.id ++ "__accordion-panel"

        header =
            data.header actualState

        actualState =
            panelStates
                |> Dict.get data.id
                |> Maybe.withDefault initialState

        noOp =
            lift (Accordion panelStates)
    in
    [ Html.dt
        ([ Attributes.attribute "role" "heading"
         , Attributes.attribute "aria-level" (String.fromInt -1) -- FIXME
         ]
            |> appendAttributes noOp views.dt
        )
        [ Html.button
            ([ Attributes.id buttonId
             , Events.onClick
                (panelStates
                    |> Dict.update data.id
                        (\maybePanelState ->
                            case maybePanelState of
                                Nothing ->
                                    case initialState of
                                        Collapsed ->
                                            Just Expanded

                                        Expanded ->
                                            Just Collapsed

                                Just Collapsed ->
                                    Just Expanded

                                Just Expanded ->
                                    Just Collapsed
                        )
                    |> Accordion
                    |> lift
                )
             ]
                |> appendAttributes noOp header.attributes
            )
            (List.map (Html.map (\_ -> noOp)) header.children)
        ]
    , Html.dd
        ([ Attributes.attribute "role" "region"
         , Attributes.attribute "aria-labelledby" buttonId
         , Attributes.id panelId
         ]
            |> applyPanelState actualState
            |> appendAttributes noOp views.dd
        )
        data.panel
    ]


applyPanelState : PanelState -> List (Html.Attribute msg) -> List (Html.Attribute msg)
applyPanelState panelState attrs =
    case panelState of
        Collapsed ->
            Attributes.style "display" "none" :: attrs

        Expanded ->
            attrs



---- VIEW HELPER


appendAttributes :
    msg
    -> List (Html.Attribute Never)
    -> List (Html.Attribute msg)
    -> List (Html.Attribute msg)
appendAttributes noOp neverAttrs attrs =
    neverAttrs
        |> List.map (Attributes.map (\_ -> noOp))
        |> List.append attrs
