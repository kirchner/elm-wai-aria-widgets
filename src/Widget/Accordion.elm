module Widget.Accordion
    exposing
        ( Msg
        , Section
        , ViewConfig
        , Views
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

import Html exposing (Html)
import Html.Attributes as Attributes
import Widget exposing (HtmlAttributes, HtmlDetails)


{-| TODO
-}
type Section msg
    = Section (SectionData msg)


type alias SectionData msg =
    { id : String
    , header : Bool -> HtmlDetails
    , panel : List (Html msg)
    }


{-| TODO
-}
section :
    { id : String
    , header : Bool -> HtmlDetails
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
view : ViewConfig -> (Msg -> msg) -> String -> List (Section msg) -> Html msg
view (ViewConfig views) lift id sections =
    Html.dl
        ([ Attributes.id id
         , Attributes.attribute "role" "presentation"
         ]
            |> appendAttributes lift views.dl
        )
        (sections
            |> List.map (viewSection views lift id)
            |> List.concat
        )


viewSection : Views -> (Msg -> msg) -> String -> Section msg -> List (Html msg)
viewSection views lift id (Section data) =
    let
        buttonId =
            id ++ "--" ++ data.id ++ "__accordion-header"

        panelId =
            id ++ "--" ++ data.id ++ "__accordion-panel"

        header =
            -- FIXME use collapsed state
            data.header False
    in
    [ Html.dt
        ([ Attributes.attribute "role" "heading"
         , Attributes.attribute "aria-level" (String.fromInt -1) -- FIXME
         ]
            |> appendAttributes lift views.dt
        )
        [ Html.button
            (appendAttributes lift
                header.attributes
                [ Attributes.id buttonId ]
            )
            (List.map (Html.map (\_ -> lift NoOp)) header.children)
        ]
    , Html.dd
        ([ Attributes.attribute "role" "region"
         , Attributes.attribute "aria-labelledby" buttonId
         , Attributes.id panelId
         ]
            |> appendAttributes lift views.dd
        )
        data.panel
    ]



---- VIEW HELPER


appendAttributes :
    (Msg -> msg)
    -> List (Html.Attribute Never)
    -> List (Html.Attribute msg)
    -> List (Html.Attribute msg)
appendAttributes lift neverAttrs attrs =
    neverAttrs
        |> List.map (Attributes.map (\_ -> lift NoOp))
        |> List.append attrs



---- UPDATE


{-| TODO
-}
type Msg
    = NoOp
