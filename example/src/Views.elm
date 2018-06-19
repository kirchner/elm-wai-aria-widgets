module Views
    exposing
        ( liChildren
        , viewCheckbox
        )

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
import Html.Events as Events


viewCheckbox checked enabled description =
    Html.div
        [ Attributes.class "field" ]
        [ Html.div
            [ Attributes.class "control" ]
            [ Html.label
                [ Attributes.class "checkbox" ]
                [ Html.input
                    [ Attributes.type_ "checkbox"
                    , Attributes.checked enabled
                    , Events.onCheck checked
                    ]
                    []
                , Html.text (" " ++ description)
                ]
            ]
        ]


liChildren : Maybe String -> String -> List (Html Never)
liChildren maybeQuery name =
    case maybeQuery of
        Nothing ->
            [ Html.text name ]

        Just query ->
            let
                queryLength =
                    String.length query
            in
            String.toLower name
                |> String.split (String.toLower query)
                |> List.map String.length
                |> List.foldl
                    (\count ( remainingName, nodes ) ->
                        case remainingName of
                            "" ->
                                ( remainingName, nodes )

                            _ ->
                                ( String.dropLeft (count + queryLength) remainingName
                                , Html.span
                                    [ Attributes.style "color" "#0091eb" ]
                                    [ Html.text (String.left queryLength (String.dropLeft count remainingName)) ]
                                    :: Html.text (String.left count remainingName)
                                    :: nodes
                                )
                    )
                    ( name, [] )
                |> Tuple.second
                |> List.reverse
