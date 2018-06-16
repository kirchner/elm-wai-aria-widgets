module Views
    exposing
        ( liChildren
        , viewCheckbox
        )

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
