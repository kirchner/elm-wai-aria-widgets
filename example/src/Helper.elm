module Helper exposing (matchesQuery)


matchesQuery : String -> String -> Bool
matchesQuery query value =
    String.toLower value
        |> String.contains (String.toLower query)
