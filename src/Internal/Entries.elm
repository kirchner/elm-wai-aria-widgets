module Internal.Entries
    exposing
        ( Entry(..)
        , Next(..)
        , Previous(..)
        , find
        , findNext
        , findPrevious
        , findWith
        , firstEntry
        , lastEntry
        , range
        )


type Entry a divider
    = Divider divider
    | Entry a



---- FIND


find : (a -> String) -> List (Entry a divider) -> String -> Maybe ( Int, a )
find =
    findHelp 0


findHelp : Int -> (a -> String) -> List (Entry a divider) -> String -> Maybe ( Int, a )
findHelp index uniqueId entries selectedId =
    case entries of
        [] ->
            Nothing

        (Divider _) :: rest ->
            findHelp (index + 1) uniqueId rest selectedId

        (Entry entry) :: rest ->
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
findWith matchesQuery uniqueId focus query entries =
    case entries of
        [] ->
            Nothing

        (Divider _) :: rest ->
            findWith matchesQuery uniqueId focus query rest

        (Entry a) :: rest ->
            if uniqueId a == focus then
                let
                    id =
                        uniqueId a
                in
                if matchesQuery query a then
                    Just id
                else
                    proceedWith matchesQuery uniqueId id query rest
            else
                findWith matchesQuery uniqueId focus query rest


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

        (Entry a) :: rest ->
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

        (Entry a) :: _ ->
            Just a



---- PREVIOUS


type Previous a
    = Previous a
    | Last a


findPrevious : (a -> String) -> List (Entry a divider) -> String -> Maybe (Previous a)
findPrevious uniqueId entries currentId =
    case entries of
        [] ->
            Nothing

        (Divider _) :: rest ->
            findPrevious uniqueId rest currentId

        (Entry first) :: rest ->
            if uniqueId first == currentId then
                entries
                    |> lastEntry
                    |> Maybe.map Last
            else
                findPreviousHelp first uniqueId rest currentId


findPreviousHelp : a -> (a -> String) -> List (Entry a divider) -> String -> Maybe (Previous a)
findPreviousHelp previous uniqueId entries currentId =
    case entries of
        [] ->
            Nothing

        (Divider _) :: rest ->
            findPreviousHelp previous uniqueId rest currentId

        (Entry first) :: rest ->
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

        (Entry first) :: rest ->
            if uniqueId first == currentId then
                firstEntry rest
                    |> Maybe.map Next
            else
                Just (findNextHelp first uniqueId rest currentId)


findNextHelp : a -> (a -> String) -> List (Entry a divider) -> String -> Next a
findNextHelp first uniqueId entries currentId =
    case entries of
        [] ->
            First first

        (Divider _) :: rest ->
            findNextHelp first uniqueId rest currentId

        (Entry a) :: rest ->
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

        (Entry a) :: rest ->
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

        (Entry a) :: rest ->
            if uniqueId a == end then
                a :: collected
            else
                rangeHelp uniqueId (a :: collected) end rest
