module Reporter exposing (Error, File, formatReport)

import Array exposing (Array)
import Text exposing (Text)


{-| Error
-}
type alias Error =
    { moduleName : Maybe String
    , ruleName : String
    , message : String
    , details : List String
    , range : Range
    }


type alias File =
    { path : String
    , source : String
    }


type alias Range =
    { start : { row : Int, column : Int }
    , end : { row : Int, column : Int }
    }


formatReport : List ( File, List Error ) -> List { str : String, color : Maybe ( Int, Int, Int ) }
formatReport errors =
    let
        numberOfErrors : Int
        numberOfErrors =
            totalNumberOfErrors errors
    in
    if List.isEmpty errors then
        "I found no linting errors.\nYou're all good!"
            |> Text.from
            |> Text.toRecord
            |> List.singleton

    else
        [ errors
            |> List.filter (Tuple.second >> List.isEmpty >> not)
            |> formatReports
        , [ Text.from "\n" ]
        ]
            |> List.concat
            |> List.map Text.toRecord


formatReportForFileWithExtract : ( File, List Error ) -> List Text
formatReportForFileWithExtract ( file, errors ) =
    let
        formattedErrors : List (List Text)
        formattedErrors =
            List.map (formatErrorWithExtract file) errors

        prefix : String
        prefix =
            "-- ELM-LINT ERROR "

        header : Text
        header =
            (prefix ++ String.padLeft (80 - String.length prefix) '-' (" " ++ fileIdentifier ( file, errors )))
                |> Text.from
                |> Text.inBlue
    in
    header :: Text.from "\n\n" :: Text.join "\n\n\n" formattedErrors


formatErrorWithExtract : File -> Error -> List Text
formatErrorWithExtract file { ruleName, message, details, range } =
    let
        title : List Text
        title =
            [ Text.from ruleName
                |> Text.inRed
            , Text.from <| ": " ++ message
            ]

        codeExtract_ : List Text
        codeExtract_ =
            codeExtract file range

        details_ : List Text
        details_ =
            List.map Text.from details
                |> List.intersperse (Text.from "\n\n")
    in
    [ title, codeExtract_, details_ ]
        |> List.filter (List.isEmpty >> not)
        |> List.intersperse [ Text.from "\n\n" ]
        |> List.concat


codeExtract : File -> Range -> List Text
codeExtract file =
    let
        getRowAtLine_ : Int -> String
        getRowAtLine_ =
            getRowAtLine file
    in
    \({ start, end } as range) ->
        if range.start == range.end then
            []

        else if start.row == end.row then
            [ Text.from <| getRowAtLine_ (start.row - 2)
            , Text.from <| getRowAtLine_ (start.row - 1)
            , underlineError (start.row - 1) { start = start.column, end = end.column }
            , Text.from <| getRowAtLine_ end.row
            ]

        else
            let
                startLine : String
                startLine =
                    getRowAtLine_ (start.row - 1)

                linesBetweenStartAndEnd : List String
                linesBetweenStartAndEnd =
                    List.range start.row (end.row - 2)
                        |> List.map getRowAtLine_

                endLine : String
                endLine =
                    getRowAtLine_ (end.row - 1)
            in
            List.concat
                [ [ Text.from <| getRowAtLine_ (start.row - 2)
                  , Text.from <| startLine
                  , underlineError
                        (start.row - 1)
                        { start = start.column
                        , end = String.length startLine - offsetBecauseOfLineNumber (start.row - 1)
                        }
                  ]
                , linesBetweenStartAndEnd
                    |> List.indexedMap Tuple.pair
                    |> List.concatMap
                        (\( lineNumber, line ) ->
                            [ Text.from <| line
                            , underlineError
                                lineNumber
                                { start = getIndexOfFirstNonSpace (offsetBecauseOfLineNumber lineNumber) line
                                , end = String.length line - offsetBecauseOfLineNumber lineNumber
                                }
                            ]
                        )
                , [ Text.from <| endLine
                  , underlineError
                        (end.row - 1)
                        { start = getIndexOfFirstNonSpace (offsetBecauseOfLineNumber (end.row - 1)) endLine
                        , end = String.length endLine - offsetBecauseOfLineNumber (end.row - 1)
                        }
                  , Text.from <| getRowAtLine_ end.row
                  ]
                ]


getIndexOfFirstNonSpace : Int -> String -> Int
getIndexOfFirstNonSpace offset string =
    string
        |> String.indexes (String.trim <| String.dropLeft offset string)
        |> List.head
        |> Maybe.withDefault 0
        |> (\n -> n - offset + 1)


getRowAtLine : File -> Int -> String
getRowAtLine file =
    let
        lines : Array String
        lines =
            file.source
                |> String.lines
                |> Array.fromList
    in
    \rowIndex ->
        case Array.get rowIndex lines of
            Just line ->
                if String.trim line /= "" then
                    String.fromInt (rowIndex + 1) ++ "| " ++ line ++ "\n"

                else
                    ""

            Nothing ->
                ""


underlineError : Int -> { start : Int, end : Int } -> Text
underlineError lineNumber { start, end } =
    let
        baseText : String
        baseText =
            String.repeat (offsetBecauseOfLineNumber lineNumber + start - 1) " " ++ String.repeat (end - start) "^" ++ "\n"
    in
    baseText
        |> Text.from
        |> Text.inRed


offsetBecauseOfLineNumber : Int -> Int
offsetBecauseOfLineNumber lineNumber =
    lineNumber
        |> String.fromInt
        |> String.length
        |> (+) 2


totalNumberOfErrors : List ( File, List Error ) -> Int
totalNumberOfErrors errors =
    errors
        |> List.concatMap Tuple.second
        |> List.length


formatReports : List ( File, List Error ) -> List Text
formatReports errors =
    case errors of
        [] ->
            []

        [ error ] ->
            formatReportForFileWithExtract error

        a :: b :: restOfErrors ->
            List.concat
                [ formatReportForFileWithExtract a
                , [ fileSeparator a b ]
                , formatReports (b :: restOfErrors)
                ]


fileSeparator : ( File, List Error ) -> ( File, List Error ) -> Text
fileSeparator a b =
    let
        str : String
        str =
            "\n\n"
                ++ String.padLeft 80 ' ' (fileIdentifier a ++ "  ↑    ")
                ++ "\n====o======================================================================o===="
                ++ "\n    ↓  "
                ++ fileIdentifier b
                ++ "\n\n\n"
    in
    Text.from str
        |> Text.inRed


fileIdentifier : ( File, List Error ) -> String
fileIdentifier ( file, errors ) =
    case List.head errors |> Maybe.andThen .moduleName of
        Just moduleName ->
            moduleName

        Nothing ->
            file.path
