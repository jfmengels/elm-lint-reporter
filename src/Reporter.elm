module Reporter exposing
    ( Error, File, TextContent
    , Mode(..), formatReport
    , formatFixProposal
    )

{-| Formats the result of `elm-lint` in a nice human-readable way.


# Types

@docs Error, File, TextContent


# Report

@docs Mode, formatReport


# Fix

@docs formatFixProposal

-}

import Array exposing (Array)
import Diff
import Text exposing (Text)


{-| Contents of an error. Convert the errors from
[`Lint`](https://package.elm-lang.org/packages/jfmengels/elm-lint/4.0.0/Lint#Error)
to this type.
-}
type alias Error =
    { moduleName : Maybe String
    , ruleName : String
    , message : String
    , details : List String
    , range : Range
    , hasFix : Bool
    }


{-| Represents a file.

  - path is the file path
  - source is the raw contents of the file

-}
type alias File =
    { path : String
    , source : String
    }


{-| Represents styled text.

Formatter functions return a list of these, that should be
styled in the viewing medium according to the associated colors and joined
together without spaces.

-}
type alias TextContent =
    -- Should be the same as Text.TextContent
    { str : String
    , color : Maybe ( Int, Int, Int )
    , backgroundColor : Maybe ( Int, Int, Int )
    }


type alias Range =
    { start :
        { row : Int
        , column : Int
        }
    , end :
        { row : Int
        , column : Int
        }
    }


{-| Mode in which `elm-lint` is running.
-}
type Mode
    = Linting
    | Fixing


{-| Reports the errors reported by `elm-lint` in a nice human-readable way.
-}
formatReport : Mode -> List ( File, List Error ) -> List TextContent
formatReport lintMode errors =
    let
        numberOfErrors : Int
        numberOfErrors =
            totalNumberOfErrors errors
    in
    if numberOfErrors == 0 then
        "I found no linting errors.\nYou're all good!\n"
            |> Text.from
            |> Text.toRecord
            |> List.singleton

    else
        [ errors
            |> List.filter (Tuple.second >> List.isEmpty >> not)
            |> formatReports lintMode
        , [ Text.from "\n" ]
        ]
            |> List.concat
            |> List.map Text.toRecord


formatReportForFileWithExtract : Mode -> ( File, List Error ) -> List Text
formatReportForFileWithExtract lintMode ( file, errors ) =
    let
        formattedErrors : List (List Text)
        formattedErrors =
            List.map (formatErrorWithExtract lintMode file) errors

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


formatErrorWithExtract : Mode -> File -> Error -> List Text
formatErrorWithExtract lintMode file { ruleName, message, details, range, hasFix } =
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
    [ title
    , codeExtract_
    , details_
    , case lintMode of
        Linting ->
            if hasFix then
                [ Text.from "I think I know how to fix this problem. If you run "
                , "elm-lint --fix" |> Text.from |> Text.inBlue
                , Text.from ", I can\nsuggest a solution and you can validate it."
                ]

            else
                []

        Fixing ->
            []
    ]
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
            List.concat
                [ [ Text.from <| getRowAtLine_ (start.row - 2)
                  , Text.from <| getRowAtLine_ (start.row - 1)
                  ]
                , underlineError (start.row - 1) { start = start.column, end = end.column }
                , [ Text.from <| getRowAtLine_ end.row ]
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
                  ]
                , underlineError
                    (start.row - 1)
                    { start = start.column
                    , end = String.length startLine - offsetBecauseOfLineNumber (start.row - 1)
                    }
                , linesBetweenStartAndEnd
                    |> List.indexedMap Tuple.pair
                    |> List.concatMap
                        (\( lineNumber, line ) ->
                            (Text.from <| line)
                                :: underlineError
                                    lineNumber
                                    { start = getIndexOfFirstNonSpace (offsetBecauseOfLineNumber lineNumber) line
                                    , end = String.length line - offsetBecauseOfLineNumber lineNumber
                                    }
                        )
                , [ Text.from <| endLine ]
                , underlineError
                    (end.row - 1)
                    { start = getIndexOfFirstNonSpace (offsetBecauseOfLineNumber (end.row - 1)) endLine
                    , end = String.length endLine - offsetBecauseOfLineNumber (end.row - 1)
                    }
                , [ Text.from <| getRowAtLine_ end.row ]
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
                    (line ++ "\n")
                        |> prependWithLineNumber rowIndex

                else
                    ""

            Nothing ->
                ""


prependWithLineNumber : Int -> String -> String
prependWithLineNumber rowIndex line =
    String.fromInt (rowIndex + 1) ++ "| " ++ line


underlineError : Int -> { start : Int, end : Int } -> List Text
underlineError lineNumber { start, end } =
    [ Text.from <| String.repeat (offsetBecauseOfLineNumber lineNumber + start - 1) " "
    , String.repeat (end - start) "^"
        |> Text.from
        |> Text.inRed
    , Text.from "\n"
    ]


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


formatReports : Mode -> List ( File, List Error ) -> List Text
formatReports lintMode errors =
    case errors of
        [] ->
            []

        [ error ] ->
            formatReportForFileWithExtract lintMode error

        firstError :: secondError :: restOfErrors ->
            List.concat
                [ formatReportForFileWithExtract lintMode firstError
                , fileSeparator firstError secondError
                , formatReports lintMode (secondError :: restOfErrors)
                ]


fileSeparator : ( File, List Error ) -> ( File, List Error ) -> List Text
fileSeparator a b =
    let
        identifierAbove : String
        identifierAbove =
            fileIdentifier a
    in
    [ Text.from <| "\n\n" ++ String.repeat (73 - String.length identifierAbove) " "
    , (fileIdentifier a ++ "  ↑")
        ++ "\n====o======================================================================o===="
        ++ "\n    ↓  "
        ++ fileIdentifier b
        |> Text.from
        |> Text.inRed
    , Text.from "\n\n\n"
    ]


fileIdentifier : ( File, List Error ) -> String
fileIdentifier ( file, _ ) =
    file.path



-- FIX


{-| Reports a fix proposal for a single error reported by `elm-lint` in a nice human-readable way.
-}
formatFixProposal : File -> Error -> String -> List TextContent
formatFixProposal file error fixedSource =
    List.concat
        [ Text.join "\n\n"
            [ formatReportForFileWithExtract Fixing ( file, [ error ] )
            , [ "I think I can fix this. Here is my proposal:"
                    |> Text.from
                    |> Text.inBlue
              ]
            , diff file.source fixedSource
            ]
        , [ Text.from "\n" ]
        ]
        |> List.map Text.toRecord


diff : String -> String -> List Text
diff before after =
    Diff.diffLines before after
        |> addLineNumbers
        |> List.map extractValueFromChange
        |> List.intersperse (Text.from "\n")


addLineNumbers : List (Diff.Change String) -> List (Diff.Change Text)
addLineNumbers changes =
    List.foldl
        (\change ( lineNumber, diffLines ) ->
            case change of
                Diff.NoChange str ->
                    ( lineNumber + 1, Diff.NoChange (Text.from <| prependWithLineNumber lineNumber str) :: diffLines )

                Diff.Removed str ->
                    let
                        line : Text
                        line =
                            prependWithLineNumber lineNumber str
                                |> Text.from
                                |> Text.inRed
                    in
                    ( lineNumber + 1, Diff.Removed line :: diffLines )

                Diff.Added str ->
                    let
                        line : Text
                        line =
                            prependWithLineNumber lineNumber str
                                |> Text.from
                                |> Text.inGreen
                    in
                    ( lineNumber, Diff.Added line :: diffLines )
        )
        ( 0, [] )
        changes
        |> Tuple.second
        |> dropNonInterestingUnchangedLines
        |> List.reverse
        |> dropNonInterestingUnchangedLines


extractValueFromChange : Diff.Change a -> a
extractValueFromChange change =
    case change of
        Diff.NoChange value ->
            value

        Diff.Removed value ->
            value

        Diff.Added value ->
            value


dropNonInterestingUnchangedLines : List (Diff.Change a) -> List (Diff.Change a)
dropNonInterestingUnchangedLines changes =
    case findIndex (not << isNoChange) changes of
        Nothing ->
            changes

        Just index ->
            List.drop (index - 1) changes


findIndex : (a -> Bool) -> List a -> Maybe Int
findIndex predicate list =
    findIndexInternal predicate 0 list


findIndexInternal : (a -> Bool) -> Int -> List a -> Maybe Int
findIndexInternal predicate index list =
    case list of
        [] ->
            Nothing

        item :: rest ->
            if predicate item then
                Just index

            else
                findIndexInternal predicate (index + 1) rest


isNoChange : Diff.Change a -> Bool
isNoChange change =
    case change of
        Diff.NoChange _ ->
            True

        Diff.Removed _ ->
            False

        Diff.Added _ ->
            False
