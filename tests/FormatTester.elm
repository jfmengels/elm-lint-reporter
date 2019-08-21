module FormatTester exposing (expect)

{-| Write expectations for the result of formatting functions.

@docs expect

-}

import Expect exposing (Expectation)
import Reporter exposing (TextContent)


{-| Make an expectation of the result of a formatting function.

Two assertions will be made:

  - The result without colors: This will be the easiest to read when testing

  - The result with colors: A bit harder to get right from the start, but at
    least this will make sure all the colors are in the right location.

        test "report a single error in a file" <|
            \() ->
                formatFunction someInput
                    |> FormatTester.expect
                        { withoutColors = "some highlighted text with background and with both"
                        , withColors = "some [highlighted](255-0-0) text with [background](bg-16-16-16) and with [both](255-255-255-bg-16-16-16)"
                        }

-}
expect : { withoutColors : String, withColors : String } -> List TextContent -> Expectation
expect { withoutColors, withColors } =
    Expect.all
        [ \textList -> Expect.equal withoutColors (formatWithoutColors textList)
        , \textList -> Expect.equal withColors (formatWithColors textList)
        ]


formatWithoutColors : List TextContent -> String
formatWithoutColors textList =
    textList
        |> List.map .str
        |> String.join ""


formatWithColors : List TextContent -> String
formatWithColors textList =
    textList
        |> List.map
            (\{ str, color, backgroundColor } ->
                case ( color, backgroundColor ) of
                    ( Just color_, Just backgroundColor_ ) ->
                        "[" ++ str ++ "](" ++ colorToString color_ ++ "-br-" ++ colorToString backgroundColor_ ++ ")"

                    ( Nothing, Just backgroundColor_ ) ->
                        "[" ++ str ++ "](bg-" ++ colorToString backgroundColor_ ++ ")"

                    ( Just color_, Nothing ) ->
                        "[" ++ str ++ "](" ++ colorToString color_ ++ ")"

                    ( Nothing, Nothing ) ->
                        str
            )
        |> String.join ""


colorToString : ( Int, Int, Int ) -> String
colorToString ( r, g, b ) =
    String.fromInt r ++ "-" ++ String.fromInt g ++ "-" ++ String.fromInt b
