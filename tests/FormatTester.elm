module FormatTester exposing (expect)

{-| Write expectations for the result of formatting functions.

@docs expect

-}

import Expect exposing (Expectation)


type alias Text =
    { str : String
    , color : Maybe ( Int, Int, Int )
    }


{-| Make an expectation of the result of a formatting function.

Two assertions will be made:

  - The result without colors: This will be the easiest to read when testing

  - The result with colors: A bit harder to get right from the start, but at
    least this will make sure all the colors are in the right location.

        test "report a single error in a file" <|
            \() ->
                formatFunction someInput
                    |> FormatTester.expect
                        { withoutColors = "some highlighted text"
                        , withColors = "some [highlighted](255-255-255) text"
                        }

-}
expect : { withoutColors : String, withColors : String } -> List Text -> Expectation
expect { withoutColors, withColors } =
    Expect.all
        [ \textList -> Expect.equal withoutColors (formatWithoutColors textList)
        , \textList -> Expect.equal withColors (formatWithColors textList)
        ]


formatWithoutColors : List Text -> String
formatWithoutColors textList =
    textList
        |> List.map .str
        |> String.join ""


formatWithColors : List Text -> String
formatWithColors textList =
    textList
        |> List.map
            (\{ str, color } ->
                case color of
                    Just ( r, g, b ) ->
                        "[" ++ str ++ "](" ++ String.fromInt r ++ "-" ++ String.fromInt r ++ "-" ++ String.fromInt r ++ ")"

                    Nothing ->
                        str
            )
        |> String.join ""
