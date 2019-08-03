module ReporterTest exposing (suite)

import Expect exposing (Expectation)
import Reporter
import Test exposing (Test, describe, test)


type alias Text =
    { str : String
    , color : Maybe ( Int, Int, Int )
    }


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


expect : { withoutColors : String, withColors : String } -> List Text -> Expectation
expect { withoutColors, withColors } =
    Expect.all
        [ \textList -> Expect.equal withoutColors (formatWithoutColors textList)
        , \textList -> Expect.equal withColors (formatWithColors textList)
        ]


dummySource : String
dummySource =
    """module A exposing (a)
a = Debug.log "debug" 1"""


suite : Test
suite =
    describe "formatReport"
        [ test "report that all is fine when there are no errors"
            (\() ->
                [ ( { path = "src/FileA.elm", source = dummySource }, [] )
                , ( { path = "src/FileB.elm", source = dummySource }, [] )
                ]
                    |> Reporter.formatReport
                    |> expect
                        { withoutColors = "I found no linting errors.\nYou're all good!"
                        , withColors = "I found no linting errors.\nYou're all good!"
                        }
            )
            )
        ]
