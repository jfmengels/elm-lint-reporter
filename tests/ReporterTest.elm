module ReporterTest exposing (suite)

import Expect
import Reporter
import Test exposing (Test, describe, test)


withoutColors : List { r | str : String } -> String
withoutColors textList =
    textList
        |> List.map .str
        |> String.join ""


dummySource : String
dummySource =
    "module A exposing (a)\na = 1"


suite : Test
suite =
    describe "formatReport"
        [ test "report that all is fine when there are no errors"
            (\() ->
                [ ( { path = "src/FileA.elm", source = dummySource }, [] )
                , ( { path = "src/FileB.elm", source = dummySource }, [] )
                ]
                    |> Reporter.formatReport
                    |> withoutColors
                    |> Expect.equal "I found no linting errors.\nYou're all good!"
            )
        ]
