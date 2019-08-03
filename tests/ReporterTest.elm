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
        , test "report a single error in a file"
            (\() ->
                [ ( { path = "src/FileA.elm", source = dummySource }
                  , [ { moduleName = Just "SomeModule.Name"
                      , ruleName = "NoDebug"
                      , message = "Do not use Debug"
                      , details =
                            [ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Vestibulum cursus erat ullamcorper, commodo leo quis, sollicitudin eros. Sed semper mattis ex, vitae dignissim lectus. Integer eu risus augue. Nam egestas lacus non lacus molestie mattis. Phasellus magna dui, ultrices eu massa nec, interdum tincidunt eros. Aenean rutrum a purus nec cursus. Integer ullamcorper leo non lectus dictum, in vulputate justo vulputate. Donec ullamcorper finibus quam sed dictum."
                            , "Donec sed ligula ac mi pretium mattis et in nisi. Nulla nec ex hendrerit, sollicitudin eros at, mattis tortor. Ut lacinia ornare lectus in vestibulum. Nam congue ultricies dolor, in venenatis nulla sagittis nec. In ac leo sit amet diam iaculis ornare eu non odio. Proin sed orci et urna tincidunt tincidunt quis a lacus. Donec euismod odio nulla, sit amet iaculis lorem interdum sollicitudin. Vivamus bibendum quam urna, in tristique lacus iaculis id. In tempor lectus ipsum, vehicula bibendum magna pretium vitae. Cras ullamcorper rutrum nunc non sollicitudin. Curabitur tempus eleifend nunc, sed ornare nisl tincidunt vel. Maecenas eu nisl ligula."
                            ]
                      , range =
                            { start = { row = 2, column = 5 }
                            , end = { row = 2, column = 10 }
                            }
                      }
                    ]
                  )
                , ( { path = "src/FileB.elm", source = dummySource }, [] )
                ]
                    |> Reporter.formatReport
                    |> expect
                        { withoutColors = """-- ELM-LINT ERROR ---------------------------------------------- SomeModule.Name

NoDebug: Do not use Debug

1| module A exposing (a)
2| a = Debug.log "debug" 1
       ^^^^^


Lorem ipsum dolor sit amet, consectetur adipiscing elit. Vestibulum cursus erat ullamcorper, commodo leo quis, sollicitudin eros. Sed semper mattis ex, vitae dignissim lectus. Integer eu risus augue. Nam egestas lacus non lacus molestie mattis. Phasellus magna dui, ultrices eu massa nec, interdum tincidunt eros. Aenean rutrum a purus nec cursus. Integer ullamcorper leo non lectus dictum, in vulputate justo vulputate. Donec ullamcorper finibus quam sed dictum.

Donec sed ligula ac mi pretium mattis et in nisi. Nulla nec ex hendrerit, sollicitudin eros at, mattis tortor. Ut lacinia ornare lectus in vestibulum. Nam congue ultricies dolor, in venenatis nulla sagittis nec. In ac leo sit amet diam iaculis ornare eu non odio. Proin sed orci et urna tincidunt tincidunt quis a lacus. Donec euismod odio nulla, sit amet iaculis lorem interdum sollicitudin. Vivamus bibendum quam urna, in tristique lacus iaculis id. In tempor lectus ipsum, vehicula bibendum magna pretium vitae. Cras ullamcorper rutrum nunc non sollicitudin. Curabitur tempus eleifend nunc, sed ornare nisl tincidunt vel. Maecenas eu nisl ligula.
"""
                        , withColors = """[-- ELM-LINT ERROR ---------------------------------------------- SomeModule.Name](51-51-51)

[NoDebug](255-255-255): Do not use Debug

1| module A exposing (a)
2| a = Debug.log "debug" 1
       [^^^^^](255-255-255)


Lorem ipsum dolor sit amet, consectetur adipiscing elit. Vestibulum cursus erat ullamcorper, commodo leo quis, sollicitudin eros. Sed semper mattis ex, vitae dignissim lectus. Integer eu risus augue. Nam egestas lacus non lacus molestie mattis. Phasellus magna dui, ultrices eu massa nec, interdum tincidunt eros. Aenean rutrum a purus nec cursus. Integer ullamcorper leo non lectus dictum, in vulputate justo vulputate. Donec ullamcorper finibus quam sed dictum.

Donec sed ligula ac mi pretium mattis et in nisi. Nulla nec ex hendrerit, sollicitudin eros at, mattis tortor. Ut lacinia ornare lectus in vestibulum. Nam congue ultricies dolor, in venenatis nulla sagittis nec. In ac leo sit amet diam iaculis ornare eu non odio. Proin sed orci et urna tincidunt tincidunt quis a lacus. Donec euismod odio nulla, sit amet iaculis lorem interdum sollicitudin. Vivamus bibendum quam urna, in tristique lacus iaculis id. In tempor lectus ipsum, vehicula bibendum magna pretium vitae. Cras ullamcorper rutrum nunc non sollicitudin. Curabitur tempus eleifend nunc, sed ornare nisl tincidunt vel. Maecenas eu nisl ligula.
"""
                        }
            )
        ]
