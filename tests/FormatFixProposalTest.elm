module FormatFixProposalTest exposing (suite)

import Expect exposing (Expectation)
import FormatTester exposing (expect)
import Reporter exposing (Error, File)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "formatFixProposal"
        [ singleSegmentDiffTest
        ]


singleSegmentDiffTest : Test
singleSegmentDiffTest =
    test "propose fix where the diff is only a single segment"
        (\() ->
            let
                error : Error
                error =
                    { moduleName = Just "FileA"
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
                    , hasFix = True
                    }

                file : File
                file =
                    { path = "src/FileA.elm"
                    , source = """module FileA exposing (a)
a = Debug.log "debug" 1
other=lines
other2=lines2
"""
                    }

                fixedSource : String
                fixedSource =
                    """module FileA exposing (a)
a = 1
other=lines
other2=lines2
"""
            in
            Reporter.formatFixProposal file error fixedSource
                |> expect
                    { withoutColors = """-- ELM-LINT ERROR -------------------------------------------------------- FileA

NoDebug: Do not use Debug

1| module FileA exposing (a)
2| a = Debug.log "debug" 1
       ^^^^^
3| other=lines


Lorem ipsum dolor sit amet, consectetur adipiscing elit. Vestibulum cursus erat ullamcorper, commodo leo quis, sollicitudin eros. Sed semper mattis ex, vitae dignissim lectus. Integer eu risus augue. Nam egestas lacus non lacus molestie mattis. Phasellus magna dui, ultrices eu massa nec, interdum tincidunt eros. Aenean rutrum a purus nec cursus. Integer ullamcorper leo non lectus dictum, in vulputate justo vulputate. Donec ullamcorper finibus quam sed dictum.

Donec sed ligula ac mi pretium mattis et in nisi. Nulla nec ex hendrerit, sollicitudin eros at, mattis tortor. Ut lacinia ornare lectus in vestibulum. Nam congue ultricies dolor, in venenatis nulla sagittis nec. In ac leo sit amet diam iaculis ornare eu non odio. Proin sed orci et urna tincidunt tincidunt quis a lacus. Donec euismod odio nulla, sit amet iaculis lorem interdum sollicitudin. Vivamus bibendum quam urna, in tristique lacus iaculis id. In tempor lectus ipsum, vehicula bibendum magna pretium vitae. Cras ullamcorper rutrum nunc non sollicitudin. Curabitur tempus eleifend nunc, sed ornare nisl tincidunt vel. Maecenas eu nisl ligula.

I think I can fix this. Here is my proposal:

1| module FileA exposing (a)
2| a = Debug.log "debug" 1
   a = 1
3| other=lines
"""
                    , withColors = """[-- ELM-LINT ERROR -------------------------------------------------------- FileA](51-187-200)

[NoDebug](255-0-0): Do not use Debug

1| module FileA exposing (a)
2| a = Debug.log "debug" 1
       [^^^^^](255-0-0)
3| other=lines


Lorem ipsum dolor sit amet, consectetur adipiscing elit. Vestibulum cursus erat ullamcorper, commodo leo quis, sollicitudin eros. Sed semper mattis ex, vitae dignissim lectus. Integer eu risus augue. Nam egestas lacus non lacus molestie mattis. Phasellus magna dui, ultrices eu massa nec, interdum tincidunt eros. Aenean rutrum a purus nec cursus. Integer ullamcorper leo non lectus dictum, in vulputate justo vulputate. Donec ullamcorper finibus quam sed dictum.

Donec sed ligula ac mi pretium mattis et in nisi. Nulla nec ex hendrerit, sollicitudin eros at, mattis tortor. Ut lacinia ornare lectus in vestibulum. Nam congue ultricies dolor, in venenatis nulla sagittis nec. In ac leo sit amet diam iaculis ornare eu non odio. Proin sed orci et urna tincidunt tincidunt quis a lacus. Donec euismod odio nulla, sit amet iaculis lorem interdum sollicitudin. Vivamus bibendum quam urna, in tristique lacus iaculis id. In tempor lectus ipsum, vehicula bibendum magna pretium vitae. Cras ullamcorper rutrum nunc non sollicitudin. Curabitur tempus eleifend nunc, sed ornare nisl tincidunt vel. Maecenas eu nisl ligula.

[I think I can fix this. Here is my proposal:](51-187-200)

1| module FileA exposing (a)
[2| a = Debug.log "debug" 1](255-0-0)
[   a = 1](0-128-0)
3| other=lines
"""
                    }
        )
