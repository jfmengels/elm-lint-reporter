module FormatFixProposalTest exposing (suite)

import Expect exposing (Expectation)
import FormatTester exposing (expect)
import Reporter exposing (Error, File)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "formatFixProposal"
        [ test "propose fix where the diff is only a single segment"
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
        , test "propose fix where the diff contains blank lines"
            (\() ->
                let
                    error : Error
                    error =
                        { moduleName = Just "Some.File"
                        , ruleName = "SomeRuleName"
                        , message = "Some message"
                        , details = [ "Some details" ]
                        , range =
                            { start = { row = 2, column = 1 }
                            , end = { row = 2, column = 2 }
                            }
                        , hasFix = True
                        }

                    file : File
                    file =
                        { path = "src/Some/File"
                        , source = """module Some.File exposing (a)
a =
    1

b =
    a
"""
                        }

                    fixedSource : String
                    fixedSource =
                        """module Some.File exposing (a)


b =
    a
"""
                in
                Reporter.formatFixProposal file error fixedSource
                    |> expect
                        { withoutColors =
                            """-- ELM-LINT ERROR ---------------------------------------------------- Some.File

SomeRuleName: Some message

1| module Some.File exposing (a)
2| a =
   ^
3|     1


Some details

I think I can fix this. Here is my proposal:

1| module Some.File exposing (a)
2| a =
3|     1
4| """ ++ "\n   " ++ """
5| b =
"""
                        , withColors =
                            """[-- ELM-LINT ERROR ---------------------------------------------------- Some.File](51-187-200)

[SomeRuleName](255-0-0): Some message

1| module Some.File exposing (a)
2| a =
   [^](255-0-0)
3|     1


Some details

[I think I can fix this. Here is my proposal:](51-187-200)

1| module Some.File exposing (a)
[2| a =](255-0-0)
[3|     1](255-0-0)
4| """ ++ """
[   ](0-128-0)
5| b =
"""
                        }
            )
        ]
