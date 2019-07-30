module Text exposing
    ( Text
    , from
    , inBlue, inRed
    , join
    , toRecords
    )

{-| Represents text with some styling applied to it.

    text : List Text
    text =
        [ Text.from "My name is "
        , Text.from "John"
            |> Text.withColor
        , Text.from "."
        ]


# Definition

@docs Text


# Constructors

@docs from


# Modifiers

@docs inBlue, inRed


# Working with lists

@docs join


# Access

@docs toRecords

-}

-- DEFINITION


{-| Represents text with some styling applied to it.
-}
type Text
    = Text
        { str : String
        , color : Maybe ( Int, Int, Int )
        }



-- CONSTRUCTORS


{-| Create an unstyled `Text` from a string.
-}
from : String -> Text
from value =
    Text
        { str = value
        , color = Nothing
        }



-- MODIFIERS


inBlue : Text -> Text
inBlue (Text text) =
    Text { text | color = Just ( 51, 187, 200 ) }


inRed : Text -> Text
inRed (Text text) =
    Text { text | color = Just ( 255, 0, 0 ) }



-- WORKING WITH LISTS


join : String -> List (List Text) -> List Text
join sep chunks =
    List.intersperse [ from sep ] chunks
        |> List.concatMap identity



-- ACCESS


{-| Transform a list of texts into a list of records that can then be
transformed for display in different mediums.
-}
toRecords : List Text -> List { str : String, color : Maybe ( Int, Int, Int ) }
toRecords items =
    List.map (\(Text text) -> text) items
