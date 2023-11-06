module Gen.Sound exposing (Sound(..), toString, fromString, asList)

{-| This module was generated. Any changes will be overwritten.

@docs Sound, toString, fromString, asList

-}
    
{-| Reprentation of Sound
-}
type Sound = ClickButton | Theme

{-| List of all playable sounds
-}
asList : List Sound
asList =
    [ ClickButton, Theme ]

{-| returns the path to the sound
-}
toString : Sound -> String
toString sound =
    case sound of
      ClickButton -> "ClickButton.mp3"

      Theme -> "theme.mp3"

fromString : String -> Maybe Sound
fromString string =
    case string of
      "ClickButton.mp3" -> Just ClickButton

      "theme.mp3" -> Just Theme
      _ -> Nothing   
