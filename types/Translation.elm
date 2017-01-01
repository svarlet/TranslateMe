module Types.Translation exposing (Translation, Translations)

import List.Nonempty exposing(Nonempty(..))

{- A Translation associates an english expression to one or many french
expressions.
-}
type alias Translation =
    { englishWord : String
    , frenchTranslation : Nonempty String
    }

{- The Translations type represents a list of Translations.
-}
type alias Translations
    = List Translation
