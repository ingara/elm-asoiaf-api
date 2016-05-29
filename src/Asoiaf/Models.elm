module Asoiaf.Models
    exposing
        ( Character
        , decodeCharacter
        , encodeCharacter
        , House
        , decodeHouse
        , encodeHouse
        , Book
        , decodeBook
        , encodeBook
        , Url
        , Id
        , getId
        )

{-| Models

# Types
@docs Character, House, Book, Url, Id

# Utils
@docs getId

# Decoders
@docs decodeCharacter, decodeHouse, decodeBook

# Encoders
@docs encodeCharacter, encodeHouse, encodeBook

-}

import Json.Decode
import Json.Encode
import Json.Decode.Pipeline
import String
import Regex


{-| Represents a url
-}
type alias Url =
    String


{-| Represents an id
-}
type alias Id =
    Int


{-| Represents a character
-}
type alias Character =
    { url : String
    , name : String
    , gender : String
    , culture : String
    , born : String
    , died : String
    , titles : List String
    , aliases : List String
    , father : String
    , mother : String
    , spouse : String
    , allegiances : List String
    , books : List String
    , povBooks : List String
    , tvSeries : List String
    , playedBy : List String
    }


{-| Represents a house
-}
type alias House =
    { url : String
    , name : String
    , region : String
    , coatOfArms : String
    , words : String
    , titles : List String
    , seats : List String
    , currentLord : String
    , heir : String
    , overlord : String
    , founded : String
    , founder : String
    , diedOut : String
    , ancestralWeapons : List String
    , cadetBranches : List String
    , swornMembers : List String
    }


{-| Represents a book
-}
type alias Book =
    { url : String
    , name : String
    , isbn : String
    , authors : List String
    , numberOfPages : Int
    , publisher : String
    , country : String
    , mediaType : String
    , released : String
    , characters : List String
    , povCharacters : List String
    }


{-| Json decode a character
-}
decodeCharacter : Json.Decode.Decoder Character
decodeCharacter =
    Json.Decode.Pipeline.decode Character
        |> Json.Decode.Pipeline.required "url" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "name" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "gender" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "culture" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "born" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "died" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "titles" (Json.Decode.list Json.Decode.string)
        |> Json.Decode.Pipeline.required "aliases" (Json.Decode.list Json.Decode.string)
        |> Json.Decode.Pipeline.required "father" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "mother" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "spouse" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "allegiances" (Json.Decode.list Json.Decode.string)
        |> Json.Decode.Pipeline.required "books" (Json.Decode.list Json.Decode.string)
        |> Json.Decode.Pipeline.required "povBooks" (Json.Decode.list Json.Decode.string)
        |> Json.Decode.Pipeline.required "tvSeries" (Json.Decode.list Json.Decode.string)
        |> Json.Decode.Pipeline.required "playedBy" (Json.Decode.list Json.Decode.string)


{-| Json encode a character
-}
encodeCharacter : Character -> Json.Encode.Value
encodeCharacter record =
    Json.Encode.object
        [ ( "url", Json.Encode.string <| record.url )
        , ( "name", Json.Encode.string <| record.name )
        , ( "gender", Json.Encode.string <| record.gender )
        , ( "culture", Json.Encode.string <| record.culture )
        , ( "born", Json.Encode.string <| record.born )
        , ( "died", Json.Encode.string <| record.died )
        , ( "titles", Json.Encode.list <| List.map Json.Encode.string <| record.titles )
        , ( "aliases", Json.Encode.list <| List.map Json.Encode.string <| record.aliases )
        , ( "father", Json.Encode.string <| record.father )
        , ( "mother", Json.Encode.string <| record.mother )
        , ( "spouse", Json.Encode.string <| record.spouse )
        , ( "allegiances", Json.Encode.list <| List.map Json.Encode.string <| record.allegiances )
        , ( "books", Json.Encode.list <| List.map Json.Encode.string <| record.books )
        , ( "povBooks", Json.Encode.list <| List.map Json.Encode.string <| record.povBooks )
        , ( "tvSeries", Json.Encode.list <| List.map Json.Encode.string <| record.tvSeries )
        , ( "playedBy", Json.Encode.list <| List.map Json.Encode.string <| record.playedBy )
        ]


{-| Json decode a house
-}
decodeHouse : Json.Decode.Decoder House
decodeHouse =
    Json.Decode.Pipeline.decode House
        |> Json.Decode.Pipeline.required "url" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "name" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "region" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "coatOfArms" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "words" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "titles" (Json.Decode.list Json.Decode.string)
        |> Json.Decode.Pipeline.required "seats" (Json.Decode.list Json.Decode.string)
        |> Json.Decode.Pipeline.required "currentLord" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "heir" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "overlord" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "founded" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "founder" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "diedOut" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "ancestralWeapons" (Json.Decode.list Json.Decode.string)
        |> Json.Decode.Pipeline.required "cadetBranches" (Json.Decode.list Json.Decode.string)
        |> Json.Decode.Pipeline.required "swornMembers" (Json.Decode.list Json.Decode.string)


{-| Json encode a House
-}
encodeHouse : House -> Json.Encode.Value
encodeHouse record =
    Json.Encode.object
        [ ( "url", Json.Encode.string <| record.url )
        , ( "name", Json.Encode.string <| record.name )
        , ( "region", Json.Encode.string <| record.region )
        , ( "coatOfArms", Json.Encode.string <| record.coatOfArms )
        , ( "words", Json.Encode.string <| record.words )
        , ( "titles", Json.Encode.list <| List.map Json.Encode.string <| record.titles )
        , ( "seats", Json.Encode.list <| List.map Json.Encode.string <| record.seats )
        , ( "currentLord", Json.Encode.string <| record.currentLord )
        , ( "heir", Json.Encode.string <| record.heir )
        , ( "overlord", Json.Encode.string <| record.overlord )
        , ( "founded", Json.Encode.string <| record.founded )
        , ( "founder", Json.Encode.string <| record.founder )
        , ( "diedOut", Json.Encode.string <| record.diedOut )
        , ( "ancestralWeapons", Json.Encode.list <| List.map Json.Encode.string <| record.ancestralWeapons )
        , ( "cadetBranches", Json.Encode.list <| List.map Json.Encode.string <| record.cadetBranches )
        , ( "swornMembers", Json.Encode.list <| List.map Json.Encode.string <| record.swornMembers )
        ]


{-| Json decode a book
-}
decodeBook : Json.Decode.Decoder Book
decodeBook =
    Json.Decode.Pipeline.decode Book
        |> Json.Decode.Pipeline.required "url" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "name" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "isbn" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "authors" (Json.Decode.list Json.Decode.string)
        |> Json.Decode.Pipeline.required "numberOfPages" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "publisher" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "country" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "mediaType" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "released" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "characters" (Json.Decode.list Json.Decode.string)
        |> Json.Decode.Pipeline.required "povCharacters" (Json.Decode.list Json.Decode.string)


{-| Json encode a book
-}
encodeBook : Book -> Json.Encode.Value
encodeBook record =
    Json.Encode.object
        [ ( "url", Json.Encode.string <| record.url )
        , ( "name", Json.Encode.string <| record.name )
        , ( "isbn", Json.Encode.string <| record.isbn )
        , ( "authors", Json.Encode.list <| List.map Json.Encode.string <| record.authors )
        , ( "numberOfPages", Json.Encode.int <| record.numberOfPages )
        , ( "publisher", Json.Encode.string <| record.publisher )
        , ( "country", Json.Encode.string <| record.country )
        , ( "mediaType", Json.Encode.string <| record.mediaType )
        , ( "released", Json.Encode.string <| record.released )
        , ( "characters", Json.Encode.list <| List.map Json.Encode.string <| record.characters )
        , ( "povCharacters", Json.Encode.list <| List.map Json.Encode.string <| record.povCharacters )
        ]


{-| Gets the id from the end of a URL as used in the ASOIAF API.

    getId "http://www.anapioficeandfire.com/api/houses/362" == 362

Useful for for example getting ids of the books a character is appearing in

    getBookIds : Character -> List Int
    getBookIds character =
      List.map getId character.books
-}
getId : Url -> Id
getId url =
    url
        |> Regex.find (Regex.AtMost 1) (Regex.regex "\\d+$")
        |> List.head
        |> Maybe.map .match
        |> Maybe.withDefault ""
        |> String.toInt
        |> Result.withDefault -1
