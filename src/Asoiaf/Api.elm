module Asoiaf.Api
    exposing
        ( getCharacters
        , getCharacter
        , queryCharacters
        , getHouses
        , getHouse
        , queryHouses
        , getBooks
        , getBook
        , queryBooks
        , CharacterQuery
        , defaultCharacterQuery
        , HouseQuery
        , defaultHouseQuery
        , BookQuery
        , defaultBookQuery
        , Pagination
        , defaultPagination
        )

{-|

# Queries
@docs CharacterQuery, HouseQuery, BookQuery, defaultCharacterQuery, defaultHouseQuery, defaultBookQuery

# Pagination
@docs Pagination, defaultPagination

# Characters
@docs getCharacters, getCharacter, queryCharacters

# Houses
@docs getHouses, getHouse, queryHouses

# Books
@docs getBooks, getBook, queryBooks

-}

import Task
import Http
import Date exposing (Date)
import Json.Decode
import Asoiaf.Models as Models


-- TYPES


type Query
    = Character CharacterQuery
    | House HouseQuery
    | Book BookQuery


{-| All requests are paginated. Use this to specify which page to request and
what page size to use.
-}
type alias Pagination =
    { page : Int
    , pageSize : Int
    }


{-| Represents the fields we can query characters by
-}
type alias CharacterQuery =
    { name : Maybe String
    , gender : Maybe String
    , culture : Maybe String
    , born : Maybe Int
    , died : Maybe Int
    , isAlive : Maybe Bool
    }


{-| Represents the fields we can query houses by
-}
type alias HouseQuery =
    { name : Maybe String
    , region : Maybe String
    , words : Maybe String
    , hasWords : Maybe Bool
    , hasTitles : Maybe Bool
    , hasSeats : Maybe Bool
    , hasDiedOut : Maybe Bool
    , hasAncestralWeapons : Maybe Bool
    }


{-| Represents the fields we can query books by
-}
type alias BookQuery =
    { name : Maybe String
    , fromReleaseDate : Maybe Date
    , toReleaseDate : Maybe Date
    }


{-| Default pagination.

    { page = 1
    , pageSize = 10
    }
-}
defaultPagination : Pagination
defaultPagination =
    { page = 1
    , pageSize = 10
    }


{-| Default character query. No fields specified.
-}
defaultCharacterQuery : CharacterQuery
defaultCharacterQuery =
    CharacterQuery Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing


{-| Default house query. No fields specified.
-}
defaultHouseQuery : HouseQuery
defaultHouseQuery =
    HouseQuery Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing


{-| Default book query. No fields specified.
-}
defaultBookQuery : BookQuery
defaultBookQuery =
    BookQuery Nothing
        Nothing
        Nothing



-- URLS


baseUrl : Models.Url
baseUrl =
    "http://www.anapioficeandfire.com/api"


charactersUrl : Models.Url
charactersUrl =
    baseUrl ++ "/characters/"


booksUrl : Models.Url
booksUrl =
    baseUrl ++ "/books/"


housesUrl : Models.Url
housesUrl =
    baseUrl ++ "/houses/"



-- QUERIES


{-| Query characters

    pagination =
      { page = 1
      , pageSize = 25
      }
    query =
      { defaultCharacterQuery
      | name = Just "Jon Snow"
      }
    queryCharacters pagination query == [ { name = "Jon Snow", ... } ]
-}
queryCharacters : Pagination -> CharacterQuery -> Task.Task Http.Error (List Models.Character)
queryCharacters pagination query =
    getMultiple Models.decodeCharacter charactersUrl pagination (Character query)


{-| Query books

    pagination =
      { page = 1
      , pageSize = 25
      }
    query =
      { defaultBookQuery
      | name = Just "A Game of Thrones"
      }
    queryBooks pagination query == [ { name = "A Game of Thrones", ... } ]
-}
queryBooks : Pagination -> BookQuery -> Task.Task Http.Error (List Models.Book)
queryBooks pagination query =
    getMultiple Models.decodeBook booksUrl pagination (Book query)


{-| Query houses

    pagination =
      { page = 1
      , pageSize = 25
      }
    query =
      { defaultBookQuery
      | words = Just "Winter is Coming"
      }
    queryHouses pagination query == [ { name = "House Stark of Winterfell", ... } ]
-}
queryHouses : Pagination -> HouseQuery -> Task.Task Http.Error (List Models.House)
queryHouses pagination query =
    getMultiple Models.decodeHouse housesUrl pagination (House query)



-- GETTERS


{-| Get a list of characters
-}
getCharacters : Pagination -> Task.Task Http.Error (List Models.Character)
getCharacters pagination =
    queryCharacters pagination defaultCharacterQuery


{-| Get a character by id
-}
getCharacter : Models.Id -> Task.Task Http.Error Models.Character
getCharacter id =
    getSingle Models.decodeCharacter charactersUrl id


{-| Get a list of books
-}
getBooks : Pagination -> Task.Task Http.Error (List Models.Book)
getBooks pagination =
    queryBooks pagination defaultBookQuery


{-| Get a book by id
-}
getBook : Models.Id -> Task.Task Http.Error Models.Book
getBook id =
    getSingle Models.decodeBook booksUrl id


{-| Get a list of houses
-}
getHouses : Pagination -> Task.Task Http.Error (List Models.House)
getHouses pagination =
    queryHouses pagination defaultHouseQuery


{-| Get a house by id
-}
getHouse : Models.Id -> Task.Task Http.Error Models.House
getHouse id =
    getSingle Models.decodeHouse housesUrl id



-- HELPERS


isJust : Maybe a -> Bool
isJust mb =
    case mb of
        Just _ ->
            True

        Nothing ->
            False


queryToList : Query -> List ( String, String )
queryToList query =
    let
        queryList : List ( String, Maybe String )
        queryList =
            case query of
                Book q ->
                    [ ( "name", q.name )
                    , ( "fromReleaseDate", q.fromReleaseDate |> Maybe.map toString )
                    , ( "toReleaseTade", q.toReleaseDate |> Maybe.map toString )
                    ]

                Character q ->
                    [ ( "name", q.name )
                    , ( "gender", q.gender )
                    , ( "culture", q.culture )
                    , ( "born", q.born |> Maybe.map toString )
                    , ( "died", q.died |> Maybe.map toString )
                    , ( "isAlive", q.isAlive |> Maybe.map toString )
                    ]

                House q ->
                    [ ( "name", q.name )
                    , ( "region", q.region )
                    , ( "words", q.words )
                    , ( "hasWords", q.hasWords |> Maybe.map toString )
                    , ( "hasTitles", q.hasTitles |> Maybe.map toString )
                    , ( "hasSeats", q.hasSeats |> Maybe.map toString )
                    , ( "hasDiedOut", q.hasDiedOut |> Maybe.map toString )
                    , ( "hasAncestralWeapons", q.hasAncestralWeapons |> Maybe.map toString )
                    ]

        sndWithDefault ( a, b ) =
            ( a, b |> Maybe.withDefault "" )
    in
        queryList
            |> List.filter (snd >> isJust)
            |> List.map sndWithDefault


paginationToList : Pagination -> List ( String, String )
paginationToList pagination =
    [ ( "page", pagination.page |> toString )
    , ( "pageSize", pagination.pageSize |> toString )
    ]


get : Json.Decode.Decoder a -> Models.Url -> Task.Task Http.Error a
get decoder url =
    Http.get decoder url


getSingle : Json.Decode.Decoder a -> Models.Url -> Models.Id -> Task.Task Http.Error a
getSingle decoder url id =
    get decoder (url ++ toString id)


getMultiple : Json.Decode.Decoder a -> Models.Url -> Pagination -> Query -> Task.Task Http.Error (List a)
getMultiple decoder url pagination query =
    let
        queryParams =
            queryToList query ++ paginationToList pagination
    in
        Http.url url queryParams
            |> get (Json.Decode.list decoder)
