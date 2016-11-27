module Example exposing (..)

import Asoiaf.Api as Api
import Asoiaf.Models as Models
import Html exposing (..)
import Http


type alias Model =
    { characters : Maybe (List Models.Character)
    , houses : Maybe (List Models.House)
    , books : Maybe (List Models.Book)
    , errors : List Http.Error
    }


type Msg
    = CharactersFetched (Result Http.Error (List Models.Character))
    | HousesFetched (Result Http.Error (List Models.House))
    | BooksFetched (Result Http.Error (List Models.Book))


init : ( Model, Cmd Msg )
init =
    ( { characters = Nothing
      , houses = Nothing
      , books = Nothing
      , errors = []
      }
    , Cmd.batch
        [ Api.getCharacters Api.defaultPagination |> Http.send CharactersFetched
        , Api.getHouses Api.defaultPagination |> Http.send HousesFetched
        , Api.getBooks Api.defaultPagination |> Http.send BooksFetched
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CharactersFetched (Ok chars) ->
            ( { model | characters = Just chars }, Cmd.none )

        CharactersFetched (Err err) ->
            ( { model | errors = err :: model.errors }, Cmd.none )

        BooksFetched (Ok books) ->
            ( { model | books = Just books }, Cmd.none )

        BooksFetched (Err err) ->
            ( { model | errors = err :: model.errors }, Cmd.none )

        HousesFetched (Ok houses) ->
            ( { model | houses = Just houses }, Cmd.none )

        HousesFetched (Err err) ->
            ( { model | errors = err :: model.errors }, Cmd.none )


viewErrors : List Http.Error -> Html a
viewErrors errors =
    case errors of
        [] ->
            span [] []

        errs ->
            div []
                [ h3 [] [ text "Errors:" ]
                , ul [] (errs |> List.map (\e -> li [] [ e |> toString |> text ]))
                ]


viewCharacters : Maybe (List Models.Character) -> Html a
viewCharacters chars =
    div []
        [ h3 [] [ text "Characters" ]
        , case chars of
            Just chars ->
                ul [] (chars |> List.map (\c -> li [] [ c |> toString |> text ]))

            Nothing ->
                text "Not loaded"
        ]


viewBooks : Maybe (List Models.Book) -> Html a
viewBooks books =
    div []
        [ h3 [] [ text "Books" ]
        , case books of
            Just books ->
                ul [] (books |> List.map (\c -> li [] [ c |> toString |> text ]))

            Nothing ->
                text "Not loaded"
        ]


viewHouses : Maybe (List Models.House) -> Html a
viewHouses houses =
    div []
        [ h3 [] [ text "Houses" ]
        , case houses of
            Just houses ->
                ul [] (houses |> List.map (\c -> li [] [ c |> toString |> text ]))

            Nothing ->
                text "Not loaded"
        ]


view : Model -> Html a
view model =
    div []
        [ viewErrors model.errors
        , viewCharacters model.characters
        , viewBooks model.books
        , viewHouses model.houses
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
