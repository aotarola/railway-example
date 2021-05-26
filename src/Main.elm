module Main exposing (main)

import Browser
import Html exposing (Html, button, div, input, label, text)
import Html.Events exposing (onClick, onInput)



{-
   TODO:
       - [x] Better Modeling for User and Validation
       - [x] Validate: name and last name, age, and email
       - [ ] Collect all error messages (do not short-circuit on first error)
          - Idea: Use a dict to collect the errors?
-}


type ValidationError
    = EmptyName
    | InvalidAge
    | InvalidEmail String


type Validation
    = Validation (Result ValidationError User)


type alias User =
    { name : String
    , age : Int
    , email : String
    }


type alias Model =
    { name : String
    , age : String
    , email : String
    }


type Msg
    = EnteredNameInput String
    | EnteredAgeInput String
    | EnteredEmailInput String


init : Model
init =
    Model "" "" ""


update : Msg -> Model -> Model
update msg model =
    case msg of
        EnteredNameInput name ->
            { model | name = name }

        EnteredAgeInput age ->
            { model | age = age }

        EnteredEmailInput email ->
            { model | email = email }


validateName : User -> Result ValidationError User
validateName user =
    case user.name of
        "" ->
            Err EmptyName

        _ ->
            Ok user


validateAge : User -> Result ValidationError User
validateAge user =
    if user.age < 21 then
        Err InvalidAge

    else
        Ok user


validateEmail : User -> Result ValidationError User
validateEmail user =
    if String.contains "@" user.email then
        Ok user

    else
        Err <| InvalidEmail user.email


parseUser : Model -> Result ValidationError User
parseUser { name, age, email } =
    let
        intAge =
            age
                |> String.toInt
                |> Maybe.withDefault 0
    in
    User name intAge email
        |> Ok


validate : Model -> Validation
validate model =
    parseUser model
        |> Result.andThen validateName
        |> Result.andThen validateAge
        |> Result.andThen validateEmail
        |> Validation


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ label [] [ text "Name" ]
            , input [ onInput EnteredNameInput ] []
            ]
        , div []
            [ label [] [ text "Age" ]
            , input [ onInput EnteredAgeInput ] []
            ]
        , div []
            [ label [] [ text "Email" ]
            , input [ onInput EnteredEmailInput ] []
            ]
        , viewValidationInfo model
        ]


viewValidationInfo : Model -> Html msg
viewValidationInfo model =
    let
        validated =
            validate model
    in
    case validated of
        Validation (Ok _) ->
            div [] [ text "Awesome!, all is good" ]

        Validation (Err EmptyName) ->
            div [] [ text "Please provide a name" ]

        Validation (Err InvalidAge) ->
            div [] [ text "You are not old enough :(" ]

        Validation (Err (InvalidEmail email)) ->
            div [] [ text <| "The provided email is not valid: " ++ email ]


main : Program () Model Msg
main =
    Browser.sandbox { init = init, view = view, update = update }
