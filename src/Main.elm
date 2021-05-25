module Main exposing (main)

import Browser
import Html exposing (Html, button, div, input, label, text)
import Html.Events exposing (onClick, onInput)



{-
   TODO:
       - [x] Better Modeling for User and Validation
       - [ ] Validate: name and last name, age, and email
       - [ ] Collect all error messages (do not short-circuit on first error)
          - Idea: Use a dict to collect the errors
-}


type Validation
    = NotChecked
    | Validated (Result String User)


type alias User =
    { name : String
    , age : Int
    , email : String
    }


type alias Model =
    { name : String
    , age : String
    , email : String
    , validation : Validation
    }


type Msg
    = EnteredNameInput String
    | EnteredAgeInput String
    | EnteredEmailInput String
    | ClickedValidationButton


init : Model
init =
    Model "" "" "" NotChecked


update : Msg -> Model -> Model
update msg model =
    case msg of
        EnteredNameInput name ->
            { model | name = name }

        EnteredAgeInput age ->
            { model | age = age }

        EnteredEmailInput age ->
            { model | age = age }

        ClickedValidationButton ->
            validate model


validateName : User -> Result String User
validateName user =
    Ok user


validateAge : User -> Result String User
validateAge user =
    Ok user


validateEmail : User -> Result String User
validateEmail user =
    Ok user


parseUser : Model -> Result String User
parseUser model =
    Ok (User "" 3 "")


validate : Model -> Model
validate model =
    let
        validation =
            parseUser model
                |> Result.andThen validateName
                |> Result.andThen validateAge
                |> Result.andThen validateEmail
                |> Validated
    in
    { model | validation = validation }


view : Model -> Html Msg
view { validation } =
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
        , viewButton
        , viewValidationInfo validation
        ]


viewButton : Html Msg
viewButton =
    div [] [ button [ onClick ClickedValidationButton ] [ text "Validate" ] ]


viewValidationInfo : Validation -> Html msg
viewValidationInfo validation =
    case validation of
        Validated (Ok _) ->
            div [] [ text "Awesome!, all is good" ]

        Validated (Err _) ->
            div [] [ text "Got some errors :(" ]

        NotChecked ->
            div [] [ text "Please click to validate" ]


main : Program () Model Msg
main =
    Browser.sandbox { init = init, view = view, update = update }
