module Main exposing (main)

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Events exposing (onInput)



{-
   TODO:
       - Better Modeling for User and Validation
       - Validate: name and last name, age, and email
       - Collect all error messages (do not short-circuit on first error)
-}


type alias User =
    { name : String, age : Int, isValid : Bool }


type Msg
    = UserClickedValidateNameButton String


init : User
init =
    User "Alice" 18 False


update : Msg -> User -> User
update msg model =
    case msg of
        UserClickedValidateNameButton _ ->
            model


view : User -> Html Msg
view model =
    div []
        [ input [ onInput UserClickedValidateNameButton ] []
        , button [] [ text "Validate" ]
        , div [] [ text "Click to validate" ]
        , div [] [ text <| getValidationInfo model.isValid ]
        ]


getValidationInfo : Bool -> String
getValidationInfo isValid =
    if isValid then
        "great, is valid"

    else
        "not so great, invalid"


main : Program () User Msg
main =
    Browser.sandbox { init = init, view = view, update = update }
