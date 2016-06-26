port module Port exposing (..)


type alias MouseUpdate =
    { x : Float
    , y : Float
    , color : Float
    }


port mouseMove : (MouseUpdate -> msg) -> Sub msg


port mouseDown : (MouseUpdate -> msg) -> Sub msg


port mouseUp : (MouseUpdate -> msg) -> Sub msg
