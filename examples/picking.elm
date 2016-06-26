module Main exposing (..)

import Html exposing (..)
import Html.App
import Html.Attributes exposing (..)
import Math.Vector3 exposing (Vec3, vec3)
import Math.Matrix4 exposing (Mat4, makeOrtho2D, translate)
import Json.Decode
import Port
import WebGL
import Array


w =
    500


h =
    500



-- MAIN


main : Program Never
main =
    Html.App.program
        { init = init
        , view = view
        , subscriptions =
            \_ ->
                Sub.batch
                    [ Port.mouseMove MouseMove
                    , Port.mouseDown MouseDown
                    , Port.mouseUp MouseUp
                    ]
        , update = update
        }



-- INIT


generateBox : Int -> Box
generateBox i =
    { pos =
        { x = toFloat (i % 15) * 24
        , y = toFloat (floor (toFloat i / 15) * 24)
        }
    , color = toFloat i + 1
    , drag = Nothing
    }


generateBoxes : List Box
generateBoxes =
    Array.initialize 225 identity
        |> Array.toList
        |> List.map generateBox


init : ( Model, Cmd Msg )
init =
    ( { boxes = generateBoxes
      , hover = False
      }
    , Cmd.none
    )



-- MESH


type alias Vertex =
    { coordinate : Vec3
    , color : Vec3
    }


box : Float -> Float -> Float -> Float -> List ( Vertex, Vertex, Vertex )
box size x y color =
    [ ( Vertex (vec3 (x + size) y 0) (vec3 (color / 255) 0 0)
      , Vertex (vec3 x y 0) (vec3 (color / 255) 0 0)
      , Vertex (vec3 x (y + size) 0) (vec3 (color / 255) 0 0)
      )
    , ( Vertex (vec3 x (y + size) 0) (vec3 (color / 255) 0 0)
      , Vertex (vec3 (x + size) y 0) (vec3 (color / 255) 0 0)
      , Vertex (vec3 (x + size) (y + size) 0) (vec3 (color / 255) 0 0)
      )
    ]


getPosition : Box -> Position
getPosition { pos, drag } =
    case drag of
        Nothing ->
            pos

        Just { start, current } ->
            Position (pos.x + current.x - start.x)
                (pos.y + current.y - start.y)


render : Box -> List ( Vertex, Vertex, Vertex )
render b =
    let
        p =
            getPosition b
    in
        box 20 p.x p.y b.color


mesh : Model -> WebGL.Drawable Vertex
mesh model =
    List.map render model.boxes
        |> List.concat
        |> WebGL.Triangle



-- MODEL


type alias Model =
    { boxes : List Box
    , hover : Bool
    }


type alias Box =
    { pos : Position
    , color : Float
    , drag : Maybe Drag
    }


type alias Position =
    { x : Float
    , y : Float
    }


type alias Drag =
    { start : Position
    , current : Position
    }



-- UPDATE


type Msg
    = MouseMove Port.MouseUpdate
    | MouseDown Port.MouseUpdate
    | MouseUp Port.MouseUpdate


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( updateModel msg model, Cmd.none )


updateModel : Msg -> Model -> Model
updateModel msg model =
    case msg of
        MouseMove s ->
            { model
                | hover = s.color /= 0
                , boxes = List.map (updateBox s) model.boxes
            }

        MouseDown s ->
            { model
                | boxes =
                    List.map
                        (\b ->
                            if b.color == s.color then
                                let
                                    pos =
                                        { x = s.x, y = s.y }
                                in
                                    { b | drag = Just { start = pos, current = pos } }
                            else
                                b
                        )
                        model.boxes
            }

        MouseUp s ->
            { model | boxes = List.map (finishDrag s) model.boxes }


updateDrag : Port.MouseUpdate -> Drag -> Drag
updateDrag s drag =
    { drag | current = { x = s.x, y = s.y } }


updateBox : Port.MouseUpdate -> Box -> Box
updateBox s box =
    { box | drag = Maybe.map (updateDrag s) box.drag }


finishDrag : Port.MouseUpdate -> Box -> Box
finishDrag s box =
    { box | pos = getPosition box, drag = Nothing }



-- VIEW


view : Model -> Html Msg
view model =
    WebGL.toHtml
        [ width w
        , height h
        , style
            [ ( "cursor"
              , if model.hover then
                    "pointer"
                else
                    "default"
              )
            ]
        ]
        [ WebGL.render vertexShader
            fragmentShader
            (mesh model)
            { perspective = makeOrtho2D 0.0 w h 0.0 }
        ]


vertexShader : WebGL.Shader { attr | coordinate : Vec3, color : Vec3 } { unif | perspective : Mat4 } { vcolor : Vec3 }
vertexShader =
    [glsl|
attribute vec3 coordinate;
attribute vec3 color;
uniform mat4 perspective;
varying vec3 vcolor;

void main() {
    gl_Position = perspective * vec4(coordinate, 1.0);
    vcolor = color;
}
|]


fragmentShader : WebGL.Shader {} u { vcolor : Vec3 }
fragmentShader =
    [glsl|
precision mediump float;
varying vec3 vcolor;
void main () {
    gl_FragColor = vec4(vcolor, 1);
}
|]
