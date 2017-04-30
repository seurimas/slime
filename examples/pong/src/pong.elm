module Pong exposing (main)

{-|
@docs main
-}

import Dict exposing (Dict)
import Html exposing (Html)
import Color exposing (Color)
import Char exposing (toCode)
import AnimationFrame
import Char exposing (KeyCode)
import Keyboard.Extra
import Game.TwoD as Game
import Game.TwoD.Render as Render
import Game.TwoD.Camera as Camera exposing (Camera)
import Slime exposing (..)
import Slime.Engine exposing (System(..), Engine)


type alias Rect =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    , color : Color
    }


collide rect1 rect2 =
    rect1.x
        + rect1.width
        > rect2.x
        && rect1.x
        < rect2.x
        + rect2.width
        && rect1.y
        + rect1.height
        > rect2.y
        && rect1.y
        < rect2.y
        + rect2.height


type alias ControlSet =
    { up : KeyCode
    , down : KeyCode
    }


type alias Ball =
    { vx : Float, vy : Float }


type alias Paddle =
    { inputState : ( Bool, Bool ), inputScheme : ControlSet }


type alias World =
    EntitySet
        { transforms : ComponentSet Rect
        , balls : ComponentSet Ball
        , paddles : ComponentSet Paddle
        }


type alias Model =
    { world : World
    , keys : Keyboard.Extra.Model
    }


type Msg
    = Tick Float
    | Keys Keyboard.Extra.Msg


transforms : ComponentSpec Rect World
transforms =
    { getter = .transforms
    , setter = (\world -> \comps -> { world | transforms = comps })
    }


balls : ComponentSpec Ball World
balls =
    { getter = .balls
    , setter = (\world -> \comps -> { world | balls = comps })
    }


spawnBall : ( Float, Float, Float, Float ) -> World -> ( World, Int )
spawnBall ( x, y, vx, vy ) world =
    let
        rect =
            { x = x
            , y = y
            , width = 5
            , height = 5
            , color = Color.red
            }

        ball =
            { vx = vx
            , vy = vy
            }
    in
        spawnEntity2 transforms balls world { a = rect, b = ball }


paddles : ComponentSpec Paddle World
paddles =
    { getter = .paddles
    , setter = (\world -> \comps -> { world | paddles = comps })
    }


spawnPaddles world =
    let
        paddleSpawner =
            spawnEntity2 transforms paddles

        leftRect =
            { x = 10
            , y = 250
            , width = 10
            , height = 50
            , color = Color.green
            }

        rightRect =
            { x = 480
            , y = 250
            , width = 10
            , height = 50
            , color = Color.green
            }

        leftInput =
            { inputState = ( False, False )
            , inputScheme = { up = toCode 'W', down = toCode 'S' }
            }

        rightInput =
            { inputState = ( False, False )
            , inputScheme = { up = toCode 'I', down = toCode 'K' }
            }
    in
        paddleSpawner world { a = leftRect, b = leftInput }
            |> Tuple.first
            >> flip paddleSpawner { a = rightRect, b = rightInput }
            |> Tuple.first


engine : Engine World
engine =
    let
        deletor =
            deleteEntity transforms
                &-> balls
                &-> paddles

        systems =
            [ Time moveBalls
            , Basic keepBalls
            , Basic bounceBalls
            ]
    in
        Slime.Engine.initEngine deletor systems


world : World
world =
    { idSource = Slime.initIdSource
    , transforms = initComponents
    , balls = initComponents
    , paddles = initComponents
    }
        |> spawnBall ( 247.5, 247.5, 100, 80 )
        |> Tuple.first
        |> spawnBall ( 247.5, 247.5, 80, 100 )
        |> Tuple.first
        |> spawnBall ( 247.5, 247.5, -100, 80 )
        |> Tuple.first
        |> spawnBall ( 247.5, 247.5, -80, 100 )
        |> Tuple.first
        |> spawnBall ( 247.5, 247.5, 100, -80 )
        |> Tuple.first
        |> spawnBall ( 247.5, 247.5, 80, -100 )
        |> Tuple.first
        |> spawnBall ( 247.5, 247.5, -100, -80 )
        |> Tuple.first
        |> spawnBall ( 247.5, 247.5, -80, -100 )
        |> Tuple.first
        |> spawnBall ( 40, 230, -80, 100 )
        |> Tuple.first
        |> spawnPaddles


model : Model
model =
    { world = world
    , keys = Tuple.first Keyboard.Extra.init
    }


moveBalls : Float -> World -> World
moveBalls delta =
    let
        addVelocity ball transform delta =
            { transform
                | x = transform.x + ball.vx * delta
                , y = transform.y + ball.vy * delta
            }
    in
        stepEntities2 balls transforms (\ent2 -> { ent2 | b = addVelocity ent2.a ent2.b delta })


keepBalls : World -> World
keepBalls =
    let
        maybeBounce e2 =
            let
                ball =
                    e2.a

                transform =
                    e2.b
            in
                if transform.x + transform.width >= 500 && ball.vx > 0 then
                    { e2 | a = { ball | vx = -ball.vx }, b = { transform | x = 500 - transform.width } }
                else if transform.x <= 0 && ball.vx < 0 then
                    { e2 | a = { ball | vx = -ball.vx }, b = { transform | x = 0 } }
                else if transform.y + transform.height >= 500 && ball.vy > 0 then
                    { e2 | a = { ball | vy = -ball.vy }, b = { transform | y = 500 - transform.height } }
                else if transform.y <= 0 && ball.vy < 0 then
                    { e2 | a = { ball | vy = -ball.vy }, b = { transform | y = 0 } }
                else
                    e2
    in
        stepEntities2 balls transforms maybeBounce


bounceBalls : World -> World
bounceBalls =
    let
        bounce e1 e2 =
            let
                paddle =
                    e1.b

                ballVel =
                    e2.a

                ballPos =
                    e2.b
            in
                if collide ballPos paddle then
                    if (ballVel.vx < 0 && ballPos.x < paddle.x + paddle.width) then
                        { e2 | a = { ballVel | vx = -1 * ballVel.vx }, b = { ballPos | x = paddle.x + paddle.width } }
                    else if (ballVel.vx > 0 && ballPos.x + ballPos.width > paddle.x) then
                        { e2 | a = { ballVel | vx = -1 * ballVel.vx }, b = { ballPos | x = paddle.x - ballPos.width } }
                    else
                        e2
                else
                    e2

        paddleList =
            entities2 paddles transforms world
    in
        stepEntities2 balls transforms (\e2 -> List.foldr bounce e2 paddleList)


renderTransforms =
    Slime.map transforms
        (\transform ->
            Render.rectangle
                { color = transform.color
                , position = ( transform.x, transform.y )
                , size = ( transform.width, transform.height )
                }
        )


render { world } =
    Game.render
        { time = 0
        , camera = Camera.fixedHeight 500 ( 250, 250 )
        , size = ( 500, 500 )
        }
        (renderTransforms world)


subs m =
    Sub.batch
        [ Sub.map Keys Keyboard.Extra.subscriptions
        , AnimationFrame.diffs Tick
        ]


updateWorld =
    Slime.Engine.applySystems engine


update msg model =
    case msg of
        Tick delta ->
            let
                deltaMs =
                    delta / 1000
            in
                { model | world = updateWorld model.world deltaMs } ! []

        Keys kMsg ->
            let
                ( kModel, kCmd ) =
                    Keyboard.Extra.update kMsg model.keys
            in
                { model | keys = kModel } ! [ Cmd.map Keys kCmd ]


{-|
   The game
-}
main : Program Never Model Msg
main =
    Html.program
        { init = model ! []
        , subscriptions = subs
        , update = update
        , view = render
        }
