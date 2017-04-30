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
import Keyboard
import Game.TwoD as Game
import Game.TwoD.Render as Render
import Game.TwoD.Camera as Camera exposing (Camera)
import Slime exposing (..)
import Slime.Engine exposing (System(..), Listener(..), Engine)


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


type alias Ball =
    { vx : Float, vy : Float }


type alias Paddle =
    { inputState : ( Bool, Bool )
    , inputScheme :
        { up : KeyCode
        , down : KeyCode
        }
    , speed : Float
    }


type alias Score =
    { x : Float
    , y : Float
    , color : Color
    , radius : Float
    , lifetime : Float
    , progress : Float
    }


type alias World =
    EntitySet
        { transforms : ComponentSet Rect
        , balls : ComponentSet Ball
        , paddles : ComponentSet Paddle
        , scores : ComponentSet Score
        , score : ( Int, Int )
        }


type alias Model =
    { world : World
    }


type Msg
    = Tick Float
    | KeyDown KeyCode
    | KeyUp KeyCode


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


scores : ComponentSpec Score World
scores =
    { getter = .scores
    , setter = (\world -> \comps -> { world | scores = comps })
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
            , speed = 200
            }

        rightInput =
            { inputState = ( False, False )
            , inputScheme = { up = toCode 'I', down = toCode 'K' }
            , speed = 200
            }
    in
        paddleSpawner world { a = leftRect, b = leftInput }
            |> Tuple.first
            >> flip paddleSpawner { a = rightRect, b = rightInput }
            |> Tuple.first


engine : Engine World Msg
engine =
    let
        deletor =
            deleteEntity transforms
                &-> balls
                &-> paddles
                &-> scores

        systems =
            [ Time moveBalls
            , Basic keepBalls
            , Deletes scoreBalls
            , Basic bounceBalls
            , Time movePaddles
            , TimeAndDeletes updateScores
            ]

        listeners =
            [ Listen setPaddleKeys
            ]
    in
        Slime.Engine.initEngine deletor systems listeners


world : World
world =
    { idSource = Slime.initIdSource
    , transforms = initComponents
    , balls = initComponents
    , paddles = initComponents
    , scores = initComponents
    , score = ( 0, 0 )
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


updateScores : Float -> World -> ( World, List EntityID )
updateScores deltaTime world =
    let
        updatedWorld =
            stepEntities scores (\score -> { score | progress = score.progress + deltaTime }) world
    in
        ( updatedWorld, List.filter (\e -> e.a.progress > e.a.lifetime) (entities scores updatedWorld) |> List.map .id )


scoreBalls : World -> ( World, List EntityID )
scoreBalls world =
    let
        isScored e2 =
            let
                ball =
                    e2.a

                transform =
                    e2.b
            in
                transform.x == 0 || transform.x == 500 - transform.width

        scoreMe e2 =
            let
                transform =
                    e2.b
            in
                { a =
                    { x = transform.x + transform.width / 2
                    , y = transform.y + transform.height / 2
                    , color = Color.blue
                    , radius = 50
                    , lifetime = 1
                    , progress = 0
                    }
                }

        newBall e2 =
            let
                ball =
                    e2.a

                transform =
                    e2.b
            in
                { a = ball
                , b = { transform | x = 247.5, y = 247.5 }
                }

        scoredBalls =
            entities2 balls transforms world
                |> List.filter isScored

        newScores =
            List.map scoreMe scoredBalls

        updatedScore =
            ( Tuple.first world.score + List.length (List.filter (\score -> score.a.x > 400) newScores)
            , Tuple.second world.score + List.length (List.filter (\score -> score.a.x < 100) newScores)
            )

        newBalls =
            List.map newBall scoredBalls

        ( updatedWorld1, _ ) =
            spawnEntities scores world newScores

        ( updatedWorld2, _ ) =
            spawnEntities2 balls transforms updatedWorld1 newBalls
    in
        ( { updatedWorld2 | score = updatedScore }, List.map .id scoredBalls )


updateKeyState key isDown paddle =
    if paddle.inputScheme.up == key then
        { paddle | inputState = ( isDown, Tuple.second paddle.inputState ) }
    else if paddle.inputScheme.down == key then
        { paddle | inputState = ( Tuple.first paddle.inputState, isDown ) }
    else
        paddle


setPaddleKeys : Msg -> World -> World
setPaddleKeys msg =
    case msg of
        KeyUp key ->
            stepEntities paddles (updateKeyState key False)

        KeyDown key ->
            stepEntities paddles (updateKeyState key True)

        _ ->
            identity


movement : ( Bool, Bool ) -> Float
movement ( up, down ) =
    if up && down then
        0
    else if up then
        1
    else if down then
        -1
    else
        0


movePaddles : Float -> World -> World
movePaddles delta =
    let
        movePaddle e2 =
            let
                paddle =
                    e2.a

                transform =
                    e2.b

                moved =
                    movement paddle.inputState

                newY =
                    (transform.y + delta * moved * paddle.speed)
                        |> clamp 0 450
            in
                if moved /= 0 then
                    { e2 | b = { transform | y = newY } }
                else
                    e2
    in
        stepEntities2 paddles transforms movePaddle


bounceBalls : World -> World
bounceBalls world =
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
        stepEntities2 balls transforms (\e2 -> List.foldr bounce e2 paddleList) world


renderTransforms =
    Slime.map transforms
        (\transform ->
            Render.shape Render.rectangle
                { color = transform.color
                , position = ( transform.x, transform.y )
                , size = ( transform.width, transform.height )
                }
        )


setAlpha : Color -> Float -> Color
setAlpha color alpha =
    let
        { red, green, blue } =
            Color.toRgb color
    in
        Color.rgba red green blue alpha


lerp : Color -> Color -> Float -> Color
lerp c1 c2 progress =
    let
        rgb1 =
            Color.toRgb c1

        rgb2 =
            Color.toRgb c2

        red =
            round (toFloat rgb1.red + toFloat (rgb2.red - rgb1.red) * progress * progress)

        green =
            round (toFloat rgb1.green + toFloat (rgb2.green - rgb1.green) * progress * progress)

        blue =
            round (toFloat rgb1.blue + toFloat (rgb2.blue - rgb1.blue) * progress * progress)
    in
        Color.rgb red green blue


renderScores =
    Slime.map scores
        (\score ->
            let
                scale =
                    clamp 0 1 (score.progress / score.lifetime)

                size =
                    score.radius * scale
            in
                Render.shape Render.circle
                    { color = lerp score.color Color.white scale
                    , position = ( score.x - size / 2, score.y - size / 2 )
                    , size = ( size, size )
                    }
        )


render { world } =
    Game.render
        { time = 0
        , camera = Camera.fixedHeight 500 ( 250, 250 )
        , size = ( 500, 500 )
        }
        ((renderTransforms world)
            ++ (renderScores world)
        )


subs m =
    Sub.batch
        [ Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        , AnimationFrame.diffs Tick
        ]


updateWorld =
    Slime.Engine.applySystems engine


takeMessage =
    Slime.Engine.applyMessage engine


update msg model =
    case msg of
        Tick delta ->
            let
                deltaMs =
                    delta / 1000
            in
                { model | world = updateWorld model.world deltaMs } ! []

        _ ->
            { model | world = takeMessage model.world msg } ! []


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
