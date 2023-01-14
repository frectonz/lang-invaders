module Config exposing (..)

import Vector


enemiesGap : number
enemiesGap =
    20


enemySize : number
enemySize =
    50


rows : number
rows =
    5


cols : number
cols =
    11


enemiesGroupWidth : Float
enemiesGroupWidth =
    toFloat cols * (enemySize + enemiesGap)


playerWidth : number
playerWidth =
    100


playerHeight : number
playerHeight =
    30


missleWidth : number
missleWidth =
    35


missleHeight : number
missleHeight =
    40


scenePadding : { x : number, y : number }
scenePadding =
    { x = 800, y = 100 }


playerVelocity : Vector.Vector
playerVelocity =
    Vector.make 20 0


enemyVelocity : Vector.Vector
enemyVelocity =
    Vector.make 3 0


missleVelocity : Vector.Vector
missleVelocity =
    Vector.make 0 15
