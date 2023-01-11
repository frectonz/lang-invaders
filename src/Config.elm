module Config exposing (..)

import Vector


enemiesGap =
    20


enemySize =
    50


rows =
    5


cols =
    11


enemiesGroupWidth =
    toFloat cols * (enemySize + enemiesGap)


playerWidth =
    100


playerHeight =
    30


missleWidth =
    10


missleHeight =
    30


scenePadding =
    100


playerVelocity =
    Vector.make 20 0


enemyVelocity =
    Vector.make 5 0


missleVelocity =
    Vector.make 0 15
