module Vector exposing (Vector, add, addX, clampX, flipX, getX, getY, make, setY, substractX, substractY, zero)


type Vector
    = Vector Float Float


zero : Vector
zero =
    Vector 0 0


make : Float -> Float -> Vector
make x y =
    Vector x y


getX : Vector -> Float
getX (Vector x _) =
    x


getY : Vector -> Float
getY (Vector _ y) =
    y


setY : Float -> Vector -> Vector
setY y (Vector x _) =
    Vector x y


substractX : Float -> Vector -> Vector
substractX x (Vector x1 y) =
    Vector (x1 - x) y


substractY : Float -> Vector -> Vector
substractY y (Vector x y1) =
    Vector x (y1 - y)


addX : Float -> Vector -> Vector
addX x (Vector x1 y) =
    Vector (x1 + x) y


clampX : Float -> Float -> Vector -> Vector
clampX min max (Vector x y) =
    Vector (clamp min max x) y


flipX : Vector -> Vector
flipX (Vector x y) =
    Vector -x y


add : Vector -> Vector -> Vector
add (Vector x1 y1) (Vector x2 y2) =
    Vector (x1 + x2) (y1 + y2)


clamp : Float -> Float -> Float -> Float
clamp min max value =
    if value < min then
        min

    else if value > max then
        max

    else
        value
