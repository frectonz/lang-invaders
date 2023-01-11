module Object exposing (..)

import Canvas exposing (Renderable, rect, shapes)
import Canvas.Settings exposing (fill)
import Color exposing (Color)
import Vector exposing (Vector)


type Object
    = Object { pos : Vector, vel : Vector, width : Float, height : Float, visible : Bool }


make : Vector -> Vector -> Float -> Float -> Object
make pos vel width height =
    Object { pos = pos, vel = vel, width = width, height = height, visible = True }


withWidthAndHeight : Vector -> Float -> Float -> Object
withWidthAndHeight vel w h =
    Object { pos = Vector.make 0 0, vel = vel, width = w, height = h, visible = True }


overlaps : Object -> Object -> Bool
overlaps (Object r1) (Object r2) =
    (Vector.getX r1.pos < (Vector.getX r2.pos + r2.width))
        && ((Vector.getX r1.pos + r1.width) > Vector.getX r2.pos)
        && (Vector.getY r1.pos < (Vector.getY r2.pos + r2.height))
        && ((Vector.getY r1.pos + r1.height) > Vector.getY r2.pos)


contains : Object -> Object -> Bool
contains (Object r1) (Object r2) =
    (Vector.getX r1.pos < Vector.getX r2.pos)
        && ((Vector.getX r1.pos + r1.width) > (Vector.getX r2.pos + r2.width))
        && (Vector.getY r1.pos < Vector.getY r2.pos)
        && ((Vector.getY r1.pos + r1.height) > (Vector.getY r2.pos + r2.height))


updateY : (Float -> Float) -> Object -> Object
updateY f (Object r) =
    Object { r | pos = Vector.setY (f (Vector.getY r.pos)) r.pos }


withPos : Vector -> Object -> Object
withPos pos (Object r) =
    Object { r | pos = pos }


moveLeft : Object -> Object
moveLeft (Object r) =
    Object { r | pos = Vector.substractX (Vector.getX r.vel) r.pos }


moveRight : Object -> Object
moveRight (Object r) =
    Object { r | pos = Vector.addX (Vector.getX r.vel) r.pos }


moveUp : Object -> Object
moveUp (Object r) =
    Object { r | pos = Vector.substractY (Vector.getY r.vel) r.pos }


clampX : Float -> Float -> Object -> Object
clampX min max (Object r) =
    Object { r | pos = Vector.clampX min (max - r.width) r.pos }


midpointX : Object -> Float
midpointX (Object r) =
    Vector.getX r.pos + (r.width / 2)


update : Object -> Object
update (Object r) =
    Object { r | pos = Vector.add r.vel r.pos }


flipX : Object -> Object
flipX (Object r) =
    Object { r | vel = Vector.flipX r.vel }


getX : Object -> Float
getX (Object r) =
    Vector.getX r.pos


getY : Object -> Float
getY (Object r) =
    Vector.getY r.pos


kill : Object -> Object
kill (Object r) =
    Object { r | visible = False }


isVisible : Object -> Bool
isVisible (Object r) =
    r.visible


view : Color -> Object -> Renderable
view fillColor (Object o) =
    shapes
        [ fill
            (if o.visible then
                fillColor

             else
                Color.rgba 0 0 0 0
            )
        ]
        [ rect
            ( Vector.getX o.pos, Vector.getY o.pos )
            o.width
            o.height
        ]
