module ElmCoreDependency exposing (parsed)

import Elm.Docs
import Elm.Project
import Json.Decode


parsed :
    Maybe
        { elmJson : Elm.Project.Project
        , modules : List Elm.Docs.Module
        }
parsed =
    Result.map2 (\elmJson modules -> { elmJson = elmJson, modules = modules })
        ("""{
    "type": "package",
    "name": "elm/core",
    "summary": "Elm's standard libraries",
    "license": "BSD-3-Clause",
    "version": "1.0.5",
    "exposed-modules": {
        "Primitives": [
            "Basics",
            "String",
            "Char",
            "Bitwise",
            "Tuple"
        ],
        "Collections": [
            "List",
            "Dict",
            "Set",
            "Array"
        ],
        "Error Handling": [
            "Maybe",
            "Result"
        ],
        "Debug": [
            "Debug"
        ],
        "Effects": [
            "Platform.Cmd",
            "Platform.Sub",
            "Platform",
            "Process",
            "Task"
        ]
    },
    "elm-version": "0.19.0 <= v < 0.20.0",
    "dependencies": {},
    "test-dependencies": {}
}"""
            |> Json.Decode.decodeString Elm.Project.decoder
        )
        ("""[{"name":"Array","comment":" Fast immutable arrays. The elements in an array must have the same type.

# Arrays
@docs Array

# Creation
@docs empty, initialize, repeat, fromList

# Query
@docs isEmpty, length, get

# Manipulate
@docs set, push, append, slice

# Lists
@docs toList, toIndexedList

# Transform
@docs map, indexedMap, foldl, foldr, filter
","unions":[{"name":"Array","comment":" Representation of fast immutable arrays. You can create arrays of integers
(`Array Int`) or strings (`Array String`) or any other type of value you can
dream up.
","args":["a"],"cases":[]}],"aliases":[],"values":[{"name":"append","comment":" Append two arrays to a new one.

    append (repeat 2 42) (repeat 3 81) == fromList [42,42,81,81,81]
","type":"Array.Array a -> Array.Array a -> Array.Array a"},{"name":"empty","comment":" Return an empty array.

    length empty == 0
","type":"Array.Array a"},{"name":"filter","comment":" Keep elements that pass the test.

    filter isEven (fromList [1,2,3,4,5,6]) == (fromList [2,4,6])
","type":"(a -> Basics.Bool) -> Array.Array a -> Array.Array a"},{"name":"foldl","comment":" Reduce an array from the left. Read `foldl` as fold from the left.

    foldl (::) [] (fromList [1,2,3]) == [3,2,1]
","type":"(a -> b -> b) -> b -> Array.Array a -> b"},{"name":"foldr","comment":" Reduce an array from the right. Read `foldr` as fold from the right.

    foldr (+) 0 (repeat 3 5) == 15
","type":"(a -> b -> b) -> b -> Array.Array a -> b"},{"name":"fromList","comment":" Create an array from a `List`.
","type":"List.List a -> Array.Array a"},{"name":"get","comment":" Return `Just` the element at the index or `Nothing` if the index is out of
range.

    get  0 (fromList [0,1,2]) == Just 0
    get  2 (fromList [0,1,2]) == Just 2
    get  5 (fromList [0,1,2]) == Nothing
    get -1 (fromList [0,1,2]) == Nothing
","type":"Basics.Int -> Array.Array a -> Maybe.Maybe a"},{"name":"indexedMap","comment":" Apply a function on every element with its index as first argument.

    indexedMap (*) (fromList [5,5,5]) == fromList [0,5,10]
","type":"(Basics.Int -> a -> b) -> Array.Array a -> Array.Array b"},{"name":"initialize","comment":" Initialize an array. `initialize n f` creates an array of length `n` with
the element at index `i` initialized to the result of `(f i)`.

    initialize 4 identity    == fromList [0,1,2,3]
    initialize 4 (\\n -> n*n) == fromList [0,1,4,9]
    initialize 4 (always 0)  == fromList [0,0,0,0]
","type":"Basics.Int -> (Basics.Int -> a) -> Array.Array a"},{"name":"isEmpty","comment":" Determine if an array is empty.

    isEmpty empty == True
","type":"Array.Array a -> Basics.Bool"},{"name":"length","comment":" Return the length of an array.

    length (fromList [1,2,3]) == 3
","type":"Array.Array a -> Basics.Int"},{"name":"map","comment":" Apply a function on every element in an array.

    map sqrt (fromList [1,4,9]) == fromList [1,2,3]
","type":"(a -> b) -> Array.Array a -> Array.Array b"},{"name":"push","comment":" Push an element onto the end of an array.

    push 3 (fromList [1,2]) == fromList [1,2,3]
","type":"a -> Array.Array a -> Array.Array a"},{"name":"repeat","comment":" Creates an array with a given length, filled with a default element.

    repeat 5 0     == fromList [0,0,0,0,0]
    repeat 3 "cat" == fromList ["cat","cat","cat"]

Notice that `repeat 3 x` is the same as `initialize 3 (always x)`.
","type":"Basics.Int -> a -> Array.Array a"},{"name":"set","comment":" Set the element at a particular index. Returns an updated array.
If the index is out of range, the array is unaltered.

    set 1 7 (fromList [1,2,3]) == fromList [1,7,3]
","type":"Basics.Int -> a -> Array.Array a -> Array.Array a"},{"name":"slice","comment":" Get a sub-section of an array: `(slice start end array)`. The `start` is a
zero-based index where we will start our slice. The `end` is a zero-based index
that indicates the end of the slice. The slice extracts up to but not including
`end`.

    slice  0  3 (fromList [0,1,2,3,4]) == fromList [0,1,2]
    slice  1  4 (fromList [0,1,2,3,4]) == fromList [1,2,3]

Both the `start` and `end` indexes can be negative, indicating an offset from
the end of the array.

    slice  1 -1 (fromList [0,1,2,3,4]) == fromList [1,2,3]
    slice -2  5 (fromList [0,1,2,3,4]) == fromList [3,4]

This makes it pretty easy to `pop` the last element off of an array:
`slice 0 -1 array`
","type":"Basics.Int -> Basics.Int -> Array.Array a -> Array.Array a"},{"name":"toIndexedList","comment":" Create an indexed list from an array. Each element of the array will be
paired with its index.

    toIndexedList (fromList ["cat","dog"]) == [(0,"cat"), (1,"dog")]
","type":"Array.Array a -> List.List ( Basics.Int, a )"},{"name":"toList","comment":" Create a list of elements from an array.

    toList (fromList [3,5,8]) == [3,5,8]
","type":"Array.Array a -> List.List a"}],"binops":[]},{"name":"Basics","comment":" Tons of useful functions that get imported by default.

# Math
@docs Int, Float, (+), (-), (*), (/), (//), (^)

# Int to Float / Float to Int
@docs toFloat, round, floor, ceiling, truncate

# Equality
@docs (==), (/=)

# Comparison

These functions only work on `comparable` types. This includes numbers,
characters, strings, lists of comparable things, and tuples of comparable
things.

@docs (<), (>), (<=), (>=), max, min, compare, Order

# Booleans
@docs Bool, not, (&&), (||), xor

# Append Strings and Lists
@docs (++)

# Fancier Math
@docs modBy, remainderBy, negate, abs, clamp, sqrt, logBase, e

# Angles
@docs degrees, radians, turns

# Trigonometry
@docs pi, cos, sin, tan, acos, asin, atan, atan2

# Polar Coordinates
@docs toPolar, fromPolar

# Floating Point Checks
@docs isNaN, isInfinite

# Function Helpers
@docs identity, always, (<|), (|>), (<<), (>>), Never, never

","unions":[{"name":"Bool","comment":" A “Boolean” value. It can either be `True` or `False`.

**Note:** Programmers coming from JavaScript, Java, etc. tend to reach for
boolean values way too often in Elm. Using a [union type][ut] is often clearer
and more reliable. You can learn more about this from Jeremy [here][jf] or
from Richard [here][rt].

[ut]: https://guide.elm-lang.org/types/union_types.html
[jf]: https://youtu.be/6TDKHGtAxeg?t=1m25s
[rt]: https://youtu.be/IcgmSRJHu_8?t=1m14s
","args":[],"cases":[["True",[]],["False",[]]]},{"name":"Float","comment":" A `Float` is a [floating-point number][fp]. Valid syntax for floats includes:

    0
    42
    3.14
    0.1234
    6.022e23   -- == (6.022 * 10^23)
    6.022e+23  -- == (6.022 * 10^23)
    1.602e−19  -- == (1.602 * 10^-19)
    1e3        -- == (1 * 10^3) == 1000

**Historical Note:** The particular details of floats (e.g. `NaN`) are
specified by [IEEE 754][ieee] which is literally hard-coded into almost all
CPUs in the world. That means if you think `NaN` is weird, you must
successfully overtake Intel and AMD with a chip that is not backwards
compatible with any widely-used assembly language.

[fp]: https://en.wikipedia.org/wiki/Floating-point_arithmetic
[ieee]: https://en.wikipedia.org/wiki/IEEE_754
","args":[],"cases":[]},{"name":"Int","comment":" An `Int` is a whole number. Valid syntax for integers includes:

    0
    42
    9000
    0xFF   -- 255 in hexadecimal
    0x000A --  10 in hexadecimal

**Note:** `Int` math is well-defined in the range `-2^31` to `2^31 - 1`. Outside
of that range, the behavior is determined by the compilation target. When
generating JavaScript, the safe range expands to `-2^53` to `2^53 - 1` for some
operations, but if we generate WebAssembly some day, we would do the traditional
[integer overflow][io]. This quirk is necessary to get good performance on
quirky compilation targets.

**Historical Note:** The name `Int` comes from the term [integer][]. It appears
that the `int` abbreviation was introduced in [ALGOL 68][68], shortening it
from `integer` in [ALGOL 60][60]. Today, almost all programming languages use
this abbreviation.

[io]: https://en.wikipedia.org/wiki/Integer_overflow
[integer]: https://en.wikipedia.org/wiki/Integer
[60]: https://en.wikipedia.org/wiki/ALGOL_60
[68]: https://en.wikipedia.org/wiki/ALGOL_68
","args":[],"cases":[]},{"name":"Never","comment":" A value that can never happen! For context:

  - The boolean type `Bool` has two values: `True` and `False`
  - The unit type `()` has one value: `()`
  - The never type `Never` has no values!

You may see it in the wild in `Html Never` which means this HTML will never
produce any messages. You would need to write an event handler like
`onClick ??? : Attribute Never` but how can we fill in the question marks?!
So there cannot be any event handlers on that HTML.

You may also see this used with tasks that never fail, like `Task Never ()`.

The `Never` type is useful for restricting *arguments* to a function. Maybe my
API can only accept HTML without event handlers, so I require `Html Never` and
users can give `Html msg` and everything will go fine. Generally speaking, you
do not want `Never` in your return types though.
","args":[],"cases":[]},{"name":"Order","comment":" Represents the relative ordering of two things.
The relations are less than, equal to, and greater than.
","args":[],"cases":[["LT",[]],["EQ",[]],["GT",[]]]}],"aliases":[],"values":[{"name":"abs","comment":" Get the [absolute value][abs] of a number.

    abs 16   == 16
    abs -4   == 4
    abs -8.5 == 8.5
    abs 3.14 == 3.14

[abs]: https://en.wikipedia.org/wiki/Absolute_value
","type":"number -> number"},{"name":"acos","comment":" Figure out the arccosine for `adjacent / hypotenuse` in radians:

    acos (1/2) == 1.0471975511965979 -- 60° or pi/3 radians

","type":"Basics.Float -> Basics.Float"},{"name":"always","comment":" Create a function that *always* returns the same value. Useful with
functions like `map`:

    List.map (always 0) [1,2,3,4,5] == [0,0,0,0,0]

    -- List.map (\\_ -> 0) [1,2,3,4,5] == [0,0,0,0,0]
    -- always = (\\x _ -> x)
","type":"a -> b -> a"},{"name":"asin","comment":" Figure out the arcsine for `opposite / hypotenuse` in radians:

    asin (1/2) == 0.5235987755982989 -- 30° or pi/6 radians

","type":"Basics.Float -> Basics.Float"},{"name":"atan","comment":" This helps you find the angle (in radians) to an `(x,y)` coordinate, but
in a way that is rarely useful in programming. **You probably want
[`atan2`](#atan2) instead!**

This version takes `y/x` as its argument, so there is no way to know whether
the negative signs comes from the `y` or `x` value. So as we go counter-clockwise
around the origin from point `(1,1)` to `(1,-1)` to `(-1,-1)` to `(-1,1)` we do
not get angles that go in the full circle:

    atan (  1 /  1 ) ==  0.7853981633974483 --  45° or   pi/4 radians
    atan (  1 / -1 ) == -0.7853981633974483 -- 315° or 7*pi/4 radians
    atan ( -1 / -1 ) ==  0.7853981633974483 --  45° or   pi/4 radians
    atan ( -1 /  1 ) == -0.7853981633974483 -- 315° or 7*pi/4 radians

Notice that everything is between `pi/2` and `-pi/2`. That is pretty useless
for figuring out angles in any sort of visualization, so again, check out
[`atan2`](#atan2) instead!
","type":"Basics.Float -> Basics.Float"},{"name":"atan2","comment":" This helps you find the angle (in radians) to an `(x,y)` coordinate.
So rather than saying `atan (y/x)` you say `atan2 y x` and you can get a full
range of angles:

    atan2  1  1 ==  0.7853981633974483 --  45° or   pi/4 radians
    atan2  1 -1 ==  2.356194490192345  -- 135° or 3*pi/4 radians
    atan2 -1 -1 == -2.356194490192345  -- 225° or 5*pi/4 radians
    atan2 -1  1 == -0.7853981633974483 -- 315° or 7*pi/4 radians

","type":"Basics.Float -> Basics.Float -> Basics.Float"},{"name":"ceiling","comment":" Ceiling function, rounding up.

    ceiling 1.0 == 1
    ceiling 1.2 == 2
    ceiling 1.5 == 2
    ceiling 1.8 == 2

    ceiling -1.2 == -1
    ceiling -1.5 == -1
    ceiling -1.8 == -1
","type":"Basics.Float -> Basics.Int"},{"name":"clamp","comment":" Clamps a number within a given range. With the expression
`clamp 100 200 x` the results are as follows:

    100     if x < 100
     x      if 100 <= x < 200
    200     if 200 <= x
","type":"number -> number -> number -> number"},{"name":"compare","comment":" Compare any two comparable values. Comparable values include `String`,
`Char`, `Int`, `Float`, or a list or tuple containing comparable values. These
are also the only values that work as `Dict` keys or `Set` members.

    compare 3 4 == LT
    compare 4 4 == EQ
    compare 5 4 == GT
","type":"comparable -> comparable -> Basics.Order"},{"name":"cos","comment":" Figure out the cosine given an angle in radians.

    cos (degrees 60)     == 0.5000000000000001
    cos (turns (1/6))    == 0.5000000000000001
    cos (radians (pi/3)) == 0.5000000000000001
    cos (pi/3)           == 0.5000000000000001

","type":"Basics.Float -> Basics.Float"},{"name":"degrees","comment":" Convert degrees to standard Elm angles (radians).

    degrees 180 == 3.141592653589793
","type":"Basics.Float -> Basics.Float"},{"name":"e","comment":" An approximation of e.
","type":"Basics.Float"},{"name":"floor","comment":" Floor function, rounding down.

    floor 1.0 == 1
    floor 1.2 == 1
    floor 1.5 == 1
    floor 1.8 == 1

    floor -1.2 == -2
    floor -1.5 == -2
    floor -1.8 == -2
","type":"Basics.Float -> Basics.Int"},{"name":"fromPolar","comment":" Convert polar coordinates (r,&theta;) to Cartesian coordinates (x,y).

    fromPolar (sqrt 2, degrees 45) == (1, 1)
","type":"( Basics.Float, Basics.Float ) -> ( Basics.Float, Basics.Float )"},{"name":"identity","comment":" Given a value, returns exactly the same value. This is called
[the identity function](https://en.wikipedia.org/wiki/Identity_function).
","type":"a -> a"},{"name":"isInfinite","comment":" Determine whether a float is positive or negative infinity.

    isInfinite (0/0)     == False
    isInfinite (sqrt -1) == False
    isInfinite (1/0)     == True
    isInfinite 1         == False

Notice that NaN is not infinite! For float `n` to be finite implies that
`not (isInfinite n || isNaN n)` evaluates to `True`.
","type":"Basics.Float -> Basics.Bool"},{"name":"isNaN","comment":" Determine whether a float is an undefined or unrepresentable number.
NaN stands for *not a number* and it is [a standardized part of floating point
numbers](https://en.wikipedia.org/wiki/NaN).

    isNaN (0/0)     == True
    isNaN (sqrt -1) == True
    isNaN (1/0)     == False  -- infinity is a number
    isNaN 1         == False
","type":"Basics.Float -> Basics.Bool"},{"name":"logBase","comment":" Calculate the logarithm of a number with a given base.

    logBase 10 100 == 2
    logBase 2 256 == 8
","type":"Basics.Float -> Basics.Float -> Basics.Float"},{"name":"max","comment":" Find the larger of two comparables.

    max 42 12345678 == 12345678
    max "abc" "xyz" == "xyz"
","type":"comparable -> comparable -> comparable"},{"name":"min","comment":" Find the smaller of two comparables.

    min 42 12345678 == 42
    min "abc" "xyz" == "abc"
","type":"comparable -> comparable -> comparable"},{"name":"modBy","comment":" Perform [modular arithmetic](https://en.wikipedia.org/wiki/Modular_arithmetic).
A common trick is to use (n mod 2) to detect even and odd numbers:

    modBy 2 0 == 0
    modBy 2 1 == 1
    modBy 2 2 == 0
    modBy 2 3 == 1

Our `modBy` function works in the typical mathematical way when you run into
negative numbers:

    List.map (modBy 4) [ -5, -4, -3, -2, -1,  0,  1,  2,  3,  4,  5 ]
    --                 [  3,  0,  1,  2,  3,  0,  1,  2,  3,  0,  1 ]

Use [`remainderBy`](#remainderBy) for a different treatment of negative numbers,
or read Daan Leijen’s [Division and Modulus for Computer Scientists][dm] for more
information.

[dm]: https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
","type":"Basics.Int -> Basics.Int -> Basics.Int"},{"name":"negate","comment":" Negate a number.

    negate 42 == -42
    negate -42 == 42
    negate 0 == 0
","type":"number -> number"},{"name":"never","comment":" A function that can never be called. Seems extremely pointless, but it
*can* come in handy. Imagine you have some HTML that should never produce any
messages. And say you want to use it in some other HTML that *does* produce
messages. You could say:

    import Html exposing (..)

    embedHtml : Html Never -> Html msg
    embedHtml staticStuff =
      div []
        [ text "hello"
        , Html.map never staticStuff
        ]

So the `never` function is basically telling the type system, make sure no one
ever calls me!
","type":"Basics.Never -> a"},{"name":"not","comment":" Negate a boolean value.

    not True == False
    not False == True
","type":"Basics.Bool -> Basics.Bool"},{"name":"pi","comment":" An approximation of pi.
","type":"Basics.Float"},{"name":"radians","comment":" Convert radians to standard Elm angles (radians).

    radians pi == 3.141592653589793
","type":"Basics.Float -> Basics.Float"},{"name":"remainderBy","comment":" Get the remainder after division. Here are bunch of examples of dividing by four:

    List.map (remainderBy 4) [ -5, -4, -3, -2, -1,  0,  1,  2,  3,  4,  5 ]
    --                       [ -1,  0, -3, -2, -1,  0,  1,  2,  3,  0,  1 ]

Use [`modBy`](#modBy) for a different treatment of negative numbers,
or read Daan Leijen’s [Division and Modulus for Computer Scientists][dm] for more
information.

[dm]: https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
","type":"Basics.Int -> Basics.Int -> Basics.Int"},{"name":"round","comment":" Round a number to the nearest integer.

    round 1.0 == 1
    round 1.2 == 1
    round 1.5 == 2
    round 1.8 == 2

    round -1.2 == -1
    round -1.5 == -1
    round -1.8 == -2
","type":"Basics.Float -> Basics.Int"},{"name":"sin","comment":" Figure out the sine given an angle in radians.

    sin (degrees 30)     == 0.49999999999999994
    sin (turns (1/12))   == 0.49999999999999994
    sin (radians (pi/6)) == 0.49999999999999994
    sin (pi/6)           == 0.49999999999999994

","type":"Basics.Float -> Basics.Float"},{"name":"sqrt","comment":" Take the square root of a number.

    sqrt  4 == 2
    sqrt  9 == 3
    sqrt 16 == 4
    sqrt 25 == 5
","type":"Basics.Float -> Basics.Float"},{"name":"tan","comment":" Figure out the tangent given an angle in radians.

    tan (degrees 45)     == 0.9999999999999999
    tan (turns (1/8))    == 0.9999999999999999
    tan (radians (pi/4)) == 0.9999999999999999
    tan (pi/4)           == 0.9999999999999999
","type":"Basics.Float -> Basics.Float"},{"name":"toFloat","comment":" Convert an integer into a float. Useful when mixing `Int` and `Float`
values like this:

    halfOf : Int -> Float
    halfOf number =
      toFloat number / 2

","type":"Basics.Int -> Basics.Float"},{"name":"toPolar","comment":" Convert Cartesian coordinates (x,y) to polar coordinates (r,&theta;).

    toPolar (3, 4) == ( 5, 0.9272952180016122)
    toPolar (5,12) == (13, 1.1760052070951352)
","type":"( Basics.Float, Basics.Float ) -> ( Basics.Float, Basics.Float )"},{"name":"truncate","comment":" Truncate a number, rounding towards zero.

    truncate 1.0 == 1
    truncate 1.2 == 1
    truncate 1.5 == 1
    truncate 1.8 == 1

    truncate -1.2 == -1
    truncate -1.5 == -1
    truncate -1.8 == -1
","type":"Basics.Float -> Basics.Int"},{"name":"turns","comment":" Convert turns to standard Elm angles (radians). One turn is equal to 360°.

    turns (1/2) == 3.141592653589793
","type":"Basics.Float -> Basics.Float"},{"name":"xor","comment":" The exclusive-or operator. `True` if exactly one input is `True`.

    xor True  True  == False
    xor True  False == True
    xor False True  == True
    xor False False == False
","type":"Basics.Bool -> Basics.Bool -> Basics.Bool"}],"binops":[{"name":"&&","comment":" The logical AND operator. `True` if both inputs are `True`.

    True  && True  == True
    True  && False == False
    False && True  == False
    False && False == False

**Note:** When used in the infix position, like `(left && right)`, the operator
short-circuits. This means if `left` is `False` we do not bother evaluating `right`
and just return `False` overall.
","type":"Basics.Bool -> Basics.Bool -> Basics.Bool","associativity":"right","precedence":3},{"name":"*","comment":" Multiply numbers like `2 * 3 == 6`.

See [`(+)`](#+) for docs on the `number` type variable.
","type":"number -> number -> number","associativity":"left","precedence":7},{"name":"+","comment":" Add two numbers. The `number` type variable means this operation can be
specialized to `Int -> Int -> Int` or to `Float -> Float -> Float`. So you
can do things like this:

    3002 + 4004 == 7006  -- all ints
    3.14 + 3.14 == 6.28  -- all floats

You _cannot_ add an `Int` and a `Float` directly though. Use functions like
[toFloat](#toFloat) or [round](#round) to convert both values to the same type.
So if you needed to add a list length to a `Float` for some reason, you
could say one of these:

    3.14 + toFloat (List.length [1,2,3]) == 6.14
    round 3.14 + List.length [1,2,3]     == 6

**Note:** Languages like Java and JavaScript automatically convert `Int` values
to `Float` values when you mix and match. This can make it difficult to be sure
exactly what type of number you are dealing with. When you try to _infer_ these
conversions (as Scala does) it can be even more confusing. Elm has opted for a
design that makes all conversions explicit.
","type":"number -> number -> number","associativity":"left","precedence":6},{"name":"++","comment":" Put two appendable things together. This includes strings and lists.

    "hello" ++ "world" == "helloworld"
    [1,1,2] ++ [3,5,8] == [1,1,2,3,5,8]
","type":"appendable -> appendable -> appendable","associativity":"right","precedence":5},{"name":"-","comment":" Subtract numbers like `4 - 3 == 1`.

See [`(+)`](#+) for docs on the `number` type variable.
","type":"number -> number -> number","associativity":"left","precedence":6},{"name":"/","comment":" Floating-point division:

    10 / 4 == 2.5
    11 / 4 == 2.75
    12 / 4 == 3
    13 / 4 == 3.25
    14 / 4 == 3.5

    -1 / 4 == -0.25
    -5 / 4 == -1.25

","type":"Basics.Float -> Basics.Float -> Basics.Float","associativity":"left","precedence":7},{"name":"//","comment":" Integer division:

    10 // 4 == 2
    11 // 4 == 2
    12 // 4 == 3
    13 // 4 == 3
    14 // 4 == 3

    -1 // 4 == 0
    -5 // 4 == -1

Notice that the remainder is discarded, so `3 // 4` is giving output
similar to `truncate (3 / 4)`.

It may sometimes be useful to pair this with the [`remainderBy`](#remainderBy)
function.
","type":"Basics.Int -> Basics.Int -> Basics.Int","associativity":"left","precedence":7},{"name":"/=","comment":" Check if values are not &ldquo;the same&rdquo;.

So `(a /= b)` is the same as `(not (a == b))`.
","type":"a -> a -> Basics.Bool","associativity":"non","precedence":4},{"name":"<","comment":"","type":"comparable -> comparable -> Basics.Bool","associativity":"non","precedence":4},{"name":"<<","comment":" Function composition, passing results along in the suggested direction. For
example, the following code checks if the square root of a number is odd:

    not << isEven << sqrt

You can think of this operator as equivalent to the following:

    (g << f)  ==  (\\x -> g (f x))

So our example expands out to something like this:

    \\n -> not (isEven (sqrt n))
","type":"(b -> c) -> (a -> b) -> a -> c","associativity":"left","precedence":9},{"name":"<=","comment":"","type":"comparable -> comparable -> Basics.Bool","associativity":"non","precedence":4},{"name":"<|","comment":" Saying `f <| x` is exactly the same as `f x`.

It can help you avoid parentheses, which can be nice sometimes. Maybe you want
to apply a function to a `case` expression? That sort of thing.
","type":"(a -> b) -> a -> b","associativity":"right","precedence":0},{"name":"==","comment":" Check if values are &ldquo;the same&rdquo;.

**Note:** Elm uses structural equality on tuples, records, and user-defined
union types. This means the values `(3, 4)` and `(3, 4)` are definitely equal.
This is not true in languages like JavaScript that use reference equality on
objects.

**Note:** Do not use `(==)` with functions, JSON values from `elm/json`, or
regular expressions from `elm/regex`. It does not work. It will crash if
possible. With JSON values, decode to Elm values before doing any equality
checks!

Why is it like this? Equality in the Elm sense can be difficult or impossible
to compute. Proving that functions are the same is [undecidable][], and JSON
values can come in through ports and have functions, cycles, and new JS data
types that interact weirdly with our equality implementation. In a future
release, the compiler will detect when `(==)` is used with problematic types
and provide a helpful error message at compile time. This will require some
pretty serious infrastructure work, so the stopgap is to crash as quickly as
possible.

[undecidable]: https://en.wikipedia.org/wiki/Undecidable_problem
","type":"a -> a -> Basics.Bool","associativity":"non","precedence":4},{"name":">","comment":"","type":"comparable -> comparable -> Basics.Bool","associativity":"non","precedence":4},{"name":">=","comment":"","type":"comparable -> comparable -> Basics.Bool","associativity":"non","precedence":4},{"name":">>","comment":" Function composition, passing results along in the suggested direction. For
example, the following code checks if the square root of a number is odd:

    sqrt >> isEven >> not

","type":"(a -> b) -> (b -> c) -> a -> c","associativity":"right","precedence":9},{"name":"^","comment":" Exponentiation

    3^2 == 9
    3^3 == 27
","type":"number -> number -> number","associativity":"right","precedence":8},{"name":"|>","comment":" Saying `x |> f` is exactly the same as `f x`.

It is called the “pipe” operator because it lets you write “pipelined” code.
For example, say we have a `sanitize` function for turning user input into
integers:

    -- BEFORE
    sanitize : String -> Maybe Int
    sanitize input =
      String.toInt (String.trim input)

We can rewrite it like this:

    -- AFTER
    sanitize : String -> Maybe Int
    sanitize input =
      input
        |> String.trim
        |> String.toInt

Totally equivalent! I recommend trying to rewrite code that uses `x |> f`
into code like `f x` until there are no pipes left. That can help you build
your intuition.

**Note:** This can be overused! I think folks find it quite neat, but when you
have three or four steps, the code often gets clearer if you break out a
top-level helper function. Now the transformation has a name. The arguments are
named. It has a type annotation. It is much more self-documenting that way!
Testing the logic gets easier too. Nice side benefit!
","type":"a -> (a -> b) -> b","associativity":"left","precedence":0},{"name":"||","comment":" The logical OR operator. `True` if one or both inputs are `True`.

    True  || True  == True
    True  || False == True
    False || True  == True
    False || False == False

**Note:** When used in the infix position, like `(left || right)`, the operator
short-circuits. This means if `left` is `True` we do not bother evaluating `right`
and just return `True` overall.
","type":"Basics.Bool -> Basics.Bool -> Basics.Bool","associativity":"right","precedence":2}]},{"name":"Bitwise","comment":" Library for [bitwise operations](https://en.wikipedia.org/wiki/Bitwise_operation).

# Basic Operations
@docs and, or, xor, complement

# Bit Shifts
@docs shiftLeftBy, shiftRightBy, shiftRightZfBy
","unions":[],"aliases":[],"values":[{"name":"and","comment":" Bitwise AND
","type":"Basics.Int -> Basics.Int -> Basics.Int"},{"name":"complement","comment":" Flip each bit individually, often called bitwise NOT
","type":"Basics.Int -> Basics.Int"},{"name":"or","comment":" Bitwise OR
","type":"Basics.Int -> Basics.Int -> Basics.Int"},{"name":"shiftLeftBy","comment":" Shift bits to the left by a given offset, filling new bits with zeros.
This can be used to multiply numbers by powers of two.

    shiftLeftBy 1 5 == 10
    shiftLeftBy 5 1 == 32
","type":"Basics.Int -> Basics.Int -> Basics.Int"},{"name":"shiftRightBy","comment":" Shift bits to the right by a given offset, filling new bits with
whatever is the topmost bit. This can be used to divide numbers by powers of two.

    shiftRightBy 1  32 == 16
    shiftRightBy 2  32 == 8
    shiftRightBy 1 -32 == -16

This is called an [arithmetic right shift][ars], often written `>>`, and
sometimes called a sign-propagating right shift because it fills empty spots
with copies of the highest bit.

[ars]: https://en.wikipedia.org/wiki/Bitwise_operation#Arithmetic_shift
","type":"Basics.Int -> Basics.Int -> Basics.Int"},{"name":"shiftRightZfBy","comment":" Shift bits to the right by a given offset, filling new bits with zeros.

    shiftRightZfBy 1  32 == 16
    shiftRightZfBy 2  32 == 8
    shiftRightZfBy 1 -32 == 2147483632

This is called an [logical right shift][lrs], often written `>>>`, and
sometimes called a zero-fill right shift because it fills empty spots with
zeros.

[lrs]: https://en.wikipedia.org/wiki/Bitwise_operation#Logical_shift
","type":"Basics.Int -> Basics.Int -> Basics.Int"},{"name":"xor","comment":" Bitwise XOR
","type":"Basics.Int -> Basics.Int -> Basics.Int"}],"binops":[]},{"name":"Char","comment":" Functions for working with characters. Character literals are enclosed in
`'a'` pair of single quotes.

# Characters
@docs Char

# ASCII Letters
@docs isUpper, isLower, isAlpha, isAlphaNum

# Digits
@docs isDigit, isOctDigit, isHexDigit

# Conversion
@docs toUpper, toLower, toLocaleUpper, toLocaleLower

# Unicode Code Points
@docs toCode, fromCode
","unions":[{"name":"Char","comment":" A `Char` is a single [unicode][u] character:

    'a'
    '0'
    'Z'
    '?'
    '"'
    'Σ'
    '🙈'

    '\\t'
    '\\"'
    '\\''
    '\\u{1F648}' -- '🙈'

**Note 1:** You _cannot_ use single quotes around multiple characters like in
JavaScript. This is how we distinguish [`String`](String#String) and `Char`
values in syntax.

**Note 2:** You can use the unicode escapes from `\\u{0000}` to `\\u{10FFFF}` to
represent characters by their code point. You can also include the unicode
characters directly. Using the escapes can be better if you need one of the
many whitespace characters with different widths.

[u]: https://en.wikipedia.org/wiki/Unicode
","args":[],"cases":[]}],"aliases":[],"values":[{"name":"fromCode","comment":" Convert a Unicode [code point][cp] to a character.

    fromCode 65      == 'A'
    fromCode 66      == 'B'
    fromCode 0x6728  == '木'
    fromCode 0x1D306 == '𝌆'
    fromCode 0x1F603 == '😃'
    fromCode -1      == '�'

The full range of unicode is from `0` to `0x10FFFF`. With numbers outside that
range, you get [the replacement character][fffd].

[cp]: https://en.wikipedia.org/wiki/Code_point
[fffd]: https://en.wikipedia.org/wiki/Specials_(Unicode_block)#Replacement_character
","type":"Basics.Int -> Char.Char"},{"name":"isAlpha","comment":" Detect upper case and lower case ASCII characters.

    isAlpha 'a' == True
    isAlpha 'b' == True
    isAlpha 'E' == True
    isAlpha 'Y' == True

    isAlpha '0' == False
    isAlpha '-' == False
    isAlpha 'π' == False
","type":"Char.Char -> Basics.Bool"},{"name":"isAlphaNum","comment":" Detect upper case and lower case ASCII characters.

    isAlphaNum 'a' == True
    isAlphaNum 'b' == True
    isAlphaNum 'E' == True
    isAlphaNum 'Y' == True
    isAlphaNum '0' == True
    isAlphaNum '7' == True

    isAlphaNum '-' == False
    isAlphaNum 'π' == False
","type":"Char.Char -> Basics.Bool"},{"name":"isDigit","comment":" Detect digits `0123456789`

    isDigit '0' == True
    isDigit '1' == True
    ...
    isDigit '9' == True

    isDigit 'a' == False
    isDigit 'b' == False
    isDigit 'A' == False
","type":"Char.Char -> Basics.Bool"},{"name":"isHexDigit","comment":" Detect hexadecimal digits `0123456789abcdefABCDEF`
","type":"Char.Char -> Basics.Bool"},{"name":"isLower","comment":" Detect lower case ASCII characters.

    isLower 'a' == True
    isLower 'b' == True
    ...
    isLower 'z' == True

    isLower '0' == False
    isLower 'A' == False
    isLower '-' == False
    isLower 'π' == False
","type":"Char.Char -> Basics.Bool"},{"name":"isOctDigit","comment":" Detect octal digits `01234567`

    isOctDigit '0' == True
    isOctDigit '1' == True
    ...
    isOctDigit '7' == True

    isOctDigit '8' == False
    isOctDigit 'a' == False
    isOctDigit 'A' == False
","type":"Char.Char -> Basics.Bool"},{"name":"isUpper","comment":" Detect upper case ASCII characters.

    isUpper 'A' == True
    isUpper 'B' == True
    ...
    isUpper 'Z' == True

    isUpper '0' == False
    isUpper 'a' == False
    isUpper '-' == False
    isUpper 'Σ' == False
","type":"Char.Char -> Basics.Bool"},{"name":"toCode","comment":" Convert to the corresponding Unicode [code point][cp].

[cp]: https://en.wikipedia.org/wiki/Code_point

    toCode 'A' == 65
    toCode 'B' == 66
    toCode '木' == 0x6728
    toCode '𝌆' == 0x1D306
    toCode '😃' == 0x1F603
","type":"Char.Char -> Basics.Int"},{"name":"toLocaleLower","comment":" Convert to lower case, according to any locale-specific case mappings. ","type":"Char.Char -> Char.Char"},{"name":"toLocaleUpper","comment":" Convert to upper case, according to any locale-specific case mappings. ","type":"Char.Char -> Char.Char"},{"name":"toLower","comment":" Convert to lower case. ","type":"Char.Char -> Char.Char"},{"name":"toUpper","comment":" Convert to upper case. ","type":"Char.Char -> Char.Char"}],"binops":[]},{"name":"Debug","comment":" This module can be useful while _developing_ an application. It is not
available for use in packages or production.

# Debugging
@docs toString, log, todo
","unions":[],"aliases":[],"values":[{"name":"log","comment":" Log a tagged value on the developer console, and then return the value.

    1 + log "number" 1        -- equals 2, logs "number: 1"
    length (log "start" [])   -- equals 0, logs "start: []"

It is often possible to sprinkle this around to see if values are what you
expect. It is kind of old-school to do it this way, but it works!

**Note:** This is not available with `elm make --optimize` because (1) it
relies on `toString` which has the same restriction and (2) it is not a pure
function and would therefore have unpredictable behavior when paired with
compiler optimizations that move code around.

**Note:** If you want to create a terminal application that prints stuff out,
use ports for now. That will give you full access to reading and writing in the
terminal. We may have a package in Elm for this someday, but browser
applications are the primary focus of platform development for now.
","type":"String.String -> a -> a"},{"name":"toString","comment":" Turn any kind of value into a string.

    toString 42                == "42"
    toString [1,2]             == "[1,2]"
    toString ('a', "cat", 13)  == "('a', \\"cat\\", 13)"
    toString "he said, \\"hi\\"" == "\\"he said, \\\\\\"hi\\\\\\"\\""

Notice that with strings, this is not the `identity` function. It escapes
characters so if you say `Html.text (toString "he said, \\"hi\\"")` it will
show `"he said, \\"hi\\""` rather than `he said, "hi"`. This makes it nice
for viewing Elm data structures.

**Note:** This is not available with `elm make --optimize` which gets rid of
a bunch of runtime metadata. For example, it shortens record field names, and
we need that info to `toString` the value! As a consequence, packages cannot
use `toString` because they may be used in `--optimize` mode.
","type":"a -> String.String"},{"name":"todo","comment":" This is a placeholder for code that you will write later.

For example, if you are working with a large union type and have partially
completed a case expression, it may make sense to do this:

    type Entity = Ship | Fish | Captain | Seagull

    drawEntity entity =
      case entity of
        Ship ->
          ...

        Fish ->
          ...

        _ ->
          Debug.todo "handle Captain and Seagull"

The Elm compiler recognizes each `Debug.todo` so if you run into it, you get
an **uncatchable runtime exception** that includes the module name and line
number.

**Note:** This is not available with `elm make --optimize` or packages. The
idea is that a `todo` can be useful during development, but uncatchable runtime
exceptions should not appear in the resulting applications.

**Note:** For the equivalent of try/catch error handling in Elm, use modules
like [`Maybe`](#Maybe) and [`Result`](#Result) which guarantee that no error
goes unhandled!
","type":"String.String -> a"}],"binops":[]},{"name":"Dict","comment":" A dictionary mapping unique keys to values. The keys can be any comparable
type. This includes `Int`, `Float`, `Time`, `Char`, `String`, and tuples or
lists of comparable types.

Insert, remove, and query operations all take *O(log n)* time.

# Dictionaries
@docs Dict

# Build
@docs empty, singleton, insert, update, remove

# Query
@docs isEmpty, member, get, size

# Lists
@docs keys, values, toList, fromList

# Transform
@docs map, foldl, foldr, filter, partition

# Combine
@docs union, intersect, diff, merge

","unions":[{"name":"Dict","comment":" A dictionary of keys and values. So a `Dict String User` is a dictionary
that lets you look up a `String` (such as user names) and find the associated
`User`.

    import Dict exposing (Dict)

    users : Dict String User
    users =
      Dict.fromList
        [ ("Alice", User "Alice" 28 1.65)
        , ("Bob"  , User "Bob"   19 1.82)
        , ("Chuck", User "Chuck" 33 1.75)
        ]

    type alias User =
      { name : String
      , age : Int
      , height : Float
      }
","args":["k","v"],"cases":[]}],"aliases":[],"values":[{"name":"diff","comment":" Keep a key-value pair when its key does not appear in the second dictionary.
","type":"Dict.Dict comparable a -> Dict.Dict comparable b -> Dict.Dict comparable a"},{"name":"empty","comment":" Create an empty dictionary. ","type":"Dict.Dict k v"},{"name":"filter","comment":" Keep only the key-value pairs that pass the given test. ","type":"(comparable -> v -> Basics.Bool) -> Dict.Dict comparable v -> Dict.Dict comparable v"},{"name":"foldl","comment":" Fold over the key-value pairs in a dictionary from lowest key to highest key.

    import Dict exposing (Dict)

    getAges : Dict String User -> List String
    getAges users =
      Dict.foldl addAge [] users

    addAge : String -> User -> List String -> List String
    addAge _ user ages =
      user.age :: ages

    -- getAges users == [33,19,28]
","type":"(k -> v -> b -> b) -> b -> Dict.Dict k v -> b"},{"name":"foldr","comment":" Fold over the key-value pairs in a dictionary from highest key to lowest key.

    import Dict exposing (Dict)

    getAges : Dict String User -> List String
    getAges users =
      Dict.foldr addAge [] users

    addAge : String -> User -> List String -> List String
    addAge _ user ages =
      user.age :: ages

    -- getAges users == [28,19,33]
","type":"(k -> v -> b -> b) -> b -> Dict.Dict k v -> b"},{"name":"fromList","comment":" Convert an association list into a dictionary. ","type":"List.List ( comparable, v ) -> Dict.Dict comparable v"},{"name":"get","comment":" Get the value associated with a key. If the key is not found, return
`Nothing`. This is useful when you are not sure if a key will be in the
dictionary.

    animals = fromList [ ("Tom", Cat), ("Jerry", Mouse) ]

    get "Tom"   animals == Just Cat
    get "Jerry" animals == Just Mouse
    get "Spike" animals == Nothing

","type":"comparable -> Dict.Dict comparable v -> Maybe.Maybe v"},{"name":"insert","comment":" Insert a key-value pair into a dictionary. Replaces value when there is
a collision. ","type":"comparable -> v -> Dict.Dict comparable v -> Dict.Dict comparable v"},{"name":"intersect","comment":" Keep a key-value pair when its key appears in the second dictionary.
Preference is given to values in the first dictionary.
","type":"Dict.Dict comparable v -> Dict.Dict comparable v -> Dict.Dict comparable v"},{"name":"isEmpty","comment":" Determine if a dictionary is empty.

    isEmpty empty == True
","type":"Dict.Dict k v -> Basics.Bool"},{"name":"keys","comment":" Get all of the keys in a dictionary, sorted from lowest to highest.

    keys (fromList [(0,"Alice"),(1,"Bob")]) == [0,1]
","type":"Dict.Dict k v -> List.List k"},{"name":"map","comment":" Apply a function to all values in a dictionary.
","type":"(k -> a -> b) -> Dict.Dict k a -> Dict.Dict k b"},{"name":"member","comment":" Determine if a key is in a dictionary. ","type":"comparable -> Dict.Dict comparable v -> Basics.Bool"},{"name":"merge","comment":" The most general way of combining two dictionaries. You provide three
accumulators for when a given key appears:

  1. Only in the left dictionary.
  2. In both dictionaries.
  3. Only in the right dictionary.

You then traverse all the keys from lowest to highest, building up whatever
you want.
","type":"(comparable -> a -> result -> result) -> (comparable -> a -> b -> result -> result) -> (comparable -> b -> result -> result) -> Dict.Dict comparable a -> Dict.Dict comparable b -> result -> result"},{"name":"partition","comment":" Partition a dictionary according to some test. The first dictionary
contains all key-value pairs which passed the test, and the second contains
the pairs that did not.
","type":"(comparable -> v -> Basics.Bool) -> Dict.Dict comparable v -> ( Dict.Dict comparable v, Dict.Dict comparable v )"},{"name":"remove","comment":" Remove a key-value pair from a dictionary. If the key is not found,
no changes are made. ","type":"comparable -> Dict.Dict comparable v -> Dict.Dict comparable v"},{"name":"singleton","comment":" Create a dictionary with one key-value pair. ","type":"comparable -> v -> Dict.Dict comparable v"},{"name":"size","comment":" Determine the number of key-value pairs in the dictionary. ","type":"Dict.Dict k v -> Basics.Int"},{"name":"toList","comment":" Convert a dictionary into an association list of key-value pairs, sorted by keys. ","type":"Dict.Dict k v -> List.List ( k, v )"},{"name":"union","comment":" Combine two dictionaries. If there is a collision, preference is given
to the first dictionary.
","type":"Dict.Dict comparable v -> Dict.Dict comparable v -> Dict.Dict comparable v"},{"name":"update","comment":" Update the value of a dictionary for a specific key with a given function. ","type":"comparable -> (Maybe.Maybe v -> Maybe.Maybe v) -> Dict.Dict comparable v -> Dict.Dict comparable v"},{"name":"values","comment":" Get all of the values in a dictionary, in the order of their keys.

    values (fromList [(0,"Alice"),(1,"Bob")]) == ["Alice", "Bob"]
","type":"Dict.Dict k v -> List.List v"}],"binops":[]},{"name":"List","comment":" You can create a `List` in Elm with the `[1,2,3]` syntax, so lists are
used all over the place. This module has a bunch of functions to help you work
with them!

# Create
@docs singleton, repeat, range, (::)

# Transform
@docs map, indexedMap, foldl, foldr, filter, filterMap

# Utilities
@docs length, reverse, member, all, any, maximum, minimum, sum, product

# Combine
@docs append, concat, concatMap, intersperse, map2, map3, map4, map5

# Sort
@docs sort, sortBy, sortWith

# Deconstruct
@docs isEmpty, head, tail, take, drop, partition, unzip

","unions":[],"aliases":[],"values":[{"name":"all","comment":" Determine if all elements satisfy some test.

    all isEven [2,4] == True
    all isEven [2,3] == False
    all isEven [] == True
","type":"(a -> Basics.Bool) -> List.List a -> Basics.Bool"},{"name":"any","comment":" Determine if any elements satisfy some test.

    any isEven [2,3] == True
    any isEven [1,3] == False
    any isEven [] == False
","type":"(a -> Basics.Bool) -> List.List a -> Basics.Bool"},{"name":"append","comment":" Put two lists together.

    append [1,1,2] [3,5,8] == [1,1,2,3,5,8]
    append ['a','b'] ['c'] == ['a','b','c']

You can also use [the `(++)` operator](Basics#++) to append lists.
","type":"List.List a -> List.List a -> List.List a"},{"name":"concat","comment":" Concatenate a bunch of lists into a single list:

    concat [[1,2],[3],[4,5]] == [1,2,3,4,5]
","type":"List.List (List.List a) -> List.List a"},{"name":"concatMap","comment":" Map a given function onto a list and flatten the resulting lists.

    concatMap f xs == concat (map f xs)
","type":"(a -> List.List b) -> List.List a -> List.List b"},{"name":"drop","comment":" Drop the first *n* members of a list.

    drop 2 [1,2,3,4] == [3,4]
","type":"Basics.Int -> List.List a -> List.List a"},{"name":"filter","comment":" Keep elements that satisfy the test.

    filter isEven [1,2,3,4,5,6] == [2,4,6]
","type":"(a -> Basics.Bool) -> List.List a -> List.List a"},{"name":"filterMap","comment":" Filter out certain values. For example, maybe you have a bunch of strings
from an untrusted source and you want to turn them into numbers:

    numbers : List Int
    numbers =
      filterMap String.toInt ["3", "hi", "12", "4th", "May"]

    -- numbers == [3, 12]

","type":"(a -> Maybe.Maybe b) -> List.List a -> List.List b"},{"name":"foldl","comment":" Reduce a list from the left.

    foldl (+)  0  [1,2,3] == 6
    foldl (::) [] [1,2,3] == [3,2,1]

So `foldl step state [1,2,3]` is like saying:

    state
      |> step 1
      |> step 2
      |> step 3
","type":"(a -> b -> b) -> b -> List.List a -> b"},{"name":"foldr","comment":" Reduce a list from the right.

    foldr (+)  0  [1,2,3] == 6
    foldr (::) [] [1,2,3] == [1,2,3]

So `foldr step state [1,2,3]` is like saying:

    state
      |> step 3
      |> step 2
      |> step 1
","type":"(a -> b -> b) -> b -> List.List a -> b"},{"name":"head","comment":" Extract the first element of a list.

    head [1,2,3] == Just 1
    head [] == Nothing

**Note:** It is usually preferable to use a `case` to deconstruct a `List`
because it gives you `(x :: xs)` and you can work with both subparts.
","type":"List.List a -> Maybe.Maybe a"},{"name":"indexedMap","comment":" Same as `map` but the function is also applied to the index of each
element (starting at zero).

    indexedMap Tuple.pair ["Tom","Sue","Bob"] == [ (0,"Tom"), (1,"Sue"), (2,"Bob") ]
","type":"(Basics.Int -> a -> b) -> List.List a -> List.List b"},{"name":"intersperse","comment":" Places the given value between all members of the given list.

    intersperse "on" ["turtles","turtles","turtles"] == ["turtles","on","turtles","on","turtles"]
","type":"a -> List.List a -> List.List a"},{"name":"isEmpty","comment":" Determine if a list is empty.

    isEmpty [] == True

**Note:** It is usually preferable to use a `case` to test this so you do not
forget to handle the `(x :: xs)` case as well!
","type":"List.List a -> Basics.Bool"},{"name":"length","comment":" Determine the length of a list.

    length [1,2,3] == 3
","type":"List.List a -> Basics.Int"},{"name":"map","comment":" Apply a function to every element of a list.

    map sqrt [1,4,9] == [1,2,3]

    map not [True,False,True] == [False,True,False]

So `map func [ a, b, c ]` is the same as `[ func a, func b, func c ]`
","type":"(a -> b) -> List.List a -> List.List b"},{"name":"map2","comment":" Combine two lists, combining them with the given function.
If one list is longer, the extra elements are dropped.

    totals : List Int -> List Int -> List Int
    totals xs ys =
      List.map2 (+) xs ys

    -- totals [1,2,3] [4,5,6] == [5,7,9]

    pairs : List a -> List b -> List (a,b)
    pairs xs ys =
      List.map2 Tuple.pair xs ys

    -- pairs ["alice","bob","chuck"] [2,5,7,8]
    --   == [("alice",2),("bob",5),("chuck",7)]

","type":"(a -> b -> result) -> List.List a -> List.List b -> List.List result"},{"name":"map3","comment":"","type":"(a -> b -> c -> result) -> List.List a -> List.List b -> List.List c -> List.List result"},{"name":"map4","comment":"","type":"(a -> b -> c -> d -> result) -> List.List a -> List.List b -> List.List c -> List.List d -> List.List result"},{"name":"map5","comment":"","type":"(a -> b -> c -> d -> e -> result) -> List.List a -> List.List b -> List.List c -> List.List d -> List.List e -> List.List result"},{"name":"maximum","comment":" Find the maximum element in a non-empty list.

    maximum [1,4,2] == Just 4
    maximum []      == Nothing
","type":"List.List comparable -> Maybe.Maybe comparable"},{"name":"member","comment":" Figure out whether a list contains a value.

    member 9 [1,2,3,4] == False
    member 4 [1,2,3,4] == True
","type":"a -> List.List a -> Basics.Bool"},{"name":"minimum","comment":" Find the minimum element in a non-empty list.

    minimum [3,2,1] == Just 1
    minimum []      == Nothing
","type":"List.List comparable -> Maybe.Maybe comparable"},{"name":"partition","comment":" Partition a list based on some test. The first list contains all values
that satisfy the test, and the second list contains all the value that do not.

    partition (\\x -> x < 3) [0,1,2,3,4,5] == ([0,1,2], [3,4,5])
    partition isEven        [0,1,2,3,4,5] == ([0,2,4], [1,3,5])
","type":"(a -> Basics.Bool) -> List.List a -> ( List.List a, List.List a )"},{"name":"product","comment":" Get the product of the list elements.

    product [2,2,2] == 8
    product [3,3,3] == 27
    product []      == 1

","type":"List.List number -> number"},{"name":"range","comment":" Create a list of numbers, every element increasing by one.
You give the lowest and highest number that should be in the list.

    range 3 6 == [3, 4, 5, 6]
    range 3 3 == [3]
    range 6 3 == []
","type":"Basics.Int -> Basics.Int -> List.List Basics.Int"},{"name":"repeat","comment":" Create a list with *n* copies of a value:

    repeat 3 (0,0) == [(0,0),(0,0),(0,0)]
","type":"Basics.Int -> a -> List.List a"},{"name":"reverse","comment":" Reverse a list.

    reverse [1,2,3,4] == [4,3,2,1]
","type":"List.List a -> List.List a"},{"name":"singleton","comment":" Create a list with only one element:

    singleton 1234 == [1234]
    singleton "hi" == ["hi"]
","type":"a -> List.List a"},{"name":"sort","comment":" Sort values from lowest to highest

    sort [3,1,5] == [1,3,5]
","type":"List.List comparable -> List.List comparable"},{"name":"sortBy","comment":" Sort values by a derived property.

    alice = { name="Alice", height=1.62 }
    bob   = { name="Bob"  , height=1.85 }
    chuck = { name="Chuck", height=1.76 }

    sortBy .name   [chuck,alice,bob] == [alice,bob,chuck]
    sortBy .height [chuck,alice,bob] == [alice,chuck,bob]

    sortBy String.length ["mouse","cat"] == ["cat","mouse"]
","type":"(a -> comparable) -> List.List a -> List.List a"},{"name":"sortWith","comment":" Sort values with a custom comparison function.

    sortWith flippedComparison [1,2,3,4,5] == [5,4,3,2,1]

    flippedComparison a b =
        case compare a b of
          LT -> GT
          EQ -> EQ
          GT -> LT

This is also the most general sort function, allowing you
to define any other: `sort == sortWith compare`
","type":"(a -> a -> Basics.Order) -> List.List a -> List.List a"},{"name":"sum","comment":" Get the sum of the list elements.

    sum [1,2,3] == 6
    sum [1,1,1] == 3
    sum []      == 0

","type":"List.List number -> number"},{"name":"tail","comment":" Extract the rest of the list.

    tail [1,2,3] == Just [2,3]
    tail [] == Nothing

**Note:** It is usually preferable to use a `case` to deconstruct a `List`
because it gives you `(x :: xs)` and you can work with both subparts.
","type":"List.List a -> Maybe.Maybe (List.List a)"},{"name":"take","comment":" Take the first *n* members of a list.

    take 2 [1,2,3,4] == [1,2]
","type":"Basics.Int -> List.List a -> List.List a"},{"name":"unzip","comment":" Decompose a list of tuples into a tuple of lists.

    unzip [(0, True), (17, False), (1337, True)] == ([0,17,1337], [True,False,True])
","type":"List.List ( a, b ) -> ( List.List a, List.List b )"}],"binops":[{"name":"::","comment":" Add an element to the front of a list.

    1 :: [2,3] == [1,2,3]
    1 :: [] == [1]

This operator is pronounced *cons* for historical reasons, but you can think
of it like pushing an entry onto a stack.
","type":"a -> List.List a -> List.List a","associativity":"right","precedence":5}]},{"name":"Maybe","comment":" This library fills a bunch of important niches in Elm. A `Maybe` can help
you with optional arguments, error handling, and records with optional fields.

# Definition
@docs Maybe

# Common Helpers
@docs withDefault, map, map2, map3, map4, map5

# Chaining Maybes
@docs andThen
","unions":[{"name":"Maybe","comment":" Represent values that may or may not exist. It can be useful if you have a
record field that is only filled in sometimes. Or if a function takes a value
sometimes, but does not absolutely need it.

    -- A person, but maybe we do not know their age.
    type alias Person =
        { name : String
        , age : Maybe Int
        }

    tom = { name = "Tom", age = Just 42 }
    sue = { name = "Sue", age = Nothing }
","args":["a"],"cases":[["Just",["a"]],["Nothing",[]]]}],"aliases":[],"values":[{"name":"andThen","comment":" Chain together many computations that may fail. It is helpful to see its
definition:

    andThen : (a -> Maybe b) -> Maybe a -> Maybe b
    andThen callback maybe =
        case maybe of
            Just value ->
                callback value

            Nothing ->
                Nothing

This means we only continue with the callback if things are going well. For
example, say you need to parse some user input as a month:

    parseMonth : String -> Maybe Int
    parseMonth userInput =
        String.toInt userInput
          |> andThen toValidMonth

    toValidMonth : Int -> Maybe Int
    toValidMonth month =
        if 1 <= month && month <= 12 then
            Just month
        else
            Nothing

In the `parseMonth` function, if `String.toInt` produces `Nothing` (because
the `userInput` was not an integer) this entire chain of operations will
short-circuit and result in `Nothing`. If `toValidMonth` results in `Nothing`,
again the chain of computations will result in `Nothing`.
","type":"(a -> Maybe.Maybe b) -> Maybe.Maybe a -> Maybe.Maybe b"},{"name":"map","comment":" Transform a `Maybe` value with a given function:

    map sqrt (Just 9) == Just 3
    map sqrt Nothing  == Nothing

    map sqrt (String.toFloat "9") == Just 3
    map sqrt (String.toFloat "x") == Nothing

","type":"(a -> b) -> Maybe.Maybe a -> Maybe.Maybe b"},{"name":"map2","comment":" Apply a function if all the arguments are `Just` a value.

    map2 (+) (Just 3) (Just 4) == Just 7
    map2 (+) (Just 3) Nothing == Nothing
    map2 (+) Nothing (Just 4) == Nothing

    map2 (+) (String.toInt "1") (String.toInt "123") == Just 124
    map2 (+) (String.toInt "x") (String.toInt "123") == Nothing
    map2 (+) (String.toInt "1") (String.toInt "1.3") == Nothing
","type":"(a -> b -> value) -> Maybe.Maybe a -> Maybe.Maybe b -> Maybe.Maybe value"},{"name":"map3","comment":"","type":"(a -> b -> c -> value) -> Maybe.Maybe a -> Maybe.Maybe b -> Maybe.Maybe c -> Maybe.Maybe value"},{"name":"map4","comment":"","type":"(a -> b -> c -> d -> value) -> Maybe.Maybe a -> Maybe.Maybe b -> Maybe.Maybe c -> Maybe.Maybe d -> Maybe.Maybe value"},{"name":"map5","comment":"","type":"(a -> b -> c -> d -> e -> value) -> Maybe.Maybe a -> Maybe.Maybe b -> Maybe.Maybe c -> Maybe.Maybe d -> Maybe.Maybe e -> Maybe.Maybe value"},{"name":"withDefault","comment":" Provide a default value, turning an optional value into a normal
value.  This comes in handy when paired with functions like
[`Dict.get`](Dict#get) which gives back a `Maybe`.

    withDefault 100 (Just 42)   -- 42
    withDefault 100 Nothing     -- 100

    withDefault "unknown" (Dict.get "Tom" Dict.empty)   -- "unknown"

**Note:** This can be overused! Many cases are better handled by a `case`
expression. And if you end up using `withDefault` a lot, it can be a good sign
that a [custom type][ct] will clean your code up quite a bit!

[ct]: https://guide.elm-lang.org/types/custom_types.html
","type":"a -> Maybe.Maybe a -> a"}],"binops":[]},{"name":"Platform","comment":"

# Programs
@docs Program, worker

# Platform Internals

## Tasks and Processes
@docs Task, ProcessId

## Effect Manager Helpers

An extremely tiny portion of library authors should ever write effect managers.
Fundamentally, Elm needs maybe 10 of them total. I get that people are smart,
curious, etc. but that is not a substitute for a legitimate reason to make an
effect manager. Do you have an *organic need* this fills? Or are you just
curious? Public discussions of your explorations should be framed accordingly.

@docs Router, sendToApp, sendToSelf
","unions":[{"name":"ProcessId","comment":" Head over to the documentation for the [`Process`](Process) module for
information on this. It is only defined here because it is a platform
primitive.
","args":[],"cases":[]},{"name":"Program","comment":" A `Program` describes an Elm program! How does it react to input? Does it
show anything on screen? Etc.
","args":["flags","model","msg"],"cases":[]},{"name":"Router","comment":" An effect manager has access to a “router” that routes messages between
the main app and your individual effect manager.
","args":["appMsg","selfMsg"],"cases":[]},{"name":"Task","comment":" Head over to the documentation for the [`Task`](Task) module for more
information on this. It is only defined here because it is a platform
primitive.
","args":["err","ok"],"cases":[]}],"aliases":[],"values":[{"name":"sendToApp","comment":" Send the router a message for the main loop of your app. This message will
be handled by the overall `update` function, just like events from `Html`.
","type":"Platform.Router msg a -> msg -> Platform.Task x ()"},{"name":"sendToSelf","comment":" Send the router a message for your effect manager. This message will
be routed to the `onSelfMsg` function, where you can update the state of your
effect manager as necessary.

As an example, the effect manager for web sockets
","type":"Platform.Router a msg -> msg -> Platform.Task x ()"},{"name":"worker","comment":" Create a [headless][] program with no user interface.

This is great if you want to use Elm as the &ldquo;brain&rdquo; for something
else. For example, you could send messages out ports to modify the DOM, but do
all the complex logic in Elm.

[headless]: https://en.wikipedia.org/wiki/Headless_software

Initializing a headless program from JavaScript looks like this:

```javascript
var app = Elm.MyThing.init();
```

If you _do_ want to control the user interface in Elm, the [`Browser`][browser]
module has a few ways to create that kind of `Program` instead!

[headless]: https://en.wikipedia.org/wiki/Headless_software
[browser]: /packages/elm/browser/latest/Browser
","type":"{ init : flags -> ( model, Platform.Cmd.Cmd msg ), update : msg -> model -> ( model, Platform.Cmd.Cmd msg ), subscriptions : model -> Platform.Sub.Sub msg } -> Platform.Program flags model msg"}],"binops":[]},{"name":"Platform.Cmd","comment":"

> **Note:** Elm has **managed effects**, meaning that things like HTTP
> requests or writing to disk are all treated as *data* in Elm. When this
> data is given to the Elm runtime system, it can do some “query optimization”
> before actually performing the effect. Perhaps unexpectedly, this managed
> effects idea is the heart of why Elm is so nice for testing, reuse,
> reproducibility, etc.
>
> Elm has two kinds of managed effects: commands and subscriptions.

# Commands
@docs Cmd, none, batch

# Fancy Stuff
@docs map

","unions":[{"name":"Cmd","comment":" A command is a way of telling Elm, “Hey, I want you to do this thing!”
So if you want to send an HTTP request, you would need to command Elm to do it.
Or if you wanted to ask for geolocation, you would need to command Elm to go
get it.

Every `Cmd` specifies (1) which effects you need access to and (2) the type of
messages that will come back into your application.

**Note:** Do not worry if this seems confusing at first! As with every Elm user
ever, commands will make more sense as you work through [the Elm Architecture
Tutorial](https://guide.elm-lang.org/architecture/) and see how they
fit into a real application!
","args":["msg"],"cases":[]}],"aliases":[],"values":[{"name":"batch","comment":" When you need the runtime system to perform a couple commands, you
can batch them together. Each is handed to the runtime at the same time,
and since each can perform arbitrary operations in the world, there are
no ordering guarantees about the results.

**Note:** `Cmd.none` and `Cmd.batch [ Cmd.none, Cmd.none ]` and `Cmd.batch []`
all do the same thing.
","type":"List.List (Platform.Cmd.Cmd msg) -> Platform.Cmd.Cmd msg"},{"name":"map","comment":" Transform the messages produced by a command.
Very similar to [`Html.map`](/packages/elm/html/latest/Html#map).

This is very rarely useful in well-structured Elm code, so definitely read the
section on [structure][] in the guide before reaching for this!

[structure]: https://guide.elm-lang.org/webapps/structure.html
","type":"(a -> msg) -> Platform.Cmd.Cmd a -> Platform.Cmd.Cmd msg"},{"name":"none","comment":" Tell the runtime that there are no commands.

","type":"Platform.Cmd.Cmd msg"}],"binops":[]},{"name":"Platform.Sub","comment":"

> **Note:** Elm has **managed effects**, meaning that things like HTTP
> requests or writing to disk are all treated as *data* in Elm. When this
> data is given to the Elm runtime system, it can do some “query optimization”
> before actually performing the effect. Perhaps unexpectedly, this managed
> effects idea is the heart of why Elm is so nice for testing, reuse,
> reproducibility, etc.
>
> Elm has two kinds of managed effects: commands and subscriptions.

# Subscriptions
@docs Sub, none, batch

# Fancy Stuff
@docs map
","unions":[{"name":"Sub","comment":" A subscription is a way of telling Elm, “Hey, let me know if anything
interesting happens over there!” So if you want to listen for messages on a web
socket, you would tell Elm to create a subscription. If you want to get clock
ticks, you would tell Elm to subscribe to that. The cool thing here is that
this means *Elm* manages all the details of subscriptions instead of *you*.
So if a web socket goes down, *you* do not need to manually reconnect with an
exponential backoff strategy, *Elm* does this all for you behind the scenes!

Every `Sub` specifies (1) which effects you need access to and (2) the type of
messages that will come back into your application.

**Note:** Do not worry if this seems confusing at first! As with every Elm user
ever, subscriptions will make more sense as you work through [the Elm Architecture
Tutorial](https://guide.elm-lang.org/architecture/) and see how they fit
into a real application!
","args":["msg"],"cases":[]}],"aliases":[],"values":[{"name":"batch","comment":" When you need to subscribe to multiple things, you can create a `batch` of
subscriptions.

**Note:** `Sub.none` and `Sub.batch [ Sub.none, Sub.none ]` and
`Sub.batch []` all do the same thing.
","type":"List.List (Platform.Sub.Sub msg) -> Platform.Sub.Sub msg"},{"name":"map","comment":" Transform the messages produced by a subscription.
Very similar to [`Html.map`](/packages/elm/html/latest/Html#map).

This is very rarely useful in well-structured Elm code, so definitely read the
section on [structure][] in the guide before reaching for this!

[structure]: https://guide.elm-lang.org/webapps/structure.html
","type":"(a -> msg) -> Platform.Sub.Sub a -> Platform.Sub.Sub msg"},{"name":"none","comment":" Tell the runtime that there are no subscriptions.
","type":"Platform.Sub.Sub msg"}],"binops":[]},{"name":"Process","comment":"

# Processes
@docs Id, spawn, sleep, kill

## Future Plans

Right now, this library is pretty sparse. For example, there is no public API
for processes to communicate with each other. This is a really important
ability, but it is also something that is extraordinarily easy to get wrong!

I think the trend will be towards an Erlang style of concurrency, where every
process has an “event queue” that anyone can send messages to. I currently
think the API will be extended to be more like this:

    type Id exit msg

    spawn : Task exit a -> Task x (Id exit Never)

    kill : Id exit msg -> Task x ()

    send : Id exit msg -> msg -> Task x ()

A process `Id` will have two type variables to make sure all communication is
valid. The `exit` type describes the messages that are produced if the process
fails because of user code. So if processes are linked and trapping errors,
they will need to handle this. The `msg` type just describes what kind of
messages this process can be sent by strangers.

We shall see though! This is just a draft that does not cover nearly everything
it needs to, so the long-term vision for concurrency in Elm will be rolling out
slowly as I get more data and experience.

I ask that people bullish on compiling to node.js keep this in mind. I think we
can do better than the hopelessly bad concurrency model of node.js, and I hope
the Elm community will be supportive of being more ambitious, even if it takes
longer. That’s kind of what Elm is all about.
","unions":[],"aliases":[{"name":"Id","comment":" A light-weight process that runs concurrently. You can use `spawn` to
get a bunch of different tasks running in different processes. The Elm runtime
will interleave their progress. So if a task is taking too long, we will pause
it at an `andThen` and switch over to other stuff.

**Note:** We make a distinction between *concurrency* which means interleaving
different sequences and *parallelism* which means running different
sequences at the exact same time. For example, a
[time-sharing system](https://en.wikipedia.org/wiki/Time-sharing) is definitely
concurrent, but not necessarily parallel. So even though JS runs within a
single OS-level thread, Elm can still run things concurrently.
","args":[],"type":"Platform.ProcessId"}],"values":[{"name":"kill","comment":" Sometimes you `spawn` a process, but later decide it would be a waste to
have it keep running and doing stuff. The `kill` function will force a process
to bail on whatever task it is running. So if there is an HTTP request in
flight, it will also abort the request.
","type":"Process.Id -> Task.Task x ()"},{"name":"sleep","comment":" Block progress on the current process for the given number of milliseconds.
The JavaScript equivalent of this is [`setTimeout`][setTimeout] which lets you
delay work until later.

[setTimeout]: https://developer.mozilla.org/en-US/docs/Web/API/WindowTimers/setTimeout
","type":"Basics.Float -> Task.Task x ()"},{"name":"spawn","comment":" Run a task in its own light-weight process. In the following example,
`task1` and `task2` will be interleaved. If `task1` makes a long HTTP request
or is just taking a long time, we can hop over to `task2` and do some work
there.

    spawn task1
      |> Task.andThen (\\_ -> spawn task2)

**Note:** This creates a relatively restricted kind of `Process` because it
cannot receive any messages. More flexibility for user-defined processes will
come in a later release!
","type":"Task.Task x a -> Task.Task y Process.Id"}],"binops":[]},{"name":"Result","comment":" A `Result` is the result of a computation that may fail. This is a great
way to manage errors in Elm.

# Type and Constructors
@docs Result

# Mapping
@docs map, map2, map3, map4, map5

# Chaining
@docs andThen

# Handling Errors
@docs withDefault, toMaybe, fromMaybe, mapError
","unions":[{"name":"Result","comment":" A `Result` is either `Ok` meaning the computation succeeded, or it is an
`Err` meaning that there was some failure.
","args":["error","value"],"cases":[["Ok",["value"]],["Err",["error"]]]}],"aliases":[],"values":[{"name":"andThen","comment":" Chain together a sequence of computations that may fail. It is helpful
to see its definition:

    andThen : (a -> Result e b) -> Result e a -> Result e b
    andThen callback result =
        case result of
          Ok value -> callback value
          Err msg -> Err msg

This means we only continue with the callback if things are going well. For
example, say you need to use (`toInt : String -> Result String Int`) to parse
a month and make sure it is between 1 and 12:

    toValidMonth : Int -> Result String Int
    toValidMonth month =
        if month >= 1 && month <= 12
            then Ok month
            else Err "months must be between 1 and 12"

    toMonth : String -> Result String Int
    toMonth rawString =
        toInt rawString
          |> andThen toValidMonth

    -- toMonth "4" == Ok 4
    -- toMonth "9" == Ok 9
    -- toMonth "a" == Err "cannot parse to an Int"
    -- toMonth "0" == Err "months must be between 1 and 12"

This allows us to come out of a chain of operations with quite a specific error
message. It is often best to create a custom type that explicitly represents
the exact ways your computation may fail. This way it is easy to handle in your
code.
","type":"(a -> Result.Result x b) -> Result.Result x a -> Result.Result x b"},{"name":"fromMaybe","comment":" Convert from a simple `Maybe` to interact with some code that primarily
uses `Results`.

    parseInt : String -> Maybe Int

    resultParseInt : String -> Result String Int
    resultParseInt string =
        fromMaybe ("error parsing string: " ++ toString string) (parseInt string)
","type":"x -> Maybe.Maybe a -> Result.Result x a"},{"name":"map","comment":" Apply a function to a result. If the result is `Ok`, it will be converted.
If the result is an `Err`, the same error value will propagate through.

    map sqrt (Ok 4.0)          == Ok 2.0
    map sqrt (Err "bad input") == Err "bad input"
","type":"(a -> value) -> Result.Result x a -> Result.Result x value"},{"name":"map2","comment":" Apply a function if both results are `Ok`. If not, the first `Err` will
propagate through.

    map2 max (Ok 42)   (Ok 13)   == Ok 42
    map2 max (Err "x") (Ok 13)   == Err "x"
    map2 max (Ok 42)   (Err "y") == Err "y"
    map2 max (Err "x") (Err "y") == Err "x"

This can be useful if you have two computations that may fail, and you want
to put them together quickly.
","type":"(a -> b -> value) -> Result.Result x a -> Result.Result x b -> Result.Result x value"},{"name":"map3","comment":"","type":"(a -> b -> c -> value) -> Result.Result x a -> Result.Result x b -> Result.Result x c -> Result.Result x value"},{"name":"map4","comment":"","type":"(a -> b -> c -> d -> value) -> Result.Result x a -> Result.Result x b -> Result.Result x c -> Result.Result x d -> Result.Result x value"},{"name":"map5","comment":"","type":"(a -> b -> c -> d -> e -> value) -> Result.Result x a -> Result.Result x b -> Result.Result x c -> Result.Result x d -> Result.Result x e -> Result.Result x value"},{"name":"mapError","comment":" Transform an `Err` value. For example, say the errors we get have too much
information:

    parseInt : String -> Result ParseError Int

    type alias ParseError =
        { message : String
        , code : Int
        , position : (Int,Int)
        }

    mapError .message (parseInt "123") == Ok 123
    mapError .message (parseInt "abc") == Err "char 'a' is not a number"
","type":"(x -> y) -> Result.Result x a -> Result.Result y a"},{"name":"toMaybe","comment":" Convert to a simpler `Maybe` if the actual error message is not needed or
you need to interact with some code that primarily uses maybes.

    parseInt : String -> Result ParseError Int

    maybeParseInt : String -> Maybe Int
    maybeParseInt string =
        toMaybe (parseInt string)
","type":"Result.Result x a -> Maybe.Maybe a"},{"name":"withDefault","comment":" If the result is `Ok` return the value, but if the result is an `Err` then
return a given default value. The following examples try to parse integers.

    Result.withDefault 0 (Ok 123)   == 123
    Result.withDefault 0 (Err "no") == 0
","type":"a -> Result.Result x a -> a"}],"binops":[]},{"name":"Set","comment":" A set of unique values. The values can be any comparable type. This
includes `Int`, `Float`, `Time`, `Char`, `String`, and tuples or lists
of comparable types.

Insert, remove, and query operations all take *O(log n)* time.

# Sets
@docs Set

# Build
@docs empty, singleton, insert, remove

# Query
@docs isEmpty, member, size

# Combine
@docs union, intersect, diff

# Lists
@docs toList, fromList

# Transform
@docs map, foldl, foldr, filter, partition

","unions":[{"name":"Set","comment":" Represents a set of unique values. So `(Set Int)` is a set of integers and
`(Set String)` is a set of strings.
","args":["t"],"cases":[]}],"aliases":[],"values":[{"name":"diff","comment":" Get the difference between the first set and the second. Keeps values
that do not appear in the second set.
","type":"Set.Set comparable -> Set.Set comparable -> Set.Set comparable"},{"name":"empty","comment":" Create an empty set.
","type":"Set.Set a"},{"name":"filter","comment":" Only keep elements that pass the given test.

    import Set exposing (Set)

    numbers : Set Int
    numbers =
      Set.fromList [-2,-1,0,1,2]

    positives : Set Int
    positives =
      Set.filter (\\x -> x > 0) numbers

    -- positives == Set.fromList [1,2]
","type":"(comparable -> Basics.Bool) -> Set.Set comparable -> Set.Set comparable"},{"name":"foldl","comment":" Fold over the values in a set, in order from lowest to highest.
","type":"(a -> b -> b) -> b -> Set.Set a -> b"},{"name":"foldr","comment":" Fold over the values in a set, in order from highest to lowest.
","type":"(a -> b -> b) -> b -> Set.Set a -> b"},{"name":"fromList","comment":" Convert a list into a set, removing any duplicates.
","type":"List.List comparable -> Set.Set comparable"},{"name":"insert","comment":" Insert a value into a set.
","type":"comparable -> Set.Set comparable -> Set.Set comparable"},{"name":"intersect","comment":" Get the intersection of two sets. Keeps values that appear in both sets.
","type":"Set.Set comparable -> Set.Set comparable -> Set.Set comparable"},{"name":"isEmpty","comment":" Determine if a set is empty.
","type":"Set.Set a -> Basics.Bool"},{"name":"map","comment":" Map a function onto a set, creating a new set with no duplicates.
","type":"(comparable -> comparable2) -> Set.Set comparable -> Set.Set comparable2"},{"name":"member","comment":" Determine if a value is in a set.
","type":"comparable -> Set.Set comparable -> Basics.Bool"},{"name":"partition","comment":" Create two new sets. The first contains all the elements that passed the
given test, and the second contains all the elements that did not.
","type":"(comparable -> Basics.Bool) -> Set.Set comparable -> ( Set.Set comparable, Set.Set comparable )"},{"name":"remove","comment":" Remove a value from a set. If the value is not found, no changes are made.
","type":"comparable -> Set.Set comparable -> Set.Set comparable"},{"name":"singleton","comment":" Create a set with one value.
","type":"comparable -> Set.Set comparable"},{"name":"size","comment":" Determine the number of elements in a set.
","type":"Set.Set a -> Basics.Int"},{"name":"toList","comment":" Convert a set into a list, sorted from lowest to highest.
","type":"Set.Set a -> List.List a"},{"name":"union","comment":" Get the union of two sets. Keep all values.
","type":"Set.Set comparable -> Set.Set comparable -> Set.Set comparable"}],"binops":[]},{"name":"String","comment":" A built-in representation for efficient string manipulation. String literals
are enclosed in `"double quotes"`. Strings are *not* lists of characters.

# Strings
@docs String, isEmpty, length, reverse, repeat, replace

# Building and Splitting
@docs append, concat, split, join, words, lines

# Get Substrings
@docs slice, left, right, dropLeft, dropRight

# Check for Substrings
@docs contains, startsWith, endsWith, indexes, indices

# Int Conversions
@docs toInt, fromInt

# Float Conversions
@docs toFloat, fromFloat

# Char Conversions
@docs fromChar, cons, uncons

# List Conversions
@docs toList, fromList

# Formatting
Cosmetic operations such as padding with extra characters or trimming whitespace.

@docs toUpper, toLower, pad, padLeft, padRight, trim, trimLeft, trimRight

# Higher-Order Functions
@docs map, filter, foldl, foldr, any, all
","unions":[{"name":"String","comment":" A `String` is a chunk of text:

    "Hello!"
    "How are you?"
    "🙈🙉🙊"

    -- strings with escape characters
    "this\\n\\t\\"that\\""
    "\\u{1F648}\\u{1F649}\\u{1F64A}" -- "🙈🙉🙊"

    -- multiline strings
    \"\"\"Triple double quotes let you
    create "multiline strings" which
    can have unescaped quotes and newlines.
    \"\"\"

A `String` can represent any sequence of [unicode characters][u]. You can use
the unicode escapes from `\\u{0000}` to `\\u{10FFFF}` to represent characters
by their code point. You can also include the unicode characters directly.
Using the escapes can be better if you need one of the many whitespace
characters with different widths.

[u]: https://en.wikipedia.org/wiki/Unicode

**Note:** JavaScript lets you use double quotes and single quotes interchangably.
This is not true in Elm. You must use double quotes for a `String`, and you must
use single quotes for a [`Char`](Char#Char).
","args":[],"cases":[]}],"aliases":[],"values":[{"name":"all","comment":" Determine whether *all* characters pass the test.

    all isDigit "90210" == True
    all isDigit "R2-D2" == False
    all isDigit "heart" == False
","type":"(Char.Char -> Basics.Bool) -> String.String -> Basics.Bool"},{"name":"any","comment":" Determine whether *any* characters pass the test.

    any isDigit "90210" == True
    any isDigit "R2-D2" == True
    any isDigit "heart" == False
","type":"(Char.Char -> Basics.Bool) -> String.String -> Basics.Bool"},{"name":"append","comment":" Append two strings. You can also use [the `(++)` operator](Basics#++)
to do this.

    append "butter" "fly" == "butterfly"
","type":"String.String -> String.String -> String.String"},{"name":"concat","comment":" Concatenate many strings into one.

    concat ["never","the","less"] == "nevertheless"
","type":"List.List String.String -> String.String"},{"name":"cons","comment":" Add a character to the beginning of a string.

    cons 'T' "he truth is out there" == "The truth is out there"
","type":"Char.Char -> String.String -> String.String"},{"name":"contains","comment":" See if the second string contains the first one.

    contains "the" "theory" == True
    contains "hat" "theory" == False
    contains "THE" "theory" == False

","type":"String.String -> String.String -> Basics.Bool"},{"name":"dropLeft","comment":" Drop *n* characters from the left side of a string.

    dropLeft 2 "The Lone Gunmen" == "e Lone Gunmen"
","type":"Basics.Int -> String.String -> String.String"},{"name":"dropRight","comment":" Drop *n* characters from the right side of a string.

    dropRight 2 "Cigarette Smoking Man" == "Cigarette Smoking M"
","type":"Basics.Int -> String.String -> String.String"},{"name":"endsWith","comment":" See if the second string ends with the first one.

    endsWith "the" "theory" == False
    endsWith "ory" "theory" == True
","type":"String.String -> String.String -> Basics.Bool"},{"name":"filter","comment":" Keep only the characters that pass the test.

    filter isDigit "R2-D2" == "22"
","type":"(Char.Char -> Basics.Bool) -> String.String -> String.String"},{"name":"foldl","comment":" Reduce a string from the left.

    foldl cons "" "time" == "emit"
","type":"(Char.Char -> b -> b) -> b -> String.String -> b"},{"name":"foldr","comment":" Reduce a string from the right.

    foldr cons "" "time" == "time"
","type":"(Char.Char -> b -> b) -> b -> String.String -> b"},{"name":"fromChar","comment":" Create a string from a given character.

    fromChar 'a' == "a"
","type":"Char.Char -> String.String"},{"name":"fromFloat","comment":" Convert a `Float` to a `String`.

    String.fromFloat 123 == "123"
    String.fromFloat -42 == "-42"
    String.fromFloat 3.9 == "3.9"

Check out [`Debug.toString`](Debug#toString) to convert *any* value to a string
for debugging purposes.
","type":"Basics.Float -> String.String"},{"name":"fromInt","comment":" Convert an `Int` to a `String`.

    String.fromInt 123 == "123"
    String.fromInt -42 == "-42"

Check out [`Debug.toString`](Debug#toString) to convert *any* value to a string
for debugging purposes.
","type":"Basics.Int -> String.String"},{"name":"fromList","comment":" Convert a list of characters into a String. Can be useful if you
want to create a string primarily by consing, perhaps for decoding
something.

    fromList ['a','b','c'] == "abc"
    fromList ['🙈','🙉','🙊'] == "🙈🙉🙊"
","type":"List.List Char.Char -> String.String"},{"name":"indexes","comment":" Get all of the indexes for a substring in another string.

    indexes "i" "Mississippi"   == [1,4,7,10]
    indexes "ss" "Mississippi"  == [2,5]
    indexes "needle" "haystack" == []
","type":"String.String -> String.String -> List.List Basics.Int"},{"name":"indices","comment":" Alias for `indexes`. ","type":"String.String -> String.String -> List.List Basics.Int"},{"name":"isEmpty","comment":" Determine if a string is empty.

    isEmpty "" == True
    isEmpty "the world" == False
","type":"String.String -> Basics.Bool"},{"name":"join","comment":" Put many strings together with a given separator.

    join "a" ["H","w","ii","n"]        == "Hawaiian"
    join " " ["cat","dog","cow"]       == "cat dog cow"
    join "/" ["home","evan","Desktop"] == "home/evan/Desktop"
","type":"String.String -> List.List String.String -> String.String"},{"name":"left","comment":" Take *n* characters from the left side of a string.

    left 2 "Mulder" == "Mu"
","type":"Basics.Int -> String.String -> String.String"},{"name":"length","comment":" Get the length of a string.

    length "innumerable" == 11
    length "" == 0

","type":"String.String -> Basics.Int"},{"name":"lines","comment":" Break a string into lines, splitting on newlines.

    lines "How are you?\\nGood?" == ["How are you?", "Good?"]
","type":"String.String -> List.List String.String"},{"name":"map","comment":" Transform every character in a string

    map (\\c -> if c == '/' then '.' else c) "a/b/c" == "a.b.c"
","type":"(Char.Char -> Char.Char) -> String.String -> String.String"},{"name":"pad","comment":" Pad a string on both sides until it has a given length.

    pad 5 ' ' "1"   == "  1  "
    pad 5 ' ' "11"  == "  11 "
    pad 5 ' ' "121" == " 121 "
","type":"Basics.Int -> Char.Char -> String.String -> String.String"},{"name":"padLeft","comment":" Pad a string on the left until it has a given length.

    padLeft 5 '.' "1"   == "....1"
    padLeft 5 '.' "11"  == "...11"
    padLeft 5 '.' "121" == "..121"
","type":"Basics.Int -> Char.Char -> String.String -> String.String"},{"name":"padRight","comment":" Pad a string on the right until it has a given length.

    padRight 5 '.' "1"   == "1...."
    padRight 5 '.' "11"  == "11..."
    padRight 5 '.' "121" == "121.."
","type":"Basics.Int -> Char.Char -> String.String -> String.String"},{"name":"repeat","comment":" Repeat a string *n* times.

    repeat 3 "ha" == "hahaha"
","type":"Basics.Int -> String.String -> String.String"},{"name":"replace","comment":" Replace all occurrences of some substring.

    replace "." "-" "Json.Decode.succeed" == "Json-Decode-succeed"
    replace "," "/" "a,b,c,d,e"           == "a/b/c/d/e"

**Note:** If you need more advanced replacements, check out the
[`elm/parser`][parser] or [`elm/regex`][regex] package.

[parser]: /packages/elm/parser/latest
[regex]: /packages/elm/regex/latest
","type":"String.String -> String.String -> String.String -> String.String"},{"name":"reverse","comment":" Reverse a string.

    reverse "stressed" == "desserts"
","type":"String.String -> String.String"},{"name":"right","comment":" Take *n* characters from the right side of a string.

    right 2 "Scully" == "ly"
","type":"Basics.Int -> String.String -> String.String"},{"name":"slice","comment":" Take a substring given a start and end index. Negative indexes
are taken starting from the *end* of the list.

    slice  7  9 "snakes on a plane!" == "on"
    slice  0  6 "snakes on a plane!" == "snakes"
    slice  0 -7 "snakes on a plane!" == "snakes on a"
    slice -6 -1 "snakes on a plane!" == "plane"
","type":"Basics.Int -> Basics.Int -> String.String -> String.String"},{"name":"split","comment":" Split a string using a given separator.

    split "," "cat,dog,cow"        == ["cat","dog","cow"]
    split "/" "home/evan/Desktop/" == ["home","evan","Desktop", ""]

","type":"String.String -> String.String -> List.List String.String"},{"name":"startsWith","comment":" See if the second string starts with the first one.

    startsWith "the" "theory" == True
    startsWith "ory" "theory" == False
","type":"String.String -> String.String -> Basics.Bool"},{"name":"toFloat","comment":" Try to convert a string into a float, failing on improperly formatted strings.

    String.toFloat "123" == Just 123.0
    String.toFloat "-42" == Just -42.0
    String.toFloat "3.1" == Just 3.1
    String.toFloat "31a" == Nothing

If you are extracting a number from some raw user input, you will typically
want to use [`Maybe.withDefault`](Maybe#withDefault) to handle bad data:

    Maybe.withDefault 0 (String.toFloat "42.5") == 42.5
    Maybe.withDefault 0 (String.toFloat "cats") == 0
","type":"String.String -> Maybe.Maybe Basics.Float"},{"name":"toInt","comment":" Try to convert a string into an int, failing on improperly formatted strings.

    String.toInt "123" == Just 123
    String.toInt "-42" == Just -42
    String.toInt "3.1" == Nothing
    String.toInt "31a" == Nothing

If you are extracting a number from some raw user input, you will typically
want to use [`Maybe.withDefault`](Maybe#withDefault) to handle bad data:

    Maybe.withDefault 0 (String.toInt "42") == 42
    Maybe.withDefault 0 (String.toInt "ab") == 0
","type":"String.String -> Maybe.Maybe Basics.Int"},{"name":"toList","comment":" Convert a string to a list of characters.

    toList "abc" == ['a','b','c']
    toList "🙈🙉🙊" == ['🙈','🙉','🙊']
","type":"String.String -> List.List Char.Char"},{"name":"toLower","comment":" Convert a string to all lower case. Useful for case-insensitive comparisons.

    toLower "X-FILES" == "x-files"
","type":"String.String -> String.String"},{"name":"toUpper","comment":" Convert a string to all upper case. Useful for case-insensitive comparisons
and VIRTUAL YELLING.

    toUpper "skinner" == "SKINNER"
","type":"String.String -> String.String"},{"name":"trim","comment":" Get rid of whitespace on both sides of a string.

    trim "  hats  \\n" == "hats"
","type":"String.String -> String.String"},{"name":"trimLeft","comment":" Get rid of whitespace on the left of a string.

    trimLeft "  hats  \\n" == "hats  \\n"
","type":"String.String -> String.String"},{"name":"trimRight","comment":" Get rid of whitespace on the right of a string.

    trimRight "  hats  \\n" == "  hats"
","type":"String.String -> String.String"},{"name":"uncons","comment":" Split a non-empty string into its head and tail. This lets you
pattern match on strings exactly as you would with lists.

    uncons "abc" == Just ('a',"bc")
    uncons ""    == Nothing
","type":"String.String -> Maybe.Maybe ( Char.Char, String.String )"},{"name":"words","comment":" Break a string into words, splitting on chunks of whitespace.

    words "How are \\t you? \\n Good?" == ["How","are","you?","Good?"]
","type":"String.String -> List.List String.String"}],"binops":[]},{"name":"Task","comment":" Tasks make it easy to describe asynchronous operations that may fail, like
HTTP requests or writing to a database.

# Tasks
@docs Task, perform, attempt

# Chains
@docs andThen, succeed, fail, sequence

# Maps
@docs map, map2, map3, map4, map5

# Errors
@docs onError, mapError

","unions":[],"aliases":[{"name":"Task","comment":" Here are some common tasks:

- [`now : Task x Posix`][now]
- [`focus : String -> Task Error ()`][focus]
- [`sleep : Float -> Task x ()`][sleep]

[now]: /packages/elm/time/latest/Time#now
[focus]: /packages/elm/browser/latest/Browser-Dom#focus
[sleep]: /packages/elm/core/latest/Process#sleep

In each case we have a `Task` that will resolve successfully with an `a` value
or unsuccessfully with an `x` value. So `Browser.Dom.focus` we may fail with an
`Error` if the given ID does not exist. Whereas `Time.now` never fails so
I cannot be more specific than `x`. No such value will ever exist! Instead it
always succeeds with the current POSIX time.

More generally a task is a _description_ of what you need to do. Like a todo
list. Or like a grocery list. Or like GitHub issues. So saying "the task is
to tell me the current POSIX time" does not complete the task! You need
[`perform`](#perform) tasks or [`attempt`](#attempt) tasks.
","args":["x","a"],"type":"Platform.Task x a"}],"values":[{"name":"andThen","comment":" Chain together a task and a callback. The first task will run, and if it is
successful, you give the result to the callback resulting in another task. This
task then gets run. We could use this to make a task that resolves an hour from
now:

    import Time -- elm install elm/time
    import Process

    timeInOneHour : Task x Time.Posix
    timeInOneHour =
      Process.sleep (60 * 60 * 1000)
        |> andThen (\\_ -> Time.now)

First the process sleeps for an hour **and then** it tells us what time it is.
","type":"(a -> Task.Task x b) -> Task.Task x a -> Task.Task x b"},{"name":"attempt","comment":" This is very similar to [`perform`](#perform) except it can handle failures!
So we could _attempt_ to focus on a certain DOM node like this:

    import Browser.Dom  -- elm install elm/browser
    import Task

    type Msg
      = Click
      | Search String
      | Focus (Result Browser.DomError ())

    focus : Cmd Msg
    focus =
      Task.attempt Focus (Browser.Dom.focus "my-app-search-box")

So the task is "focus on this DOM node" and we are turning it into the command
"Hey Elm, attempt to focus on this DOM node and give me a `Msg` about whether
you succeeded or failed."

**Note:** Definitely work through [`guide.elm-lang.org`][guide] to get a
feeling for how commands fit into The Elm Architecture.

[guide]: https://guide.elm-lang.org/
","type":"(Result.Result x a -> msg) -> Task.Task x a -> Platform.Cmd.Cmd msg"},{"name":"fail","comment":" A task that fails immediately when run. Like with `succeed`, this can be
used with `andThen` to check on the outcome of another task.

    type Error = NotFound

    notFound : Task Error a
    notFound =
      fail NotFound
","type":"x -> Task.Task x a"},{"name":"map","comment":" Transform a task. Maybe you want to use [`elm/time`][time] to figure
out what time it will be in one hour:

    import Task exposing (Task)
    import Time -- elm install elm/time

    timeInOneHour : Task x Time.Posix
    timeInOneHour =
      Task.map addAnHour Time.now

    addAnHour : Time.Posix -> Time.Posix
    addAnHour time =
      Time.millisToPosix (Time.posixToMillis time + 60 * 60 * 1000)

[time]: /packages/elm/time/latest/
","type":"(a -> b) -> Task.Task x a -> Task.Task x b"},{"name":"map2","comment":" Put the results of two tasks together. For example, if we wanted to know
the current month, we could use [`elm/time`][time] to ask:

    import Task exposing (Task)
    import Time -- elm install elm/time

    getMonth : Task x Int
    getMonth =
      Task.map2 Time.toMonth Time.here Time.now

**Note:** Say we were doing HTTP requests instead. `map2` does each task in
order, so it would try the first request and only continue after it succeeds.
If it fails, the whole thing fails!

[time]: /packages/elm/time/latest/
","type":"(a -> b -> result) -> Task.Task x a -> Task.Task x b -> Task.Task x result"},{"name":"map3","comment":"","type":"(a -> b -> c -> result) -> Task.Task x a -> Task.Task x b -> Task.Task x c -> Task.Task x result"},{"name":"map4","comment":"","type":"(a -> b -> c -> d -> result) -> Task.Task x a -> Task.Task x b -> Task.Task x c -> Task.Task x d -> Task.Task x result"},{"name":"map5","comment":"","type":"(a -> b -> c -> d -> e -> result) -> Task.Task x a -> Task.Task x b -> Task.Task x c -> Task.Task x d -> Task.Task x e -> Task.Task x result"},{"name":"mapError","comment":" Transform the error value. This can be useful if you need a bunch of error
types to match up.

    type Error
      = Http Http.Error
      | WebGL WebGL.Error

    getResources : Task Error Resource
    getResources =
      sequence
        [ mapError Http serverTask
        , mapError WebGL textureTask
        ]
","type":"(x -> y) -> Task.Task x a -> Task.Task y a"},{"name":"onError","comment":" Recover from a failure in a task. If the given task fails, we use the
callback to recover.

    fail "file not found"
      |> onError (\\msg -> succeed 42)
      -- succeed 42

    succeed 9
      |> onError (\\msg -> succeed 42)
      -- succeed 9
","type":"(x -> Task.Task y a) -> Task.Task x a -> Task.Task y a"},{"name":"perform","comment":" Like I was saying in the [`Task`](#Task) documentation, just having a
`Task` does not mean it is done. We must command Elm to `perform` the task:

    import Time  -- elm install elm/time
    import Task

    type Msg
      = Click
      | Search String
      | NewTime Time.Posix

    getNewTime : Cmd Msg
    getNewTime =
      Task.perform NewTime Time.now

If you have worked through [`guide.elm-lang.org`][guide] (highly recommended!)
you will recognize `Cmd` from the section on The Elm Architecture. So we have
changed a task like "make delicious lasagna" into a command like "Hey Elm, make
delicious lasagna and give it to my `update` function as a `Msg` value."

[guide]: https://guide.elm-lang.org/
","type":"(a -> msg) -> Task.Task Basics.Never a -> Platform.Cmd.Cmd msg"},{"name":"sequence","comment":" Start with a list of tasks, and turn them into a single task that returns a
list. The tasks will be run in order one-by-one and if any task fails the whole
sequence fails.

    sequence [ succeed 1, succeed 2 ] == succeed [ 1, 2 ]

","type":"List.List (Task.Task x a) -> Task.Task x (List.List a)"},{"name":"succeed","comment":" A task that succeeds immediately when run. It is usually used with
[`andThen`](#andThen). You can use it like `map` if you want:

    import Time -- elm install elm/time

    timeInMillis : Task x Int
    timeInMillis =
      Time.now
        |> andThen (\\t -> succeed (Time.posixToMillis t))

","type":"a -> Task.Task x a"}],"binops":[]},{"name":"Tuple","comment":" Elm has built-in syntax for tuples, so you can define 2D points like this:

    origin : (Float, Float)
    origin =
      (0, 0)

    position : (Float, Float)
    position =
      (3, 4)

This module is a bunch of helpers for working with 2-tuples.

**Note 1:** For more complex data, it is best to switch to records. So instead
of representing a 3D point as `(3,4,5)` and not having any helper functions,
represent it as `{ x = 3, y = 4, z = 5 }` and use all the built-in record
syntax!

**Note 2:** If your record contains a bunch of `Bool` and `Maybe` values,
you may want to upgrade to union types. Check out [Joël’s post][ut] for more
info on this. (Picking appropriate data structures is super important in Elm!)

[ut]: https://robots.thoughtbot.com/modeling-with-union-types

# Create
@docs pair

# Access
@docs first, second

# Map
@docs mapFirst, mapSecond, mapBoth

","unions":[],"aliases":[],"values":[{"name":"first","comment":" Extract the first value from a tuple.

    first (3, 4) == 3
    first ("john", "doe") == "john"
","type":"( a, b ) -> a"},{"name":"mapBoth","comment":" Transform both parts of a tuple.

    import String

    mapBoth String.reverse sqrt  ("stressed", 16) == ("desserts", 4)
    mapBoth String.length negate ("stressed", 16) == (8, -16)
","type":"(a -> x) -> (b -> y) -> ( a, b ) -> ( x, y )"},{"name":"mapFirst","comment":" Transform the first value in a tuple.

    import String

    mapFirst String.reverse ("stressed", 16) == ("desserts", 16)
    mapFirst String.length  ("stressed", 16) == (8, 16)
","type":"(a -> x) -> ( a, b ) -> ( x, b )"},{"name":"mapSecond","comment":" Transform the second value in a tuple.

    mapSecond sqrt   ("stressed", 16) == ("stressed", 4)
    mapSecond negate ("stressed", 16) == ("stressed", -16)
","type":"(b -> y) -> ( a, b ) -> ( a, y )"},{"name":"pair","comment":" Create a 2-tuple.

    -- pair 3 4 == (3, 4)

    zip : List a -> List b -> List (a, b)
    zip xs ys =
      List.map2 Tuple.pair xs ys
","type":"a -> b -> ( a, b )"},{"name":"second","comment":" Extract the second value from a tuple.

    second (3, 4) == 4
    second ("john", "doe") == "doe"
","type":"( a, b ) -> b"}],"binops":[]}]"""
            |> Json.Decode.decodeString (Json.Decode.list Elm.Docs.decoder)
        )
        |> Result.toMaybe
