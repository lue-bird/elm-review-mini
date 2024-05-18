module Diff exposing (Change(..), diff)

{-| Derived from (edited) <https://github.com/jinjor/elm-diff/>

The following is the original license

---

Copyright (c) 2016-present, Yosuke Torii
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

  - Redistributions of source code must retain the above copyright notice, this
    list of conditions and the following disclaimer.

  - Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.

  - Neither the name of elm-diff nor the names of its
    contributors may be used to endorse or promote products derived from
    this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

---

Compares two list and returns how they have changed.
Each function internally uses Wu's [O(NP) algorithm](http://myerslab.mpi-cbg.de/wp-content/uploads/2014/06/np_diff.pdf).

@docs Change, diff

-}

import Array exposing (Array)


{-| How each line has changed plus its value
-}
type Change a
    = Added a
    | Removed a
    | NoChange a


type StepResult
    = Continue (Array (List ( Int, Int )))
    | Found (List ( Int, Int ))


type BugReport
    = CannotGetA
    | CannotGetB
    | UnexpectedPath


{-| Compares general lists.

    diff [ 1, 3 ] [ 2, 3 ]
    --> [ Removed 1, Added 2, NoChange 3 ]

-}
diff : List a -> List a -> List (Change a)
diff a b =
    case testDiff a b of
        Ok changes ->
            changes

        Err _ ->
            []


{-| Test the algorithm itself.
If it returns Err, it should be a bug.
-}
testDiff : List a -> List a -> Result BugReport (List (Change a))
testDiff a b =
    arrayTestDiff (a |> Array.fromList) (b |> Array.fromList)


arrayTestDiff : Array a -> Array a -> Result BugReport (List (Change a))
arrayTestDiff arrA arrB =
    let
        -- Elm's Array doesn't allow null element,
        -- so we'll use shifted index to access source.
        getA : Int -> Maybe a
        getA =
            \x -> Array.get (x - 1) arrA

        getB : Int -> Maybe a
        getB =
            \y -> Array.get (y - 1) arrB

        aLength : Int
        aLength =
            Array.length arrA

        bLength : Int
        bLength =
            Array.length arrB
    in
    makeChanges
        getA
        getB
        (onpLoopP (snake getA getB)
            (bLength - aLength)
            aLength
            0
            (Array.initialize (aLength + bLength + 1) (\_ -> []))
        )


makeChanges :
    (Int -> Maybe a)
    -> (Int -> Maybe a)
    -> List ( Int, Int )
    -> Result BugReport (List (Change a))
makeChanges getA getB path =
    case path of
        [] ->
            Ok []

        latest :: tail ->
            makeChangesHelp [] getA getB latest tail


makeChangesHelp :
    List (Change a)
    -> (Int -> Maybe a)
    -> (Int -> Maybe a)
    -> ( Int, Int )
    -> List ( Int, Int )
    -> Result BugReport (List (Change a))
makeChangesHelp changes getA getB ( x, y ) path =
    -- IGNORE TCO
    case path of
        [] ->
            Ok changes

        ( prevX, prevY ) :: tail ->
            let
                makeChangesHelpWith : Change a -> Result BugReport (List (Change a))
                makeChangesHelpWith c =
                    makeChangesHelp (c :: changes) getA getB ( prevX, prevY ) tail
            in
            if x - 1 == prevX && y - 1 == prevY then
                case getA x of
                    Just a ->
                        NoChange a |> makeChangesHelpWith

                    Nothing ->
                        Err CannotGetA

            else if x == prevX then
                case getB y of
                    Just b ->
                        Added b |> makeChangesHelpWith

                    Nothing ->
                        Err CannotGetB

            else if y == prevY then
                case getA x of
                    Just a ->
                        Removed a |> makeChangesHelpWith

                    Nothing ->
                        Err CannotGetA

            else
                Err UnexpectedPath



-- Myers's O(ND) algorithm (http://www.xmailserver.org/diff2.pdf)


snake :
    (Int -> Maybe a)
    -> (Int -> Maybe a)
    -> Int
    -> Int
    -> List ( Int, Int )
    -> ( List ( Int, Int ), Bool )
snake getA getB nextX nextY path =
    case ( getA nextX, getB nextY ) of
        ( Just a, Just b ) ->
            if a == b then
                snake
                    getA
                    getB
                    (nextX + 1)
                    (nextY + 1)
                    (( nextX, nextY ) :: path)

            else
                ( path, False )

        -- reached bottom-right corner
        ( Nothing, Nothing ) ->
            ( path, True )

        _ ->
            ( path, False )



-- Wu's O(NP) algorithm (http://myerslab.mpi-cbg.de/wp-content/uploads/2014/06/np_diff.pdf)


onpLoopP :
    (Int -> Int -> List ( Int, Int ) -> ( List ( Int, Int ), Bool ))
    -> Int
    -> Int
    -> Int
    -> Array (List ( Int, Int ))
    -> List ( Int, Int )
onpLoopP snake_ delta offset p v =
    let
        ks : List Int
        ks =
            if delta > 0 then
                List.reverse (List.range (delta + 1) (delta + p))
                    ++ List.range -p delta

            else
                List.reverse (List.range (delta + 1) p)
                    ++ List.range (-p + delta) delta
    in
    case onpLoopK snake_ offset ks v of
        Found path ->
            path

        Continue v_ ->
            onpLoopP snake_ delta offset (p + 1) v_


onpLoopK :
    (Int -> Int -> List ( Int, Int ) -> ( List ( Int, Int ), Bool ))
    -> Int
    -> List Int
    -> Array (List ( Int, Int ))
    -> StepResult
onpLoopK snake_ offset ks v =
    case ks of
        [] ->
            Continue v

        k :: ks_ ->
            case step snake_ offset k v of
                Found path ->
                    Found path

                Continue v_ ->
                    onpLoopK snake_ offset ks_ v_


step :
    (Int -> Int -> List ( Int, Int ) -> ( List ( Int, Int ), Bool ))
    -> Int
    -> Int
    -> Array (List ( Int, Int ))
    -> StepResult
step snake_ offset k v =
    let
        fromLeft : List ( Int, Int )
        fromLeft =
            Maybe.withDefault [] (Array.get (k - 1 + offset) v)

        fromTop : List ( Int, Int )
        fromTop =
            Maybe.withDefault [] (Array.get (k + 1 + offset) v)

        ( path, ( x, y ) ) =
            case fromLeft of
                [] ->
                    case fromTop of
                        [] ->
                            ( [], ( 0, 0 ) )

                        ( topX, topY ) :: topTail ->
                            ( ( topX, topY ) :: topTail, ( topX + 1, topY ) )

                ( leftX, leftY ) :: leftTail ->
                    case fromTop of
                        [] ->
                            ( ( leftX, leftY ) :: leftTail, ( leftX, leftY + 1 ) )

                        ( topX, topY ) :: topTail ->
                            -- this implies "remove" comes always earlier than "add"
                            if leftY + 1 >= topY then
                                ( ( leftX, leftY ) :: leftTail, ( leftX, leftY + 1 ) )

                            else
                                ( ( topX, topY ) :: topTail, ( topX + 1, topY ) )

        ( newPath, goal ) =
            snake_ (x + 1) (y + 1) (( x, y ) :: path)
    in
    if goal then
        Found newPath

    else
        Continue (Array.set (k + offset) newPath v)
