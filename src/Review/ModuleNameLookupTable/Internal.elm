module Review.ModuleNameLookupTable.Internal exposing (ModuleNameLookupTable(..), RangeLike, empty, fromDict, fromList, toRangeLike)

import Bitwise
import Dict exposing (Dict)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Range exposing (Range)


type ModuleNameLookupTable
    = ModuleNameLookupTable ModuleName (Dict RangeLike ModuleName)


empty : ModuleName -> ModuleNameLookupTable
empty currentModuleName =
    ModuleNameLookupTable currentModuleName Dict.empty


fromDict : ModuleName -> Dict RangeLike ModuleName -> ModuleNameLookupTable
fromDict =
    ModuleNameLookupTable


fromList : ModuleName -> List ( Range, ModuleName ) -> ModuleNameLookupTable
fromList fileModuleName entries =
    ModuleNameLookupTable fileModuleName
        (List.foldl
            (\( range, moduleOrigin ) soFar -> Dict.insert (toRangeLike range) moduleOrigin soFar)
            Dict.empty
            entries
        )


type alias RangeLike =
    ( Int, Int )


toRangeLike : Range -> RangeLike
toRangeLike { start, end } =
    -- TODO Optimization with elm-syntax v8
    -- They We could only look the at the start because it is not possible for 2 nodes to have the same position.
    --
    --   type alias RangeLike =
    --       Int
    --
    --   toRangeLike { start } =
    --       Bitwise.shiftLeftBy 16 start.row + start.column
    --
    -- Unfortunately with v7 this is not possible, because we do not have the position of the
    -- operator with `Expression.OperatorApplication`, creating a collision when looking for the
    -- module name for `+` in `a + b`, as that conflicts with the position for `a`.
    ( Bitwise.shiftLeftBy 16 start.row + start.column
    , Bitwise.shiftLeftBy 16 end.row + end.column
    )
