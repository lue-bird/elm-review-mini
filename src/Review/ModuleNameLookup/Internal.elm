module Review.ModuleNameLookup.Internal exposing (ModuleNameLookup(..), add, empty, fromList, toRangeLike)

import Bitwise
import Dict exposing (Dict)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Range exposing (Range)


type ModuleNameLookup
    = ModuleNameLookup ModuleName (Dict RangeLike ModuleName)


type alias RangeLike =
    Int


empty : ModuleName -> ModuleNameLookup
empty currentModuleName =
    ModuleNameLookup currentModuleName Dict.empty


fromList : ModuleName -> List ( Range, ModuleName ) -> ModuleNameLookup
fromList fileModuleName list =
    List.foldl
        (\( range, moduleName ) acc -> add range moduleName acc)
        (empty fileModuleName)
        list


add : Range -> ModuleName -> ModuleNameLookup -> ModuleNameLookup
add range moduleName (ModuleNameLookup currentModuleName moduleNameLookup) =
    ModuleNameLookup currentModuleName (Dict.insert (toRangeLike range) moduleName moduleNameLookup)


toRangeLike : Range -> RangeLike
toRangeLike { start } =
    -- We are able to only take a look the start because the lookup table because it is not possible for 2 nodes
    -- that can have a module name to have the same start position. This does have the tradeoff that when a lookup
    -- is done on a node that can't have a module name (like `A.a + B.b`) that a module name will be returned, but
    -- that would be a misuse of the API.
    Bitwise.shiftLeftBy 16 start.row + start.column
