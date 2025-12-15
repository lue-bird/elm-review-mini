module Review.ModuleNameLookupTable.Builder exposing
    ( ModuleNameLookupTableBuilder(..)
    , add
    , empty
    , finalize
    )

import Dict exposing (Dict)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Range exposing (Range)
import Review.ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.ModuleNameLookupTable.Internal as Internal


type ModuleNameLookupTableBuilder
    = ModuleNameLookupTableBuilder (Dict Internal.RangeLike ModuleName)


empty : ModuleNameLookupTableBuilder
empty =
    ModuleNameLookupTableBuilder Dict.empty


add : Range -> ModuleName -> ModuleNameLookupTableBuilder -> ModuleNameLookupTableBuilder
add range moduleName (ModuleNameLookupTableBuilder builder) =
    ModuleNameLookupTableBuilder (Dict.insert (Internal.toRangeLike range) moduleName builder)


finalize : ModuleName -> ModuleNameLookupTableBuilder -> ModuleNameLookupTable
finalize fileModuleName (ModuleNameLookupTableBuilder builder) =
    Internal.fromDict fileModuleName builder
