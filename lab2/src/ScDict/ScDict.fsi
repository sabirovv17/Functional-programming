namespace ScDict

type ScDict<'Key, 'Value when 'Key: equality>

module ScDict =
    val empty<'Key, 'Value when 'Key: equality> : ScDict<'Key, 'Value>
    val emptyWithCapacity : int -> ScDict<'Key, 'Value>
    val isEmpty : ScDict<'Key, 'Value> -> bool
    val size : ScDict<'Key, 'Value> -> int
    val containsKey : 'Key -> ScDict<'Key, 'Value> -> bool
    val tryFind : 'Key -> ScDict<'Key, 'Value> -> 'Value option
    val add : 'Key -> 'Value -> ScDict<'Key, 'Value> -> ScDict<'Key, 'Value>
    val remove : 'Key -> ScDict<'Key, 'Value> -> ScDict<'Key, 'Value>
    val filter : ('Key -> 'Value -> bool) -> ScDict<'Key, 'Value> -> ScDict<'Key, 'Value>
    val map : ('Key -> 'Value -> 'Value2) -> ScDict<'Key, 'Value> -> ScDict<'Key, 'Value2>
    val foldLeft : ('State -> 'Key -> 'Value -> 'State) -> 'State -> ScDict<'Key, 'Value> -> 'State
    val foldRight : ('Key -> 'Value -> 'State -> 'State) -> ScDict<'Key, 'Value> -> 'State -> 'State
    val toSeq : ScDict<'Key, 'Value> -> seq<'Key * 'Value>
    val ofSeq : seq<'Key * 'Value> -> ScDict<'Key, 'Value>
    val append : ScDict<'Key, 'Value> -> ScDict<'Key, 'Value> -> ScDict<'Key, 'Value>
    val equals : ScDict<'Key, 'Value> -> ScDict<'Key, 'Value> -> bool when 'Value: equality
