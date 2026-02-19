module WhatToCook.Storage

open System
open System.IO
open System.Text.Json
open System.Text.Json.Serialization
open WhatToCook.Domain

let private jsonOptions =
    let opts = JsonSerializerOptions(WriteIndented = true)
    opts.Converters.Add(JsonFSharpConverter())
    opts

let private dataDir =
    let dir = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), ".whattocook")
    Directory.CreateDirectory(dir) |> ignore
    dir

let private filePath name = Path.Combine(dataDir, name)

let private load<'T> (file: string) (fallback: 'T) : 'T =
    let path = filePath file
    if File.Exists(path) then
        try
            let json = File.ReadAllText(path)
            JsonSerializer.Deserialize<'T>(json, jsonOptions)
        with _ -> fallback
    else fallback

let private save<'T> (file: string) (data: 'T) =
    let json = JsonSerializer.Serialize(data, jsonOptions)
    File.WriteAllText(filePath file, json)

let loadPantry () : Pantry = load<Pantry> "pantry.json" []

let savePantry (pantry: Pantry) = save "pantry.json" pantry

let addToPantry (item: PantryItem) (pantry: Pantry) : Pantry =
    let norm = normaliseName item.Name
    let updated =
        pantry |> List.map (fun existing ->
            if normaliseName existing.Name = norm then
                { existing with
                    Quantity = existing.Quantity + item.Quantity
                    ExpiresAt = item.ExpiresAt |> Option.orElse existing.ExpiresAt }
            else existing)
    if updated |> List.exists (fun i -> normaliseName i.Name = norm) then
        updated
    else
        pantry @ [ item ]

let removeFromPantry (name: string) (pantry: Pantry) : Pantry =
    let norm = normaliseName name
    pantry |> List.filter (fun i -> normaliseName i.Name <> norm)

let loadShoppingList () : ShoppingList = load<ShoppingList> "shopping.json" []

let saveShoppingList (list: ShoppingList) = save "shopping.json" list

let addToShoppingList (item: ShoppingItem) (list: ShoppingList) : ShoppingList =
    let norm = normaliseName item.Name
    if list |> List.exists (fun i -> normaliseName i.Name = norm) then list
    else list @ [ item ]

let toggleShoppingItem (name: string) (list: ShoppingList) : ShoppingList =
    let norm = normaliseName name
    list |> List.map (fun i ->
        if normaliseName i.Name = norm then { i with Checked = not i.Checked }
        else i)

let removeChecked (list: ShoppingList) : ShoppingList =
    list |> List.filter (fun i -> not i.Checked)

type CacheEntry =
    { Json: string
      FetchedAt: DateTime }

type Cache = Map<string, CacheEntry>

let loadCache () : Cache = load<Cache> "cache.json" Map.empty

let saveCache (cache: Cache) = save "cache.json" cache

let tryGetCached (key: string) (ttlHours: int) (cache: Cache) : string option =
    cache
    |> Map.tryFind key
    |> Option.bind (fun entry ->
        if DateTime.UtcNow - entry.FetchedAt < TimeSpan.FromHours(float ttlHours)
        then Some entry.Json
        else None)

let putCache (key: string) (json: string) (cache: Cache) : Cache =
    cache |> Map.add key { Json = json; FetchedAt = DateTime.UtcNow }
