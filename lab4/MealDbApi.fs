module WhatToCook.MealDbApi

open System
open System.Net.Http
open System.Text.Json
open WhatToCook.Domain
open WhatToCook.Storage

let private httpClient = new HttpClient()

let private fetchJson (url: string) = async {
    let! response = httpClient.GetStringAsync(url) |> Async.AwaitTask
    return response
}

let private parseFilterResponse (json: string) : (string * string * string) list =
    try
        let doc = JsonDocument.Parse(json)
        let root = doc.RootElement
        match root.TryGetProperty("meals") with
        | true, meals when meals.ValueKind = JsonValueKind.Array ->
            [ for m in meals.EnumerateArray() do
                let id = m.GetProperty("idMeal").GetString()
                let name = m.GetProperty("strMeal").GetString()
                let thumb = m.GetProperty("strMealThumb").GetString()
                yield (id, name, thumb) ]
        | _ -> []
    with _ -> []

let private parseLookupResponse (json: string) : Recipe option =
    try
        let doc = JsonDocument.Parse(json)
        let root = doc.RootElement
        match root.TryGetProperty("meals") with
        | true, meals when meals.ValueKind = JsonValueKind.Array ->
            let arr = meals.EnumerateArray() |> Seq.tryHead
            arr |> Option.map (fun m ->
                let str (prop: string) =
                    match m.TryGetProperty(prop) with
                    | true, v when v.ValueKind = JsonValueKind.String -> v.GetString()
                    | _ -> ""
                let ingredients =
                    [ for i in 1..20 do
                        let ingr = str (sprintf "strIngredient%d" i)
                        let meas = str (sprintf "strMeasure%d" i)
                        if not (String.IsNullOrWhiteSpace ingr) then
                            yield { Name = ingr.Trim(); Measure = meas.Trim() } ]
                { Id = str "idMeal"
                  Title = str "strMeal"
                  Category = str "strCategory"
                  Area = str "strArea"
                  Thumbnail = str "strMealThumb"
                  Ingredients = ingredients
                  Instructions = str "strInstructions" })
        | _ -> None
    with _ -> None

let searchByIngredient (baseUrl: string) (cache: Cache) (ttlHours: int) (ingredient: string) = async {
    let key = sprintf "filter:%s" (normaliseName ingredient)
    match tryGetCached key ttlHours cache with
    | Some json ->
        return parseFilterResponse json, cache
    | None ->
        let url = sprintf "%s/filter.php?i=%s" baseUrl (Uri.EscapeDataString ingredient)
        let! json = fetchJson url
        let cache' = putCache key json cache
        return parseFilterResponse json, cache'
}

let getRecipeDetails (baseUrl: string) (cache: Cache) (ttlHours: int) (mealId: string) = async {
    let key = sprintf "lookup:%s" mealId
    match tryGetCached key ttlHours cache with
    | Some json ->
        return parseLookupResponse json, cache
    | None ->
        let url = sprintf "%s/lookup.php?i=%s" baseUrl mealId
        let! json = fetchJson url
        let cache' = putCache key json cache
        return parseLookupResponse json, cache'
}

let discoverRecipes (config: AppConfig) (cache: Cache) (pantry: Pantry) (topN: int) = async {
    let ingredients = pantry |> List.map (fun i -> i.Name)

    let mutable currentCache = cache
    let mutable mealHits : Map<string, int> = Map.empty

    for ingr in ingredients do
        let! (meals, c) = searchByIngredient config.MealDbBaseUrl currentCache config.CacheHours ingr
        currentCache <- c
        for (id, _, _) in meals do
            mealHits <-
                mealHits
                |> Map.tryFind id
                |> Option.defaultValue 0
                |> fun count -> Map.add id (count + 1) mealHits

    let topIds =
        mealHits
        |> Map.toList
        |> List.sortByDescending snd
        |> List.truncate topN
        |> List.map fst

    let mutable recipes : Recipe list = []
    for id in topIds do
        let! (recipeOpt, c) = getRecipeDetails config.MealDbBaseUrl currentCache config.CacheHours id
        currentCache <- c
        match recipeOpt with
        | Some r -> recipes <- recipes @ [ r ]
        | None -> ()

    return recipes, currentCache
}
