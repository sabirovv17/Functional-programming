module WhatToCook.Scoring

open System
open WhatToCook.Domain

let scoreRecipe (pantrySet: Set<string>) (expiringSet: Set<string>) (recipe: Recipe) : ScoredRecipe =
    let recipeIngredients =
        recipe.Ingredients |> List.map (fun i -> normaliseName i.Name)

    let total = float (List.length recipeIngredients)
    let matched =
        recipeIngredients |> List.filter (fun n -> Set.contains n pantrySet)
    let missing =
        recipeIngredients |> List.filter (fun n -> not (Set.contains n pantrySet))
    let usesExpiring =
        recipeIngredients |> List.filter (fun n -> Set.contains n expiringSet)

    let matchedCount = List.length matched
    let missingCount = List.length missing

    let coverage = if total > 0.0 then float matchedCount / total else 0.0
    let expiryBonus = min 0.6 (float (List.length usesExpiring) * 0.2)
    let missingPenalty = float missingCount * 0.1
    let score = coverage + expiryBonus - missingPenalty

    let kind =
        if missingCount = 0 then CanCook
        elif List.length usesExpiring > 0 then EatSoonPriority
        elif missingCount <= 3 then AlmostCanCook
        else AlmostCanCook

    { Recipe = recipe
      Score = score
      MatchedCount = matchedCount
      MissingIngredients = missing
      UsesExpiring = usesExpiring
      Kind = kind }

let classifyRecipes
    (pantry: Pantry)
    (expiringSoonDays: int)
    (now: DateTime)
    (recipes: Recipe list)
    : ScoredRecipe list * ScoredRecipe list * ScoredRecipe list =

    let pantrySet = pantryNames pantry
    let expiringItems = expiringSoon expiringSoonDays now pantry
    let expiringSet = pantryNames expiringItems

    let scored =
        recipes
        |> List.map (scoreRecipe pantrySet expiringSet)
        |> List.sortByDescending (fun sr -> sr.Score)

    let canCook =
        scored |> List.filter (fun sr -> sr.Kind = CanCook)

    let almostCanCook =
        scored
        |> List.filter (fun sr ->
            sr.Kind = AlmostCanCook && sr.MissingIngredients.Length <= 3)

    let eatSoon =
        scored |> List.filter (fun sr -> sr.Kind = EatSoonPriority)

    (canCook, almostCanCook, eatSoon)

let buildShoppingList (selected: ScoredRecipe list) : ShoppingList =
    selected
    |> List.collect (fun sr ->
        sr.MissingIngredients
        |> List.map (fun name ->
            let measure =
                sr.Recipe.Ingredients
                |> List.tryFind (fun i -> normaliseName i.Name = name)
                |> Option.map (fun i -> i.Measure)
                |> Option.defaultValue ""
            { Name = name; Measure = measure; Checked = false }))
    |> List.distinctBy (fun i -> normaliseName i.Name)
