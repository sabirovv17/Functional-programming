module WhatToCook.Domain

open System

type Unit =
    | Gram
    | Kilogram
    | Millilitre
    | Litre
    | Piece
    | Custom of string

type PantryItem =
    { Name: string
      Quantity: float
      Unit: Unit
      ExpiresAt: DateTime option }

type Pantry = PantryItem list

type Ingredient =
    { Name: string
      Measure: string }

type Recipe =
    { Id: string
      Title: string
      Category: string
      Area: string
      Thumbnail: string
      Ingredients: Ingredient list
      Instructions: string }

type MatchKind =
    | CanCook
    | AlmostCanCook
    | EatSoonPriority

type ScoredRecipe =
    { Recipe: Recipe
      Score: float
      MatchedCount: int
      MissingIngredients: string list
      UsesExpiring: string list
      Kind: MatchKind }

type ShoppingItem =
    { Name: string
      Measure: string
      Checked: bool }

type ShoppingList = ShoppingItem list

type AppConfig =
    { MealDbBaseUrl: string
      CacheHours: int
      ExpiringSoonDays: int }

let defaultConfig =
    { MealDbBaseUrl = "https://www.themealdb.com/api/json/v1/1"
      CacheHours = 6
      ExpiringSoonDays = 7 }

let normaliseName (s: string) =
    s.Trim().ToLowerInvariant()

let fuzzyContains (pantrySet: Set<string>) (ingredientName: string) : bool =
    let norm = normaliseName ingredientName
    pantrySet |> Set.exists (fun p ->
        norm.Contains(p) || p.Contains(norm))

let expiringSoon (days: int) (now: DateTime) (pantry: Pantry) : Pantry =
    pantry
    |> List.filter (fun item ->
        match item.ExpiresAt with
        | Some exp -> exp <= now.AddDays(float days) && exp >= now
        | None -> false)

let pantryNames (pantry: Pantry) : Set<string> =
    pantry |> List.map (fun i -> normaliseName i.Name) |> Set.ofList
