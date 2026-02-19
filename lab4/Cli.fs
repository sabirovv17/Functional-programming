module WhatToCook.Cli

open System
open WhatToCook.Domain
open WhatToCook.Storage
open WhatToCook.MealDbApi
open WhatToCook.Scoring

let private color clr text =
    Console.ForegroundColor <- clr
    printf "%s" text
    Console.ResetColor()

let private colorLn clr text =
    Console.ForegroundColor <- clr
    printfn "%s" text
    Console.ResetColor()

let private header txt =
    printfn ""
    colorLn ConsoleColor.Cyan (sprintf "═══ %s ═══" txt)

let private prompt txt =
    color ConsoleColor.Yellow (sprintf "%s" txt)
    Console.ReadLine().Trim()

let private parseUnit (s: string) =
    match s.ToLowerInvariant() with
    | "g" | "gram" | "grams" -> Gram
    | "kg" | "kilogram" -> Kilogram
    | "ml" | "millilitre" -> Millilitre
    | "l" | "litre" | "liter" -> Litre
    | "pc" | "piece" | "pcs" -> Piece
    | other -> Custom other

let private formatUnit u =
    match u with
    | Gram -> "g" | Kilogram -> "kg" | Millilitre -> "ml"
    | Litre -> "l" | Piece -> "pc" | Custom s -> s

let private showPantry (pantry: Pantry) =
    header "🗄  КЛАДОВКА"
    if List.isEmpty pantry then
        printfn "  (пусто)"
    else
        pantry |> List.iteri (fun i item ->
            let expStr =
                match item.ExpiresAt with
                | Some d -> sprintf " (до %s)" (d.ToString("dd.MM.yyyy"))
                | None -> ""
            printfn "  %d. %s — %.1f %s%s" (i+1) item.Name item.Quantity (formatUnit item.Unit) expStr)

let private addItemDialog () : PantryItem option =
    let name = prompt "Название продукта (или пусто для отмены): "
    if String.IsNullOrWhiteSpace name then None
    else
        let qtyStr = prompt "Количество: "
        let qty = match Double.TryParse(qtyStr) with true, v -> v | _ -> 1.0
        let unitStr = prompt "Единица (g/kg/ml/l/pc или своя): "
        let unit = parseUnit unitStr
        let expStr = prompt "Срок годности (дд.мм.гггг или пусто): "
        let expires =
            match DateTime.TryParseExact(expStr, "dd.MM.yyyy", Globalization.CultureInfo.InvariantCulture, Globalization.DateTimeStyles.None) with
            | true, d -> Some d
            | _ -> None
        Some { Name = name; Quantity = qty; Unit = unit; ExpiresAt = expires }

let private pantryMenu (pantry: Pantry) : Pantry =
    let rec loop p =
        showPantry p
        printfn ""
        printfn "  [1] Добавить продукт"
        printfn "  [2] Удалить продукт"
        printfn "  [0] ← Назад"
        let choice = prompt "> "
        match choice with
        | "1" ->
            match addItemDialog () with
            | Some item ->
                let p' = addToPantry item p
                savePantry p'
                colorLn ConsoleColor.Green (sprintf "  ✓ %s добавлен" item.Name)
                loop p'
            | None -> loop p
        | "2" ->
            let name = prompt "Название для удаления: "
            let p' = removeFromPantry name p
            savePantry p'
            loop p'
        | "0" | "" -> p
        | _ -> loop p
    loop pantry

let private showExpiring (config: AppConfig) (pantry: Pantry) =
    header (sprintf "⏰  ЗАКОНЧИТСЯ СКОРО (в ближайшие %d дн.)" config.ExpiringSoonDays)
    let items = expiringSoon config.ExpiringSoonDays DateTime.Now pantry
    if List.isEmpty items then
        printfn "  Все продукты свежие 👍"
    else
        items |> List.iteri (fun i item ->
            let daysLeft =
                match item.ExpiresAt with
                | Some d -> (d - DateTime.Now).Days
                | None -> 999
            let clr = if daysLeft <= 1 then ConsoleColor.Red else ConsoleColor.DarkYellow
            color clr (sprintf "  %d. %s" (i+1) item.Name)
            printfn " — осталось %d дн." daysLeft)
    items

let private showScoredSection (title: string) (icon: string) (clr: ConsoleColor) (recipes: ScoredRecipe list) =
    if not (List.isEmpty recipes) then
        colorLn clr (sprintf "\n  %s %s" icon title)
        recipes |> List.iteri (fun i sr ->
            let missing =
                if List.isEmpty sr.MissingIngredients then ""
                else sprintf " | не хватает: %s" (String.Join(", ", sr.MissingIngredients))
            let expiring =
                if List.isEmpty sr.UsesExpiring then ""
                else sprintf " | 🔥 использует: %s" (String.Join(", ", sr.UsesExpiring))
            printfn "    %d. %s (совпало %d/%d%s%s)"
                (i+1) sr.Recipe.Title sr.MatchedCount
                (List.length sr.Recipe.Ingredients) missing expiring)

let private suggestRecipes (config: AppConfig) (pantry: Pantry) (cache: Cache) = async {
    header "🍳  ИДЕИ БЛЮД"
    if List.isEmpty pantry then
        printfn "  Сначала добавьте продукты в кладовку!"
        return [], cache
    else
        printfn "  Ищем рецепты..."
        let! (recipes, cache') = discoverRecipes config cache pantry 25
        let (canCook, almost, eatSoon) =
            classifyRecipes pantry config.ExpiringSoonDays DateTime.Now recipes

        showScoredSection "МОЖНО ПРИГОТОВИТЬ" "✅" ConsoleColor.Green canCook
        showScoredSection "ПОЧТИ МОЖНО (что докупить)" "🟡" ConsoleColor.Yellow almost
        showScoredSection "ЛУЧШЕ СЪЕСТЬ СЕЙЧАС" "🔥" ConsoleColor.Red eatSoon

        if List.isEmpty canCook && List.isEmpty almost && List.isEmpty eatSoon then
            printfn "  Не нашлось подходящих рецептов. Попробуйте добавить больше продуктов."

        let all = canCook @ eatSoon @ almost
        return all, cache'
}

let private showShoppingList (list: ShoppingList) =
    header "🛒  СПИСОК ПОКУПОК"
    if List.isEmpty list then
        printfn "  (пусто)"
    else
        list |> List.iteri (fun i item ->
            let check = if item.Checked then "☑" else "☐"
            let measStr = if String.IsNullOrWhiteSpace item.Measure then "" else sprintf " (%s)" item.Measure
            printfn "  %s %d. %s%s" check (i+1) item.Name measStr)

let private shoppingMenu (list: ShoppingList) : ShoppingList =
    let rec loop l =
        showShoppingList l
        printfn ""
        printfn "  [1] Отметить/снять позицию"
        printfn "  [2] Добавить позицию"
        printfn "  [3] Удалить отмеченные"
        printfn "  [0] ← Назад"
        let choice = prompt "> "
        match choice with
        | "1" ->
            let name = prompt "Название: "
            let l' = toggleShoppingItem name l
            saveShoppingList l'
            loop l'
        | "2" ->
            let name = prompt "Название: "
            let measure = prompt "Количество/мера: "
            let l' = addToShoppingList { Name = name; Measure = measure; Checked = false } l
            saveShoppingList l'
            loop l'
        | "3" ->
            let l' = removeChecked l
            saveShoppingList l'
            loop l'
        | "0" | "" -> l
        | _ -> loop l
    loop list

let private selectRecipesForShopping (scored: ScoredRecipe list) (shopList: ShoppingList) : ShoppingList =
    if List.isEmpty scored then shopList
    else
        printfn "\n  Введите номера рецептов через запятую (например: 1,3) или пусто для пропуска:"
        let input = prompt "  > "
        if String.IsNullOrWhiteSpace input then shopList
        else
            let indices =
                input.Split([| ','; ' '; ';' |], StringSplitOptions.RemoveEmptyEntries)
                |> Array.choose (fun s -> match Int32.TryParse(s.Trim()) with true, n -> Some (n-1) | _ -> None)
                |> Array.filter (fun i -> i >= 0 && i < List.length scored)
            let selected = indices |> Array.map (fun i -> scored.[i]) |> Array.toList
            let newItems = buildShoppingList selected
            let combined =
                newItems |> List.fold (fun acc item -> addToShoppingList item acc) shopList
            saveShoppingList combined
            colorLn ConsoleColor.Green (sprintf "  ✓ Добавлено %d позиций в список покупок" (List.length newItems))
            combined

let run (config: AppConfig) =
    let rec mainLoop (pantry: Pantry) (shopList: ShoppingList) (cache: Cache) =
        header "🏠  ЧТО ПРИГОТОВИТЬ?"
        printfn "  [1] 🗄  Кладовка"
        printfn "  [2] ⏰  Закончится скоро"
        printfn "  [3] 🍳  Предложить рецепты"
        printfn "  [4] 🛒  Список покупок"
        printfn "  [0] Выход"
        let choice = prompt "> "
        match choice with
        | "1" ->
            let pantry' = pantryMenu pantry
            mainLoop pantry' shopList cache
        | "2" ->
            let expItems = showExpiring config pantry
            if not (List.isEmpty expItems) then
                let ans = prompt "\n  Подобрать рецепты с этими продуктами? (д/н): "
                if ans.ToLowerInvariant().StartsWith("д") || ans.ToLowerInvariant().StartsWith("y") then
                    let (scored, cache') =
                        suggestRecipes config pantry cache |> Async.RunSynchronously
                    let shopList' = selectRecipesForShopping scored shopList
                    mainLoop pantry shopList' cache'
                else
                    mainLoop pantry shopList cache
            else
                mainLoop pantry shopList cache
        | "3" ->
            let (scored, cache') =
                suggestRecipes config pantry cache |> Async.RunSynchronously
            let shopList' = selectRecipesForShopping scored shopList
            mainLoop pantry shopList' cache'
        | "4" ->
            let shopList' = shoppingMenu shopList
            mainLoop pantry shopList' cache
        | "0" ->
            saveCache cache
            colorLn ConsoleColor.Cyan "  До встречи! 👋"
        | _ ->
            mainLoop pantry shopList cache

    let pantry = loadPantry ()
    let shopList = loadShoppingList ()
    let cache = loadCache ()
    mainLoop pantry shopList cache
