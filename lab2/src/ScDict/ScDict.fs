namespace ScDict

open System

type ScDict<'Key, 'Value when 'Key: equality> =
    private {
        BucketCount: int
        Buckets: ('Key * 'Value) list array
        Count: int
    }

module ScDict =
    let private defaultCapacity = 16

    let private safeCapacity capacity =
        if capacity < 1 then 1 else capacity

    let emptyWithCapacity capacity =
        let bucketCount = safeCapacity capacity
        {
            BucketCount = bucketCount
            Buckets = Array.init bucketCount (fun _ -> [])
            Count = 0
        }

    let empty<'Key, 'Value when 'Key: equality> : ScDict<'Key, 'Value> =
        emptyWithCapacity defaultCapacity

    let isEmpty dict = dict.Count = 0

    let size dict = dict.Count

    let private bucketIndex bucketCount key =
        (hash key &&& Int32.MaxValue) % bucketCount

    let private findInBucket key bucket =
        bucket
        |> List.tryFind (fun (k, _) -> k = key)
        |> Option.map snd

    let tryFind key dict =
        let index = bucketIndex dict.BucketCount key
        findInBucket key dict.Buckets[index]

    let containsKey key dict =
        tryFind key dict |> Option.isSome

    let private upsertInBucket key value bucket =
        let rec loop acc rest =
            match rest with
            | [] -> (List.rev ((key, value) :: acc), false)
            | (k, _) :: tail when k = key ->
                (List.rev acc @ ((key, value) :: tail), true)
            | head :: tail -> loop (head :: acc) tail
        loop [] bucket

    let private removeFromBucket key bucket =
        let rec loop acc rest =
            match rest with
            | [] -> (List.rev acc, false)
            | (k, _) :: tail when k = key ->
                (List.rev acc @ tail, true)
            | head :: tail -> loop (head :: acc) tail
        loop [] bucket

    let add key value dict =
        let index = bucketIndex dict.BucketCount key
        let bucket = dict.Buckets[index]
        let newBucket, replaced = upsertInBucket key value bucket
        let newCount = if replaced then dict.Count else dict.Count + 1
        let newBuckets =
            dict.Buckets
            |> Array.mapi (fun i current -> if i = index then newBucket else current)
        { dict with Buckets = newBuckets; Count = newCount }

    let remove key dict =
        let index = bucketIndex dict.BucketCount key
        let bucket = dict.Buckets[index]
        let newBucket, removed = removeFromBucket key bucket
        if not removed then dict
        else
            let newBuckets =
                dict.Buckets
                |> Array.mapi (fun i current -> if i = index then newBucket else current)
            { dict with Buckets = newBuckets; Count = dict.Count - 1 }

    let toSeq dict =
        dict.Buckets
        |> Seq.collect Seq.ofList

    let foldLeft folder state dict =
        dict.Buckets
        |> Array.fold (fun acc bucket ->
            List.fold (fun innerAcc (k, v) -> folder innerAcc k v) acc bucket) state

    let foldRight (folder: 'Key -> 'Value -> 'State -> 'State) (dict: ScDict<'Key, 'Value>) (state: 'State) : 'State =
        dict
        |> toSeq
        |> Seq.rev
        |> Seq.fold (fun acc (k, v) -> folder k v acc) state

    let map mapper dict =
        let newBuckets =
            dict.Buckets
            |> Array.map (List.map (fun (k, v) -> k, mapper k v))
        {
            BucketCount = dict.BucketCount
            Buckets = newBuckets
            Count = dict.Count
        }

    let filter predicate dict =
        let newBuckets =
            dict.Buckets
            |> Array.map (List.filter (fun (k, v) -> predicate k v))
        let newCount = newBuckets |> Array.sumBy List.length
        {
            BucketCount = dict.BucketCount
            Buckets = newBuckets
            Count = newCount
        }

    let ofSeq pairs =
        pairs
        |> Seq.fold (fun acc (k, v) -> add k v acc) empty

    let append left right =
        foldLeft (fun acc k v -> add k v acc) left right

    let equals left right =
        if left.Count <> right.Count then false
        else
            foldLeft (fun acc k v -> acc && tryFind k right = Some v) true left
