open System
open System.Globalization
open Interp.Core

type Options =
	{ UseLinear: bool
	  UseNewton: bool
	  NewtonN: int
	  Step: float }

let private usage () =
	[ "Usage: interp --linear [--newton --n N] --step STEP"
	  "  --linear           enable linear interpolation"
	  "  --newton           enable Newton interpolation"
	  "  --n N              window size for Newton (default 4)"
	  "  --step STEP        sampling step (positive)" ]
	|> String.concat Environment.NewLine

let private parseArgs (args: string array) =
	let rec loop opts rest =
		match rest with
		| [] -> Ok opts
		| "--linear" :: tail -> loop { opts with UseLinear = true } tail
		| "--newton" :: tail -> loop { opts with UseNewton = true } tail
		| "--n" :: value :: tail ->
			match Int32.TryParse value with
			| true, n when n >= 2 -> loop { opts with NewtonN = n } tail
			| _ -> Error "--n expects integer >= 2"
		| "--step" :: value :: tail ->
			match Double.TryParse(value, NumberStyles.Float, CultureInfo.InvariantCulture) with
			| true, step when step > 0.0 -> loop { opts with Step = step } tail
			| _ -> Error "--step expects positive float"
		| unknown :: _ -> Error (sprintf "Unknown option: %s" unknown)

	let defaults = { UseLinear = false; UseNewton = false; NewtonN = 4; Step = 0.0 }
	loop defaults (args |> Array.toList)
	|> Result.bind (fun opts ->
		if opts.Step <= 0.0 then Error "--step is required"
		elif not opts.UseLinear && not opts.UseNewton then Error "Select --linear and/or --newton"
		else Ok opts)

let private tryParsePoint (line: string) =
	let parts =
		line.Split([| ';'; '\t'; ','; ' ' |], StringSplitOptions.RemoveEmptyEntries)
		|> Array.toList
	match parts with
	| [ x; y ] ->
		let okX, xVal = Double.TryParse(x, NumberStyles.Float, CultureInfo.InvariantCulture)
		let okY, yVal = Double.TryParse(y, NumberStyles.Float, CultureInfo.InvariantCulture)
		if okX && okY then Some { X = xVal; Y = yVal } else None
	| _ -> None

let private readPoints () =
	Seq.unfold (fun () ->
		match Console.ReadLine() with
		| null -> None
		| line -> Some(line, ())) ()
	|> Seq.choose (fun line ->
		let trimmed = line.Trim()
		if String.IsNullOrWhiteSpace trimmed then None
		else tryParsePoint trimmed)

let private printOutput label (point: Point) =
	printfn "%s: %s %s" label (point.X.ToString("G", CultureInfo.InvariantCulture)) (point.Y.ToString("G", CultureInfo.InvariantCulture))

[<EntryPoint>]
let main args =
	match parseArgs args with
	| Error message ->
		eprintfn "%s" message
		eprintfn "%s" (usage())
		1
	| Ok opts ->
		let points = readPoints ()
		let mutable linearState = Streaming.initialLinear opts.Step
		let mutable newtonState = Streaming.initialNewton opts.Step opts.NewtonN

		for point in points do
			if opts.UseLinear then
				let nextState, outputs = Streaming.updateLinear linearState point
				linearState <- nextState
				outputs |> List.iter (printOutput "linear")

			if opts.UseNewton then
				let nextState, outputs = Streaming.updateNewton newtonState point
				newtonState <- nextState
				outputs |> List.iter (printOutput "newton")
		0
