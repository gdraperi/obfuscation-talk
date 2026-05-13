open System
open dnlib.DotNet
open dnlib.DotNet.Emit

// Type which represent simple expression.
type Expr =
| Int32 of int32
| Add of Expr * Expr
| Sub of Expr * Expr
| Mul of Expr * Expr
| Div of Expr * Expr
| Mod of Expr * Expr

let rec generator fuel = 
    if fuel <= 0 then
        let value = Random.Shared.Next(100)
        (Int32 value, value)
    else
        let op = Random.Shared.Next(6)
        let (left, leftValue) = generator (fuel - 1)
        let (right, rightValue) = generator (fuel - 1)
        match (op, rightValue) with
        | (0, _) -> 
            let value = Random.Shared.Next(100)
            (Int32 value, value)
        | (1, _) -> (Add (left, right), leftValue + rightValue)
        | (2, _) -> (Sub (left, right), leftValue - rightValue)
        | (3, _) -> (Mul (left, right), leftValue * rightValue)
        | (4, x) when x <> 0 -> (Div (left, right), leftValue / rightValue)
        | (_, x) when x <> 0 -> (Mod (left, right), leftValue % rightValue)
        | (_, _) -> generator (fuel - 1)

let rec generateMethodBody (expr: Expr) (il: CilBody) =
    match expr with
    | Int32 value ->
        il.Instructions.Add(Instruction.Create(OpCodes.Ldc_I4, value))
    | Add (left, right) ->
        generateMethodBody left il
        generateMethodBody right il
        il.Instructions.Add(Instruction.Create(OpCodes.Add))
    | Sub (left, right) ->
        generateMethodBody left il
        generateMethodBody right il
        il.Instructions.Add(Instruction.Create(OpCodes.Sub))
    | Mul (left, right) ->
        generateMethodBody left il
        generateMethodBody right il
        il.Instructions.Add(Instruction.Create(OpCodes.Mul))
    | Div (left, right) ->
        generateMethodBody left il
        generateMethodBody right il
        il.Instructions.Add(Instruction.Create(OpCodes.Div))
    | Mod (left, right) ->
        generateMethodBody left il
        generateMethodBody right il
        il.Instructions.Add(Instruction.Create(OpCodes.Rem))

let (expr, value) = generator 3
let body = new CilBody()
generateMethodBody expr body

printfn "Expression"
printfn "%A" expr
printfn "Value"
printfn "%d" value

body.ToString() |> printfn "IL Code\n%s"
for instr in body.Instructions do
    printfn "%s" (instr.ToString())