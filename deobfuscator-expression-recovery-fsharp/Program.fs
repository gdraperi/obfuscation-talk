open System
open dnlib.DotNet
open dnlib.DotNet.Emit

type Expr =
    | StringLiteral of string
    | IntLiteral of int
    | Var of string
    | InstructionInvocation of Emit.Instruction
    | Add of Expr * Expr
    | Sub of Expr * Expr
    | Mul of Expr * Expr
    | Div of Expr * Expr

type BasicBlock =
    { Instructions: Emit.Instruction list }

type FlowGraph =
    { BasicBlocks: BasicBlock list }

let buildFlowGraph (method: MethodDef) =
    let mutable basicBlocksStart = [0]
    let mutable skipNextInstruction = false
    for i = 1 to method.Body.Instructions.Count - 1 do
        if skipNextInstruction then
            skipNextInstruction <- false
        else
            let instr = method.Body.Instructions[i]
            if (instr.IsBr() || instr.IsConditionalBranch() || instr.OpCode = OpCodes.Ret) then
                if (instr.IsConditionalBranch()) then
                    let instructionIndex = method.Body.Instructions.IndexOf(instr.Operand :?> Emit.Instruction)
                    basicBlocksStart <- basicBlocksStart @ [instructionIndex]

                if (i + 1 < method.Body.Instructions.Count) then
                    basicBlocksStart <- basicBlocksStart @ [i + 1]
                    skipNextInstruction <- true
    let basicBlocksStart = basicBlocksStart |> List.distinct |> List.sort
    let mutable blocks = []
    basicBlocksStart |> List.iteri(fun i startIndex ->
        let finish = 
            if i = basicBlocksStart.Length - 1 then
                method.Body.Instructions.Count 
            else basicBlocksStart[i + 1];
        let mutable instructions = []
        for j = startIndex to finish - 1 do
            instructions <- instructions @ [method.Body.Instructions[j]]

        blocks <- blocks @ [{ Instructions = instructions }]
    )
    { BasicBlocks = blocks }

let saveMethod (method: MethodDef) (graph: FlowGraph)=
    let body = method.Body
    body.Instructions.Clear()
    graph.BasicBlocks |> List.iter(fun block ->
        block.Instructions |> List.iter(fun instr ->
            body.Instructions.Add(instr) |> ignore
        )
    )
    //body.KeepOldMaxStack <- true
    ()

let isLdcI4 (instruction: Instruction) =
    instruction.IsLdcI4()

let simplifyBasicBlock (block: BasicBlock) =
    let instructions = block.Instructions
    let mutable simplifiedInstructions: Instruction list = []
    for instr in instructions do
        if instr.OpCode = OpCodes.Add then
            let op1 = simplifiedInstructions[simplifiedInstructions.Length - 1]
            let op2 = simplifiedInstructions[simplifiedInstructions.Length - 2]
            if isLdcI4 op1 && isLdcI4 op2 then
                let op1Value = Convert.ToInt32(op1.Operand)
                let op2Value = Convert.ToInt32(op2.Operand)
                op2.Operand <- op2Value + op1Value 
                simplifiedInstructions <- (simplifiedInstructions |> List.take (simplifiedInstructions.Length - 1))
            else
                simplifiedInstructions <- simplifiedInstructions @ [instr]
        elif instr.OpCode = OpCodes.Sub then
            let op1 = simplifiedInstructions[simplifiedInstructions.Length - 1]
            let op2 = simplifiedInstructions[simplifiedInstructions.Length - 2]
            if isLdcI4 op1 && isLdcI4 op2 then
                let op1Value = Convert.ToInt32(op1.Operand)
                let op2Value = Convert.ToInt32(op2.Operand)
                op2.Operand <- op2Value - op1Value
                simplifiedInstructions <- (simplifiedInstructions |> List.take (simplifiedInstructions.Length - 1))
            else
                simplifiedInstructions <- simplifiedInstructions @ [instr]
        elif instr.OpCode = OpCodes.Mul then
            let op1 = simplifiedInstructions[simplifiedInstructions.Length - 1]
            let op2 = simplifiedInstructions[simplifiedInstructions.Length - 2]
            if isLdcI4 op1 && isLdcI4 op2 then
                let op1Value = Convert.ToInt32(op1.Operand)
                let op2Value = Convert.ToInt32(op2.Operand)
                op2.Operand <- op2Value * op1Value
                simplifiedInstructions <- (simplifiedInstructions |> List.take (simplifiedInstructions.Length - 1))
            else
                simplifiedInstructions <- simplifiedInstructions @ [instr]
        elif instr.OpCode = OpCodes.Div then
            let op1 = simplifiedInstructions[simplifiedInstructions.Length - 1]
            let op2 = simplifiedInstructions[simplifiedInstructions.Length - 2]
            if isLdcI4 op1 && isLdcI4 op2 then
                let op1Value = Convert.ToInt32(op1.Operand)
                let op2Value = Convert.ToInt32(op2.Operand)
                op2.Operand <- op2Value / op1Value
                simplifiedInstructions <- (simplifiedInstructions |> List.take (simplifiedInstructions.Length - 1))
            else
                simplifiedInstructions <- simplifiedInstructions @ [instr]
        elif instr.OpCode = OpCodes.Rem then
            let op1 = simplifiedInstructions[simplifiedInstructions.Length - 1]
            let op2 = simplifiedInstructions[simplifiedInstructions.Length - 2]
            if isLdcI4 op1 && isLdcI4 op2 then
                let op1Value = Convert.ToInt32(op1.Operand)
                let op2Value = Convert.ToInt32(op2.Operand)
                op2.Operand <- op2Value % op1Value
                simplifiedInstructions <- (simplifiedInstructions |> List.take (simplifiedInstructions.Length - 1))
            else
                simplifiedInstructions <- simplifiedInstructions @ [instr]
        elif instr.OpCode = OpCodes.Call then
            let op1 = simplifiedInstructions[simplifiedInstructions.Length - 1]
            let method = instr.Operand :?> dnlib.DotNet.IMethodDefOrRef
            if method.FullName = "System.Int32 System.Math::Abs(System.Int32)" then
                let op1Value = Convert.ToInt32(op1.Operand)
                op1.Operand <- Math.Abs(op1Value)
            else
                simplifiedInstructions <- simplifiedInstructions @ [instr]
        else
            simplifiedInstructions <- simplifiedInstructions @ [instr]
    { Instructions = simplifiedInstructions }

[<EntryPoint>]
let main args =
    if args.Length < 1 then
        Console.WriteLine("deobfuscator-class-renaming mdfile targetfile")
        1
    else
        let assemblyFile = args[0]
        let targetFile = args[1]
        let modCtx = ModuleDef.CreateModuleContext()
        let module_ = ModuleDefMD.Load(assemblyFile, modCtx)
        for type_ in module_.Types do
            if UTF8String.ToSystemString type_.Name <> "<Module>" then
                ()

            type_.Methods |> Seq.iter(fun method ->
                if method.HasBody then
                    let body = method.Body
                    let graph = buildFlowGraph method
                    let graph = { BasicBlocks = graph.BasicBlocks |> List.map simplifyBasicBlock }
                    saveMethod method graph
            )

        module_.Write(targetFile);
        Console.WriteLine($"Rewritten metadata for the assembly {assemblyFile} saved to {targetFile}");
        0