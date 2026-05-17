using dnlib.DotNet;
using dnlib.DotNet.Emit;

if (args.Length < 1)
{
    Console.WriteLine("obfuscator-generate-conditions mdfile targetfile");
    return;
}

var assemblyFile = args[0];
var targetFile = args[1];
ModuleContext modCtx = ModuleDef.CreateModuleContext();
ModuleDefMD module = ModuleDefMD.Load(assemblyFile, modCtx);
foreach (var type in module.Types)
{
    foreach (var method in type.Methods)
    {
        if (!method.HasBody)
            continue;
        var flowGraph = new FlowGraph(method);
        if (flowGraph.BasicBlocks.Count == 1)
            continue;

        // Fake conditional block
        var fakeJump = new BasicBlock();
        var (expr1, value1) = Generator(3);
        GenerateMethodBody(expr1, fakeJump.Instructions);
        var (expr2, value2) = Generator(3);
        GenerateMethodBody(expr2, fakeJump.Instructions);
        fakeJump.Instructions.Add(new Instruction(
            OpCodes.Sub));
        // Select fake injection point and target for conditional jump
        var randomBB = Random.Shared.Next(flowGraph.BasicBlocks.Count - 1);
        var randomTarget = Random.Shared.Next(flowGraph.BasicBlocks.Count - 1);
        var fakeInstruction = flowGraph.BasicBlocks[randomTarget].Instructions[0];
        if (value1 != value2)
        {
            fakeJump.Instructions.Add(new Instruction(
                OpCodes.Ldc_I4_0));
        }
        else
        {
            fakeJump.Instructions.Add(new Instruction(
                OpCodes.Ldc_I4_1));
        }

        fakeJump.Instructions.Add(new Instruction(
            OpCodes.Beq,
            fakeInstruction));
        flowGraph.BasicBlocks.Insert(randomBB, fakeJump);
        var deadCode = new BasicBlock();
        var (expr, value) = Generator(3);
        GenerateMethodBody(expr, deadCode.Instructions);
        var unused = new Local(module.CorLibTypes.Int32);
        method.Body.Variables.Add(unused);
        deadCode.Instructions.Add(new Instruction(OpCodes.Stloc_S, unused));
        flowGraph.BasicBlocks.Insert(randomBB + 1, deadCode);
        flowGraph.Save(method);
    }
}

module.Write(targetFile);
Console.WriteLine($"Rewritten metadata for the assembly {assemblyFile} saved to {targetFile}");

(Expr, int) Generator(int fuel)
{
    if (fuel == 0)
    {
        var value = Random.Shared.Next(100);
        return (new ConstInt32(value), value);
    }
    var op = Random.Shared.Next(6);
    switch (op)
    {
        case 0:
            {
                var (left, leftValue) = Generator(fuel - 1);
                var (right, rightValue) = Generator(fuel - 1);
                return (new AddOperation(left, right), leftValue + rightValue);
            }
        case 1:
            {
                var (left, leftValue) = Generator(fuel - 1);
                var (right, rightValue) = Generator(fuel - 1);
                return (new SubOperation(left, right), leftValue - rightValue);
            }
        case 2:
            {
                var (left, leftValue) = Generator(fuel - 1);
                var (right, rightValue) = Generator(fuel - 1);
                return (new MulOperation(left, right), leftValue * rightValue);
            }
        case 3:
            {
                var (left, leftValue) = Generator(fuel - 1);
                var (right, rightValue) = Generator(fuel - 1);
                if (rightValue == 0)
                    return Generator(fuel - 1);
                return (new DivOperation(left, right), leftValue / rightValue);
            }
        case 4:
            {
                var (left, leftValue) = Generator(fuel - 1);
                var (right, rightValue) = Generator(fuel - 1);
                if (rightValue == 0)
                    return Generator(fuel - 1);
                return (new ModOperation(left, right), leftValue % rightValue);
            }
        case 5:
            {
                var (left, leftValue) = Generator(fuel - 1);
                return (new AbsOperation(left), leftValue);
            }
    }

    throw new InvalidOperationException("This should not happen. Check the generator logic.");
}

void GenerateMethodBody(Expr expr, IList<Instruction> il)
{
    switch (expr)
    {
        case ConstInt32 c:
            il.Add(Instruction.Create(OpCodes.Ldc_I4, c.Value));
            break;
        case AddOperation a:
            GenerateMethodBody(a.Left, il);
            GenerateMethodBody(a.Right, il);
            il.Add(Instruction.Create(OpCodes.Add));
            break;
        case SubOperation s:
            GenerateMethodBody(s.Left, il);
            GenerateMethodBody(s.Right, il);
            il.Add(Instruction.Create(OpCodes.Sub));
            break;
        case MulOperation m:
            GenerateMethodBody(m.Left, il);
            GenerateMethodBody(m.Right, il);
            il.Add(Instruction.Create(OpCodes.Mul));
            break;
        case DivOperation d:
            GenerateMethodBody(d.Left, il);
            GenerateMethodBody(d.Right, il);
            il.Add(Instruction.Create(OpCodes.Div));
            break;
        case ModOperation m:
            GenerateMethodBody(m.Left, il);
            GenerateMethodBody(m.Right, il);
            il.Add(Instruction.Create(OpCodes.Rem));
            break;
        case AbsOperation a:
            GenerateMethodBody(a.Operand, il);
            il.Add(Instruction.Create(OpCodes.Call, module.Import(typeof(Math).GetMethod("Abs", [typeof(int)]))));
            break;
    }
}

class BasicBlock
{
    public List<Instruction> Instructions { get; set; }
        = new List<Instruction>();
}

class FlowGraph
{
    public List<BasicBlock> BasicBlocks { get; set; }
        = new List<BasicBlock>();
    public FlowGraph(MethodDef method)
    {
        // Finding start of basic blocks using linear scan. Check br/ret/jump targets
        List<int> basicBlocksStart = new() { 0 };
        for (int i = 1; i < method.Body.Instructions.Count; i++)
        {
            var instr = method.Body.Instructions[i];
            if (instr.IsBr() || instr.IsConditionalBranch() || instr.OpCode == OpCodes.Ret)
            {
                if (instr.IsConditionalBranch())
                {
                    var instructionIndex = method.Body.Instructions.IndexOf((Instruction)instr.Operand);
                    basicBlocksStart.Add(instructionIndex);
                }

                if (i + 1 < method.Body.Instructions.Count)
                {
                    basicBlocksStart.Add(i + 1);
                    i++; // skip next instruction, since we already add it.
                    continue;
                }
            }
        }

        basicBlocksStart = basicBlocksStart.Distinct().ToList();
        basicBlocksStart.Sort();
        for (int i = 0; i < basicBlocksStart.Count; i++)
        {
            var block = new BasicBlock();
            var finish = i == basicBlocksStart.Count - 1
                ? method.Body.Instructions.Count
                : basicBlocksStart[i + 1];
            for (int j = basicBlocksStart[i]; j < finish; j++)
            {
                block.Instructions.Add(method.Body.Instructions[j]);
            }

            BasicBlocks.Add(block);
        }
    }

    public void Save(MethodDef method)
    {
        method.Body.Instructions.Clear();
        foreach (var block in BasicBlocks)
        {
            foreach (var instr in block.Instructions)
            {
                method.Body.Instructions.Add(instr);
            }
        }

        method.Body.SimplifyBranches();
        method.Body.OptimizeBranches();
    }
}

abstract record Expr;
record ConstInt32(int Value) : Expr;
record AddOperation(Expr Left, Expr Right) : Expr;
record SubOperation(Expr Left, Expr Right) : Expr;
record MulOperation(Expr Left, Expr Right) : Expr;
record DivOperation(Expr Left, Expr Right) : Expr;
record ModOperation(Expr Left, Expr Right) : Expr;
record AbsOperation(Expr Operand) : Expr;
