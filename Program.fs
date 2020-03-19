// Learn more about F# at http://fsharp.org

open System//open System.Reflection
//open Microsoft.FSharp.Reflection
open Mono.Cecil
let dogString = "dog"

let myMethod (parm1:string) (parm2:int) (parm3:int) = 
  if parm3=0 then raise (new ArgumentOutOfRangeException()) else
  let ratio=parm2/parm3
  dogString + parm1 + string parm2 + ":" + string ratio

type MethodSymbolCategory =
  | System_Incoming_Parameter
  | User_Incoming_Parameter
  | System_Token_In_Method
  | User_Token_In_Method
type methodSymbols =
  {
  SymbolCategory:MethodSymbolCategory
  SymbolType:String
  SymbolsWithThatType:String []
  }
  override self.ToString() = 
    sprintf "Symbol Category: %s" (string self.SymbolCategory) +
    sprintf "\nSymbol Type: %s" (string self.SymbolType) +
    sprintf "\nSymbol Name: %s" (string self.SymbolsWithThatType.[0])
(*  interface IFormattable with
    member self.ToString(format, formatProvider) =
      ""*)

let convertParameterDefinitionsIntoMethodSymbols (paramDefs:ParameterDefinition seq) =
  paramDefs
  |> Seq.map(fun x->
    {
      SymbolCategory=System_Incoming_Parameter
      SymbolType=x.ParameterType.Name
      SymbolsWithThatType=[|x.Name|]
    })
let convertMethodDefinitionsIntoMethodSymbols (paramDefs:MethodDefinition seq) cat =
  paramDefs
  |> Seq.map(fun x->
    {
      SymbolCategory=cat
      SymbolType=x.FullName
      SymbolsWithThatType=[|x.Name|]
    })

let rec getCreatedExceptions (method:Cil.MethodBody) (referencesSoFar:TypeReference seq) =
  let exceptionsForCurrentInstruction =
    method.Instructions
    |> Seq.filter(fun (x:Cil.Instruction)->
      x.OpCode=Cil.OpCodes.Newobj
      && (x.Operand:?>MemberReference).DeclaringType.Name.EndsWith("Exception"))
    |> Seq.distinct |> Seq.cast<TypeReference>
  Seq.append exceptionsForCurrentInstruction referencesSoFar |> Seq.distinct

let exceptionsForAMethodNoRecurse (method:MethodDefinition) =
  let allMethodInstructions:Cil.Instruction seq=
    if method.Body<>null 
      then method.Body.Instructions |> Seq.cast
      else Seq.empty
  let allMethodExceptions=
    allMethodInstructions
    |> Seq.filter(fun x->
      x.OpCode.Name.EndsWith "Exception"
      )
  allMethodExceptions
let rec exceptionsForAMethod (method:MethodDefinition) (exceptionsSoFar:Cil.Instruction seq):Cil.Instruction seq = 
  let plainExceptions=exceptionsForAMethodNoRecurse method
  let nonExceptionCalls =
    if method.Body=null then Seq.empty else

    method.Body.Instructions
      |> Seq.filter(fun (x:Cil.Instruction)->
        x.OpCode=Cil.OpCodes.Call
        && (x.Operand:?>MemberReference).DeclaringType.Name.EndsWith("Exception")=false)
  let exceptionsForInterestingInstructions =
    nonExceptionCalls |> Seq.map(fun inst->(inst.Operand:?>MethodReference).Resolve())
  if (exceptionsForInterestingInstructions |> Seq.toList).Length=0
    then
      Seq.append
        exceptionsSoFar
        plainExceptions
    else
      let runningList=
        exceptionsForInterestingInstructions
        |> Seq.collect(fun thisMethod->
          exceptionsForAMethod thisMethod Seq.empty)
       //exceptionsSoFar
      Seq.append
        exceptionsSoFar
        runningList
let getMethodToTest=
  let dllToTest = "test.dll"
  let moduleToTest = "Program"
  let methodToTest = "myMethod"
  let readerParameters = new ReaderParameters()
  readerParameters.ReadSymbols<-true
  readerParameters.ReadingMode<-ReadingMode.Immediate
  let moduleDefinition = ModuleDefinition.ReadModule(dllToTest,readerParameters)

  let programBlock = moduleDefinition.Types |> Seq.find(fun x->x.FullName=moduleToTest)
  let programMethods = programBlock.Methods
  let methodToInspect = programMethods |> Seq.find(fun x->x.Name=methodToTest)
  methodToInspect
let getMethodParameters (methodToInspect:MethodDefinition) =
  let methodParamters = methodToInspect.Parameters // ignoring some stuff on purpose
  //let systemDefinedMethodParamters = methodParamters |> Seq.filter(fun x->x.ParameterType.IsDefinition=false)
  let systemDefinedMethodParamters = methodParamters |> Seq.filter(fun x->x.ParameterType.IsDefinition=false)
  let userDefinedMethodParamters = methodParamters |> Seq.filter(fun x->x.ParameterType.IsDefinition)
  let genericParamters = methodParamters |> Seq.filter(fun x->x.ParameterType.IsGenericParameter=true)
  let genericParameterDefinitions = 
    genericParamters
    |> Seq.map(fun (paremDef:ParameterDefinition)->paremDef.Resolve())
  let genericParameterInfo=convertParameterDefinitionsIntoMethodSymbols genericParameterDefinitions
  let systemParameterInfo=convertParameterDefinitionsIntoMethodSymbols systemDefinedMethodParamters
  let userParameterInfo=convertParameterDefinitionsIntoMethodSymbols userDefinedMethodParamters
  (systemParameterInfo, genericParameterInfo, userParameterInfo)

[<EntryPoint>]
let main argv =
    let methodToInspect = getMethodToTest
    
    // INSTRUCTIONS
    let methodInstructions = 
      methodToInspect.Body.Instructions
      |> Seq.filter(fun inst->inst.OpCode=Cil.OpCodes.Call)
      |> Seq.map(fun inst->(inst.Operand:?>MethodReference).Resolve()) |> Seq.toList
    let methodExceptions = getCreatedExceptions methodToInspect.Body
    let methDebugInfo = methodToInspect.DebugInformation

    // PARAMETERS
    let systemParameterInfo, genericParameterInfo, userParameterInfo=getMethodParameters methodToInspect

    // INSTRUCTIONS
    let instructionsToInspect = methodToInspect.Body.Instructions
    let interestingInstructions = 
      instructionsToInspect 
      |> Seq.filter(fun x->x.Operand<>null)
      |> Seq.filter(fun x->x.OpCode.Code=Cil.Code.Call)
    let interestingSystemSymbolDefinitions =
      interestingInstructions
      |> Seq.filter(fun x->(x.Operand:?>MethodReference).IsDefinition=false)
      |> Seq.map(fun inst->(inst.Operand:?>MethodReference).Resolve())
    let systemSymbolInfo = convertMethodDefinitionsIntoMethodSymbols interestingSystemSymbolDefinitions System_Token_In_Method
    let interestingUserSymbolDefinitions =
      interestingInstructions
      |> Seq.filter(fun x->(x.Operand:?>MethodReference).IsDefinition)
      |> Seq.map(fun inst->(inst.Operand:?>MethodReference).Resolve())
    let userSymbolInfo = convertMethodDefinitionsIntoMethodSymbols interestingUserSymbolDefinitions User_Token_In_Method

    // OUTPUT
    let printMethodSymbolsWithHeader header symbols =
      if (symbols|>Seq.toList).Length=0 then () else
      printfn header
      printfn "---------------------------------------------------"
      symbols |> Seq.iter(fun x->printfn "%s\n" (string x))
      
    printMethodSymbolsWithHeader "PARAMETERS (SYSTEM)" systemParameterInfo
    printMethodSymbolsWithHeader "PARAMETERS (USER)" userParameterInfo
    printMethodSymbolsWithHeader "PARAMETERS (GENERIC)" genericParameterInfo
    printMethodSymbolsWithHeader "INSTRUCTION SYMBOLS (SYSTEM)" systemSymbolInfo
    printMethodSymbolsWithHeader "INSTRUCTION SYMBOLS (USER)" userSymbolInfo

    let temp=exceptionsForAMethod methodToInspect Seq.empty
    printfn "%A" temp

    

(*    let methodCodeCognitiveLoadShallow = 
      Seq.append
        (systemDefinedMethodParamters |> S eq.map(fun x->x.ParameterType.Name))
        (userDefinedMethodParamters |> Seq.map(fun x->x.ParameterType.Name))*)

    0 // return an integer exit code
