module IlOptimizerTests

    open Xunit 
    open Program
    open ToIl
    open IlOptimizer

    let expected expectedIl actualIl= 
        let resultats = optimize (Assembly("", [Class("", [Method([],[], "",actualIl)])]))
        Assert.Equal((Assembly("", [Class("", [Method([],[], "", expectedIl)])])), resultats)


    [<Fact>] 
    let ``Should remove store load When have only this instruction``() = 
        [Stloc(0); Ldloc(0)]
         |> expected 
         []

    [<Fact>] 
    let ``Should remove store load When have this instruction with another load``() = 
        [Stloc(0); Ldloc(0); Ldloc(1)]
         |> expected 
         [Ldloc(1)]

    [<Fact>] 
    let ``Should do nothing When have two load instruction``() = 
        [Stloc(0); Ldloc(0);Ldloc(0)]
         |> expected 
         [Stloc(0); Ldloc(0);Ldloc(0)]

