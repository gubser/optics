module Tests

open Xunit

open Optics

module public CoreDistance =
    let neighborToDistance = 
        Map [ (1, 0.1); (2, 0.4); (3, 0.3); (4, 0.8); (5, 0.2) ]
    let neighbors = neighborToDistance |> Map.toList  |> List.map fst
    let calcDistance _ b = Map.find b neighborToDistance

    [<Fact>]
    let ``get the third nearest distant of five points`` () =
        let x = calcCoreDistance 0 neighbors 3 calcDistance
        Assert.Equal(x.Value, 0.3, 5)

    [<Fact>]
    let ``not enough points in core distance`` () =
        let x = calcCoreDistance 0 neighbors 10 calcDistance
        Assert.True x.IsNone

module public Optics =
    [<Fact>]
    let ``get me some results`` () =
        let points = [
            0.1; 0.2; 0.3; 0.2; 0.4; 0.1; 0.2;
            5.0; 6.0; 4.0; 5.5; 6.5; 4.5;
            10.0; 10.1; 9.9; 10.2; 9.8;
        ]

        let calcDistance a b = abs(points.[a] - points.[b])
        let getNeighbors index = [0 .. (List.length points)-1] |> List.except [index] |> Seq.ofList

        let results = optics (List.length points) 3 getNeighbors calcDistance

        printfn "%A" results

        