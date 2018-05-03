module public Optics

open Priority_Queue

type Point = 
    { Index: int
      ReachabilityDistance: double option
      Processed: bool }

let createPoint (index:int) :Point =
    { Index = index
      ReachabilityDistance = None
      Processed = false }

let findNextUnprocessedIndex points = Array.tryFindIndex (fun p -> not p.Processed) points

let calcCoreDistance (index:int) (neighbors:int seq) (minPts:int) (calcDistance:int -> int -> double) :double option =
    if Seq.length neighbors < minPts then
        None
    else
        let calcDistanceTo = calcDistance index
        let distances = neighbors |> Seq.map calcDistanceTo |> Seq.sort |> Seq.toArray
        Some(distances.[minPts-1])

let update (index:int) (neighbors:int seq) (minPts:int) (seeds:IPriorityQueue<int, double>) (points:Point array) (calcDistance:int -> int -> double) = 
    let coreDistanceOrNone = calcCoreDistance index neighbors minPts calcDistance

    match coreDistanceOrNone with
        | None -> ()
        | Some(coreDistance) -> 
            let unprocessedNeighbors = neighbors |> Seq.map (fun n -> points.[n]) |> Seq.filter (fun p -> not p.Processed)

            for point in unprocessedNeighbors do
                let newReachDist = max coreDistance (calcDistance index point.Index)

                // use minimum reachability distance
                let updatedReachDist =
                    match point.ReachabilityDistance with
                        | None -> newReachDist
                        | Some(oldReachDist) -> min newReachDist oldReachDist

                // update points
                points.[point.Index] <- {point with ReachabilityDistance = Some(updatedReachDist)}
                
                // add to sorted list
                if seeds.Contains point.Index  then
                    seeds.Remove point.Index
                seeds.Enqueue(point.Index, updatedReachDist)

let rec opticsIterate<'P> (points:Point array) (minPts:int) (getNeighbors:int -> int seq) (calcDistance:int -> int -> double) :Point list =
    let indexOrNone = findNextUnprocessedIndex points

    let mutable output = []

    let markAsProcessed index = points.[index] <- {points.[index] with Processed = true}
    let addToOutput index = output <- points.[index] :: output
    
    match indexOrNone with
        | Some(index) ->
            markAsProcessed index
            addToOutput index

            let neighbors = getNeighbors index

            if Seq.length neighbors >= minPts then
                let seeds = new SimplePriorityQueue<int, double>()

                update index neighbors minPts seeds points calcDistance

                while seeds.Count > 0 do
                    let seed = seeds.Dequeue();

                    markAsProcessed seed
                    addToOutput seed

                    let neighbors' = getNeighbors seed
                    let coreDistance = calcCoreDistance seed neighbors' minPts calcDistance

                    if Option.isSome coreDistance then
                        update seed neighbors' minPts seeds points calcDistance

            (opticsIterate points minPts getNeighbors calcDistance) @ output
        | None -> output

/// <summary>
/// Run the OPTICS cluster detection algorithm on a number of points and return the output list.
/// </summary>
///
/// <param name="numPoints">Number of points.</param>
/// <param name="minPts"></param>
/// <param name="getNeighbors">A function that yields the indices of neighboring points of a point given by its index.</param>
/// <param name="calcDistance">A function that calculates the distance between two points given by their indices.</param>
/// <returns>A list of (index, reachability distance)-tuples in the order of processing</returns>
let public optics<'P> (numPoints:int) (minPts:int) (getNeighbors:int -> int seq) (calcDistance: int -> int -> double) :(int * double option) list =
    let points = Array.init numPoints createPoint
    
    let result = opticsIterate points minPts getNeighbors calcDistance

    result |> List.map (fun point -> (point.Index, point.ReachabilityDistance))
