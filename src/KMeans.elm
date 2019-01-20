module KMeans exposing (Cluster, Point3D, run, suite)

import Expect
import Fuzz exposing (Fuzzer, floatRange, intRange, list, map, map3)
import NonEmpty exposing (NonEmpty)
import Test exposing (Test, describe, fuzz2)



-- Types


type alias Point3D =
    { x : Float
    , y : Float
    , z : Float
    }


type alias Cluster =
    { points : List Point3D
    , center : Point3D
    }



-- Public


run : NonEmpty Point3D -> List Point3D -> NonEmpty Cluster
run centers points =
    centers
        |> NonEmpty.map (Cluster [])
        |> assignAndMoveClusters 0 points



-- Private


assignAndMoveClusters : Float -> List Point3D -> NonEmpty Cluster -> NonEmpty Cluster
assignAndMoveClusters iteration points clusters =
    if iteration > 100 then
        clusters

    else
        clusters
            |> NonEmpty.map clearCluster
            |> assignPoints points
            |> moveCenters
            |> assignAndMoveClusters (iteration + 1) points


clearCluster : Cluster -> Cluster
clearCluster cluster =
    { cluster | points = [] }


assignPoints : List Point3D -> NonEmpty Cluster -> NonEmpty Cluster
assignPoints points zeroPointClusters =
    List.foldl
        (\point clusters ->
            let
                accResult =
                    NonEmpty.foldl
                        (\cluster acc ->
                            let
                                pDistance =
                                    pointDistance point cluster.center

                                currIndex =
                                    acc.currentIndex + 1
                            in
                            if pDistance < acc.distance then
                                { currentIndex = currIndex, bestIndex = currIndex, distance = pDistance }

                            else
                                { acc | currentIndex = currIndex }
                        )
                        { currentIndex = -1, bestIndex = -1, distance = 1 / 0 }
                        clusters
            in
            NonEmpty.indexedMap
                (\index cluster ->
                    if index == accResult.bestIndex then
                        { cluster | points = List.append cluster.points [ point ] }

                    else
                        cluster
                )
                clusters
        )
        zeroPointClusters
        points


moveCenters : NonEmpty Cluster -> NonEmpty Cluster
moveCenters clusters =
    NonEmpty.map
        (\cluster ->
            { cluster
                | center =
                    { x = average .x cluster.points
                    , y = average .y cluster.points
                    , z = average .z cluster.points
                    }
            }
        )
        clusters


pointDistance : Point3D -> Point3D -> Float
pointDistance p1 p2 =
    sqrt ((p1.x - p2.x) ^ 2 + (p1.y - p2.y) ^ 2 + (p1.z - p2.z) ^ 2)


average : (Point3D -> Float) -> List Point3D -> Float
average accessor points =
    let
        total =
            sum accessor points
    in
    if total == 0 then
        0

    else
        total / (toFloat <| List.length points)


sum : (Point3D -> Float) -> List Point3D -> Float
sum accessor points =
    points
        |> List.map accessor
        |> List.foldl (+) 0



-- Fuzzers


pointFuzzer : Fuzzer Point3D
pointFuzzer =
    map3
        Point3D
        (floatRange -4 4)
        (floatRange -4 4)
        (floatRange -4 4)


pointsFuzzer : Fuzzer (List Point3D)
pointsFuzzer =
    list pointFuzzer
        |> map (List.take 30)


centerPointsFuzzer : Fuzzer (NonEmpty Point3D)
centerPointsFuzzer =
    NonEmpty.list pointFuzzer
        |> map (NonEmpty.takeFromTail 2)



-- Test suite


suite : Test
suite =
    describe "KMeans tests"
        [ centerPointsAreValid
        , pointsAreAssignedToNearestCluster
        ]



-- Tests


centerPointsAreValid : Test
centerPointsAreValid =
    fuzz2 centerPointsFuzzer pointsFuzzer "Center points shouldn't contain NaN values" <|
        \centerPoints points ->
            let
                clusterCenters =
                    run centerPoints points
                        |> NonEmpty.map .center
            in
            Expect.equal True
                (NonEmpty.all
                    (\point ->
                        not (isNaN point.x)
                            && not (isNaN point.y)
                            && not (isNaN point.z)
                    )
                    clusterCenters
                )


pointsAreAssignedToNearestCluster : Test
pointsAreAssignedToNearestCluster =
    fuzz2 centerPointsFuzzer pointsFuzzer "Points in one cluster should not be closer to another cluster" <|
        \centerPoints points ->
            let
                clusters =
                    run centerPoints points

                shouldTestPass =
                    NonEmpty.all
                        (\cluster ->
                            let
                                otherClusters =
                                    clusters
                                        |> NonEmpty.toList
                                        |> List.filter (\c -> c /= cluster)
                            in
                            List.all
                                (\point ->
                                    let
                                        distance =
                                            pointDistance point cluster.center

                                        otherDistances =
                                            List.map
                                                (\otherCluster -> pointDistance point otherCluster.center)
                                                otherClusters
                                    in
                                    List.all
                                        (\otherDistance -> otherDistance >= distance)
                                        otherDistances
                                )
                                cluster.points
                        )
                        clusters
            in
            if shouldTestPass then
                Expect.pass

            else
                Expect.fail "Found point that was assigned to the wrong cluster"
