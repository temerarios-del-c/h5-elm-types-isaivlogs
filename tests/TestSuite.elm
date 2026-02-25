module TestSuite exposing (..)

import Expect
import Fuzz
import Helper
import Test exposing (..)


floatBetween : Float -> Float -> Fuzz.Fuzzer Float
floatBetween min max =
    Fuzz.floatRange min max


categoricalGradeTest : Test
categoricalGradeTest =
    describe "Testing categoricalGrade function"
        [ fuzz (Fuzz.list (floatBetween 7 100)) "categoricalGrade {7-100} should reduce to Approved" <|
            \list ->
                Helper.categoricalGrade list
                    |> Expect.equal (List.repeat (List.length list) Helper.Approved)
        , fuzz (Fuzz.list (floatBetween 0 6.9999)) "categoricalGrade {0-6.9999} should reduce to Failed" <|
            \list ->
                Helper.categoricalGrade list
                    |> Expect.equal (List.repeat (List.length list) Helper.Failed)
        , fuzz (Fuzz.list (floatBetween -100 -0.0001)) "categoricalGrade {-100 - -0.0001} should reduce to Pending" <|
            \list ->
                Helper.categoricalGrade list
                    |> Expect.equal (List.repeat (List.length list) Helper.Pending)
        ]


airplaneScheduleActionTest : Test
airplaneScheduleActionTest =
    describe "Testing airplaneScheduleAction function"
        [ test "airplaneScheduleAction Cancelled should reduce to \"Pedir reembolso\"" <|
            \_ ->
                Helper.airplaneScheduleAction Helper.Cancelled
                    |> Expect.equal "Pedir reembolso"
        , test "airplaneScheduleAction Delayed should reduce to \"Esperar\"" <|
            \_ ->
                Helper.airplaneScheduleAction Helper.Delayed
                    |> Expect.equal "Esperar"
        , test "airplaneScheduleAction OnTime should reduce to \"Esperar\"" <|
            \_ ->
                Helper.airplaneScheduleAction Helper.OnTime
                    |> Expect.equal "Esperar"
        , test "airplaneScheduleAction Boarding should reduce to \"Buscar boleto\"" <|
            \_ ->
                Helper.airplaneScheduleAction Helper.Boarding
                    |> Expect.equal "Buscar boleto"
        ]


waitableAirplaneStatusFuzzer : Fuzz.Fuzzer Helper.AirplaneStatus
waitableAirplaneStatusFuzzer =
    Fuzz.oneOf
        [ Fuzz.constant Helper.Delayed
        , Fuzz.constant Helper.OnTime
        ]


airplaneActionTest : Test
airplaneActionTest =
    describe "Testing airplaneAction function"
        [ fuzz (Fuzz.list (Fuzz.constant Helper.Cancelled)) "airplaneAction [Cancelled] should reduce to [\"Pedir reembolso\"]" <|
            \list ->
                Helper.airportAction list
                    |> Expect.equal (List.repeat (List.length list) "Pedir reembolso")
        , fuzz (Fuzz.list (Fuzz.constant Helper.Boarding)) "airplaneAction [Delayed] should reduce to [\"Buscar boleto\"]" <|
            \list ->
                Helper.airportAction list
                    |> Expect.equal (List.repeat (List.length list) "Buscar boleto")
        , fuzz (Fuzz.list waitableAirplaneStatusFuzzer) "airplaneAction [Delayed | OnTime] should reduce to [\"Esperar\"]" <|
            \list ->
                Helper.airportAction list
                    |> Expect.equal (List.repeat (List.length list) "Esperar")
        ]
