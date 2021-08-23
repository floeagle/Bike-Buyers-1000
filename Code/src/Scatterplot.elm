module Scatterplot exposing (..)

import Axis
import Statistics
import Html exposing (Html)
import Scale exposing (ContinuousScale)
import TypedSvg exposing (circle, g, rect, style, svg, text_)
import TypedSvg.Attributes exposing (class, fontFamily, fontSize, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, width, x, y)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment(..), Length(..), Transform(..))
import Csv.Decode as Decode exposing (Decoder)


w : Float
w =
    900


h : Float
h =
    450


padding : Float
padding =
    60


radius : Float
radius =
    5.0


tickCount : Int
tickCount =
    5


defaultExtent : ( number, number1 )
defaultExtent =
    ( 0, 100 )

xScale : List Float -> ContinuousScale Float
xScale values =
    Scale.linear ( 0, w - 2 * padding ) ( wideExtent values )


yScale : List Float -> ContinuousScale Float
yScale values =
    Scale.linear ( h - 2 * padding, 0 ) ( wideExtent values )

wideExtent : List Float -> ( Float, Float )
wideExtent values =
    let
        scale: (Float, Float)
        scale = Maybe.withDefault defaultExtent (Statistics.extent values)

        range: Float
        range = -(Tuple.first scale) + (Tuple.second scale)

        down: Float
        down = (Tuple.first scale) - range/(Basics.toFloat (2*tickCount))

        up: Float
        up = (Tuple.second scale)+range/(Basics.toFloat (2*tickCount))
    in         
        if (down < 0) then 
            (0, up)
        else
            (down, up)


xAxis : List Float -> Svg msg
xAxis values =
    Axis.bottom [ Axis.tickCount tickCount ] (xScale values)


yAxis : List Float -> Svg msg
yAxis values =
    Axis.left [ Axis.tickCount tickCount ] (yScale values)


main : Html msg
main =
            
            Html.main_ []
                [ Html.h1 [] [ Html.text "Bike_Buyers" ]]



decoder : Decoder BikeBuyers
decoder =
    Decode.into BikeBuyers
        |> Decode.pipeline (Decode.field "ID" Decode.int)
        |> Decode.pipeline (Decode.field "MaritalStatus" Decode.string)
        |> Decode.pipeline (Decode.field "Gender" Decode.string)
        |> Decode.pipeline (Decode.field "Income" (Decode.blank Decode.int))
        |> Decode.pipeline (Decode.field "Children" (Decode.blank Decode.int))
        |> Decode.pipeline (Decode.field "Education" Decode.string)
        |> Decode.pipeline (Decode.field "Occupation" Decode.string)
        |> Decode.pipeline (Decode.field "HomeOwner" Decode.string)
        |> Decode.pipeline (Decode.field "Cars" (Decode.blank Decode.int))
        |> Decode.pipeline (Decode.field "CommuteDistance" Decode.string)
        |> Decode.pipeline (Decode.field "Region" Decode.string)
        |> Decode.pipeline (Decode.field "Age" (Decode.blank Decode.int))
        |> Decode.pipeline (Decode.field "PurchasedBike" Decode.string)


--Für Scatterplot
type alias Point =
    { pointName : String, x : Float, y : Float }


type alias XyData =
    { xDescription : String
    , yDescription : String
    , data : List Point
    }


-- Um Datensatz zu definieren
type alias BikeBuyers =
    { id :  Int
    , maritalStatus: String
    , gender: String
    , income : Maybe Int
    , children :  Maybe Int
    , education : String
    , occupation : String
    , homeOwner: String
    , cars: Maybe Int
    , commuteDistance: String
    , region: String
    , age:  Maybe Int
    , purchasedBike: String
    
    
    }




csv : String
csv =
     """ID,MaritalStatus,Gender,Income,Children,Education,Occupation,HomeOwner,Cars,CommuteDistance,Region,Age,PurchasedBike
12496,Married,Female,40000,1,Bachelors,Skilled Manual,Yes,0,0-1 Miles,Europe,42,No
24107,Married,Male,30000,3,Partial College,Clerical,Yes,1,0-1 Miles,Europe,43,No
14177,Married,Male,80000,5,Partial College,Professional,No,2,2-5 Miles,Europe,60,No
24381,Single,Male,70000,0,Bachelors,Professional,Yes,1,5-10 Miles,Pacific,41,Yes
25597,Single,Male,30000,0,Bachelors,Clerical,No,0,0-1 Miles,Europe,36,Yes
13507,Married,Female,10000,2,Partial College,Manual,Yes,0,1-2 Miles,Europe,50,No
27974,Single,Male,160000,2,High School,Management,Yes,4,0-1 Miles,Pacific,33,Yes
19364,Married,Male,40000,1,Bachelors,Skilled Manual,Yes,0,0-1 Miles,Europe,43,Yes
22155,Married,Male,20000,2,Partial High School,Clerical,Yes,2,5-10 Miles,Pacific,58,No
19280,Married,Male,20000,2,Partial College,Manual,Yes,1,0-1 Miles,Europe,48,Yes
22173,Married,Female,30000,3,High School,Skilled Manual,No,2,1-2 Miles,Pacific,54,Yes
12697,Single,Female,90000,0,Bachelors,Professional,No,4,10+ Miles,Pacific,36,No
11434,Married,Male,170000,5,Partial College,Professional,Yes,4,0-1 Miles,Europe,55,No
25323,Married,Male,40000,2,Partial College,Clerical,Yes,1,1-2 Miles,Europe,35,Yes
23542,Single,Male,60000,1,Partial College,Skilled Manual,No,1,0-1 Miles,Pacific,45,Yes
20870,Single,Female,10000,2,High School,Manual,Yes,1,0-1 Miles,Europe,38,Yes
23316,Single,Male,30000,3,Partial College,Clerical,No,2,1-2 Miles,Pacific,59,Yes
12610,Married,Female,30000,1,Bachelors,Clerical,Yes,0,0-1 Miles,Europe,47,No
27183,Single,Male,40000,2,Partial College,Clerical,Yes,1,1-2 Miles,Europe,35,Yes
25940,Single,Male,20000,2,Partial High School,Clerical,Yes,2,5-10 Miles,Pacific,55,Yes
25598,Married,Female,40000,0,Graduate Degree,Clerical,Yes,0,0-1 Miles,Europe,36,Yes
21564,Single,Female,80000,0,Bachelors,Professional,Yes,4,10+ Miles,Pacific,35,No
19193,Single,Male,40000,2,Partial College,Clerical,Yes,0,1-2 Miles,Europe,35,Yes
26412,Married,Female,80000,5,High School,Management,No,3,5-10 Miles,Europe,56,No
27184,Single,Male,40000,2,Partial College,Clerical,No,1,0-1 Miles,Europe,34,No
12590,Single,Male,30000,1,Bachelors,Clerical,Yes,0,0-1 Miles,Europe,63,No
17841,Single,Male,30000,0,Partial College,Clerical,No,1,0-1 Miles,Europe,29,Yes
18283,Single,Female,100000,0,Bachelors,Professional,No,1,5-10 Miles,Pacific,40,No
18299,Married,Male,70000,5,Partial College,Skilled Manual,Yes,2,5-10 Miles,Pacific,44,No
16466,Single,Female,20000,0,Partial High School,Manual,No,2,0-1 Miles,Europe,32,Yes
19273,Married,Female,20000,2,Partial College,Manual,Yes,0,0-1 Miles,Europe,63,No
22400,Married,Male,10000,0,Partial College,Manual,No,1,0-1 Miles,Pacific,26,Yes
20942,Single,Female,20000,0,High School,Manual,No,1,5-10 Miles,Europe,31,No
18484,Single,Male,80000,2,High School,Skilled Manual,No,2,1-2 Miles,Pacific,50,Yes
12291,Single,Male,90000,5,Partial College,Professional,No,2,2-5 Miles,Europe,62,Yes
28380,Single,Female,10000,5,Partial High School,Manual,No,2,0-1 Miles,Europe,41,No
17891,Married,Female,10000,2,Partial College,Manual,Yes,1,0-1 Miles,Europe,50,Yes
27832,Single,Female,30000,0,Partial College,Clerical,No,1,2-5 Miles,Europe,30,No
26863,Single,Male,20000,0,High School,Manual,No,1,2-5 Miles,Europe,28,No
16259,Single,Female,10000,4,Partial High School,Manual,Yes,2,0-1 Miles,Europe,40,Yes
27803,Single,Female,30000,2,Partial College,Clerical,No,0,0-1 Miles,Europe,43,No
14347,Single,Female,40000,2,Bachelors,Management,Yes,2,5-10 Miles,Pacific,65,Yes
17703,Married,Female,10000,1,Graduate Degree,Manual,Yes,0,0-1 Miles,Europe,40,No
17185,Married,Female,170000,4,Partial College,Professional,No,3,5-10 Miles,Europe,48,Yes
29380,Married,Female,20000,3,High School,Manual,Yes,0,0-1 Miles,Europe,41,Yes
23986,Married,Female,20000,1,Bachelors,Clerical,Yes,0,0-1 Miles,Europe,66,Yes
24466,Married,Female,60000,1,Partial College,Skilled Manual,Yes,1,5-10 Miles,Pacific,46,Yes
29097,Single,Female,40000,2,Partial College,Skilled Manual,Yes,2,5-10 Miles,Pacific,52,Yes
19487,Married,Male,30000,2,Partial College,Clerical,No,2,0-1 Miles,Europe,42,No
14939,Married,Male,40000,0,Bachelors,Clerical,Yes,0,0-1 Miles,Europe,39,Yes
13826,Single,Female,30000,0,Partial College,Clerical,No,1,0-1 Miles,Europe,28,No
20619,Single,Male,80000,0,Bachelors,Professional,No,4,10+ Miles,Pacific,35,No
12558,Married,Female,20000,1,Bachelors,Clerical,Yes,0,0-1 Miles,Europe,65,No
24871,Single,Female,90000,4,High School,Management,No,3,5-10 Miles,Europe,56,No
17319,Single,Female,70000,0,Bachelors,Professional,No,1,5-10 Miles,Pacific,42,No
28906,Married,Male,80000,4,High School,Professional,Yes,2,10+ Miles,Europe,54,No
12808,Married,Male,40000,0,Bachelors,Clerical,Yes,0,0-1 Miles,Europe,38,Yes
20567,Married,Male,130000,4,Partial College,Professional,No,4,5-10 Miles,Europe,61,Yes
25502,Married,Female,40000,1,Bachelors,Skilled Manual,Yes,0,0-1 Miles,Europe,43,Yes
15580,Married,Male,60000,2,Bachelors,Professional,Yes,1,2-5 Miles,Pacific,38,Yes
24185,Single,Female,10000,1,High School,Manual,No,1,1-2 Miles,Europe,45,No
19291,Single,Female,10000,2,High School,Manual,Yes,0,0-1 Miles,Europe,35,No
16713,Married,Male,40000,2,Bachelors,Management,Yes,1,0-1 Miles,Pacific,52,Yes
16185,Single,Male,60000,4,Bachelors,Professional,Yes,3,10+ Miles,Pacific,41,No
14927,Married,Female,30000,1,Bachelors,Clerical,Yes,0,0-1 Miles,Europe,37,Yes
29337,Single,Male,30000,2,Partial College,Clerical,Yes,2,5-10 Miles,Pacific,68,No
29355,Married,Female,40000,0,Graduate Degree,Clerical,Yes,0,0-1 Miles,Europe,37,Yes
25303,Single,Male,30000,0,High School,Manual,Yes,1,2-5 Miles,Europe,33,Yes
14813,Single,Female,20000,4,High School,Manual,Yes,1,0-1 Miles,Europe,43,Yes
16438,Married,Female,10000,0,Partial High School,Manual,No,2,0-1 Miles,Europe,30,No
14238,Married,Male,120000,0,Partial High School,Professional,Yes,4,10+ Miles,Pacific,36,Yes
16200,Single,Female,10000,0,Partial High School,Manual,No,2,0-1 Miles,Europe,35,No
24857,Married,Female,130000,3,High School,Professional,Yes,4,0-1 Miles,Europe,52,No
26956,Single,Female,20000,0,Partial College,Manual,No,1,2-5 Miles,Europe,36,Yes
14517,Married,Female,20000,3,High School,Skilled Manual,No,2,1-2 Miles,Pacific,62,No
12678,Single,Female,130000,4,High School,Management,Yes,4,0-1 Miles,Pacific,31,No
16188,Single,Female,20000,0,Partial High School,Manual,No,2,1-2 Miles,Europe,26,No
27969,Married,Male,80000,0,Bachelors,Professional,Yes,2,10+ Miles,Pacific,29,Yes
15752,Married,Male,80000,2,High School,Skilled Manual,No,2,1-2 Miles,Pacific,50,Yes
27745,Single,Male,40000,2,Bachelors,Management,Yes,2,5-10 Miles,Pacific,63,Yes
20828,Married,Female,30000,4,Graduate Degree,Clerical,Yes,0,0-1 Miles,Europe,45,Yes
19461,Single,Female,10000,4,Partial High School,Manual,Yes,2,0-1 Miles,Europe,40,No
26941,Married,Male,30000,0,Bachelors,Clerical,Yes,0,0-1 Miles,Europe,47,Yes
28412,Single,Male,20000,0,High School,Manual,No,1,2-5 Miles,Europe,29,No
24485,Single,Male,40000,2,Bachelors,Management,No,1,5-10 Miles,Pacific,52,Yes
16514,Single,Male,10000,0,Partial College,Manual,Yes,1,1-2 Miles,Pacific,26,Yes
17191,Single,Male,130000,3,Partial College,Professional,No,3,0-1 Miles,Europe,51,Yes
19608,Married,Male,80000,5,Bachelors,Professional,Yes,4,1-2 Miles,Pacific,40,No
24119,Single,Male,30000,0,Partial College,Clerical,No,1,2-5 Miles,Europe,29,No
25458,Married,Male,20000,1,High School,Manual,No,1,1-2 Miles,Europe,40,Yes
26886,Single,Female,30000,0,Partial College,Clerical,No,1,0-1 Miles,Europe,29,Yes
28436,Single,Male,30000,0,Partial College,Clerical,No,1,0-1 Miles,Europe,30,Yes
19562,Single,Female,60000,2,Bachelors,Professional,Yes,1,2-5 Miles,Pacific,37,Yes
15608,Single,Female,30000,0,Partial College,Clerical,No,1,2-5 Miles,Europe,33,No
16487,Single,Female,30000,3,High School,Skilled Manual,Yes,2,5-10 Miles,Pacific,55,No
17197,Single,Female,90000,5,Partial College,Professional,Yes,2,10+ Miles,Europe,62,No
12507,Married,Male,30000,1,Partial College,Clerical,Yes,1,0-1 Miles,Europe,43,No
23940,Married,Male,40000,1,Bachelors,Skilled Manual,Yes,1,0-1 Miles,Europe,44,Yes
19441,Single,Male,40000,0,Graduate Degree,Clerical,Yes,0,0-1 Miles,Europe,37,Yes
26852,Married,Female,20000,3,High School,Manual,Yes,2,0-1 Miles,Europe,43,No
12274,Single,Male,10000,2,High School,Manual,Yes,0,0-1 Miles,Europe,35,No
20236,Single,Male,60000,3,Bachelors,Professional,No,2,0-1 Miles,Pacific,43,Yes
24149,Married,Male,10000,2,Partial College,Manual,Yes,0,1-2 Miles,Europe,49,No
26139,Single,Male,60000,1,Partial College,Skilled Manual,Yes,1,5-10 Miles,Pacific,45,No
18491,Single,Female,70000,2,High School,Professional,Yes,2,5-10 Miles,Pacific,49,Yes
22707,Single,Female,30000,0,Partial College,Clerical,No,1,2-5 Miles,Europe,30,No
20430,Married,Male,70000,2,Partial College,Skilled Manual,Yes,2,5-10 Miles,Pacific,52,Yes
27494,Single,Female,40000,2,Partial College,Skilled Manual,No,2,1-2 Miles,Pacific,53,Yes
26829,Married,Female,40000,0,Bachelors,Clerical,Yes,0,0-1 Miles,Europe,38,Yes
28395,Single,Male,40000,0,Bachelors,Professional,No,0,0-1 Miles,Europe,39,Yes
21006,Single,Female,20000,1,Partial College,Manual,No,0,0-1 Miles,Europe,46,Yes
14682,Single,Female,70000,0,Bachelors,Professional,No,1,5-10 Miles,Pacific,38,No
17650,Single,Female,40000,2,Partial College,Clerical,Yes,2,1-2 Miles,Europe,35,No
29191,Single,Female,130000,1,Graduate Degree,Management,No,1,0-1 Miles,Pacific,36,Yes
15030,Married,Male,20000,0,Bachelors,Clerical,Yes,0,0-1 Miles,Pacific,26,Yes
24140,Single,Male,10000,0,Graduate Degree,Manual,No,0,0-1 Miles,Europe,30,Yes
22496,Married,Female,30000,1,Bachelors,Skilled Manual,Yes,2,0-1 Miles,Europe,42,No
24065,Single,Female,20000,2,High School,Manual,Yes,0,0-1 Miles,Europe,40,Yes
19914,Married,Male,80000,5,Bachelors,Management,Yes,2,2-5 Miles,Europe,62,No
12871,Single,Female,30000,0,Partial College,Clerical,No,1,2-5 Miles,Europe,29,No
22988,Married,Female,40000,2,Bachelors,Management,Yes,2,5-10 Miles,Pacific,66,Yes
15922,Married,Male,150000,2,High School,Professional,Yes,4,0-1 Miles,Europe,48,No
12344,Single,Female,80000,0,Bachelors,Professional,No,3,10+ Miles,Pacific,31,No
23627,Single,Female,100000,3,Partial College,Management,No,4,5-10 Miles,Europe,56,No
27775,Single,Female,40000,0,Bachelors,Clerical,No,0,0-1 Miles,Europe,38,Yes
29301,Married,Male,80000,5,Bachelors,Professional,Yes,4,1-2 Miles,Pacific,40,No
12716,Single,Male,30000,0,Partial College,Clerical,Yes,1,2-5 Miles,Europe,32,No
12472,Married,Male,30000,1,Bachelors,Clerical,Yes,1,2-5 Miles,Europe,39,No
20970,Single,Male,10000,2,Partial College,Manual,Yes,1,0-1 Miles,Europe,52,Yes
26818,Single,Male,10000,3,High School,Manual,Yes,1,0-1 Miles,Europe,39,Yes
12993,Married,Male,60000,2,Bachelors,Professional,Yes,1,2-5 Miles,Pacific,37,No
14192,Married,Male,90000,4,High School,Management,Yes,3,5-10 Miles,Europe,56,Yes
19477,Married,Male,40000,0,Bachelors,Professional,Yes,0,0-1 Miles,Europe,40,Yes
26796,Single,Male,40000,2,Bachelors,Management,Yes,2,5-10 Miles,Pacific,65,Yes
21094,Single,Female,30000,2,Partial College,Clerical,Yes,2,0-1 Miles,Europe,42,No
12234,Married,Male,10000,2,Partial College,Manual,Yes,1,2-5 Miles,Europe,52,No
28683,Single,Female,10000,1,High School,Manual,No,1,5-10 Miles,Europe,35,Yes
17994,Single,Male,20000,2,High School,Manual,Yes,2,0-1 Miles,Europe,42,No
24273,Married,Female,20000,2,Partial High School,Clerical,Yes,2,5-10 Miles,Pacific,55,Yes
26547,Single,Female,30000,2,Partial College,Clerical,No,2,5-10 Miles,Pacific,60,Yes
22500,Single,Male,40000,0,Bachelors,Professional,No,0,0-1 Miles,Europe,40,Yes
23993,Single,Female,10000,0,Partial College,Manual,No,1,0-1 Miles,Pacific,26,Yes
14832,Married,Male,40000,1,Bachelors,Skilled Manual,Yes,0,0-1 Miles,Europe,42,Yes
16614,Married,Female,80000,0,Bachelors,Professional,Yes,3,10+ Miles,Pacific,32,No
20877,Single,Male,30000,1,Bachelors,Clerical,Yes,0,1-2 Miles,Europe,37,Yes
20729,Married,Female,40000,2,Partial College,Clerical,No,1,0-1 Miles,Europe,34,No
22464,Married,Male,40000,0,Graduate Degree,Clerical,Yes,0,0-1 Miles,Europe,37,Yes
19475,Married,Female,40000,0,Bachelors,Professional,No,0,0-1 Miles,Europe,40,Yes
19675,Married,Male,20000,4,High School,Skilled Manual,Yes,2,5-10 Miles,Pacific,60,No
12728,Single,Male,30000,0,Partial College,Clerical,No,1,1-2 Miles,Europe,27,No
26154,Married,Male,60000,1,Partial College,Skilled Manual,Yes,1,5-10 Miles,Pacific,43,Yes
29117,Single,Male,100000,1,Bachelors,Management,No,3,0-1 Miles,Pacific,48,No
17845,Single,Female,20000,0,Partial High School,Manual,No,2,1-2 Miles,Europe,32,No
25058,Married,Male,100000,1,Bachelors,Management,Yes,3,2-5 Miles,Pacific,47,No
23426,Single,Male,80000,5,Graduate Degree,Management,Yes,3,0-1 Miles,Pacific,40,No
14798,Single,Female,10000,4,Partial High School,Manual,Yes,2,0-1 Miles,Europe,41,Yes
12664,Married,Female,130000,5,Partial College,Professional,Yes,4,0-1 Miles,Europe,59,No
23979,Single,Male,10000,2,Partial College,Manual,No,0,0-1 Miles,Europe,50,No
25605,Single,Female,20000,2,Partial College,Manual,No,1,0-1 Miles,Europe,54,Yes
20797,Married,Female,10000,1,Bachelors,Manual,Yes,0,0-1 Miles,Europe,48,No
21980,Single,Female,60000,1,Bachelors,Professional,Yes,1,5-10 Miles,Pacific,44,Yes
25460,Married,Female,20000,2,High School,Manual,Yes,0,0-1 Miles,Europe,40,Yes
29181,Single,Female,60000,2,Bachelors,Professional,No,1,0-1 Miles,Pacific,38,Yes
24279,Single,Male,40000,2,Partial College,Skilled Manual,No,2,1-2 Miles,Pacific,52,No
22402,Married,Male,10000,0,Partial College,Manual,Yes,1,2-5 Miles,Pacific,25,Yes
15465,Married,Female,10000,0,Partial College,Manual,No,1,0-1 Miles,Pacific,25,No
26757,Single,Male,90000,1,Bachelors,Professional,Yes,1,2-5 Miles,Pacific,47,Yes
14233,Single,Male,100000,0,High School,Management,Yes,3,10+ Miles,Pacific,35,No
14058,Single,Male,70000,0,Bachelors,Professional,No,1,5-10 Miles,Pacific,41,Yes
12273,Married,Male,30000,1,Bachelors,Clerical,Yes,0,0-1 Miles,Europe,47,No
17203,Married,Female,130000,4,Partial College,Professional,Yes,4,5-10 Miles,Europe,61,Yes
18144,Married,Female,80000,5,Bachelors,Management,Yes,2,2-5 Miles,Europe,61,No
23963,Married,Male,10000,0,Partial High School,Manual,No,2,0-1 Miles,Europe,33,No
17907,Married,Female,10000,0,Partial College,Manual,Yes,1,2-5 Miles,Pacific,27,No
19442,Single,Male,50000,0,Graduate Degree,Skilled Manual,Yes,0,0-1 Miles,Europe,37,Yes
17504,Single,Female,80000,2,Partial College,Skilled Manual,Yes,2,5-10 Miles,Pacific,52,Yes
12253,Single,Female,20000,0,Partial College,Manual,Yes,0,0-1 Miles,Pacific,29,Yes
27304,Single,Female,110000,2,Partial College,Professional,No,3,5-10 Miles,Europe,48,No
14191,Married,Male,160000,4,Partial College,Professional,No,2,10+ Miles,Europe,55,Yes
12212,Married,Female,10000,0,Graduate Degree,Manual,Yes,0,0-1 Miles,Europe,37,Yes
25529,Single,Male,10000,1,Graduate Degree,Manual,Yes,0,0-1 Miles,Europe,44,No
22170,Married,Female,30000,3,Partial College,Clerical,No,2,1-2 Miles,Pacific,55,Yes
19445,Married,Female,10000,2,High School,Manual,No,1,0-1 Miles,Europe,38,No
15265,Single,Male,40000,2,Bachelors,Management,Yes,2,5-10 Miles,Pacific,66,Yes
28918,Married,Female,130000,4,High School,Management,No,4,10+ Miles,Europe,58,No
15799,Married,Female,90000,1,Bachelors,Professional,Yes,1,2-5 Miles,Pacific,47,Yes
11047,Married,Female,30000,3,High School,Skilled Manual,No,2,1-2 Miles,Pacific,56,Yes
18151,Single,Male,80000,5,Partial College,Professional,No,2,10+ Miles,Europe,59,No
20606,Married,Female,70000,0,Bachelors,Professional,Yes,4,10+ Miles,Pacific,32,Yes
19482,Married,Male,30000,1,Partial College,Clerical,Yes,1,0-1 Miles,Europe,44,Yes
16489,Married,Male,30000,3,High School,Skilled Manual,Yes,2,5-10 Miles,Pacific,55,No
26944,Single,Male,10000,2,High School,Manual,Yes,0,0-1 Miles,Europe,36,Yes
15682,Single,Female,80000,5,Bachelors,Management,Yes,2,10+ Miles,Europe,62,No
26032,Married,Female,70000,5,Bachelors,Professional,Yes,4,10+ Miles,Pacific,41,No
17843,Single,Female,10000,0,Partial High School,Manual,No,2,0-1 Miles,Europe,32,No
25559,Single,Male,20000,0,Bachelors,Clerical,Yes,0,0-1 Miles,Pacific,25,Yes
16209,Single,Female,50000,0,Graduate Degree,Skilled Manual,Yes,0,1-2 Miles,Europe,36,No
11147,Married,Male,60000,2,Graduate Degree,Management,Yes,1,0-1 Miles,Pacific,67,Yes
15214,Single,Female,100000,0,Graduate Degree,Management,No,1,1-2 Miles,Pacific,39,Yes
11453,Single,Male,80000,0,Bachelors,Professional,No,3,10+ Miles,Pacific,33,Yes
24584,Single,Male,60000,0,Bachelors,Professional,No,3,2-5 Miles,Pacific,31,No
12585,Married,Male,10000,1,High School,Manual,Yes,0,2-5 Miles,Pacific,27,Yes
18626,Single,Male,40000,2,Partial College,Clerical,Yes,0,1-2 Miles,Europe,33,Yes
29298,Single,Female,60000,1,Partial College,Skilled Manual,Yes,1,5-10 Miles,Pacific,46,Yes
24842,Single,Female,90000,3,High School,Professional,No,1,2-5 Miles,Europe,51,No
15657,Married,Male,30000,3,Graduate Degree,Clerical,Yes,0,0-1 Miles,Europe,46,Yes
11415,Single,Male,90000,5,Partial College,Professional,No,2,10+ Miles,Europe,62,No
28729,Single,Female,20000,0,Partial High School,Manual,Yes,2,1-2 Miles,Europe,26,Yes
22633,Single,Female,40000,0,Graduate Degree,Clerical,Yes,0,0-1 Miles,Europe,37,Yes
25649,Single,Female,30000,3,Partial College,Clerical,Yes,0,0-1 Miles,Europe,42,Yes
14669,Married,Female,80000,4,Graduate Degree,Management,Yes,1,0-1 Miles,Pacific,36,No
19299,Married,Female,50000,0,Graduate Degree,Skilled Manual,Yes,0,0-1 Miles,Europe,36,Yes
20946,Single,Female,30000,0,Partial College,Clerical,No,1,2-5 Miles,Europe,30,No
11451,Single,Male,70000,0,Bachelors,Professional,No,4,10+ Miles,Pacific,31,Yes
25553,Married,Male,30000,1,Bachelors,Clerical,Yes,0,0-1 Miles,Europe,65,Yes
27951,Single,Male,80000,4,Partial College,Professional,No,2,2-5 Miles,Europe,54,Yes
25026,Married,Male,20000,2,Partial High School,Clerical,Yes,3,5-10 Miles,Pacific,54,No
13673,Single,Female,20000,0,Partial High School,Manual,No,2,0-1 Miles,Europe,25,No
16043,Single,Male,10000,1,Bachelors,Manual,Yes,0,0-1 Miles,Europe,48,No
22399,Single,Male,10000,0,Partial College,Manual,Yes,1,1-2 Miles,Pacific,26,Yes
27696,Married,Male,60000,1,Bachelors,Professional,Yes,1,5-10 Miles,Pacific,43,Yes
25313,Single,Male,10000,0,Partial High School,Manual,No,2,1-2 Miles,Europe,35,No
13813,Married,Female,30000,3,Partial College,Clerical,No,0,0-1 Miles,Europe,42,No
18711,Single,Female,70000,5,Bachelors,Professional,Yes,4,10+ Miles,Pacific,39,No
19650,Married,Female,30000,2,Partial College,Clerical,No,2,0-1 Miles,Pacific,67,No
14135,Married,Male,20000,1,Partial College,Manual,Yes,0,1-2 Miles,Europe,63,No
12833,Single,Female,20000,3,High School,Manual,Yes,1,0-1 Miles,Europe,42,Yes
26849,Married,Male,10000,3,Partial High School,Manual,Yes,2,0-1 Miles,Europe,43,No
20962,Married,Female,20000,1,Graduate Degree,Clerical,Yes,0,0-1 Miles,Europe,45,No
28915,Single,Male,80000,5,High School,Management,Yes,3,10+ Miles,Europe,57,No
22830,Married,Male,120000,4,Partial College,Management,Yes,3,10+ Miles,Europe,56,No
14777,Married,Female,40000,0,Bachelors,Clerical,Yes,0,0-1 Miles,Europe,38,Yes
12591,Married,Female,30000,4,Graduate Degree,Clerical,Yes,0,0-1 Miles,Europe,45,No
24174,Married,Male,20000,0,Bachelors,Clerical,Yes,0,0-1 Miles,Pacific,27,Yes
24611,Single,Male,90000,0,Bachelors,Professional,No,4,10+ Miles,Pacific,35,Yes
11340,Married,Female,10000,1,Graduate Degree,Clerical,Yes,0,0-1 Miles,Europe,70,Yes
25693,Single,Female,30000,5,Graduate Degree,Clerical,Yes,0,0-1 Miles,Europe,44,Yes
25555,Married,Female,10000,0,Partial College,Manual,No,1,0-1 Miles,Pacific,26,Yes
22006,Married,Male,70000,5,Partial College,Skilled Manual,Yes,3,5-10 Miles,Pacific,46,No
20060,Single,Female,30000,0,High School,Manual,No,1,2-5 Miles,Europe,34,Yes
17702,Married,Male,10000,1,Graduate Degree,Manual,Yes,0,0-1 Miles,Europe,37,No
12503,Single,Female,30000,3,Partial College,Clerical,Yes,2,0-1 Miles,Europe,27,No
23908,Single,Male,30000,1,Bachelors,Clerical,No,1,0-1 Miles,Europe,39,Yes
22527,Single,Female,20000,0,High School,Manual,No,1,2-5 Miles,Europe,29,No
19057,Married,Female,120000,3,Bachelors,Management,No,2,10+ Miles,Europe,52,Yes
18494,Married,Male,110000,5,Bachelors,Management,Yes,4,2-5 Miles,Pacific,48,Yes
11249,Married,Female,130000,3,Partial College,Professional,Yes,3,0-1 Miles,Europe,51,Yes
21568,Married,Female,100000,0,High School,Management,Yes,4,10+ Miles,Pacific,34,Yes
13981,Married,Female,10000,5,High School,Skilled Manual,No,3,1-2 Miles,Pacific,62,No
23432,Single,Male,70000,0,Bachelors,Professional,Yes,1,5-10 Miles,Pacific,37,Yes
22931,Married,Male,100000,5,Graduate Degree,Management,No,1,1-2 Miles,Pacific,78,Yes
18172,Married,Male,130000,4,High School,Professional,Yes,3,0-1 Miles,Europe,55,No
12666,Single,Male,60000,0,Bachelors,Professional,No,4,2-5 Miles,Pacific,31,No
20598,Married,Male,100000,3,Partial High School,Professional,Yes,0,10+ Miles,Europe,59,Yes
21375,Single,Male,20000,2,Partial High School,Clerical,Yes,2,5-10 Miles,Pacific,57,No
20839,Single,Female,30000,3,Graduate Degree,Clerical,Yes,0,0-1 Miles,Europe,47,Yes
21738,Married,Male,20000,1,Graduate Degree,Clerical,Yes,0,0-1 Miles,Europe,43,No
14164,Single,Female,50000,0,Graduate Degree,Skilled Manual,Yes,0,0-1 Miles,Europe,36,Yes
14193,Single,Female,100000,3,Partial College,Management,Yes,4,10+ Miles,Europe,56,No
12705,Married,Male,150000,0,Bachelors,Management,Yes,4,0-1 Miles,Pacific,37,Yes
22672,Single,Female,30000,2,Partial College,Clerical,Yes,0,0-1 Miles,Europe,43,No
26219,Married,Female,40000,1,Bachelors,Skilled Manual,Yes,1,1-2 Miles,Europe,33,Yes
28468,Married,Female,10000,2,Partial College,Manual,Yes,0,1-2 Miles,Europe,51,No
23419,Single,Female,70000,5,Bachelors,Professional,Yes,3,10+ Miles,Pacific,39,No
17964,Married,Male,40000,0,Graduate Degree,Clerical,Yes,0,0-1 Miles,Europe,37,Yes
20919,Single,Female,30000,2,Partial College,Clerical,Yes,2,0-1 Miles,Europe,42,No
20927,Single,Female,20000,5,High School,Manual,Yes,2,0-1 Miles,Europe,27,No
13133,Single,Male,100000,5,Bachelors,Professional,Yes,1,5-10 Miles,Pacific,47,Yes
19626,Married,Male,70000,5,Partial College,Skilled Manual,Yes,3,5-10 Miles,Pacific,45,No
21039,Single,Female,50000,0,Graduate Degree,Skilled Manual,No,0,0-1 Miles,Europe,37,Yes
12231,Single,Female,10000,2,Partial College,Manual,Yes,0,0-1 Miles,Europe,51,Yes
25665,Single,Female,20000,0,High School,Manual,No,1,1-2 Miles,Europe,28,No
24061,Married,Male,10000,4,Partial High School,Manual,Yes,1,0-1 Miles,Europe,40,Yes
26879,Single,Female,20000,0,High School,Manual,No,1,2-5 Miles,Europe,30,No
12284,Married,Female,30000,0,Bachelors,Clerical,No,0,0-1 Miles,Europe,36,Yes
26654,Married,Female,90000,1,Graduate Degree,Management,Yes,0,0-1 Miles,Pacific,37,Yes
14545,Married,Female,10000,2,Partial College,Manual,Yes,0,1-2 Miles,Europe,49,No
24201,Married,Female,10000,2,High School,Manual,Yes,0,0-1 Miles,Europe,37,Yes
20625,Married,Male,100000,0,High School,Management,Yes,3,10+ Miles,Pacific,35,Yes
16390,Single,Male,30000,1,Bachelors,Clerical,No,0,0-1 Miles,Europe,38,Yes
14804,Single,Female,10000,3,Partial High School,Manual,Yes,2,0-1 Miles,Europe,43,No
12629,Single,Male,20000,1,Partial College,Manual,No,0,0-1 Miles,Europe,37,No
14696,Single,Male,10000,0,Partial High School,Manual,No,2,0-1 Miles,Europe,34,No
22005,Married,Female,70000,5,Partial College,Skilled Manual,No,3,5-10 Miles,Pacific,46,No
14544,Single,Male,10000,1,Partial College,Manual,Yes,0,0-1 Miles,Europe,49,No
14312,Married,Female,60000,1,Partial College,Skilled Manual,Yes,1,5-10 Miles,Pacific,45,No
29120,Single,Female,100000,1,Bachelors,Management,Yes,4,2-5 Miles,Pacific,48,No
24187,Single,Female,30000,3,Graduate Degree,Clerical,No,0,0-1 Miles,Europe,46,Yes
15758,Married,Male,130000,0,Graduate Degree,Management,Yes,0,5-10 Miles,Pacific,48,No
29094,Married,Male,30000,3,High School,Skilled Manual,Yes,2,5-10 Miles,Pacific,54,Yes
28319,Single,Female,60000,1,Partial College,Skilled Manual,No,1,0-1 Miles,Pacific,46,Yes
16406,Married,Male,40000,0,Bachelors,Clerical,No,0,0-1 Miles,Europe,38,Yes
20923,Married,Female,40000,1,Bachelors,Skilled Manual,Yes,0,0-1 Miles,Europe,42,Yes
11378,Single,Female,10000,1,High School,Manual,No,1,2-5 Miles,Europe,46,Yes
20851,Single,Male,20000,0,Partial College,Manual,No,1,2-5 Miles,Europe,36,Yes
21557,Single,Female,110000,0,Partial College,Management,Yes,3,10+ Miles,Pacific,32,Yes
26663,Single,Female,60000,2,Bachelors,Professional,No,1,0-1 Miles,Pacific,39,Yes
11896,Married,Male,100000,1,Graduate Degree,Management,Yes,0,2-5 Miles,Pacific,36,Yes
14189,Married,Female,90000,4,High School,Professional,No,2,2-5 Miles,Europe,54,Yes
13136,Married,Female,30000,2,Partial College,Clerical,No,2,5-10 Miles,Pacific,69,No
25906,Single,Female,10000,5,High School,Skilled Manual,No,2,1-2 Miles,Pacific,62,No
17926,Married,Female,20000,0,Bachelors,Clerical,No,0,0-1 Miles,Pacific,28,Yes
26928,Single,Male,30000,1,Bachelors,Clerical,Yes,0,0-1 Miles,Europe,62,Yes
20897,Married,Female,30000,1,Bachelors,Skilled Manual,Yes,2,0-1 Miles,Europe,40,No
28207,Married,Male,80000,4,Graduate Degree,Management,Yes,1,0-1 Miles,Pacific,36,Yes
25923,Single,Male,10000,2,Partial High School,Clerical,Yes,2,5-10 Miles,Pacific,58,No
11000,Married,Male,90000,2,Bachelors,Professional,Yes,0,1-2 Miles,Pacific,40,Yes
20974,Married,Male,10000,2,Bachelors,Clerical,Yes,1,0-1 Miles,Europe,66,No
28758,Married,Male,40000,2,Partial College,Clerical,Yes,1,1-2 Miles,Europe,35,Yes
11381,Married,Female,20000,2,Partial College,Manual,Yes,1,2-5 Miles,Europe,47,Yes
17522,Married,Male,120000,4,Bachelors,Management,Yes,1,2-5 Miles,Pacific,47,No
21207,Married,Male,60000,1,Partial College,Skilled Manual,Yes,1,5-10 Miles,Pacific,46,No
28102,Married,Male,20000,4,High School,Skilled Manual,Yes,2,5-10 Miles,Pacific,58,Yes
23105,Single,Male,40000,3,Partial High School,Clerical,No,2,5-10 Miles,Pacific,52,Yes
18740,Married,Male,80000,5,Bachelors,Professional,No,1,0-1 Miles,Pacific,47,Yes
21213,Single,Male,70000,0,Bachelors,Professional,No,1,5-10 Miles,Pacific,41,No
17352,Married,Male,50000,2,Graduate Degree,Management,Yes,1,5-10 Miles,Pacific,64,Yes
14154,Married,Male,30000,0,Bachelors,Clerical,Yes,0,0-1 Miles,Europe,35,Yes
19066,Married,Male,130000,4,Partial College,Professional,No,3,10+ Miles,Europe,54,No
11386,Married,Female,30000,3,Bachelors,Clerical,Yes,0,0-1 Miles,Europe,45,No
20228,Married,Male,100000,0,Graduate Degree,Management,Yes,0,2-5 Miles,Pacific,40,Yes
16675,Single,Female,160000,0,Graduate Degree,Management,No,3,0-1 Miles,Pacific,47,Yes
16410,Single,Female,10000,4,Partial High School,Manual,Yes,2,0-1 Miles,Europe,41,Yes
27760,Single,Female,40000,0,Graduate Degree,Clerical,No,0,0-1 Miles,Europe,37,Yes
22930,Married,Male,90000,4,Bachelors,Professional,Yes,0,1-2 Miles,Pacific,38,Yes
23780,Single,Male,40000,2,Partial College,Clerical,No,2,0-1 Miles,Europe,36,Yes
20994,Married,Female,20000,0,Bachelors,Clerical,No,0,0-1 Miles,Pacific,26,Yes
28379,Married,Male,30000,1,Bachelors,Skilled Manual,Yes,2,0-1 Miles,Europe,40,No
14865,Single,Male,40000,2,Partial College,Clerical,Yes,2,1-2 Miles,Europe,36,No
12663,Married,Female,90000,5,Partial High School,Skilled Manual,Yes,2,10+ Miles,Europe,59,No
24898,Single,Female,80000,0,Bachelors,Professional,Yes,3,10+ Miles,Pacific,32,No
19508,Married,Male,10000,0,Partial High School,Manual,No,2,0-1 Miles,Europe,30,No
11489,Single,Female,20000,0,Partial High School,Manual,No,2,1-2 Miles,Europe,35,Yes
18160,Married,Male,130000,3,High School,Professional,Yes,4,5-10 Miles,Europe,51,Yes
25241,Married,Male,90000,2,Bachelors,Professional,Yes,1,5-10 Miles,Pacific,47,No
24369,Married,Female,80000,5,Graduate Degree,Management,No,2,0-1 Miles,Pacific,39,No
27165,Single,Male,20000,0,Partial High School,Manual,No,2,0-1 Miles,Europe,34,No
29424,Married,Male,10000,0,Partial High School,Manual,Yes,2,0-1 Miles,Europe,32,No
15926,Single,Female,120000,3,High School,Professional,Yes,4,5-10 Miles,Europe,50,Yes
14554,Married,Male,20000,1,Bachelors,Clerical,Yes,0,0-1 Miles,Europe,66,No
16468,Single,Male,30000,0,Partial College,Clerical,Yes,1,2-5 Miles,Europe,30,No
19174,Single,Female,30000,0,High School,Manual,No,1,2-5 Miles,Europe,32,Yes
19183,Single,Male,10000,0,Partial High School,Manual,Yes,2,1-2 Miles,Europe,35,No
13683,Single,Female,30000,0,High School,Manual,No,1,2-5 Miles,Europe,32,No
17848,Single,Male,30000,0,Partial College,Clerical,No,1,2-5 Miles,Europe,31,Yes
17894,Married,Female,20000,1,Bachelors,Clerical,Yes,0,0-1 Miles,Europe,50,Yes
25651,Married,Male,40000,1,Bachelors,Skilled Manual,No,0,0-1 Miles,Europe,43,Yes
22936,Single,Female,60000,1,Partial College,Skilled Manual,No,1,0-1 Miles,Pacific,45,Yes
23915,Married,Male,20000,2,High School,Manual,Yes,2,0-1 Miles,Europe,42,No
24121,Single,Female,30000,0,Partial College,Clerical,No,1,0-1 Miles,Europe,29,Yes
27878,Single,Male,20000,0,Partial College,Manual,No,0,0-1 Miles,Pacific,28,Yes
13572,Single,Male,10000,3,High School,Manual,Yes,0,0-1 Miles,Europe,37,Yes
27941,Married,Female,80000,4,Partial College,Professional,Yes,2,2-5 Miles,Europe,53,No
26354,Single,Male,40000,0,Graduate Degree,Clerical,No,0,0-1 Miles,Europe,38,Yes
14785,Single,Male,30000,1,Bachelors,Clerical,No,1,1-2 Miles,Europe,39,No
17238,Single,Male,80000,0,Bachelors,Professional,Yes,3,10+ Miles,Pacific,32,No
23608,Married,Female,150000,3,High School,Professional,Yes,3,0-1 Miles,Europe,51,Yes
22538,Single,Female,10000,0,Partial High School,Manual,Yes,2,1-2 Miles,Europe,33,No
12332,Married,Male,90000,4,High School,Management,Yes,3,5-10 Miles,Europe,58,Yes
17230,Married,Male,80000,0,Bachelors,Professional,Yes,3,10+ Miles,Pacific,30,No
13082,Single,Male,130000,0,Graduate Degree,Management,Yes,0,2-5 Miles,Pacific,48,Yes
22518,Single,Female,30000,3,Partial College,Clerical,No,2,0-1 Miles,Europe,27,Yes
13687,Married,Male,40000,1,Bachelors,Skilled Manual,Yes,1,0-1 Miles,Europe,33,Yes
23571,Married,Female,40000,2,Bachelors,Management,Yes,2,0-1 Miles,Pacific,66,Yes
19305,Single,Female,10000,2,High School,Manual,Yes,1,0-1 Miles,Europe,38,Yes
22636,Single,Female,40000,0,Bachelors,Clerical,No,0,0-1 Miles,Europe,38,Yes
17310,Married,Male,60000,1,Partial College,Skilled Manual,Yes,1,0-1 Miles,Pacific,45,Yes
12133,Married,Female,130000,3,Partial College,Professional,Yes,3,5-10 Miles,Europe,50,Yes
25918,Single,Female,30000,2,Partial College,Clerical,No,2,5-10 Miles,Pacific,60,Yes
25752,Single,Female,20000,2,Partial College,Manual,No,1,0-1 Miles,Europe,53,Yes
17324,Married,Female,100000,4,Bachelors,Professional,Yes,1,10+ Miles,Pacific,46,No
22918,Single,Male,80000,5,Graduate Degree,Management,Yes,3,0-1 Miles,Pacific,40,No
12510,Married,Male,40000,1,Bachelors,Skilled Manual,Yes,1,0-1 Miles,Europe,43,Yes
25512,Single,Male,20000,0,High School,Manual,No,1,2-5 Miles,Europe,30,No
16179,Single,Female,80000,5,Bachelors,Professional,Yes,4,1-2 Miles,Pacific,38,No
15628,Married,Female,40000,1,Bachelors,Skilled Manual,Yes,1,0-1 Miles,Europe,89,No
20977,Married,Male,20000,1,Bachelors,Clerical,Yes,0,0-1 Miles,Europe,64,Yes
18140,Married,Male,130000,3,Partial College,Professional,No,3,5-10 Miles,Europe,51,Yes
20417,Married,Male,30000,3,Partial College,Clerical,No,2,5-10 Miles,Pacific,56,No
18267,Married,Male,60000,3,Bachelors,Professional,Yes,2,5-10 Miles,Pacific,43,No
13620,Single,Male,70000,0,Bachelors,Professional,No,3,10+ Miles,Pacific,30,Yes
22974,Married,Female,30000,2,Partial College,Clerical,Yes,2,5-10 Miles,Pacific,69,No
13586,Married,Male,80000,4,Partial College,Professional,Yes,2,10+ Miles,Europe,53,No
17978,Married,Male,40000,0,Graduate Degree,Clerical,Yes,0,0-1 Miles,Europe,37,Yes
12581,Single,Female,10000,0,Partial College,Manual,No,1,0-1 Miles,Pacific,28,Yes
18018,Single,Male,30000,3,Partial College,Clerical,Yes,0,0-1 Miles,Europe,43,No
28957,Single,Female,120000,0,Partial High School,Professional,Yes,4,10+ Miles,Pacific,34,Yes
13690,Single,Female,20000,0,Partial High School,Manual,No,2,1-2 Miles,Europe,34,Yes
12568,Married,Female,30000,1,Bachelors,Clerical,Yes,0,0-1 Miles,Europe,64,No
13122,Married,Female,80000,0,Bachelors,Professional,Yes,1,1-2 Miles,Pacific,41,Yes
21184,Single,Male,70000,0,Bachelors,Professional,No,1,5-10 Miles,Pacific,38,No
26150,Single,Female,70000,0,Bachelors,Professional,No,1,0-1 Miles,Pacific,41,Yes
24151,Single,Male,20000,1,Bachelors,Clerical,No,0,0-1 Miles,Europe,51,No
23962,Married,Female,10000,0,Partial High School,Manual,Yes,2,1-2 Miles,Europe,32,No
17793,Married,Female,40000,0,Bachelors,Clerical,Yes,0,0-1 Miles,Europe,38,Yes
14926,Married,Male,30000,1,Bachelors,Clerical,Yes,0,0-1 Miles,Europe,38,Yes
16163,Single,Male,60000,2,Bachelors,Professional,Yes,1,2-5 Miles,Pacific,38,Yes
21365,Married,Female,10000,2,Partial High School,Clerical,Yes,2,5-10 Miles,Pacific,58,No
27771,Single,Male,30000,1,Bachelors,Clerical,Yes,1,1-2 Miles,Europe,39,Yes
26167,Single,Female,40000,2,Bachelors,Management,No,1,5-10 Miles,Pacific,53,Yes
25792,Single,Female,110000,3,Bachelors,Management,Yes,4,10+ Miles,Europe,53,No
11555,Married,Female,40000,1,Bachelors,Clerical,Yes,0,0-1 Miles,Europe,80,No
22381,Married,Male,10000,1,Graduate Degree,Manual,Yes,0,0-1 Miles,Europe,44,No
17882,Married,Male,20000,1,Graduate Degree,Clerical,Yes,0,0-1 Miles,Europe,44,No
22174,Married,Male,30000,3,High School,Skilled Manual,Yes,2,5-10 Miles,Pacific,54,Yes
22439,Married,Female,30000,0,Bachelors,Clerical,Yes,0,0-1 Miles,Europe,37,Yes
18012,Married,Female,40000,1,Bachelors,Skilled Manual,Yes,0,0-1 Miles,Europe,41,No
27582,Single,Female,90000,2,Bachelors,Professional,No,0,0-1 Miles,Pacific,36,Yes
12744,Single,Female,40000,2,Partial College,Clerical,Yes,0,0-1 Miles,Europe,33,No
22821,Married,Female,130000,3,Partial College,Professional,Yes,4,0-1 Miles,Europe,52,No
20171,Married,Female,20000,2,Partial College,Manual,Yes,1,0-1 Miles,Europe,46,Yes
11116,Married,Male,70000,5,Partial College,Skilled Manual,Yes,2,5-10 Miles,Pacific,43,No
20053,Single,Male,40000,2,Partial College,Clerical,Yes,0,0-1 Miles,Europe,34,No
25266,Single,Female,30000,2,Partial College,Clerical,No,2,5-10 Miles,Pacific,67,No
17960,Married,Female,40000,0,Graduate Degree,Clerical,Yes,0,0-1 Miles,Europe,35,Yes
13961,Married,Female,80000,5,Graduate Degree,Management,Yes,3,0-1 Miles,Pacific,40,No
11897,Single,Male,60000,2,Bachelors,Professional,No,1,0-1 Miles,Pacific,37,Yes
11139,Single,Female,30000,2,Partial College,Clerical,No,2,5-10 Miles,Pacific,67,No
11576,Married,Male,30000,1,Bachelors,Skilled Manual,Yes,2,0-1 Miles,Europe,41,Yes
19255,Single,Male,10000,2,Partial College,Manual,Yes,1,0-1 Miles,Europe,51,Yes
18153,Married,Female,100000,2,Bachelors,Management,Yes,4,10+ Miles,Europe,59,No
14547,Married,Male,10000,2,Partial College,Manual,Yes,0,1-2 Miles,Europe,51,No
24901,Single,Male,110000,0,Partial College,Management,No,3,10+ Miles,Pacific,32,Yes
27169,Single,Male,30000,0,High School,Manual,Yes,1,2-5 Miles,Europe,34,Yes
14805,Single,Female,10000,3,Partial High School,Manual,Yes,2,0-1 Miles,Europe,43,No
15822,Married,Male,40000,2,Bachelors,Management,Yes,2,0-1 Miles,Pacific,67,No
19389,Single,Male,30000,0,Partial College,Clerical,No,1,2-5 Miles,Europe,28,No
17048,Single,Female,90000,1,Graduate Degree,Management,Yes,0,0-1 Miles,Pacific,36,Yes
22204,Married,Male,110000,4,Bachelors,Management,Yes,3,2-5 Miles,Pacific,48,No
12718,Single,Female,30000,0,Partial College,Clerical,Yes,1,2-5 Miles,Europe,31,No
15019,Single,Female,30000,3,High School,Skilled Manual,Yes,2,5-10 Miles,Pacific,55,No
28488,Single,Male,20000,0,Partial College,Manual,Yes,0,0-1 Miles,Pacific,28,Yes
21891,Married,Female,110000,0,High School,Management,Yes,3,10+ Miles,Pacific,34,Yes
27814,Single,Female,30000,3,Partial College,Clerical,No,1,0-1 Miles,Europe,26,No
22175,Married,Female,30000,3,High School,Skilled Manual,Yes,2,5-10 Miles,Pacific,53,Yes
29447,Single,Female,10000,2,Bachelors,Clerical,No,1,2-5 Miles,Europe,68,No
19784,Married,Female,80000,2,High School,Skilled Manual,Yes,2,5-10 Miles,Pacific,50,Yes
27824,Single,Female,30000,3,Partial College,Clerical,Yes,2,0-1 Miles,Europe,28,Yes
24093,Single,Female,80000,0,Graduate Degree,Skilled Manual,No,0,0-1 Miles,Europe,40,Yes
19618,Married,Male,70000,5,Partial College,Skilled Manual,Yes,2,0-1 Miles,Pacific,44,No
21561,Single,Male,90000,0,Bachelors,Professional,No,3,10+ Miles,Pacific,34,Yes
11061,Married,Male,80000,2,Partial College,Skilled Manual,Yes,2,5-10 Miles,Pacific,52,Yes
26651,Single,Male,80000,4,Graduate Degree,Management,Yes,0,0-1 Miles,Pacific,36,Yes
21108,Married,Female,40000,1,Bachelors,Skilled Manual,Yes,1,0-1 Miles,Europe,43,Yes
12731,Single,Male,30000,0,High School,Manual,No,1,1-2 Miles,Europe,32,No
25307,Married,Female,40000,1,Bachelors,Skilled Manual,Yes,1,1-2 Miles,Europe,32,Yes
14278,Married,Female,130000,0,Graduate Degree,Management,Yes,1,10+ Miles,Pacific,48,No
20711,Married,Female,40000,1,Bachelors,Skilled Manual,Yes,0,1-2 Miles,Europe,32,Yes
11383,Married,Female,30000,3,Graduate Degree,Clerical,Yes,0,0-1 Miles,Europe,46,No
12497,Married,Female,40000,1,Bachelors,Skilled Manual,Yes,0,0-1 Miles,Europe,42,No
16559,Single,Female,10000,2,High School,Manual,Yes,0,0-1 Miles,Europe,36,Yes
11585,Married,Female,40000,1,Bachelors,Skilled Manual,Yes,0,0-1 Miles,Europe,41,No
20277,Married,Female,30000,2,Partial College,Clerical,No,2,0-1 Miles,Pacific,69,No
26765,Single,Female,70000,5,Partial College,Skilled Manual,Yes,2,5-10 Miles,Pacific,45,No
12389,Single,Male,30000,0,High School,Manual,No,1,2-5 Miles,Europe,34,No
13585,Married,Female,80000,4,Partial College,Professional,No,1,2-5 Miles,Europe,53,Yes
26385,Single,Male,120000,3,High School,Professional,No,4,5-10 Miles,Europe,50,No
12236,Married,Female,20000,1,Partial College,Manual,Yes,0,0-1 Miles,Europe,65,No
21560,Married,Male,120000,0,Partial High School,Professional,Yes,4,10+ Miles,Pacific,32,Yes
21554,Single,Female,80000,0,Bachelors,Professional,No,3,10+ Miles,Pacific,33,No
13662,Single,Male,20000,0,Partial High School,Manual,Yes,2,1-2 Miles,Europe,31,Yes
13089,Married,Female,120000,1,Bachelors,Management,Yes,2,0-1 Miles,Pacific,46,Yes
14791,Married,Female,40000,0,Bachelors,Clerical,Yes,0,0-1 Miles,Europe,39,Yes
19331,Single,Male,20000,2,High School,Manual,Yes,1,0-1 Miles,Europe,40,No
17754,Single,Female,30000,3,Bachelors,Clerical,Yes,0,0-1 Miles,Europe,46,Yes
11149,Married,Male,40000,2,Bachelors,Management,Yes,2,0-1 Miles,Pacific,65,No
16549,Single,Female,30000,3,Bachelors,Clerical,Yes,0,0-1 Miles,Europe,47,Yes
24305,Single,Male,100000,1,Bachelors,Management,No,3,0-1 Miles,Pacific,46,Yes
18253,Married,Female,80000,5,Graduate Degree,Management,Yes,3,0-1 Miles,Pacific,40,No
20147,Married,Female,30000,1,Bachelors,Clerical,Yes,0,0-1 Miles,Europe,65,No
15612,Single,Male,30000,0,High School,Manual,No,1,1-2 Miles,Europe,28,No
28323,Single,Male,70000,0,Bachelors,Professional,No,2,5-10 Miles,Pacific,43,Yes
22634,Single,Female,40000,0,Graduate Degree,Clerical,Yes,0,0-1 Miles,Europe,38,Yes
15665,Married,Female,30000,0,Bachelors,Clerical,Yes,0,0-1 Miles,Europe,47,Yes
27585,Married,Female,90000,2,Bachelors,Professional,No,0,0-1 Miles,Pacific,36,Yes
19748,Married,Male,20000,4,High School,Skilled Manual,No,2,1-2 Miles,Pacific,60,No
21974,Single,Female,70000,0,Bachelors,Professional,Yes,1,5-10 Miles,Pacific,42,Yes
14032,Married,Male,70000,2,High School,Skilled Manual,No,2,1-2 Miles,Pacific,50,Yes
22610,Married,Male,30000,0,Bachelors,Clerical,Yes,0,0-1 Miles,Europe,35,Yes
26984,Married,Male,40000,1,Bachelors,Skilled Manual,Yes,1,0-1 Miles,Europe,32,Yes
18294,Married,Female,90000,1,Bachelors,Professional,Yes,1,5-10 Miles,Pacific,46,No
28564,Single,Female,40000,2,Partial College,Clerical,Yes,0,1-2 Miles,Europe,33,Yes
28521,Single,Male,40000,0,Graduate Degree,Clerical,No,0,0-1 Miles,Europe,36,Yes
15450,Married,Male,10000,1,Graduate Degree,Clerical,Yes,0,0-1 Miles,Europe,70,No
25681,Single,Female,30000,0,Partial College,Clerical,No,1,2-5 Miles,Europe,31,Yes
19491,Single,Male,30000,2,Partial College,Clerical,Yes,2,0-1 Miles,Europe,42,No
26415,Married,Female,90000,4,Partial High School,Skilled Manual,Yes,4,10+ Miles,Europe,58,No
12821,Married,Male,40000,0,Bachelors,Clerical,Yes,0,0-1 Miles,Europe,39,No
15629,Single,Female,10000,0,Partial High School,Manual,Yes,2,1-2 Miles,Europe,34,No
27835,Married,Male,20000,0,Partial High School,Manual,Yes,2,0-1 Miles,Europe,32,No
11738,Married,Male,60000,4,Bachelors,Professional,Yes,0,2-5 Miles,North America,46,No
25065,Married,Male,70000,2,Partial High School,Skilled Manual,Yes,2,5-10 Miles,North America,48,No
26238,Single,Female,40000,3,Partial College,Clerical,Yes,1,1-2 Miles,North America,31,Yes
23707,Single,Male,70000,5,Bachelors,Management,Yes,3,10+ Miles,North America,60,Yes
27650,Married,Male,70000,4,High School,Professional,Yes,0,5-10 Miles,North America,51,No
24981,Married,Male,60000,2,Partial College,Professional,Yes,2,10+ Miles,North America,56,No
20678,Single,Female,60000,3,Bachelors,Skilled Manual,Yes,1,2-5 Miles,North America,40,Yes
15302,Single,Female,70000,1,Graduate Degree,Professional,Yes,0,2-5 Miles,North America,34,Yes
26012,Married,Male,80000,1,Partial College,Skilled Manual,Yes,1,2-5 Miles,North America,48,Yes
26575,Single,Female,40000,0,High School,Skilled Manual,No,2,1-2 Miles,North America,31,Yes
15559,Married,Male,60000,5,Bachelors,Professional,Yes,1,2-5 Miles,North America,47,No
19235,Married,Female,50000,0,Graduate Degree,Skilled Manual,Yes,0,0-1 Miles,North America,34,No
15275,Married,Male,40000,0,Partial College,Skilled Manual,Yes,1,5-10 Miles,North America,29,No
20339,Married,Female,130000,1,Bachelors,Management,Yes,4,2-5 Miles,North America,44,Yes
25405,Married,Male,70000,2,Bachelors,Skilled Manual,Yes,1,2-5 Miles,North America,38,Yes
15940,Married,Male,100000,4,Partial College,Professional,Yes,4,0-1 Miles,North America,40,No
25074,Married,Female,70000,4,Bachelors,Professional,Yes,2,2-5 Miles,North America,42,Yes
24738,Married,Female,40000,1,Partial College,Clerical,Yes,1,1-2 Miles,North America,51,Yes
16337,Married,Male,60000,0,Partial College,Skilled Manual,No,2,1-2 Miles,North America,29,No
24357,Married,Male,60000,3,Bachelors,Professional,Yes,1,2-5 Miles,North America,48,Yes
18613,Single,Male,70000,0,Bachelors,Professional,No,1,2-5 Miles,North America,37,Yes
12207,Single,Male,80000,4,Bachelors,Management,Yes,2,5-10 Miles,North America,66,Yes
18052,Married,Female,60000,1,Partial College,Skilled Manual,Yes,1,0-1 Miles,North America,45,Yes
13353,Single,Female,60000,4,Graduate Degree,Management,Yes,2,10+ Miles,North America,61,Yes
19399,Single,Male,40000,0,Bachelors,Professional,No,1,2-5 Miles,North America,45,No
16154,Married,Female,70000,5,Bachelors,Professional,Yes,2,2-5 Miles,North America,47,No
22219,Married,Female,60000,2,High School,Professional,Yes,2,5-10 Miles,North America,49,No
17269,Single,Male,60000,3,Bachelors,Professional,No,0,0-1 Miles,North America,47,Yes
23586,Married,Female,80000,0,Bachelors,Management,Yes,1,1-2 Miles,North America,34,Yes
15740,Married,Male,80000,5,Bachelors,Management,Yes,2,1-2 Miles,North America,64,No
27638,Single,Male,100000,1,Partial College,Professional,No,3,1-2 Miles,North America,44,No
18976,Single,Male,40000,4,High School,Professional,Yes,2,10+ Miles,North America,62,Yes
19413,Single,Male,60000,3,Bachelors,Professional,No,1,0-1 Miles,North America,47,Yes
13283,Married,Male,80000,3,Partial College,Professional,No,2,0-1 Miles,North America,49,Yes
17471,Single,Female,80000,4,Graduate Degree,Management,Yes,2,5-10 Miles,North America,67,No
16791,Single,Male,60000,5,Bachelors,Management,Yes,3,10+ Miles,North America,59,Yes
15382,Married,Female,110000,1,Bachelors,Management,Yes,2,1-2 Miles,North America,44,No
11641,Married,Male,50000,1,Bachelors,Skilled Manual,Yes,0,0-1 Miles,North America,36,No
11935,Single,Female,30000,0,Partial College,Skilled Manual,Yes,1,5-10 Miles,North America,28,No
13233,Married,Male,60000,2,Partial College,Professional,Yes,1,10+ Miles,North America,57,Yes
25909,Married,Male,60000,0,Partial College,Skilled Manual,Yes,1,5-10 Miles,North America,27,Yes
14092,Single,Male,30000,0,Partial High School,Clerical,Yes,2,5-10 Miles,North America,28,No
29143,Single,Female,60000,1,Bachelors,Professional,No,1,0-1 Miles,North America,44,Yes
24941,Married,Male,60000,3,Bachelors,Management,Yes,2,10+ Miles,North America,66,No
24637,Married,Male,40000,4,High School,Professional,Yes,2,10+ Miles,North America,64,No
23893,Married,Male,50000,3,Bachelors,Skilled Manual,Yes,3,10+ Miles,North America,41,No
13907,Single,Female,80000,3,Bachelors,Skilled Manual,Yes,1,0-1 Miles,North America,41,Yes
14900,Married,Female,40000,1,Partial College,Clerical,Yes,1,1-2 Miles,North America,49,Yes
11262,Married,Female,80000,4,Bachelors,Management,Yes,0,0-1 Miles,North America,42,No
22294,Single,Female,70000,0,Bachelors,Professional,No,1,2-5 Miles,North America,37,Yes
12195,Single,Female,70000,3,Graduate Degree,Management,Yes,2,1-2 Miles,North America,52,No
25375,Married,Male,50000,1,Graduate Degree,Skilled Manual,Yes,0,1-2 Miles,North America,34,No
11143,Married,Male,40000,0,High School,Skilled Manual,Yes,2,5-10 Miles,North America,29,No
25898,Married,Female,70000,2,High School,Professional,Yes,2,2-5 Miles,North America,53,No
24397,Single,Male,120000,2,Bachelors,Management,No,4,1-2 Miles,North America,40,No
19758,Single,Male,60000,0,Partial College,Skilled Manual,No,2,1-2 Miles,North America,29,No
15529,Married,Male,60000,4,Bachelors,Professional,Yes,2,2-5 Miles,North America,43,Yes
19884,Married,Male,60000,2,High School,Professional,Yes,2,2-5 Miles,North America,55,Yes
18674,Single,Female,80000,4,Graduate Degree,Skilled Manual,No,0,0-1 Miles,North America,48,No
13453,Married,Female,130000,1,Bachelors,Management,Yes,3,0-1 Miles,North America,45,Yes
14063,Single,Female,70000,0,Bachelors,Professional,No,1,0-1 Miles,Pacific,42,Yes
27393,Married,Female,50000,4,Bachelors,Management,Yes,2,10+ Miles,North America,63,No
14417,Single,Male,60000,3,High School,Professional,Yes,2,10+ Miles,North America,54,Yes
17533,Married,Male,40000,3,Partial College,Professional,No,2,5-10 Miles,North America,73,Yes
18580,Married,Female,60000,2,Graduate Degree,Professional,Yes,0,2-5 Miles,North America,36,Yes
17025,Single,Male,50000,0,Partial College,Skilled Manual,No,1,2-5 Miles,North America,39,Yes
25293,Married,Male,80000,4,Bachelors,Management,Yes,0,1-2 Miles,North America,42,No
24725,Married,Female,40000,3,Partial College,Clerical,Yes,0,1-2 Miles,North America,31,No
23200,Married,Female,50000,3,Bachelors,Skilled Manual,Yes,2,0-1 Miles,North America,41,No
15895,Single,Female,60000,2,Bachelors,Management,Yes,0,10+ Miles,North America,58,No
18577,Married,Female,60000,0,Graduate Degree,Professional,Yes,0,0-1 Miles,North America,40,No
27218,Married,Female,20000,2,Partial High School,Clerical,No,2,0-1 Miles,North America,48,No
18560,Married,Female,70000,2,Graduate Degree,Professional,Yes,0,2-5 Miles,North America,34,Yes
25006,Single,Female,30000,0,Partial College,Skilled Manual,Yes,1,5-10 Miles,North America,28,No
17369,Single,Male,30000,0,Partial College,Skilled Manual,Yes,1,5-10 Miles,North America,27,No
14495,Married,Male,40000,3,Partial College,Professional,No,2,5-10 Miles,North America,54,Yes
18847,Married,Female,60000,2,Graduate Degree,Management,Yes,2,5-10 Miles,North America,70,No
14754,Married,Male,40000,1,Partial College,Clerical,Yes,1,1-2 Miles,North America,48,Yes
23378,Married,Male,70000,1,Partial College,Skilled Manual,Yes,1,2-5 Miles,North America,44,Yes
26452,Single,Male,50000,3,Graduate Degree,Management,Yes,2,10+ Miles,North America,69,No
20370,Married,Male,70000,3,Partial High School,Skilled Manual,Yes,2,5-10 Miles,North America,52,No
20528,Married,Male,40000,2,Partial High School,Skilled Manual,Yes,2,2-5 Miles,North America,55,No
23549,Single,Male,30000,0,High School,Skilled Manual,Yes,2,5-10 Miles,North America,30,No
21751,Married,Male,60000,3,Graduate Degree,Management,Yes,2,1-2 Miles,North America,63,No
21266,Single,Female,80000,0,Bachelors,Management,Yes,1,1-2 Miles,North America,34,Yes
13388,Single,Male,60000,2,Partial College,Professional,Yes,1,10+ Miles,North America,56,No
18752,Single,Female,40000,0,High School,Skilled Manual,Yes,1,5-10 Miles,North America,31,No
16917,Married,Male,120000,1,Bachelors,Management,Yes,4,0-1 Miles,North America,38,No
15313,Married,Male,60000,4,Bachelors,Management,Yes,2,2-5 Miles,North America,59,No
25329,Single,Female,40000,3,Partial College,Clerical,No,2,0-1 Miles,North America,32,No
20380,Married,Female,60000,3,Graduate Degree,Management,Yes,2,10+ Miles,North America,69,No
23089,Married,Male,40000,0,Partial College,Skilled Manual,Yes,1,5-10 Miles,North America,28,No
13749,Married,Male,80000,4,Graduate Degree,Skilled Manual,Yes,0,1-2 Miles,North America,47,No
24943,Married,Male,60000,3,Bachelors,Management,Yes,2,10+ Miles,North America,66,No
28667,Single,Male,70000,2,Bachelors,Skilled Manual,No,1,0-1 Miles,North America,37,Yes
15194,Single,Male,120000,2,Bachelors,Management,No,3,0-1 Miles,North America,39,Yes
17436,Married,Male,60000,2,High School,Professional,No,2,1-2 Miles,North America,51,No
18935,Married,Female,130000,0,Graduate Degree,Management,Yes,3,1-2 Miles,North America,40,No
16871,Married,Female,90000,2,High School,Professional,Yes,1,10+ Miles,North America,51,Yes
12100,Single,Male,60000,2,Bachelors,Management,Yes,0,10+ Miles,North America,57,No
23158,Married,Female,60000,1,Graduate Degree,Professional,No,0,0-1 Miles,North America,35,Yes
18545,Married,Male,40000,4,High School,Professional,No,2,10+ Miles,North America,61,Yes
18391,Single,Female,80000,5,Partial College,Professional,Yes,2,5-10 Miles,North America,44,No
19812,Single,Female,70000,2,Partial College,Professional,Yes,0,5-10 Miles,North America,49,Yes
27660,Married,Male,80000,4,Graduate Degree,Management,Yes,2,5-10 Miles,North America,70,No
18058,Single,Female,20000,3,High School,Skilled Manual,Yes,2,2-5 Miles,North America,78,No
20343,Married,Female,90000,4,Partial College,Professional,Yes,1,1-2 Miles,North America,45,No
28997,Single,Male,40000,2,High School,Professional,No,1,2-5 Miles,North America,58,Yes
24398,Married,Male,130000,1,Graduate Degree,Management,Yes,4,0-1 Miles,North America,41,No
19002,Married,Female,60000,2,Partial College,Professional,Yes,1,2-5 Miles,North America,57,Yes
28609,Married,Male,30000,2,High School,Skilled Manual,No,2,0-1 Miles,North America,49,No
29231,Single,Female,80000,4,Partial College,Professional,No,2,0-1 Miles,North America,43,No
18858,Single,Male,60000,2,Partial High School,Skilled Manual,Yes,2,5-10 Miles,North America,52,Yes
20000,Married,Male,60000,1,Graduate Degree,Professional,Yes,0,0-1 Miles,North America,35,Yes
25261,Married,Male,40000,0,High School,Skilled Manual,Yes,2,5-10 Miles,North America,27,No
17458,Single,Male,70000,3,High School,Professional,Yes,0,5-10 Miles,North America,52,Yes
11644,Single,Male,40000,2,Bachelors,Skilled Manual,Yes,0,2-5 Miles,North America,36,No
16145,Single,Female,70000,5,Graduate Degree,Professional,Yes,3,10+ Miles,North America,46,Yes
16890,Married,Male,60000,3,Partial High School,Skilled Manual,Yes,2,5-10 Miles,North America,52,Yes
25983,Married,Male,70000,0,Bachelors,Professional,No,1,0-1 Miles,North America,43,No
14633,Married,Male,60000,1,Partial College,Skilled Manual,Yes,1,2-5 Miles,North America,44,No
22994,Married,Female,80000,0,Bachelors,Management,Yes,1,1-2 Miles,North America,34,Yes
22983,Single,Female,30000,0,Partial High School,Clerical,Yes,2,5-10 Miles,North America,27,No
25184,Single,Male,110000,1,Partial College,Professional,Yes,4,5-10 Miles,North America,45,Yes
14469,Married,Female,100000,3,Partial College,Professional,Yes,4,1-2 Miles,North America,45,No
11538,Single,Female,60000,4,Graduate Degree,Skilled Manual,No,0,0-1 Miles,North America,47,Yes
16245,Single,Female,80000,4,Graduate Degree,Skilled Manual,Yes,0,1-2 Miles,North America,47,No
17858,Married,Male,40000,4,High School,Skilled Manual,Yes,2,2-5 Miles,North America,44,Yes
25347,Single,Female,20000,3,Partial High School,Clerical,No,2,0-1 Miles,North America,49,No
15814,Single,Female,40000,0,High School,Skilled Manual,Yes,1,5-10 Miles,North America,30,No
11259,Married,Female,100000,4,Partial College,Professional,Yes,4,2-5 Miles,North America,41,Yes
11200,Married,Male,70000,4,Bachelors,Management,Yes,1,1-2 Miles,North America,58,No
25101,Married,Male,60000,5,Bachelors,Professional,Yes,1,2-5 Miles,North America,47,No
21801,Married,Female,70000,4,Partial College,Professional,Yes,1,1-2 Miles,North America,55,No
25943,Single,Female,70000,0,Partial College,Skilled Manual,No,2,0-1 Miles,North America,27,Yes
22127,Married,Male,60000,3,Graduate Degree,Management,Yes,2,1-2 Miles,North America,67,No
20414,Married,Female,60000,0,Partial College,Skilled Manual,Yes,2,5-10 Miles,North America,29,No
23672,Married,Female,60000,3,Graduate Degree,Management,Yes,2,1-2 Miles,North America,67,No
29255,Single,Male,80000,3,Partial College,Professional,No,1,1-2 Miles,North America,51,Yes
28815,Married,Female,50000,1,Graduate Degree,Skilled Manual,Yes,0,0-1 Miles,North America,35,No
27753,Married,Male,40000,0,High School,Skilled Manual,No,2,1-2 Miles,North America,30,No
27643,Single,Male,70000,5,Partial College,Professional,Yes,3,2-5 Miles,North America,44,No
13754,Single,Female,80000,4,Graduate Degree,Skilled Manual,Yes,0,1-2 Miles,North America,48,No
22088,Married,Female,130000,1,Bachelors,Management,Yes,2,0-1 Miles,North America,45,Yes
27388,Married,Male,60000,3,Bachelors,Management,No,2,1-2 Miles,North America,66,No
24745,Single,Female,30000,2,High School,Skilled Manual,No,2,0-1 Miles,North America,49,No
29237,Single,Female,120000,4,Partial College,Professional,Yes,3,5-10 Miles,North America,43,Yes
15272,Single,Male,40000,0,High School,Skilled Manual,No,2,1-2 Miles,North America,30,No
18949,Single,Male,70000,3,Graduate Degree,Management,Yes,2,5-10 Miles,North America,74,Yes
14507,Married,Male,100000,2,Graduate Degree,Management,Yes,3,1-2 Miles,North America,65,No
25886,Married,Female,60000,2,Partial College,Professional,Yes,2,2-5 Miles,North America,56,Yes
21441,Married,Male,50000,4,Bachelors,Management,Yes,2,10+ Miles,North America,64,No
21741,Married,Female,70000,3,Partial College,Professional,Yes,2,5-10 Miles,North America,50,Yes
14572,Married,Female,70000,3,Graduate Degree,Professional,Yes,0,2-5 Miles,North America,35,Yes
23368,Married,Female,60000,5,Bachelors,Skilled Manual,Yes,3,10+ Miles,North America,41,No
16217,Single,Female,60000,0,Graduate Degree,Skilled Manual,Yes,0,0-1 Miles,North America,39,No
16247,Single,Female,60000,4,Graduate Degree,Skilled Manual,Yes,0,1-2 Miles,North America,47,No
22010,Single,Male,40000,0,High School,Skilled Manual,Yes,2,5-10 Miles,North America,31,No
25872,Single,Female,70000,2,Bachelors,Management,No,1,2-5 Miles,North America,58,Yes
19164,Single,Female,70000,0,Bachelors,Professional,No,1,2-5 Miles,North America,38,Yes
18435,Single,Female,70000,5,Graduate Degree,Management,Yes,2,10+ Miles,North America,67,Yes
14284,Single,Male,60000,0,Partial College,Professional,No,2,1-2 Miles,North America,32,Yes
11287,Married,Male,70000,5,Partial College,Professional,No,3,5-10 Miles,North America,45,No
13066,Single,Male,30000,0,High School,Skilled Manual,No,2,1-2 Miles,North America,31,Yes
29106,Single,Male,40000,0,High School,Skilled Manual,No,2,1-2 Miles,North America,31,Yes
26236,Married,Female,40000,3,Partial College,Clerical,Yes,1,0-1 Miles,North America,31,No
17531,Married,Male,60000,2,High School,Professional,No,2,5-10 Miles,North America,50,No
12964,Married,Male,70000,1,Partial College,Skilled Manual,Yes,1,0-1 Miles,North America,44,No
19133,Single,Male,50000,2,Bachelors,Skilled Manual,Yes,1,2-5 Miles,North America,38,Yes
24643,Single,Female,60000,4,Bachelors,Management,Yes,2,10+ Miles,North America,63,No
21599,Married,Female,60000,1,Graduate Degree,Professional,Yes,0,2-5 Miles,North America,36,Yes
22976,Single,Male,40000,0,High School,Skilled Manual,No,2,0-1 Miles,North America,28,Yes
27637,Single,Female,100000,1,Partial College,Professional,No,3,1-2 Miles,North America,44,No
11890,Married,Female,70000,5,Graduate Degree,Professional,Yes,1,0-1 Miles,North America,47,No
28580,Married,Female,80000,0,Graduate Degree,Skilled Manual,Yes,0,1-2 Miles,North America,40,Yes
14443,Married,Male,130000,1,Graduate Degree,Management,Yes,4,0-1 Miles,North America,40,No
17864,Married,Female,60000,1,Partial College,Skilled Manual,Yes,1,2-5 Miles,North America,46,Yes
20505,Married,Female,40000,5,High School,Professional,No,2,10+ Miles,North America,61,No
14592,Married,Female,60000,0,Graduate Degree,Professional,Yes,0,0-1 Miles,North America,40,No
22227,Married,Female,60000,2,High School,Professional,Yes,2,5-10 Miles,North America,50,No
21471,Married,Male,70000,2,Partial College,Professional,Yes,1,10+ Miles,North America,59,No
22252,Single,Female,60000,1,Graduate Degree,Professional,Yes,0,2-5 Miles,North America,36,Yes
21260,Single,Female,40000,0,High School,Skilled Manual,Yes,2,5-10 Miles,North America,30,No
11817,Single,Female,70000,4,Graduate Degree,Professional,Yes,0,2-5 Miles,North America,35,Yes
19223,Married,Female,30000,2,High School,Skilled Manual,Yes,2,1-2 Miles,North America,48,No
18517,Married,Male,100000,3,Bachelors,Management,Yes,4,0-1 Miles,North America,41,No
21717,Married,Male,40000,2,Partial College,Clerical,Yes,1,0-1 Miles,North America,47,No
13760,Married,Male,60000,4,Graduate Degree,Skilled Manual,No,0,0-1 Miles,North America,47,No
18145,Married,Male,80000,5,Bachelors,Management,No,2,2-5 Miles,Europe,62,No
21770,Married,Male,60000,4,Bachelors,Management,Yes,2,10+ Miles,North America,60,No
11165,Married,Female,60000,0,Partial College,Skilled Manual,No,1,1-2 Miles,North America,33,No
16377,Single,Female,80000,4,Graduate Degree,Skilled Manual,No,0,0-1 Miles,North America,47,No
26248,Married,Male,20000,3,Partial High School,Clerical,No,2,0-1 Miles,North America,52,No
23461,Married,Female,90000,5,Partial College,Professional,Yes,3,2-5 Miles,North America,40,No
29133,Single,Female,60000,4,Bachelors,Skilled Manual,No,2,0-1 Miles,North America,42,No
27673,Single,Female,60000,3,Graduate Degree,Management,Yes,2,5-10 Miles,North America,53,Yes
12774,Married,Female,40000,1,Partial College,Clerical,Yes,1,1-2 Miles,North America,51,Yes
18910,Single,Male,30000,0,Partial College,Skilled Manual,Yes,2,5-10 Miles,North America,30,No
11699,Single,Male,60000,4,Bachelors,Skilled Manual,No,2,0-1 Miles,North America,43,No
16725,Married,Male,30000,0,High School,Skilled Manual,Yes,2,5-10 Miles,North America,26,No
28269,Single,Female,130000,1,Bachelors,Management,No,1,2-5 Miles,North America,45,No
23144,Married,Male,50000,1,Bachelors,Skilled Manual,Yes,0,0-1 Miles,North America,34,Yes
23376,Married,Male,70000,1,Bachelors,Professional,Yes,1,2-5 Miles,North America,44,Yes
25970,Single,Female,60000,4,Bachelors,Skilled Manual,No,2,0-1 Miles,North America,41,Yes
28068,Single,Female,80000,3,Graduate Degree,Professional,No,0,0-1 Miles,North America,36,Yes
18390,Married,Male,80000,5,Partial College,Professional,Yes,2,0-1 Miles,North America,44,No
29112,Single,Male,60000,0,Partial College,Professional,No,2,1-2 Miles,North America,30,No
14090,Married,Female,30000,0,Partial High School,Clerical,No,2,0-1 Miles,North America,28,No
27040,Married,Male,20000,2,Partial High School,Clerical,Yes,2,1-2 Miles,North America,49,No
23479,Single,Male,90000,0,Partial College,Professional,No,2,0-1 Miles,North America,43,Yes
16795,Married,Female,70000,4,Bachelors,Management,Yes,1,1-2 Miles,North America,59,No
22014,Single,Male,30000,0,High School,Skilled Manual,Yes,2,5-10 Miles,North America,26,No
13314,Married,Male,120000,1,High School,Professional,Yes,4,5-10 Miles,North America,46,Yes
11619,Single,Female,50000,0,Graduate Degree,Skilled Manual,Yes,0,1-2 Miles,North America,33,No
29132,Single,Female,40000,0,Bachelors,Professional,Yes,1,2-5 Miles,North America,42,Yes
11199,Married,Female,70000,4,Bachelors,Management,Yes,1,10+ Miles,North America,59,No
20296,Single,Female,60000,0,Partial College,Skilled Manual,No,1,1-2 Miles,North America,33,Yes
17546,Married,Female,70000,1,Partial College,Skilled Manual,Yes,1,0-1 Miles,North America,44,Yes
18069,Married,Male,70000,5,Bachelors,Management,Yes,4,10+ Miles,North America,60,No
23712,Single,Female,70000,2,Bachelors,Management,Yes,1,10+ Miles,North America,59,No
23358,Married,Male,60000,0,High School,Professional,Yes,2,5-10 Miles,North America,32,Yes
20518,Married,Female,70000,2,Partial College,Professional,Yes,1,10+ Miles,North America,58,No
28026,Married,Female,40000,2,High School,Professional,No,2,2-5 Miles,North America,59,No
11669,Single,Female,70000,2,Bachelors,Skilled Manual,Yes,1,2-5 Miles,North America,38,No
16020,Married,Male,40000,0,High School,Skilled Manual,Yes,2,5-10 Miles,North America,28,Yes
27090,Married,Female,60000,1,Graduate Degree,Professional,Yes,0,2-5 Miles,North America,37,Yes
27198,Single,Female,80000,0,Graduate Degree,Skilled Manual,No,0,0-1 Miles,North America,40,No
19661,Single,Male,90000,4,Bachelors,Management,Yes,1,1-2 Miles,North America,38,Yes
26327,Married,Male,70000,4,Graduate Degree,Professional,Yes,0,2-5 Miles,North America,36,Yes
26341,Married,Female,70000,5,Graduate Degree,Professional,Yes,2,0-1 Miles,North America,37,No
24958,Single,Female,40000,5,High School,Professional,No,3,2-5 Miles,North America,60,Yes
13287,Single,Male,110000,4,Bachelors,Management,Yes,4,5-10 Miles,North America,42,Yes
14493,Single,Female,70000,3,Graduate Degree,Management,No,2,1-2 Miles,North America,53,No
26678,Single,Female,80000,2,Partial High School,Skilled Manual,Yes,2,5-10 Miles,North America,49,No
23275,Married,Male,30000,2,High School,Skilled Manual,Yes,2,1-2 Miles,North America,49,No
11270,Married,Male,130000,2,Graduate Degree,Management,Yes,3,0-1 Miles,North America,42,Yes
20084,Married,Male,20000,2,High School,Manual,No,2,0-1 Miles,North America,53,No
16144,Married,Male,70000,1,Graduate Degree,Professional,Yes,1,0-1 Miles,North America,46,Yes
27731,Married,Male,40000,0,High School,Skilled Manual,Yes,2,5-10 Miles,North America,27,No
11886,Married,Female,60000,3,Bachelors,Professional,Yes,1,0-1 Miles,North America,48,Yes
24324,Single,Female,60000,4,Bachelors,Skilled Manual,Yes,2,2-5 Miles,North America,41,Yes
22220,Married,Male,60000,2,High School,Professional,No,2,1-2 Miles,North America,49,Yes
26625,Single,Female,60000,0,Graduate Degree,Professional,Yes,1,2-5 Miles,North America,38,Yes
23027,Single,Male,130000,1,Bachelors,Management,No,4,0-1 Miles,North America,44,No
16867,Single,Female,130000,1,Bachelors,Management,No,3,0-1 Miles,North America,45,Yes
14514,Single,Female,30000,0,Partial College,Skilled Manual,Yes,1,5-10 Miles,North America,26,No
19634,Married,Male,40000,0,High School,Skilled Manual,Yes,1,5-10 Miles,North America,31,No
18504,Married,Male,70000,2,Partial High School,Skilled Manual,No,2,1-2 Miles,North America,49,No
28799,Single,Female,40000,2,Partial College,Clerical,No,1,1-2 Miles,North America,47,Yes
11225,Married,Female,60000,2,Partial College,Professional,Yes,1,10+ Miles,North America,55,No
17657,Married,Male,40000,4,Partial College,Clerical,No,0,0-1 Miles,North America,30,No
14913,Married,Female,40000,1,Partial College,Clerical,Yes,1,1-2 Miles,North America,48,Yes
14077,Single,Male,30000,0,High School,Skilled Manual,Yes,2,5-10 Miles,North America,30,No
13296,Married,Male,110000,1,Bachelors,Management,Yes,3,5-10 Miles,North America,45,No
20535,Married,Female,70000,4,Partial College,Professional,Yes,1,10+ Miles,North America,56,No
12452,Married,Male,60000,4,Graduate Degree,Skilled Manual,Yes,0,1-2 Miles,North America,47,Yes
28043,Married,Female,60000,2,Bachelors,Management,Yes,0,10+ Miles,North America,56,No
12957,Single,Female,70000,1,Bachelors,Professional,No,1,0-1 Miles,North America,44,No
15412,Married,Male,130000,2,Graduate Degree,Management,Yes,3,2-5 Miles,North America,69,No
20514,Married,Female,70000,2,Partial College,Professional,Yes,1,2-5 Miles,North America,59,No
20758,Married,Male,30000,2,High School,Skilled Manual,Yes,2,1-2 Miles,North America,50,No
11801,Married,Male,60000,1,Graduate Degree,Professional,Yes,0,2-5 Miles,North America,36,No
22211,Married,Male,60000,0,Partial College,Professional,Yes,2,5-10 Miles,North America,32,No
28087,Single,Female,40000,0,Partial College,Skilled Manual,No,1,1-2 Miles,North America,27,No
23668,Married,Female,40000,4,High School,Professional,Yes,2,5-10 Miles,North America,59,Yes
27441,Married,Male,60000,3,High School,Professional,No,2,2-5 Miles,North America,53,No
27261,Married,Male,40000,1,Bachelors,Skilled Manual,No,1,0-1 Miles,North America,36,Yes
18649,Single,Male,30000,1,High School,Clerical,Yes,2,1-2 Miles,North America,51,Yes
21714,Single,Female,80000,5,Graduate Degree,Skilled Manual,No,0,0-1 Miles,North America,47,No
23217,Single,Female,60000,3,Graduate Degree,Professional,Yes,0,2-5 Miles,North America,43,Yes
23797,Single,Male,20000,3,Partial High School,Clerical,No,2,0-1 Miles,North America,50,No
13216,Married,Female,60000,5,Bachelors,Management,Yes,3,10+ Miles,North America,59,No
20657,Single,Male,50000,2,Bachelors,Skilled Manual,Yes,0,2-5 Miles,North America,37,Yes
12882,Married,Male,50000,1,Graduate Degree,Skilled Manual,Yes,0,0-1 Miles,North America,33,Yes
25908,Married,Female,60000,0,Partial College,Skilled Manual,No,1,1-2 Miles,North America,27,No
16753,Single,Female,70000,0,Partial College,Skilled Manual,Yes,2,5-10 Miles,North America,34,Yes
14608,Married,Male,50000,4,Bachelors,Skilled Manual,Yes,3,10+ Miles,North America,42,No
24979,Married,Female,60000,2,Partial College,Professional,Yes,2,2-5 Miles,North America,57,Yes
13313,Married,Female,120000,1,High School,Professional,No,4,2-5 Miles,North America,45,No
18952,Married,Female,100000,4,Bachelors,Management,Yes,4,0-1 Miles,North America,40,No
17699,Married,Male,60000,1,Graduate Degree,Skilled Manual,No,0,0-1 Miles,North America,35,No
14657,Married,Male,80000,1,Partial College,Skilled Manual,No,1,0-1 Miles,North America,47,Yes
11540,Single,Male,60000,4,Graduate Degree,Skilled Manual,Yes,0,1-2 Miles,North America,47,Yes
11783,Married,Female,60000,1,Graduate Degree,Skilled Manual,Yes,0,0-1 Miles,North America,34,No
14602,Married,Female,80000,3,Graduate Degree,Professional,Yes,0,0-1 Miles,North America,36,Yes
29030,Married,Male,70000,2,Partial High School,Skilled Manual,Yes,2,10+ Miles,North America,54,No
26490,Single,Male,70000,2,Bachelors,Management,No,1,2-5 Miles,North America,59,Yes
13151,Single,Male,40000,0,High School,Skilled Manual,Yes,2,5-10 Miles,North America,27,No
17260,Married,Male,90000,5,Partial College,Professional,Yes,3,0-1 Miles,North America,41,No
15372,Married,Male,80000,3,Partial College,Professional,No,2,2-5 Miles,North America,50,Yes
18105,Married,Female,60000,2,Partial College,Professional,Yes,1,10+ Miles,North America,55,No
19660,Married,Male,80000,4,Bachelors,Management,Yes,0,0-1 Miles,North America,43,No
16112,Single,Male,70000,4,Bachelors,Professional,Yes,2,2-5 Miles,North America,43,Yes
20698,Married,Male,60000,4,Bachelors,Skilled Manual,Yes,3,5-10 Miles,North America,42,No
20076,Single,Female,10000,2,High School,Manual,Yes,2,1-2 Miles,North America,53,Yes
24496,Single,Female,40000,0,High School,Skilled Manual,No,2,0-1 Miles,North America,28,Yes
15468,Married,Female,50000,1,Bachelors,Skilled Manual,Yes,1,0-1 Miles,North America,35,No
28031,Single,Female,70000,2,Bachelors,Management,No,1,2-5 Miles,North America,59,Yes
26270,Single,Female,20000,2,Partial High School,Clerical,Yes,2,1-2 Miles,North America,49,No
22221,Married,Male,60000,2,High School,Professional,No,2,1-2 Miles,North America,48,Yes
28228,Single,Female,80000,2,Partial High School,Skilled Manual,No,2,1-2 Miles,North America,50,No
18363,Married,Male,40000,0,High School,Skilled Manual,Yes,2,5-10 Miles,North America,28,Yes
23256,Single,Male,30000,1,High School,Clerical,No,1,5-10 Miles,North America,52,No
12768,Married,Male,30000,1,High School,Clerical,Yes,1,2-5 Miles,North America,52,Yes
20361,Married,Male,50000,2,Graduate Degree,Management,Yes,2,5-10 Miles,North America,69,No
21306,Single,Male,60000,2,High School,Professional,Yes,2,5-10 Miles,North America,51,No
13382,Married,Male,70000,5,Partial College,Professional,Yes,2,1-2 Miles,North America,57,Yes
20310,Single,Male,60000,0,Partial College,Skilled Manual,Yes,1,5-10 Miles,North America,27,Yes
22971,Single,Female,30000,0,High School,Skilled Manual,No,2,0-1 Miles,North America,25,Yes
15287,Single,Female,50000,1,Graduate Degree,Skilled Manual,Yes,0,1-2 Miles,North America,33,Yes
15532,Single,Male,60000,4,Bachelors,Professional,Yes,2,2-5 Miles,North America,43,Yes
11255,Married,Male,70000,4,Graduate Degree,Management,Yes,2,5-10 Miles,North America,73,No
28090,Married,Male,40000,0,Partial College,Skilled Manual,Yes,1,5-10 Miles,North America,27,No
15255,Married,Male,40000,0,High School,Skilled Manual,Yes,2,5-10 Miles,North America,28,Yes
13154,Married,Male,40000,0,High School,Skilled Manual,No,2,0-1 Miles,North America,27,Yes
26778,Single,Female,40000,0,High School,Skilled Manual,Yes,2,5-10 Miles,North America,31,No
23248,Married,Female,10000,2,High School,Manual,Yes,2,1-2 Miles,North America,53,No
21417,Single,Female,60000,0,Partial College,Professional,No,2,1-2 Miles,North America,32,Yes
17668,Single,Male,30000,2,High School,Skilled Manual,Yes,2,1-2 Miles,North America,50,Yes
27994,Married,Female,40000,4,High School,Professional,Yes,2,5-10 Miles,North America,69,No
20376,Single,Female,70000,3,Graduate Degree,Management,Yes,2,5-10 Miles,North America,52,Yes
25954,Married,Male,60000,0,Partial College,Skilled Manual,No,2,1-2 Miles,North America,31,No
15749,Single,Female,70000,4,Bachelors,Management,Yes,2,10+ Miles,North America,61,No
25899,Married,Female,70000,2,High School,Professional,Yes,2,10+ Miles,North America,53,No
13351,Single,Female,70000,4,Bachelors,Management,Yes,2,1-2 Miles,North America,62,Yes
23333,Married,Male,40000,0,Partial College,Skilled Manual,No,2,1-2 Miles,North America,30,No
21660,Married,Female,60000,3,Graduate Degree,Professional,Yes,0,2-5 Miles,North America,43,Yes
17012,Married,Female,60000,3,Graduate Degree,Professional,Yes,0,2-5 Miles,North America,42,Yes
24514,Married,Male,40000,0,Partial College,Skilled Manual,Yes,1,5-10 Miles,North America,30,No
27505,Single,Female,40000,0,High School,Skilled Manual,Yes,2,5-10 Miles,North America,30,No
29243,Single,Male,110000,1,Bachelors,Management,Yes,1,5-10 Miles,North America,43,No
26582,Married,Male,60000,0,Partial College,Skilled Manual,Yes,2,5-10 Miles,North America,33,Yes
14271,Married,Male,30000,0,High School,Skilled Manual,Yes,2,5-10 Miles,North America,32,No
23041,Single,Female,70000,4,High School,Professional,Yes,0,5-10 Miles,North America,50,Yes
29048,Single,Male,110000,2,Bachelors,Management,No,3,0-1 Miles,North America,37,Yes
24433,Married,Male,70000,3,High School,Professional,No,1,1-2 Miles,North America,52,Yes
15501,Married,Male,70000,4,Graduate Degree,Professional,Yes,0,2-5 Miles,North America,36,Yes
13911,Single,Female,80000,3,Bachelors,Skilled Manual,Yes,2,2-5 Miles,North America,41,Yes
20421,Single,Female,40000,0,Partial High School,Clerical,Yes,2,5-10 Miles,North America,26,No
16009,Single,Male,170000,1,Graduate Degree,Management,No,4,0-1 Miles,North America,66,No
18411,Married,Male,60000,2,High School,Professional,No,2,5-10 Miles,North America,51,No
19163,Married,Female,70000,4,Bachelors,Professional,Yes,2,0-1 Miles,North America,43,Yes
18572,Married,Female,60000,0,Graduate Degree,Professional,Yes,0,0-1 Miles,North America,39,No
27540,Single,Female,70000,0,Bachelors,Professional,No,1,0-1 Miles,North America,37,Yes
19889,Single,Female,70000,2,Partial High School,Skilled Manual,No,2,2-5 Miles,North America,54,Yes
12922,Single,Female,60000,3,Bachelors,Skilled Manual,Yes,0,2-5 Miles,North America,40,Yes
18891,Married,Female,40000,0,Partial College,Skilled Manual,Yes,2,5-10 Miles,North America,28,No
16773,Married,Male,60000,1,Graduate Degree,Skilled Manual,Yes,0,0-1 Miles,North America,33,No
19143,Single,Female,80000,3,Bachelors,Skilled Manual,Yes,2,2-5 Miles,North America,41,Yes
23882,Single,Female,80000,3,Graduate Degree,Professional,Yes,0,0-1 Miles,North America,37,Yes
11233,Married,Male,70000,4,Partial College,Professional,Yes,2,10+ Miles,North America,53,No
12056,Married,Male,120000,2,Graduate Degree,Management,Yes,3,5-10 Miles,North America,64,No
15555,Married,Female,60000,1,Partial College,Skilled Manual,Yes,1,2-5 Miles,North America,45,Yes
18423,Single,Male,80000,2,Partial High School,Skilled Manual,No,2,1-2 Miles,North America,52,No
22743,Married,Female,40000,5,High School,Professional,Yes,2,10+ Miles,North America,60,No
25343,Single,Female,20000,3,Partial High School,Clerical,Yes,2,1-2 Miles,North America,50,No
13390,Married,Female,70000,4,Partial College,Professional,No,1,1-2 Miles,North America,56,No
17482,Single,Female,40000,0,Partial High School,Clerical,Yes,2,5-10 Miles,North America,29,No
13176,Single,Male,130000,0,Graduate Degree,Management,No,2,0-1 Miles,North America,38,Yes
20504,Married,Female,40000,5,High School,Professional,No,2,2-5 Miles,North America,60,No
12205,Single,Female,130000,2,Bachelors,Management,No,4,0-1 Miles,North America,67,No
16751,Married,Male,60000,0,Partial College,Skilled Manual,Yes,1,5-10 Miles,North America,32,Yes
21613,Single,Male,50000,2,Bachelors,Skilled Manual,No,1,0-1 Miles,North America,39,Yes
24801,Single,Male,60000,1,Graduate Degree,Professional,Yes,0,2-5 Miles,North America,35,Yes
17519,Married,Female,60000,0,Partial College,Professional,Yes,2,5-10 Miles,North America,32,No
18347,Single,Female,30000,0,Partial College,Skilled Manual,No,1,1-2 Miles,North America,31,No
29052,Single,Male,40000,0,Partial College,Skilled Manual,Yes,1,5-10 Miles,North America,27,No
11745,Married,Female,60000,1,Bachelors,Professional,Yes,1,0-1 Miles,North America,47,Yes
19147,Married,Male,40000,0,Bachelors,Professional,No,1,0-1 Miles,North America,42,No
19217,Married,Male,30000,2,High School,Skilled Manual,Yes,2,1-2 Miles,North America,49,No
15839,Single,Male,30000,0,Partial College,Skilled Manual,Yes,1,5-10 Miles,North America,32,No
13714,Married,Female,20000,2,High School,Manual,No,2,1-2 Miles,North America,53,Yes
22330,Married,Male,50000,0,Graduate Degree,Skilled Manual,Yes,0,1-2 Miles,North America,32,Yes
18783,Single,Male,80000,0,Bachelors,Management,No,1,0-1 Miles,North America,38,Yes
25041,Single,Male,40000,0,High School,Skilled Manual,Yes,2,5-10 Miles,North America,31,No
22046,Single,Female,80000,0,Bachelors,Management,No,1,0-1 Miles,North America,38,Yes
28052,Married,Male,60000,2,High School,Professional,Yes,2,10+ Miles,North America,55,No
26693,Married,Male,70000,3,Partial College,Professional,Yes,1,5-10 Miles,North America,49,No
24955,Single,Male,30000,5,Partial High School,Skilled Manual,Yes,3,10+ Miles,North America,60,Yes
26065,Single,Female,110000,3,Bachelors,Management,No,4,1-2 Miles,North America,42,No
13942,Married,Male,60000,1,Partial College,Skilled Manual,Yes,1,0-1 Miles,North America,46,No
11219,Married,Male,60000,2,High School,Professional,Yes,2,10+ Miles,North America,55,No
22118,Single,Female,70000,3,Graduate Degree,Management,Yes,2,5-10 Miles,North America,53,Yes
23197,Married,Male,50000,3,Bachelors,Skilled Manual,Yes,2,2-5 Miles,North America,40,No
14883,Married,Female,30000,1,Bachelors,Skilled Manual,Yes,1,5-10 Miles,North America,53,Yes
27279,Single,Female,70000,2,Bachelors,Skilled Manual,Yes,0,2-5 Miles,North America,38,Yes
18322,Single,Male,30000,0,Partial High School,Clerical,No,2,0-1 Miles,North America,26,No
15879,Married,Male,70000,5,Bachelors,Management,Yes,2,2-5 Miles,North America,61,No
28278,Married,Male,50000,2,Graduate Degree,Management,Yes,2,5-10 Miles,North America,71,No
24416,Married,Male,90000,4,High School,Professional,Yes,2,1-2 Miles,North America,45,No
28066,Married,Male,80000,2,Graduate Degree,Professional,Yes,0,0-1 Miles,North America,37,Yes
11275,Married,Female,80000,4,Graduate Degree,Management,Yes,2,0-1 Miles,North America,72,Yes
14872,Married,Male,30000,0,Graduate Degree,Skilled Manual,Yes,0,0-1 Miles,North America,32,No
16151,Married,Female,60000,1,Bachelors,Professional,Yes,1,2-5 Miles,North America,48,Yes
19731,Married,Male,80000,4,Graduate Degree,Management,Yes,2,5-10 Miles,North America,68,No
23801,Married,Female,20000,2,Partial High School,Clerical,Yes,2,0-1 Miles,North America,49,No
11807,Married,Male,70000,3,Graduate Degree,Professional,Yes,0,2-5 Miles,North America,34,No
11622,Married,Male,50000,0,Graduate Degree,Skilled Manual,Yes,0,0-1 Miles,North America,32,No
26597,Single,Female,60000,4,Bachelors,Skilled Manual,No,2,0-1 Miles,North America,42,No
27074,Married,Female,70000,1,Graduate Degree,Skilled Manual,Yes,0,0-1 Miles,North America,35,Yes
19228,Married,Female,40000,2,Partial College,Clerical,Yes,1,0-1 Miles,North America,48,No
13415,Single,Male,100000,1,Graduate Degree,Management,Yes,3,2-5 Miles,North America,73,Yes
17000,Single,Female,70000,4,Bachelors,Skilled Manual,Yes,2,2-5 Miles,North America,43,Yes
14569,Married,Male,60000,1,Graduate Degree,Professional,Yes,0,0-1 Miles,North America,35,No
13873,Married,Male,70000,3,Graduate Degree,Professional,Yes,0,0-1 Miles,North America,35,Yes
20401,Married,Female,50000,4,Bachelors,Management,Yes,2,1-2 Miles,North America,64,Yes
21583,Married,Female,50000,1,Bachelors,Skilled Manual,Yes,0,0-1 Miles,North America,34,Yes
12029,Married,Male,30000,0,Partial High School,Clerical,No,2,0-1 Miles,North America,28,No
18066,Single,Male,70000,5,Bachelors,Management,Yes,3,10+ Miles,North America,60,Yes
28192,Married,Female,70000,5,Graduate Degree,Professional,Yes,3,10+ Miles,North America,46,No
16122,Married,Male,40000,4,High School,Skilled Manual,Yes,2,0-1 Miles,North America,44,Yes
18607,Single,Female,60000,4,Bachelors,Skilled Manual,Yes,2,2-5 Miles,North America,42,Yes
28858,Single,Male,80000,3,Bachelors,Skilled Manual,Yes,0,2-5 Miles,North America,40,No
14432,Single,Male,90000,4,Graduate Degree,Management,Yes,1,5-10 Miles,North America,73,No
26305,Single,Female,60000,2,Bachelors,Skilled Manual,No,0,0-1 Miles,North America,36,Yes
22050,Single,Male,90000,4,Bachelors,Management,Yes,1,1-2 Miles,North America,38,Yes
25394,Married,Male,60000,1,Graduate Degree,Professional,Yes,0,2-5 Miles,North America,34,Yes
19747,Married,Male,50000,4,Bachelors,Management,Yes,2,10+ Miles,North America,63,No
23195,Single,Male,50000,3,Bachelors,Skilled Manual,Yes,2,2-5 Miles,North America,41,Yes
21695,Married,Male,60000,0,Graduate Degree,Skilled Manual,Yes,0,1-2 Miles,North America,39,Yes
13934,Married,Male,40000,4,High School,Skilled Manual,Yes,2,2-5 Miles,North America,46,No
13337,Married,Female,80000,5,Bachelors,Management,Yes,2,5-10 Miles,North America,64,No
27190,Married,Female,40000,3,Partial College,Clerical,Yes,1,1-2 Miles,North America,32,No
28657,Single,Male,60000,2,Bachelors,Skilled Manual,Yes,0,2-5 Miles,North America,36,Yes
21713,Single,Male,80000,5,Graduate Degree,Skilled Manual,No,0,0-1 Miles,North America,47,No
21752,Married,Male,60000,3,Graduate Degree,Management,Yes,2,10+ Miles,North America,64,No
27273,Single,Male,70000,3,Graduate Degree,Professional,No,0,0-1 Miles,North America,35,Yes
22719,Single,Male,110000,3,Bachelors,Management,Yes,4,2-5 Miles,North America,40,Yes
22042,Married,Female,70000,0,Partial College,Skilled Manual,Yes,2,5-10 Miles,North America,34,Yes
21451,Married,Female,40000,4,High School,Professional,Yes,2,10+ Miles,North America,61,No
20754,Married,Male,30000,2,High School,Skilled Manual,Yes,2,1-2 Miles,North America,51,No
12153,Single,Female,70000,3,Partial College,Professional,Yes,1,5-10 Miles,North America,49,Yes
16895,Married,Female,40000,3,Partial College,Professional,No,2,1-2 Miles,North America,54,Yes
26728,Single,Male,70000,3,Graduate Degree,Management,No,2,1-2 Miles,North America,53,Yes
11090,Single,Male,90000,2,Partial College,Professional,Yes,1,2-5 Miles,North America,48,Yes
15862,Single,Female,50000,0,Graduate Degree,Skilled Manual,Yes,0,1-2 Miles,North America,33,Yes
26495,Single,Female,40000,2,High School,Professional,Yes,2,10+ Miles,North America,57,No
11823,Married,Female,70000,0,Graduate Degree,Professional,Yes,0,2-5 Miles,North America,39,No
23449,Married,Male,60000,2,High School,Professional,Yes,2,5-10 Miles,North America,48,No
23459,Married,Male,60000,2,High School,Professional,Yes,2,5-10 Miles,North America,50,No
19543,Married,Male,70000,5,Graduate Degree,Professional,No,3,10+ Miles,North America,47,No
14914,Married,Female,40000,1,Partial College,Clerical,Yes,1,1-2 Miles,North America,49,Yes
12033,Single,Female,40000,0,High School,Skilled Manual,No,2,0-1 Miles,North America,27,Yes
11941,Single,Male,60000,0,Partial College,Skilled Manual,Yes,2,5-10 Miles,North America,29,No
14389,Married,Male,60000,2,Bachelors,Management,Yes,0,2-5 Miles,North America,59,No
18050,Married,Female,60000,1,Partial College,Skilled Manual,Yes,1,0-1 Miles,North America,45,Yes
19856,Married,Female,60000,4,Bachelors,Management,Yes,2,2-5 Miles,North America,60,No
11663,Married,Male,70000,4,Graduate Degree,Professional,Yes,0,0-1 Miles,North America,36,Yes
27740,Married,Female,40000,0,High School,Skilled Manual,Yes,2,5-10 Miles,North America,27,No
23455,Single,Male,80000,2,Partial High School,Skilled Manual,No,2,1-2 Miles,North America,50,No
15292,Single,Female,60000,1,Graduate Degree,Skilled Manual,Yes,0,1-2 Miles,North America,35,No
21587,Married,Female,60000,1,Graduate Degree,Skilled Manual,Yes,0,2-5 Miles,North America,34,Yes
23513,Married,Female,40000,3,Partial College,Professional,Yes,2,5-10 Miles,North America,54,No
24322,Married,Female,60000,4,Bachelors,Skilled Manual,Yes,2,0-1 Miles,North America,42,No
26298,Married,Female,50000,1,Bachelors,Skilled Manual,Yes,0,2-5 Miles,North America,34,Yes
25419,Single,Male,50000,2,Bachelors,Skilled Manual,No,1,0-1 Miles,North America,38,Yes
13343,Married,Female,90000,5,Bachelors,Management,Yes,2,1-2 Miles,North America,63,Yes
11303,Single,Female,90000,4,High School,Professional,No,3,1-2 Miles,North America,45,Yes
21693,Single,Female,60000,0,Graduate Degree,Skilled Manual,No,0,0-1 Miles,North America,40,No
28056,Married,Male,70000,2,Partial High School,Skilled Manual,Yes,2,10+ Miles,North America,53,No
11788,Single,Female,70000,1,Graduate Degree,Professional,Yes,0,2-5 Miles,North America,34,No
22296,Married,Male,70000,0,Bachelors,Professional,No,1,0-1 Miles,North America,38,No
15319,Married,Female,70000,4,Bachelors,Management,No,1,1-2 Miles,North America,59,No
17654,Single,Female,40000,3,Partial College,Clerical,Yes,1,1-2 Miles,North America,30,Yes
14662,Married,Male,60000,1,Bachelors,Professional,Yes,1,0-1 Miles,North America,48,Yes
17541,Married,Female,40000,4,High School,Skilled Manual,Yes,2,2-5 Miles,North America,43,No
13886,Married,Female,70000,4,Graduate Degree,Professional,Yes,0,2-5 Miles,North America,35,Yes
13073,Married,Female,60000,0,Partial College,Professional,Yes,2,5-10 Miles,North America,30,No
21940,Married,Male,90000,5,Graduate Degree,Professional,Yes,0,0-1 Miles,North America,47,Yes
20196,Married,Male,60000,1,Partial College,Skilled Manual,Yes,1,2-5 Miles,North America,45,Yes
23491,Single,Male,100000,3,Partial College,Professional,No,4,1-2 Miles,North America,45,No
16651,Married,Female,120000,2,Bachelors,Management,Yes,3,5-10 Miles,North America,62,No
16813,Married,Male,60000,2,Partial College,Professional,Yes,2,10+ Miles,North America,55,No
16007,Married,Female,90000,5,Bachelors,Management,Yes,2,1-2 Miles,North America,66,Yes
27434,Single,Male,70000,4,Partial College,Professional,Yes,1,10+ Miles,North America,56,No
27756,Single,Female,50000,3,Bachelors,Skilled Manual,No,1,0-1 Miles,North America,40,No
23818,Married,Female,50000,0,Graduate Degree,Skilled Manual,Yes,0,1-2 Miles,North America,33,Yes
19012,Married,Male,80000,3,Bachelors,Management,Yes,1,1-2 Miles,North America,56,No
18329,Single,Male,30000,0,Partial High School,Clerical,No,2,5-10 Miles,North America,27,No
29037,Married,Male,60000,0,Graduate Degree,Professional,No,0,0-1 Miles,North America,39,No
26576,Married,Female,60000,0,Partial College,Skilled Manual,Yes,2,5-10 Miles,North America,31,No
12192,Single,Female,60000,2,Partial High School,Skilled Manual,No,2,1-2 Miles,North America,51,No
14887,Married,Female,30000,1,High School,Clerical,Yes,1,5-10 Miles,North America,52,No
11734,Married,Male,60000,1,Partial College,Skilled Manual,No,1,0-1 Miles,North America,47,No
17462,Married,Male,70000,3,Graduate Degree,Management,Yes,2,5-10 Miles,North America,53,Yes
20659,Married,Male,70000,3,Graduate Degree,Professional,Yes,0,0-1 Miles,North America,35,Yes
28004,Married,Female,60000,3,Bachelors,Management,Yes,2,10+ Miles,North America,66,No
19741,Single,Female,80000,4,Graduate Degree,Management,Yes,2,5-10 Miles,North America,65,No
17450,Married,Male,80000,5,Partial College,Professional,Yes,3,5-10 Miles,North America,45,No
17337,Single,Male,40000,0,High School,Skilled Manual,Yes,1,5-10 Miles,North America,31,No
18594,Single,Female,80000,3,Bachelors,Skilled Manual,Yes,3,10+ Miles,North America,40,Yes
15982,Married,Male,110000,5,Partial College,Professional,Yes,4,2-5 Miles,North America,46,No
28625,Single,Male,40000,2,Partial College,Clerical,No,1,1-2 Miles,North America,47,Yes
11269,Married,Male,130000,2,Graduate Degree,Management,Yes,2,0-1 Miles,North America,41,No
25148,Married,Male,60000,2,High School,Professional,No,2,1-2 Miles,North America,48,Yes
13920,Single,Female,50000,4,Bachelors,Skilled Manual,Yes,2,0-1 Miles,North America,42,No
23704,Single,Male,40000,5,High School,Professional,Yes,4,10+ Miles,North America,60,Yes
28972,Single,Female,60000,3,Graduate Degree,Management,Yes,2,10+ Miles,North America,66,No
22730,Married,Male,70000,5,Bachelors,Management,Yes,2,10+ Miles,North America,63,No
29134,Married,Male,60000,4,Bachelors,Skilled Manual,No,3,10+ Miles,North America,42,No
14332,Single,Female,30000,0,High School,Skilled Manual,No,2,5-10 Miles,North America,26,No
19117,Single,Female,60000,1,Graduate Degree,Professional,Yes,0,2-5 Miles,North America,36,Yes
22864,Married,Male,90000,2,Partial College,Professional,No,0,5-10 Miles,North America,49,Yes
11292,Single,Male,150000,1,Partial College,Professional,No,3,0-1 Miles,North America,44,Yes
13466,Married,Male,80000,5,Partial College,Professional,Yes,3,1-2 Miles,North America,46,No
23731,Married,Male,60000,2,High School,Professional,Yes,2,2-5 Miles,North America,54,Yes
28672,Single,Male,70000,4,Graduate Degree,Professional,Yes,0,2-5 Miles,North America,35,Yes
11809,Married,Male,60000,2,Bachelors,Skilled Manual,Yes,0,0-1 Miles,North America,38,Yes
19664,Single,Male,100000,3,Bachelors,Management,No,3,1-2 Miles,North America,38,No
12121,Single,Male,60000,3,High School,Professional,Yes,2,10+ Miles,North America,53,Yes"""                