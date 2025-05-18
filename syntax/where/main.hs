bmiTell :: Float -> Float -> String
bmiTell weight height
    | bmi <= 18.5 = "Underweight"
    | bmi <= 25.0 = "Normal"
    | otherwise   = "Overweight"
    where bmi = weight / height ^ 2