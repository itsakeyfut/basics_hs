-- if/then/else
signOf :: Int -> String
signOf x = if x > 0
            then "Positive"
            else if x < 0
                then "Negative"
                else "Zero"

-- guards
signOf' :: Int -> String
signOf' x
    | x > 0     = "Positive"
    | x < 0     = "Negative"
    | otherwise = "Zero"

-- guards in Function Definition
bmiTell :: Float -> Float -> String
bmiTell weight height
    | bmi <= 18.5 = "Underweight"
    | bmi <= 25.0 = "Normal"
    | bmi <= 30.0 = "Overweight"
    | otherwise   = "Obese"
    where bmi = weight / height ^ 2