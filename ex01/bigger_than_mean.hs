

mean_3 a b c = (a+b+c)/3

func :: Bool -> Int
func True = 1 
func False = 0

dim_biggers a b c = func (a > mean_3 a b c) + func (b > mean_3 a b c) + func( c > mean_3 a b c)  


