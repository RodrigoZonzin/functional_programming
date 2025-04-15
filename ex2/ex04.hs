
func :: Char -> Char
func value = if IsLower(value) == True then toUpper(value) else value
