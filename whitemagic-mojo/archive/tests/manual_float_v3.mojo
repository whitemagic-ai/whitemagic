fn parse_float(s: String) -> Float32:
    var res: Float32 = 0.0
    var sign: Float32 = 1.0
    var divisor: Float32 = 1.0
    var seen_decimal = False
    var bytes = s.as_bytes()
    
    for i in range(len(bytes)):
        var b = bytes[i]
        if b == ord('-'):
            sign = -1.0
        elif b == ord('.'):
            seen_decimal = True
        elif b >= ord('0') and b <= ord('9'):
            var digit = Float32(Int(b) - Int(ord('0')))
            if not seen_decimal:
                res = res * 10.0 + digit
            else:
                divisor = divisor * 10.0
                res = res + (digit / divisor)
    return res * sign

fn main():
    var s: String = "123.45"
    var f = parse_float(s)
    print("Parsed value is:", f)
