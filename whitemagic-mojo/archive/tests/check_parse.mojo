fn main():
    var s: String = "123.45"
    try:
        var f = Float64(s)
        print("Float64: ", f)
    except:
        print("Float64 failed")
    
    var s2: String = "123"
    try:
        var i = Int(s2)
        print("Int: ", i)
    except:
        print("Int failed")
