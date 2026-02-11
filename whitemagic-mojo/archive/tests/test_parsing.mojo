fn main() raises:
    var s: String = "123.45"
    try:
        var f = Float64(s)
        print("Float64 works")
    except:
        print("Float64 fails")
    
    try:
        var i = Int(s)
        print("Int works")
    except:
        print("Int fails")
