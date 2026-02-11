fn main() raises:
    var s: String = "123.45"
    # Try converting to Float64
    try:
        # Some versions use Float64(s)
        var f = Float64(s)
        print("Float64 works: ", f)
    except:
        print("Float64 failed")
    
    try:
        # Some versions use int(s) or Int(s)
        var i = Int(s.split(".")[0])
        print("Int works: ", i)
    except:
        print("Int failed")
