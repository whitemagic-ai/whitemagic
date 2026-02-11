fn main() raises:
    try:
        var s: String = "1.23"
        var x = Float64(s) # Some versions allow this
        print("Float64(String) worked")
    except:
        print("Float64(String) failed")
    
    try:
        var s: String = "123"
        var y = Int(s)
        print("Int(String) worked")
    except:
        print("Int(String) failed")
