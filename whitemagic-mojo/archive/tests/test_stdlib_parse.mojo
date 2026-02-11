import time
fn main() raises:
    var s: String = "123.45"
    var f = Float64(s)
    print("Parsed Float:", f)
    var i = Int(s.split(".")[0])
    print("Parsed Int:", i)
