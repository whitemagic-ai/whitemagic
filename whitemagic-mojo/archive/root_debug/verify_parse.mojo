fn main():
    try:
        var s: String = "123.45"
        var f = Float64(s)
        print("Mojo Parsed Float:", f)
    except:
        print("Mojo Float64(String) failed")
