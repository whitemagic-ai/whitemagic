fn main() raises:
    var s: String = "123.45"
    # In some recent Mojo versions, float(s) or int(s) works
    # but since it's a dev version, let's check what's available
    print("Checking native conversions...")
