fn main() raises:
    var s: String = "a,b,c"
    var parts = s.split(",")
    print("Parts count: ", len(parts))
    for i in range(len(parts)):
        print(parts[i])
