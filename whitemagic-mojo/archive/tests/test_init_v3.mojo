struct MyStruct:
    var x: Int
    fn __init__(out self, x: Int):
        self.x = x

fn main():
    var s = MyStruct(10)
    print(s.x)
