import sys
fn main():
    var args = sys.argv()
    print("Args count: ", len(args))
    for i in range(len(args)):
        print("Arg ", i, ": ", args[i])
