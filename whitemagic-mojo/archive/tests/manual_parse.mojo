fn parse_int(s: String) -> Int:
    var res: Int = 0
    for i in range(len(s)):
        var c = s[i]
        if c >= '0' and c <= '9':
            res = res * 10 + (ord(c) - ord('0'))
    return res

fn main():
    var s: String = "123"
    var i = parse_int(s)
    print("Parsed: ", i)
