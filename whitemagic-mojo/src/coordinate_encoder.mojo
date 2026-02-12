import sys
from python import Python
from collections import List


struct MemoryData:
    var id: String
    var content: String
    var title: String
    var tags: List[String]
    var importance: Float64
    var created_timestamp: Int

    fn __init__(
        out self,
        id: String,
        content: String,
        title: String,
        var tags: List[String],
        importance: Float64,
        created_timestamp: Int,
    ):
        self.id = id
        self.content = content
        self.title = title
        self.tags = tags^
        self.importance = importance
        self.created_timestamp = created_timestamp


struct CoordinateEncoder:
    fn __init__(out self):
        pass

    fn _contains(self, list: List[String], word: String) -> Bool:
        for i in range(len(list)):
            if list[i] == word:
                return True
        return False

    fn _count_intersections(self, tags: List[String], keywords: List[String]) -> Int:
        var count: Int = 0
        for i in range(len(tags)):
            if self._contains(keywords, tags[i]):
                count += 1
        return count

    fn encode(self, memory: MemoryData) -> String:
        # X: Logic vs Emotion
        var valence: Float64 = 0.0  # Placeholder
        var x_score: Float64 = -0.5 * valence

        var logic_tags = List[String]()
        logic_tags.append("logic")
        logic_tags.append("strategy")
        logic_tags.append("code")
        logic_tags.append("architecture")
        logic_tags.append("technical")
        logic_tags.append("system")
        logic_tags.append("algorithm")
        logic_tags.append("fix")
        logic_tags.append("database")
        logic_tags.append("rust")
        logic_tags.append("python")

        var emotion_tags = List[String]()
        emotion_tags.append("joy")
        emotion_tags.append("fear")
        emotion_tags.append("love")
        emotion_tags.append("meditation")
        emotion_tags.append("dream")
        emotion_tags.append("feeling")
        emotion_tags.append("emotion")
        emotion_tags.append("heart")
        emotion_tags.append("wonder")
        emotion_tags.append("beauty")

        var logic_count = self._count_intersections(memory.tags, logic_tags)
        var emotion_count = self._count_intersections(memory.tags, emotion_tags)

        if logic_count > emotion_count:
            x_score += 0.4 + (0.1 * Float64(min(logic_count, 5)))
        elif emotion_count > logic_count:
            x_score -= 0.4 + (0.1 * Float64(min(emotion_count, 5)))

        # Y: Micro vs Macro
        var y_score: Float64 = 0.0
        if memory.content.find("pattern") != -1 or memory.content.find("wisdom") != -1:
            y_score = 0.6
        elif memory.content.find("error") != -1 or memory.content.find("debug") != -1:
            y_score = -0.6

        # Z: Time (Chronos)
        var z_score: Float64 = -0.5

        # W: Importance (Gravity)
        var w_score: Float64 = (memory.importance * 0.4) + 0.6

        return (
            String(x_score) + "," + String(y_score) + "," + String(z_score) + "," + String(w_score)
        )


fn main() raises:
    var args = sys.argv()
    if len(args) < 2:
        print("Usage: --batch <json_data> | <legacy_args>")
        return

    if args[1] == "--batch":
        var py = Python.import_module("json")
        var builtins = Python.import_module("builtins")
        var sys_mod = Python.import_module("sys")
        var stdin_data = sys_mod.stdin.read()
        var data = py.loads(stdin_data)
        var items = data["items"]

        var encoder = CoordinateEncoder()
        for i in range(len(items)):
            var item = items[i]
            var tags = List[String]()
            var item_tags = item["tags"]
            for j in range(len(item_tags)):
                tags.append(String(item_tags[j]))

            var imp_py_str = builtins.str(item["importance"])
            var ts_py_str = builtins.str(item["created_timestamp"])

            var id_str: String = String(item["id"])
            var content_str: String = String(item["content"])
            var title_str: String = String(item["title"])

            var importance_val: Float64 = Float64(String(imp_py_str))
            var timestamp_val: Int = Int(String(ts_py_str))

            var mem = MemoryData(
                id_str, content_str, title_str, tags^, importance_val, timestamp_val
            )
            print("ID:" + mem.id + "|RES:" + encoder.encode(mem))
        return

    print("RESULT_X: 0.0")
