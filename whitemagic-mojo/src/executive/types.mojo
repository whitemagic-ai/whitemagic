struct ConductorConfig:
    var max_iterations: Int
    var clones_per_iteration: Int
    var token_limit: Int
    var timeout_minutes: Int

    fn __init__(out self, max_iter: Int, clones: Int, tokens: Int, timeout: Int):
        self.max_iterations = max_iter
        self.clones_per_iteration = clones
        self.token_limit = tokens
        self.timeout_minutes = timeout

struct OrchestrationEvent:
    var event_type: String
    var source_system: String
    var severity: String
    var data_json: String
    var timestamp: String

    fn __init__(out self, event_type: String, source: String, severity: String, data: String, ts: String):
        self.event_type = event_type
        self.source_system = source
        self.severity = severity
        self.data_json = data
        self.timestamp = ts
