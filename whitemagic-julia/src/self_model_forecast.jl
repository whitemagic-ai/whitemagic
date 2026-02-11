"""
    WhiteMagic Self-Model Forecasting
    ==================================
    Time-series forecasting for the Self-Model predictive introspection system.

    Replaces Python's simple linear regression with:
    - Exponential smoothing (Holt-Winters additive)
    - Anomaly detection via residual z-scores
    - Multi-metric correlation analysis
    - Forecast confidence intervals
    - Batch forecasting for all Self-Model metrics

    Designed to be called from Python via the JuliaBridge (JSON over stdio).
"""
module SelfModelForecast

using Statistics
using LinearAlgebra

export forecast_metric,
       detect_anomalies,
       correlation_matrix,
       batch_forecast,
       holt_winters_forecast

# ---------------------------------------------------------------------------
# Holt-Winters exponential smoothing (additive)
# ---------------------------------------------------------------------------

"""
    HoltWintersState

State for Holt-Winters double exponential smoothing.
"""
mutable struct HoltWintersState
    level::Float64        # Smoothed level
    trend::Float64        # Smoothed trend
    alpha::Float64        # Level smoothing (0-1)
    beta::Float64         # Trend smoothing (0-1)
    residuals::Vector{Float64}
end

"""
    holt_winters_fit(series::Vector{Float64}; alpha=0.3, beta=0.1)

Fit Holt-Winters double exponential smoothing to a time series.
"""
function holt_winters_fit(series::Vector{Float64}; alpha::Float64=0.3, beta::Float64=0.1)
    n = length(series)
    n < 2 && return HoltWintersState(
        n > 0 ? series[1] : 0.0, 0.0, alpha, beta, Float64[]
    )

    # Initialize: level = first value, trend = first difference
    level = series[1]
    trend = series[2] - series[1]
    residuals = Float64[]

    for t in 2:n
        # Forecast for this step
        forecast = level + trend
        push!(residuals, series[t] - forecast)

        # Update
        new_level = alpha * series[t] + (1 - alpha) * (level + trend)
        new_trend = beta * (new_level - level) + (1 - beta) * trend
        level = new_level
        trend = new_trend
    end

    HoltWintersState(level, trend, alpha, beta, residuals)
end

"""
    holt_winters_forecast(state::HoltWintersState, steps::Int)

Generate forecasts with confidence intervals.
"""
function holt_winters_forecast(state::HoltWintersState, steps::Int)
    forecasts = Float64[]
    lower_80 = Float64[]
    upper_80 = Float64[]
    lower_95 = Float64[]
    upper_95 = Float64[]

    # Estimate forecast error from residuals
    σ = length(state.residuals) > 1 ? std(state.residuals; corrected=true) : 0.1

    for h in 1:steps
        f = state.level + h * state.trend
        # Prediction interval widens with horizon
        se = σ * sqrt(h)
        push!(forecasts, round(f; digits=6))
        push!(lower_80, round(f - 1.28 * se; digits=6))
        push!(upper_80, round(f + 1.28 * se; digits=6))
        push!(lower_95, round(f - 1.96 * se; digits=6))
        push!(upper_95, round(f + 1.96 * se; digits=6))
    end

    Dict(
        "forecasts" => forecasts,
        "confidence_80" => Dict("lower" => lower_80, "upper" => upper_80),
        "confidence_95" => Dict("lower" => lower_95, "upper" => upper_95),
        "level" => round(state.level; digits=6),
        "trend" => round(state.trend; digits=6),
        "residual_std" => round(σ; digits=6),
    )
end

# ---------------------------------------------------------------------------
# Single metric forecasting
# ---------------------------------------------------------------------------

"""
    forecast_metric(values::Vector{Float64}; steps=5, alpha=0.3, beta=0.1)

Forecast a single Self-Model metric using Holt-Winters smoothing.
"""
function forecast_metric(values::Vector{Float64}; steps::Int=5, alpha::Float64=0.3, beta::Float64=0.1)
    n = length(values)
    n == 0 && return Dict("error" => "Empty time series")

    # Fit model
    state = holt_winters_fit(values; alpha=alpha, beta=beta)

    # Generate forecasts
    fc = holt_winters_forecast(state, steps)

    # Add summary stats
    fc["series_length"] = n
    fc["last_value"] = values[end]
    fc["series_mean"] = round(mean(values); digits=6)
    fc["series_std"] = n > 1 ? round(std(values; corrected=true); digits=6) : 0.0
    fc["trend_direction"] = state.trend > 0.001 ? "increasing" : (state.trend < -0.001 ? "decreasing" : "stable")

    return fc
end

# ---------------------------------------------------------------------------
# Anomaly detection on residuals
# ---------------------------------------------------------------------------

"""
    detect_anomalies(values::Vector{Float64}; threshold=2.5, alpha=0.3, beta=0.1)

Detect anomalies in a time series using Holt-Winters residual z-scores.
"""
function detect_anomalies(values::Vector{Float64}; threshold::Float64=2.5, alpha::Float64=0.3, beta::Float64=0.1)
    n = length(values)
    n < 3 && return Dict("anomalies" => [], "count" => 0)

    state = holt_winters_fit(values; alpha=alpha, beta=beta)
    residuals = state.residuals
    σ = std(residuals; corrected=true)
    μ = mean(residuals)

    anomalies = []
    for (i, r) in enumerate(residuals)
        z = σ > 0 ? abs(r - μ) / σ : 0.0
        if z > threshold
            push!(anomalies, Dict(
                "index" => i + 1,  # 1-indexed, offset by 1 since residuals start at t=2
                "value" => round(values[i + 1]; digits=6),
                "expected" => round(values[i + 1] - r; digits=6),
                "residual" => round(r; digits=6),
                "z_score" => round(z; digits=4),
                "direction" => r > 0 ? "above" : "below",
            ))
        end
    end

    Dict(
        "anomalies" => anomalies,
        "count" => length(anomalies),
        "residual_mean" => round(μ; digits=6),
        "residual_std" => round(σ; digits=6),
        "threshold" => threshold,
    )
end

# ---------------------------------------------------------------------------
# Multi-metric correlation analysis
# ---------------------------------------------------------------------------

"""
    correlation_matrix(metrics::Dict{String, Vector{Float64}})

Compute pairwise Pearson correlations between Self-Model metrics.
Returns which metrics predict which.
"""
function correlation_matrix(metrics::Dict{String, Vector{Float64}})
    names = collect(keys(metrics))
    n = length(names)
    n < 2 && return Dict("error" => "Need at least 2 metrics")

    # Ensure all vectors have the same length (truncate to shortest)
    min_len = minimum(length(v) for v in values(metrics))
    min_len < 3 && return Dict("error" => "Need at least 3 data points per metric")

    corr = zeros(Float64, n, n)
    for i in 1:n
        for j in 1:n
            vi = metrics[names[i]][1:min_len]
            vj = metrics[names[j]][1:min_len]
            corr[i, j] = cor(vi, vj)
        end
    end

    # Find strong correlations (|r| > 0.7)
    strong = []
    for i in 1:n
        for j in (i+1):n
            r = corr[i, j]
            if abs(r) > 0.7
                push!(strong, Dict(
                    "metric_a" => names[i],
                    "metric_b" => names[j],
                    "correlation" => round(r; digits=4),
                    "relationship" => r > 0 ? "positive" : "negative",
                    "strength" => abs(r) > 0.9 ? "very_strong" : "strong",
                ))
            end
        end
    end

    Dict(
        "metric_names" => names,
        "correlation_matrix" => [round.(corr[i, :]; digits=4) for i in 1:n],
        "strong_correlations" => strong,
        "sample_size" => min_len,
    )
end

# ---------------------------------------------------------------------------
# Batch forecasting
# ---------------------------------------------------------------------------

"""
    batch_forecast(metrics::Dict{String, Vector{Float64}}; steps=5, alpha=0.3, beta=0.1)

Forecast all Self-Model metrics in one call.
"""
function batch_forecast(metrics::Dict{String, Vector{Float64}}; steps::Int=5, alpha::Float64=0.3, beta::Float64=0.1)
    forecasts = Dict{String, Any}()
    alerts = []

    for (name, values) in metrics
        fc = forecast_metric(values; steps=steps, alpha=alpha, beta=beta)
        forecasts[name] = fc

        # Generate alerts for concerning trends
        if haskey(fc, "trend_direction")
            trend = fc["trend_direction"]
            last_val = fc["last_value"]

            # Alert thresholds (domain-specific)
            if name == "energy" && trend == "decreasing" && last_val < 0.4
                push!(alerts, Dict("metric" => name, "alert" => "low_energy_declining", "severity" => "warning"))
            elseif name == "error_rate" && trend == "increasing" && last_val > 0.2
                push!(alerts, Dict("metric" => name, "alert" => "error_rate_rising", "severity" => "critical"))
            elseif name == "karma_debt" && trend == "increasing" && last_val > 0.5
                push!(alerts, Dict("metric" => name, "alert" => "karma_debt_growing", "severity" => "warning"))
            end
        end
    end

    Dict(
        "forecasts" => forecasts,
        "alerts" => alerts,
        "metrics_count" => length(metrics),
        "forecast_horizon" => steps,
    )
end

# ---------------------------------------------------------------------------
# JSON stdio interface (for Python bridge)
# ---------------------------------------------------------------------------

"""
    handle_request(request::Dict)

Process a JSON request from the Python bridge.
"""
function handle_request(request::Dict)
    cmd = get(request, "command", "")

    try
        if cmd == "forecast"
            values = Float64.(get(request, "values", []))
            steps = Int(get(request, "steps", 5))
            alpha = Float64(get(request, "alpha", 0.3))
            beta = Float64(get(request, "beta", 0.1))
            return forecast_metric(values; steps=steps, alpha=alpha, beta=beta)

        elseif cmd == "detect_anomalies"
            values = Float64.(get(request, "values", []))
            threshold = Float64(get(request, "threshold", 2.5))
            return detect_anomalies(values; threshold=threshold)

        elseif cmd == "correlations"
            raw = get(request, "metrics", Dict())
            metrics = Dict{String, Vector{Float64}}(k => Float64.(v) for (k, v) in raw)
            return correlation_matrix(metrics)

        elseif cmd == "batch_forecast"
            raw = get(request, "metrics", Dict())
            metrics = Dict{String, Vector{Float64}}(k => Float64.(v) for (k, v) in raw)
            steps = Int(get(request, "steps", 5))
            return batch_forecast(metrics; steps=steps)

        else
            return Dict("error" => "Unknown command: $cmd")
        end
    catch e
        return Dict("error" => string(e))
    end
end

end  # module SelfModelForecast
