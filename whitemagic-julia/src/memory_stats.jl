"""
    WhiteMagic Statistical Memory Analysis
    =======================================
    Numerical analysis of the memory corpus using Julia's scientific computing stack.

    Provides:
    - Memory importance distribution analysis (moments, outlier detection)
    - Galactic zone transition probabilities (Markov chain model)
    - Cluster significance testing for constellation detection
    - Retention score distribution analysis
    - Zone-aware sampling strategies

    Designed to be called from Python via the JuliaBridge (JSON over stdio).
"""
module MemoryStats

using Statistics
using LinearAlgebra

export analyze_importance_distribution,
       zone_transition_matrix,
       detect_outliers,
       cluster_significance,
       zone_sampling_weights,
       full_memory_analysis

# ---------------------------------------------------------------------------
# Importance distribution analysis
# ---------------------------------------------------------------------------

"""
    analyze_importance_distribution(scores::Vector{Float64})

Compute comprehensive statistics for memory importance scores.
Returns a Dict with moments, percentiles, and distribution shape metrics.
"""
function analyze_importance_distribution(scores::Vector{Float64})
    n = length(scores)
    n == 0 && return Dict(
        "count" => 0, "mean" => 0.0, "std" => 0.0,
        "skewness" => 0.0, "kurtosis" => 0.0,
        "percentiles" => Dict(), "outlier_count" => 0
    )

    μ = mean(scores)
    σ = std(scores; corrected=true)
    sorted = sort(scores)

    # Moments
    skew = σ > 0 ? mean(((scores .- μ) ./ σ) .^ 3) : 0.0
    kurt = σ > 0 ? mean(((scores .- μ) ./ σ) .^ 4) - 3.0 : 0.0

    # Percentiles
    pcts = Dict(
        "p5"  => quantile(sorted, 0.05),
        "p25" => quantile(sorted, 0.25),
        "p50" => quantile(sorted, 0.50),
        "p75" => quantile(sorted, 0.75),
        "p95" => quantile(sorted, 0.95),
        "p99" => quantile(sorted, 0.99),
    )

    # IQR-based outlier detection
    iqr = pcts["p75"] - pcts["p25"]
    lower_fence = pcts["p25"] - 1.5 * iqr
    upper_fence = pcts["p75"] + 1.5 * iqr
    outlier_count = count(s -> s < lower_fence || s > upper_fence, scores)

    Dict(
        "count" => n,
        "mean" => round(μ; digits=6),
        "std" => round(σ; digits=6),
        "min" => minimum(scores),
        "max" => maximum(scores),
        "skewness" => round(skew; digits=4),
        "kurtosis" => round(kurt; digits=4),
        "percentiles" => pcts,
        "outlier_count" => outlier_count,
        "outlier_fraction" => round(outlier_count / n; digits=4),
        "iqr" => round(iqr; digits=6),
        "lower_fence" => round(lower_fence; digits=6),
        "upper_fence" => round(upper_fence; digits=6),
    )
end

# ---------------------------------------------------------------------------
# Galactic zone transition matrix
# ---------------------------------------------------------------------------

const ZONE_NAMES = ["CORE", "INNER_RIM", "MID_BAND", "OUTER_RIM", "FAR_EDGE"]
const ZONE_BOUNDS = [0.0, 0.15, 0.40, 0.65, 0.85, 1.0]

"""
    distance_to_zone(d::Float64) -> Int

Map a galactic distance to zone index (1-5).
"""
function distance_to_zone(d::Float64)
    for i in 1:(length(ZONE_BOUNDS)-1)
        if d < ZONE_BOUNDS[i+1]
            return i
        end
    end
    return length(ZONE_BOUNDS) - 1  # FAR_EDGE
end

"""
    zone_transition_matrix(before::Vector{Float64}, after::Vector{Float64})

Compute Markov transition probabilities between galactic zones.
`before` and `after` are galactic distances before/after a sweep.
Returns a 5×5 transition matrix as a Dict.
"""
function zone_transition_matrix(before::Vector{Float64}, after::Vector{Float64})
    @assert length(before) == length(after) "Vectors must have same length"

    n = 5
    counts = zeros(Int, n, n)

    for (b, a) in zip(before, after)
        zb = distance_to_zone(clamp(b, 0.0, 0.999))
        za = distance_to_zone(clamp(a, 0.0, 0.999))
        counts[zb, za] += 1
    end

    # Normalize rows to probabilities
    probs = zeros(Float64, n, n)
    for i in 1:n
        row_sum = sum(counts[i, :])
        if row_sum > 0
            probs[i, :] = counts[i, :] ./ row_sum
        end
    end

    Dict(
        "transition_matrix" => [round.(probs[i, :]; digits=4) for i in 1:n],
        "zone_names" => ZONE_NAMES,
        "zone_counts_before" => [count(d -> distance_to_zone(clamp(d, 0.0, 0.999)) == i, before) for i in 1:n],
        "zone_counts_after" => [count(d -> distance_to_zone(clamp(d, 0.0, 0.999)) == i, after) for i in 1:n],
        "total_memories" => length(before),
        "movements" => count(i -> distance_to_zone(clamp(before[i], 0.0, 0.999)) != distance_to_zone(clamp(after[i], 0.0, 0.999)), 1:length(before)),
    )
end

# ---------------------------------------------------------------------------
# Outlier detection (z-score based)
# ---------------------------------------------------------------------------

"""
    detect_outliers(values::Vector{Float64}; threshold=3.0)

Detect outliers using modified z-score (MAD-based for robustness).
Returns indices and values of outliers.
"""
function detect_outliers(values::Vector{Float64}; threshold::Float64=3.0)
    n = length(values)
    n < 3 && return Dict("outliers" => [], "count" => 0)

    med = median(values)
    mad = median(abs.(values .- med))
    # Modified z-score (0.6745 is the 0.75 quantile of standard normal)
    mad_scale = mad > 0 ? 0.6745 / mad : 0.0

    outliers = []
    for (i, v) in enumerate(values)
        mz = abs(v - med) * mad_scale
        if mz > threshold
            push!(outliers, Dict("index" => i, "value" => round(v; digits=6), "modified_zscore" => round(mz; digits=4)))
        end
    end

    Dict(
        "outliers" => outliers,
        "count" => length(outliers),
        "median" => round(med; digits=6),
        "mad" => round(mad; digits=6),
        "threshold" => threshold,
    )
end

# ---------------------------------------------------------------------------
# Cluster significance testing
# ---------------------------------------------------------------------------

"""
    cluster_significance(cluster_sizes::Vector{Int}, total_points::Int, volume::Float64)

Test whether detected clusters are statistically significant compared to
a uniform random distribution (Poisson null model).

Returns p-values for each cluster (lower = more significant).
"""
function cluster_significance(cluster_sizes::Vector{Int}, total_points::Int, volume::Float64)
    volume <= 0 && return Dict("error" => "Volume must be positive")

    # Expected density under uniform distribution
    expected_density = total_points / volume
    results = []

    for (i, size) in enumerate(cluster_sizes)
        # For a Poisson process, the probability of observing >= k events
        # in a region is computed via the Poisson CDF complement
        # We approximate the cluster region volume as size/expected_density
        cluster_volume = size / expected_density
        λ = expected_density * cluster_volume  # expected count = size (degenerate)

        # Use a more meaningful test: compare cluster density to global density
        # If cluster occupies fraction f of volume but has fraction g of points,
        # significance is based on binomial deviation
        f = cluster_volume / volume
        g = size / total_points

        # Z-test for binomial proportion
        if f > 0 && f < 1
            z = (g - f) / sqrt(f * (1 - f) / total_points)
            # One-sided p-value (is this cluster denser than expected?)
            p_value = 0.5 * erfc(z / sqrt(2))
        else
            p_value = 1.0
        end

        push!(results, Dict(
            "cluster_index" => i,
            "size" => size,
            "density_ratio" => round(g / max(f, 1e-10); digits=4),
            "z_score" => round(z; digits=4),
            "p_value" => round(p_value; digits=6),
            "significant" => p_value < 0.05,
        ))
    end

    Dict(
        "clusters" => results,
        "total_points" => total_points,
        "volume" => volume,
        "expected_density" => round(expected_density; digits=4),
        "significant_count" => count(r -> r["significant"], results),
    )
end

# Complementary error function (stdlib)
# erfc is available in Base

# ---------------------------------------------------------------------------
# Zone-aware sampling weights
# ---------------------------------------------------------------------------

"""
    zone_sampling_weights(distances::Vector{Float64})

Compute sampling weights that ensure zone diversity.
Rarer zones get higher per-memory weight so sampling isn't dominated by OUTER_RIM.
"""
function zone_sampling_weights(distances::Vector{Float64})
    n = length(distances)
    n == 0 && return Dict("weights" => Float64[], "zone_counts" => Int[])

    zones = [distance_to_zone(clamp(d, 0.0, 0.999)) for d in distances]
    zone_counts = [count(==(i), zones) for i in 1:5]

    # Inverse frequency weighting
    weights = zeros(Float64, n)
    for (i, z) in enumerate(zones)
        zc = zone_counts[z]
        weights[i] = zc > 0 ? 1.0 / zc : 0.0
    end
    # Normalize to sum to 1
    total = sum(weights)
    if total > 0
        weights ./= total
    end

    Dict(
        "weights" => round.(weights; digits=8),
        "zone_counts" => zone_counts,
        "zone_names" => ZONE_NAMES,
        "effective_sample_size" => round(1.0 / sum(weights .^ 2); digits=2),
    )
end

# ---------------------------------------------------------------------------
# Full analysis pipeline
# ---------------------------------------------------------------------------

"""
    full_memory_analysis(importance::Vector{Float64}, distances::Vector{Float64})

Run complete statistical analysis on memory corpus.
"""
function full_memory_analysis(importance::Vector{Float64}, distances::Vector{Float64})
    Dict(
        "importance_distribution" => analyze_importance_distribution(importance),
        "distance_distribution" => analyze_importance_distribution(distances),
        "importance_outliers" => detect_outliers(importance),
        "distance_outliers" => detect_outliers(distances),
        "zone_sampling" => zone_sampling_weights(distances),
        "memory_count" => length(importance),
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
        if cmd == "importance_distribution"
            scores = Float64.(get(request, "scores", []))
            return analyze_importance_distribution(scores)
        elseif cmd == "zone_transitions"
            before = Float64.(get(request, "before", []))
            after = Float64.(get(request, "after", []))
            return zone_transition_matrix(before, after)
        elseif cmd == "detect_outliers"
            values = Float64.(get(request, "values", []))
            threshold = Float64(get(request, "threshold", 3.0))
            return detect_outliers(values; threshold=threshold)
        elseif cmd == "cluster_significance"
            sizes = Int.(get(request, "cluster_sizes", []))
            total = Int(get(request, "total_points", 0))
            vol = Float64(get(request, "volume", 1.0))
            return cluster_significance(sizes, total, vol)
        elseif cmd == "zone_sampling"
            distances = Float64.(get(request, "distances", []))
            return zone_sampling_weights(distances)
        elseif cmd == "full_analysis"
            importance = Float64.(get(request, "importance", []))
            distances = Float64.(get(request, "distances", []))
            return full_memory_analysis(importance, distances)
        else
            return Dict("error" => "Unknown command: $cmd")
        end
    catch e
        return Dict("error" => string(e))
    end
end

end  # module MemoryStats
