
"""
Causal Resonance (感应) — Layer 2 Verification
==============================================
Models the system of clusters as a network of coupled oscillators.
Verifies causal links by checking for energy transfer (resonance).
"""

using OrdinaryDiffEq
using JSON

function coupled_oscillators!(du, u, p, t)
    num_nodes, edges, gamma, omega_sq = p
    
    # u[1:n] = displacements, u[n+1:2n] = velocities
    x = @view u[1:num_nodes]
    v = @view u[num_nodes+1:end]
    
    dx = @view du[1:num_nodes]
    dv = @view du[num_nodes+1:end]
    
    # Standard damped harmonic motion
    for i in 1:num_nodes
        dx[i] = v[i]
        dv[i] = -gamma * v[i] - omega_sq * x[i]
    end
    
    # Mutual Correlative Resonance (Coupling)
    # If edge (i -> j) exists, i drives j
    coupling_strength = 0.5
    for (src, dst) in edges
        # Simple unidirectional energy transfer
        dv[dst] += coupling_strength * (x[src] - x[dst])
    end
end

function verify_resonance(input_data)
    nodes = input_data["nodes"]
    edges_raw = input_data["edges"]
    num_nodes = length(nodes)
    node_to_idx = Dict(name => i for (i, name) in enumerate(nodes))
    
    # Map edges to indices
    edges = []
    for (src, dst) in edges_raw
        if haskey(node_to_idx, src) && haskey(node_to_idx, dst)
            push!(edges, (node_to_idx[src], node_to_idx[dst]))
        end
    end
    
    # Parameters
    gamma = 0.1
    omega_sq = 1.0
    p = (num_nodes, edges, gamma, omega_sq)
    
    # Initial state: Impulse at "root" nodes (those with no parents)
    u0 = zeros(2 * num_nodes)
    has_parent = fill(false, num_nodes)
    for (src, dst) in edges; has_parent[dst] = true; end
    
    for i in 1:num_nodes
        if !has_parent[i]
            u0[num_nodes + i] = 1.0 # Give it a 'kick'
        end
    end
    
    tspan = (0.0, 20.0)
    prob = ODEProblem(coupled_oscillators!, u0, tspan, p)
    sol = solve(prob, Tsit5(), saveat=0.5)
    
    # Energy analysis
    # resonance_score[node] = max amplitude reached
    scores = zeros(num_nodes)
    for i in 1:num_nodes
        displacements = [state[i] for state in sol.u]
        scores[i] = maximum(abs.(displacements))
    end
    
    results = Dict()
    for (i, name) in enumerate(nodes)
        results[name] = scores[i]
    end
    
    return results
end

function main()
    if length(ARGS) > 0
        try
            input = JSON.parse(ARGS[1])
            results = verify_resonance(input)
            println(JSON.json(results))
        catch e
            println(JSON.json(Dict("error" => string(e))))
        end
    else
        println(JSON.json(Dict("status" => "NO_INPUT")))
    end
end

main()
