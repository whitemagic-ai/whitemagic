"""
Gan Ying (感应) - Resonant Response Engine
==========================================
Uses OrdinaryDiffEq to model the "vibrational resonance" of the system.
When the system receives an "impulse" (new memory), this solver calculates
how long that memory "echoes" in the holographic lattice.
"""

using OrdinaryDiffEq
using JSON
using Dates

# -- Physics Model --
# d²x/dt² + γ(dx/dt) + ω₀²x = F(t)
# Damped Harmonic Oscillator with driving force
function resonance_driver!(du, u, p, t)
    x, v = u
    γ, ω₀, F_mag = p
    
    # Differential Equations
    du[1] = v    # dx/dt = velocity
    du[2] = -γ*v - (ω₀^2)*x + F_mag * sin(t) # Acceleration
end

function calculate_resonance(impulse_magnitude::Float64, damping::Float64, frequency::Float64)
    u0 = [0.0, impulse_magnitude] # Initial state: displacement=0, velocity=impulse
    tspan = (0.0, 50.0)           # Time horizon
    p = (damping, frequency, 0.5) # Params: gamma, omega, F_forcing
    
    # Solve ODE
    prob = ODEProblem(resonance_driver!, u0, tspan, p)
    sol = solve(prob, Tsit5(), saveat=0.1)
    
    # Analyze energy (Amplitude squared)
    energy = [u[1]^2 + u[2]^2 for u in sol.u]
    total_resonance = sum(energy)
    half_life = tspan[2] # Fallback
    
    # Find half-life (when energy drops below max/2)
    max_e = maximum(energy)
    for (i, t) in enumerate(sol.t)
        if energy[i] < max_e / 2
            half_life = t
            break
        end
    end

    return Dict(
        "impulse" => impulse_magnitude,
        "total_resonance" => total_resonance,
        "half_life" => half_life,
        "peak_amplitude" => sqrt(max_e),
        "status" => "CONVERGED"
    )
end

# -- CLI Interface --
function main()
    if length(ARGS) > 0
        input = JSON.parse(ARGS[1])
        mag = Float64(get(input, "magnitude", 1.0))
        damp = Float64(get(input, "damping", 0.1))
        freq = Float64(get(input, "frequency", 1.0))
        
        result = calculate_resonance(mag, damp, freq)
        println(JSON.json(result))
    else
        println(JSON.json(Dict("status" => "NO_INPUT")))
    end
end

main()
