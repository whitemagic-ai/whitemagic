# constellations.jl - High-performance spatial neighbor search for 198k memories
using NearestNeighbors, StaticArrays, JSON3, SQLite, DataFrames, LinearAlgebra

function main()
    # In a real build, we'd use environment variables for DB path
    db_path = get(ENV, "WM_DB_PATH", joinpath(homedir(), ".whitemagic/memory/whitemagic.db"))
    
    if !isfile(db_path)
        println(JSON3.write(Dict("error" => "Database not found")))
        return
    end

    db = SQLite.DB(db_path)
    df = DataFrame(SQLite.DBInterface.execute(db, "SELECT memory_id, x, y, z, w FROM holographic_coords"))
    
    if size(df, 1) == 0
        println(JSON3.write(Dict("error" => "No memories found")))
        return
    end

    # Build KD-Tree for 4D space
    points = [SVector{4, Float64}(df[i, :x], df[i, :y], df[i, :z], df[i, :w]) for i in 1:size(df, 1)]
    tree = KDTree(points)

    println(JSON3.write(Dict("status" => "ready")))
    flush(stdout)

    # Process queries from stdin (JSON)
    for line in eachline(stdin)
        try
            query = JSON3.read(line)
            if haskey(query, "point") && haskey(query, "radius")
                p = SVector{4, Float64}(query.point...)
                indices = inrange(tree, p, query.radius)
                
                results = [Dict(
                    "id" => df[idx, :memory_id],
                    "dist" => norm(points[idx] - p)
                ) for idx in indices]
                
                println(JSON3.write(results))
                flush(stdout)
            end
        catch e
            # Silent skip on error for high-speed pipe
        end
    end
end

if abspath(PROGRAM_FILE) == @__FILE__
    main()
end
