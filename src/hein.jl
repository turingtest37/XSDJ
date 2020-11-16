

function gump()
    name = nothing
    id = nothing
    d = Dict("name" => "Doug", "id" => "2")
    att = (:name, :id)
    for s in att
        k = string(s)
        if haskey(d, k)
            v = d[k]
            # Works outside of function scope, but not here
            @eval $s = $v
        end
    end
    @show name id
end

# THIS IS THE ANSWER!
function bump()
    println("bump")
    name = nothing
    id = nothing
    d = Dict("name" => "Doug", "id" => "2")
    att = (:name, :id, :nope, :mope, :rope, :dope)
    k = string.(att)
    whaat = get.(Ref(d), k, nothing)
    
    @show name id whaat
end
# Does not work
# for s in (:one, :two)
#     @eval $s = parse(Int, $d[string($s)])
# end

gump()
bump()
spy(e) = @show e

# cblk() = """

# name = id = nothing
# d = Dict("name" => "Doug", "id" => "2")
# att=(:name, :id, :nope)
# for s in att
#     k = string(s)
#     if haskey(d, k)
#         v = d[k]
#         @eval $s = $v
#     end
# end
# @show name id
# """;

# include_string(spy, Main, cblk())
