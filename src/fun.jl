# using EzXML
# using Dates
# using Logging


# include("utils.jl")
# include("datatypes.jl")

# abstract type RestrictionType end
# abstract type SillyType end
# struct ID <: SillyType end

# template = ((:enumeration, Type{<:SillyType}),)

# typnm = nothing

using Dates

const XSD_DATE_FORMAT = dateformat"yyyy-mm-ddTHH:MM:SS-zzzzzz"

struct XsdEnumeration{T}
    value::T
end
process(e::XsdEnumeration{String}, s::String) = e(s)

simpletypes = (String, UInt, DateTime, Int, Float64, Bool)

for T in simpletypes
    @show T
    @eval begin
        if $T <: Number
            # println("Number!")
            XsdEnumeration{$T}(s::AbstractString) = XsdEnumeration{$T}(parse($T,s))
            process(e::XsdEnumeration{$T}, s::$T) = e.value == s
        elseif $T <: TimeType
            # println("Time!  T ")
            XsdEnumeration{$T}(s::AbstractString) = XsdEnumeration{$T}(parse($T, s, XSD_DATE_FORMAT))
        end
    end
end

process(e::XsdEnumeration{DateTime}, s::DateTime) = s < e.value



# struct XsdEnumeration{String}
#     v::String
# end

# struct XsdEnumeration{UInt}
#     v:UInt
# end
# XsdEnumeration{UInt}(s::AbstractString) = XsdEnumeration{UInt}(parse(UInt,s))

# struct XsdEnumeration{DateTime}
#     v::DateTime
# end
# XsdEnumeration{DateTime}(s::AbstractString) = XsdEnumeration{DateTime}(parse(dateformat"hahhd",s))




# function build(rtypes)
#     global typnm
#     # build the type dynamically
#     for t in rtypes
#         typnm = Symbol("Xsd", uppercasefirst(string(t[1])))
#         n = typnm
#         T = t[2]
#         @debug "T" T
#         @eval begin
#             # @debug "Building type n with T" :(n) :($T)
#             struct $n <: RestrictionType
#                 id::Union{Nothing, ID}
#                 value::$T
#             end
#             # $prc(::$n, v::AbstractString) = $n(nothing, process($T, v))
#             # $prc(::$n, id::Union{Nothing, ID}, v::AbstractString) = $n(id, process($T, v))
#             # $n(y::AbstractString, z::AbstractString) = $n(ID(y), process($T, z))
#             process(t::$T, v::AbstractString) = t(v)

#             function $n(v::AbstractString)
#                 @debug "n,T,v" $n $T v
#                 println("types:")
#                 println(fieldtypes($n))
#                 d = fieldtypes($n)[2]
#                 println(typeof(d))
#                 $n(nothing, process(d, v))
#             end
#         end
#     end
# end

# println("Before build")
# @eval dump($typnm)
# build(template)
# println("After build")
# @eval dump($typnm)
# @show methods(typnm)
# @show methods(process)

@show process(XsdEnumeration{UInt}("20"), unsigned(21))
@show process(XsdEnumeration{String}("20"), "20")
@show process(XsdEnumeration{DateTime}("2020-10-29T18:49:21"), now())

methods(process)
