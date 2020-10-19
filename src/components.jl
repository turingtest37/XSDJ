
## QName
struct QName{T<:AbstractString}
    pfx::Union{T,Nothing}
    nm::T

end
QName(nm::AbstractString) = QName(nameparts(nm)...)

import Base: show, getindex, print
Base.show(io::IO, n::QName) = write(io, isnothing(n.pfx) ? n.nm : string(n.pfx,':',n.nm))
Base.getindex(q::QName, i::Int) = getindex(string(q),i)
Base.print(io::IO, n::QName) = print(io, isnothing(n.pfx) ? n.nm : string(n.pfx,':',n.nm))

import Base: ==
==(x::AbstractString, y::QName) = string(x) == string(y)
==(x::QName, y::AbstractString) = ==(y,x)
==(x::QName, y::QName) = ==(string(x),string(y))

struct ID
    id::AbstractString
end


import Base.:*
*(a::QName, b::QName) = string(a,b)


## AnyURI
import URIParser: URI
AnyURI = URI


abstract type XsdList end
abstract type XsdAll end
abstract type XsdChoice end
abstract type XsdSequence end
abstract type XsdUnion end
abstract type XsdRestriction end

struct XsdMaxLengthRestriction <: XsdRestriction
    base::QName
    maxlength::Union{Int,Nothing}
end

struct XsdEnumerationRestriction <: XsdRestriction
    base::QName
    enumeration::Union{Vector{String}, Nothing}
end
import Base: in
in(v::AbstractString, r::XsdEnumerationRestriction) = in(v, r.enumeration)

struct XsdInclude
    schemaLocation::AnyURI
end
XsdInclude(u::AbstractString) = XsdInclude(AnyURI(u))

#TODO Decide whether to include schema element name in Xsd... types
struct XsdImport
    en::String # SHOULD WE ALWAYS INCLUDE THIS? WHAT GOOD?
    namespace::Union{Nothing,String}
    schemaLocation::Union{Nothing,AnyURI}
    XsdImport(n,s) = new("import",n,s)
end

function XsdImport(n::EzXML.Node)
    d = attributes(n)
    n = haskey(d, "namespace") ? d["namespace"] : nothing
    s = haskey(d, "schemaLocation") ? d["schemaLocation"] : nothing
    XsdImport(n,s)
end

struct XsdRedefine
    id::String
    schemaLocation::AnyURI
end
XsdRedefine(u::AbstractString) = XsdRedefine(AnyURI(u))

struct XsdAnnotation
    id::String
    text::String
end

struct XsdAttribute end

struct XsdNotation end

struct XsdGroup
    id::String
    maxoccurs::UInt16
    minoccurs::UInt16
    name::String
    ref::QName
    contents::Union{XsdAll,XsdChoice,XsdSequence}
end

struct XsdAttributeGroup end

abstract type XsdSimpleType end

struct XsdAnonSimpleType <: XsdSimpleType
    id::Union{Nothing,ID}
    final::Union{Nothing,Bool}
    variety::Union{XsdRestriction, XsdList, XsdUnion}
end

struct XsdNamedSimpleType <: XsdSimpleType
    name::QName
    id::Union{Nothing,ID}
    final::Union{Nothing,Bool}
    variety::Union{XsdRestriction, XsdList, XsdUnion}
end

import Base: show
# show(io::IO, t::XsdNamedSimpleType) = t.name
# show(io::IO, t::XsdAnonSimpleType) = ""t.variety

(t::XsdSimpleType)(x::AbstractString) = build(x, t.variety)
# (t::XsdAnonType)(x::AbstractString) = build(x, t.variety)

revdict(d::AbstractDict) = Dict(d[k]=>k for k in keys(d))

function simpletype(t::EzXML.Node)

    global schema
    tns = schema.tns

    # extract the target namespace prefix
    rd = revdict(Dict(namespaces(t)))
    tnspfx = haskey(rd, tns) ? rd[tns] : nothing

    # create `final`, `id` and `name` variables from the attributes
    for a in (:final, :id)
        v = haskey(t, string(a)) ? t[string(a)] : nothing
        !isnothing(v) && @eval $a = $v
    end

    childnd = firstelement(t)
    childnm = nodename(childnd)
    if childnm == "restriction"
        variety = restriction(childnd)
    elseif childnm == "list"
        variety = list(childnd)
    elseif childnm == "union"
        variety = union(childnd)
    else
        error("Illegal child element '$(childnm)'")
    end
    @show variety

    # name is required
    nm = haskey(t, "name") ? t["name"] : nothing
    @show nm
    if nm !== nothing
        v = XsdNamedSimpleType(QName(tnspfx,nm), variety)
    else
        v = XsdAnonSimpleType(variety)
    end

    @show v
    # store the simple type in the Schema   
    push!(schema.simpleTypes, v)
    # store the simple type in the global dictionary, keyed against the target namespace prefix
    simpletypedict[QName(tnspfx,nm)] = v
    return v
end


struct XsdComplexType end

struct XsdElement
    name::String
    type::Union{XsdSimpleType, XsdComplexType}
    minoccurs::Union{Nothing, Int}
    maxoccurs::Union{Nothing, Int, String}
end


mutable struct XsdSchema
    tns::Union{AnyURI,Nothing}
    ns::Dict{String,AnyURI}
    inc::Union{Nothing,Vector{XsdInclude}}
    imp::Union{Nothing,Vector{XsdImport}}
    redefines::Union{Nothing,Vector{XsdRedefine}}
    annotations::Union{Nothing,Vector{XsdAnnotation}}
    simpleTypes::Vector{XsdSimpleType}
    complexTypes::Vector{XsdComplexType}
    groups::Union{Nothing,Vector{XsdGroup}}
    attributeGroups::Union{Nothing, Vector{XsdAttributeGroup}}
    elements::Vector{XsdElement}
    attributes::Union{Nothing, Vector{XsdAttribute}}
    notations::Union{Nothing, Vector{XsdNotation}}
end

function XsdSchema(n::EzXML.Node)
    #process includes and imports
    tns = haskey(n, "targetNamespace") ? AnyURI(n["targetNamespace"]) : nothing
    child = firstelement(n)
    while (f = Symbol(nodename(child))) in (:include,:import)
        @show f
        @eval $f(firstelement($n))
        child = first
    end
    nsd = merge(Dict(k=>AnyURI(v) for (k,v) in namespaces(n)), DICT_DEFAULT_NS)
    XsdSchema(tns, nsd, nothing,nothing,nothing,nothing,XsdSimpleType[],XsdComplexType[],nothing,nothing,XsdElement[],nothing,nothing)
end
