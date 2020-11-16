# Data type hierarchy
abstract type XsdAnyType end
abstract type XsdAnySimpleType <: XsdAnyType end
abstract type RestrictionType end

import Base: show

"White space options"
@enum WSOPTIONS begin
    wscollapse
    wspreserve
    wsreplace
end
WSOPTIONS(s::AbstractString) = @eval $(Symbol(:ws,s))

"Qualified/unqualified for element and attribute names"
@enum QUAL begin
    qualified
    unqualified
end
QUAL(s::AbstractString) = @eval $(Symbol(s))

@enum UNBOUNDED unbounded
UNBOUNDED(s::AbstractString) = @eval $(Symbol(s))
UNBOUNDED(Nothing) = nothing


@enum ZEROONE begin
    occurszero
    occursone
end
function ZEROONE(s::AbstractString)
    @eval $(Symbol("occurs", (parse(Int,s)==0 ? "zero" : parse(Int,s)==1 ? "one" : error("$s must be 0 or 1"))))
end
ZEROONE(i) = ZEROONE(string(i))
# I am going to need to make ZEROONE act like a number. How to do this?

@enum PROC_CONTENTS begin
    lax
    skip
    strict
end
PROC_CONTENTS(s::AbstractString) = @eval $(Symbol(s))

"""
See https://www.w3.org/TR/xmlschema-1/#Wildcard_details
"""
@enum NS_CONSTRAINT begin
    nsany # = '##any'
    nsother # = '##other' This specifically means DO NOT use the tns from <schema>
end
NS_CONSTRAINT(s::AbstractString) = @eval $(Symbol("ns",replace(s,r"^##(\w+)"=>s"\1")))

@enum TNSLOCAL begin
    nstargetNamespace # = '##targetNamespace'
    nslocal # = '##local'
end
TNSLOCAL(s::AbstractString) = @eval $(Symbol("ns",replace(s,r"^##(\w+)"=>s"\1")))

@enum ATTUSE begin
    optional
    prohibited
    required
end

const NCREGEX = r"^[\w_][\w\d\.\-_\W]*"
## NCName

struct NCName <: XsdAnySimpleType
    s::String
    NCName(x::AbstractString) = !isnothing(match(NCREGEX, (chomp ∘ strip)(x))) ? new((chomp ∘ strip)(x)) : error("'$(x)' is not an NCName. The string must conform to $(NCREGEX)")
end

import Base: show
Base.show(io::IO, n::NCName) = write(io, n.s)
Base.getindex(q::NCName, i::Int) = getindex(q.s,i)
Base.print(io::IO, n::NCName) = print(io, n.s)

## QName
struct QName <: XsdAnySimpleType
    pfx::Union{String,Nothing}
    nm::NCName
end

function QName(nm::AbstractString)
    p,n = nameparts(nm)
    QName(p,NCName(n))
end

QName(pfx, nm::AbstractString) = QName(pfx, NCName(nm))
QName(o::QName) = QName(o.pfx, o.nm)

## NOTATION
struct XsdNotation <: XsdAnySimpleType
end

## HexBinary
struct XsdHexBinary <: XsdAnySimpleType
end

## Base64Binary
struct XsdBase64Binary <: XsdAnySimpleType
end


struct XsdDuration <: XsdAnySimpleType
    y::Int
    m::Int
    d::Int
    h::Int
    min::Int
    s::Int
end

struct XsdString <: XsdAnySimpleType
    s::String
    XsdString(x) = new(string(x))
end
XsdString() = XsdString("")
Base.show(io::IO, x::XsdString) = print(io, x.s)

NCName(xs::XsdString) = NCName(xs.s)
QName(x::XsdString, n::NCName) = QName(x.s, n)
# QName(x::XsdString, n) = QName(x.s, n)
QName(x::XsdString, n::XsdString) = QName(x.s, NCName(n))
QName(p, x::XsdString) = QName(string(p), NCName(x))


struct XsdInt <: XsdAnySimpleType
    v::Int
end
XsdInt(s::AbstractString) = XsdInt(parse(Int,s))
XsdInt() = XsdInt(0)

struct XsdNumber <: XsdAnySimpleType
    v::Float64
end
XsdNumber(s::AbstractString) = XsdNumber(parse(Float64,s))
XsdNumber() = XsdNumber(NaN)


struct XsdToken <: XsdAnySimpleType
    s::String
    XsdToken(v) = new((chomp ∘ strip ∘ thin)(string(v)))
end
XsdToken() = XsdToken("")

struct XsdBoolean <: XsdAnySimpleType
    v::Bool
end
XsdBoolean(s::AbstractString) = XsdBoolean(parse(Bool,s))
# XsdBoolean(x) = XsdBoolean(convert(Bool,x)) # or convert to string?????
XsdBoolean() = XsdBoolean(false)

struct XsdNonNegativeInt <: XsdAnySimpleType
    v::UInt
end

XsdNonNegativeInt(s::AbstractString) = (parse(UInt,s) >= 0) ? XsdNonNegativeInt(parse(UInt,s)) : error("s must represent a non-negative integer >= 0. Got $(s)")
XsdNonNegativeInt(x::Int) = XsdNonNegativeInt(unsigned(x))

struct XsdPositiveInt <: XsdAnySimpleType
    v::UInt
end

XsdPositiveInt(s::AbstractString) = (parse(UInt,s) > 0) ? XsdPositiveInt(parse(UInt,s)) : error("s must represent a positive integer > 0. Got $(s)")
XsdPositiveInt(x::Int) = XsdPositiveInt(unsigned(x))


"""
See [https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/datatypes.html#dateTime](@ref)
"""
const XSD_DATE_FORMAT = dateformat"yyyy-mm-ddTHH:MM:SS-zzzzzz"

struct XsdDateTime <: XsdAnySimpleType
    dt::DateTime
end
XsdDateTime(s::AbstractString) = XsdDateTime(parse(DateTime, s, XSD_DATE_FORMAT))
XsdDateTime() = XsdDateTime(now())


struct XsdDate <: XsdAnySimpleType
    dt::Date
end
XsdDate(s::AbstractString) = XsdDate(parse(Date, s, XSD_DATE_FORMAT)) #THIS WILL LIKELY FAIL


struct XsdTime <: XsdAnySimpleType
    t::Time
end
XsdTime(s::AbstractString) = XsdTime(parse(Time, s, XSD_DATE_FORMAT)) #THIS WILL LIKELY FAIL

import Base: show, getindex, print, convert
Base.show(io::IO, n::QName) = write(io, isnothing(n.pfx) ? n.nm : string("QName(",n.pfx,':',n.nm,")"))
Base.getindex(q::QName, i::Int) = getindex(string(q),i)
Base.print(io::IO, n::QName) = print(io, isnothing(n.pfx) ? n.nm : string(n.pfx,':',n.nm))
# convert(t::Type{QName}, s::AbstractString) = t(s)

import Base: ==
==(x::AbstractString, y::QName) = string(x) == string(y)
==(x::QName, y::AbstractString) = ==(y,x)
==(x::QName, y::QName) = ==(string(x),string(y))
(::QName)(s::AbstractString) = QName(s)
# Base.convert(::Type{QName}, s::String) = QName(s)

## ID

struct ID <: XsdAnySimpleType
    id::String
    ID(s) = ID((chomp ∘ strip ∘ thin)(string(s)))
end
# ID(x::T) where {T<:Number} = ID(string(x))

Base.show(io::IO, x::ID) = write(io, x.id)
# convert(t::Type{ID}, s::AbstractString) = t(s)

==(x::AbstractString, y::ID) = string(x) == string(y)
==(x::ID, y::AbstractString) = ==(y,x)
==(x::ID, y::ID) = ==(string(x),string(y))

import Base.:*
*(a::QName, b::QName) = string(a,b)

## AnyURI
import URIParser: URI
# AnyURI = URI

struct AnyURI <: XsdAnySimpleType
    uri::URI
end
AnyURI(s::AbstractString) = AnyURI(URI(s))

"Language designations must respect this regex."
const LANGREGEX = r"[a-zA-Z]{1,8}(-[a-zA-Z0-9]{1,8})*"

struct XsdLanguage <: XsdAnySimpleType
    v::XsdToken
end
XsdLanguage(s::AbstractString) = !isnothing(match,LANGREGEX,s) ? XsdLanguage(XsdToken(s)) : error("'$(s)' does not designate a valid XML Language. The string must conform to $(LANGREGEX)")

"""

    prefix4ns(n::Union{AnyURI,AbstractString})

Return the prefix for a given namespace URI or string that resolves to a URI.

```
Given a namespace declaration such as
`<schema xmlns:xs="http://www.w3.org/2001/XMLSchema" ...>`

and a namespace

`ns = "http://www.w3.org/2001/XMLSchema"`

julia> prefix4ns(ns)

```

"""
# function prefix4ns(n::Union{AnyURI,AbstractString})
#     global schema
#     d = revdict(schema.ns)
#     return get(d, n, nothing)
# end
