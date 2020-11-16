IDN = Union{ID,Nothing}
URIN = Union{AnyURI,Nothing}
NNIUNB = Union{XsdNonNegativeInt, UNBOUNDED}

abstract type XsdSimpleType end
abstract type ModelGroupType end
"""
See https://www.w3.org/TR/xmlschema-1/#Identity-constraint_Definition_details
"""
abstract type XsdIdentityConstraint end


struct XsdAppInfo
    source::URIN
    content::Any
end


struct XsdDocumentation
    source::URIN
    xml_lang::Union{XsdLanguage, Nothing}
    content::Any
end

"""
See https://www.w3.org/TR/xmlschema-1/#Annotation_details
"""
struct XsdAnnotation
    id::IDN
    content::Vector{Union{XsdAppInfo, XsdDocumentation}} # 0 or more of either
end

XsdAnnotation(id, d::XsdDocumentation) = XsdAnnotation(id, [d])
XsdAnnotation(id, d::XsdAppInfo) = XsdAnnotation(id, [d])

ANNOTN = Union{XsdAnnotation,Nothing}

#TODO Implement the rest of Identity Constraints


struct XsdField
    id::IDN
    xpath::AbstractString #required
    annot::ANNOTN
end

struct XsdSelector
    id::IDN
    xpath::AbstractString #required
    annot::ANNOTN
end


struct XsdKeyRef <: XsdIdentityConstraint
    id::IDN
    name::NCName #required
    refer::QName # required
    annot::ANNOTN
    selector::XsdSelector #required
    fields::Vector{XsdField} # >= 1
end
XsdKeyRef(id,name::AbstractString,refer::AbstractString,annot,selector,fields) = XsdKeyRef(id,NCName(name),QName(refer),annot,selector,fields)


struct XsdKey <: XsdIdentityConstraint
    id::IDN
    name::NCName #required
    annot::ANNOTN
    selector::XsdSelector #required
    fields::Vector{XsdField} # >= 1
end
XsdKey(id,name::AbstractString,annot,selector,fields) = XsdKey(id,NCName(name),annot,selector,fields)


struct XsdUnique <: XsdIdentityConstraint
    tns::URIN
    id::IDN
    name::NCName #required
    annot::ANNOTN
    selector::XsdSelector
    fields::Vector{XsdField} # >= 1
end
XsdUnique(tns,id,s::AbstractString,annot,selector,fields) = XsdUnique(tns,id,NCName(s),annot,selector,fields)




struct XsdAnyAttribute
    id::IDN
    namespace::URIN
    processContents::PROC_CONTENTS #required
    content::Union{XsdAnnotation, Nothing}
end


# (:whiteSpace, true, tuple(WSOPTIONS)),

xsdsimple = (XsdDuration, XsdString, XsdInt, XsdToken, XsdBoolean, XsdNonNegativeInt, XsdPositiveInt, XsdDateTime, QName, ID)

#(minExclusive | minInclusive | maxExclusive | maxInclusive | totalDigits | fractionDigits | length | minLength | maxLength | enumeration | whiteSpace | pattern)
# RestrictionTypes: value of `restriction` elements
xtypes = (
(:enumeration,      false,  xsdsimple,          xsdrequired.("value", xsdsimple)),
(:minExclusive,     true,   xsdsimple,          xsdrequired.("value", xsdsimple)),
(:minInclusive,     true,   xsdsimple,          xsdrequired.("value", xsdsimple)),
(:maxExclusive,     true,   xsdsimple,          xsdrequired.("value", xsdsimple)),
(:maxInclusive,     true,   xsdsimple,          xsdrequired.("value", xsdsimple))
)

stypes = (
(:totalDigits,      true,   XsdPositiveInt,     xsdrequired("value", XsdPositiveInt)),
(:fractionDigits,   true,   XsdNonNegativeInt,  xsdrequired("value", XsdNonNegativeInt)),
(:length,           true,   XsdNonNegativeInt,  xsdrequired("value", XsdNonNegativeInt)),
(:minLength,        true,   XsdNonNegativeInt,  xsdrequired("value", XsdNonNegativeInt)),
(:maxLength,        true,   XsdNonNegativeInt,  xsdrequired("value", XsdNonNegativeInt)),
(:pattern,          false,  XsdString,          xsdrequired("value", XsdString))
)

for t in xtypes # outer types 
    nm, fixed, typs, defvals = t
    # @debug "outer types" nm fixed typs defvals
    ot = Symbol("Xsd", uppercasefirst(string(nm)))
    snm = Symbol("start", lowercase(string(nm)), "!")
    enm = Symbol("end", lowercase(string(nm)), "!")
    if fixed
        @eval begin 
            struct $ot{T <: XsdAnySimpleType} <: RestrictionType
                fixed::Bool
                id::IDN
                value::T
                annot::ANNOTN
            end
        end
        for it in typs # inner types 
            @eval begin
                # $ot(t::T) where {T<:XsdAnySimpleType} = $ot(false,nothing,t)
                $ot{$it}(z::AbstractString) = $ot(false,nothing,$it(z),nothing)
                $ot{$it}(x::Bool, id::IDN, z::AbstractString, annot::ANNOTN, nothing) = $ot(x, id, $it(z), annot)
                # $ot{$it}(id::IDN, z::AbstractString) = $ot{$it}(false, id, nothing, z)
                # $ot{$it}(y::AbstractString, z::AbstractString) = $ot{$it}(ID(y), z)
                # $ot{$it}(f::Function, fixed::Bool, id::IDN, annot::ANNOTN, s::AbstractString) = $ot(fixed,id,annot,f(s))
                # $ot{$it}(f::Function, id::IDN, annot::ANNOTN, s::AbstractString) = $ot{$it}(f,false,id,annot,s)
                # $ot{$it}(f::Function, id::AbstractString, z::AbstractString) = $ot{$it}(false, ID(id), nothing, z)
                # $ot{$it}(f::Function, s::AbstractString) = $ot{$it}(f,false,nothing,s)
                $ot{$it}(z::Function) = z()
            end
        end

        @eval begin
            $snm(reader::EzXML.StreamReader) = push(reader, 
            template(reader, $ot, (
                ("fixed", Bool, false),
                ("id", ID, nothing),
                ("value", Type{<:XsdSimpleType}, xsdrequired("value", Type{<:XsdSimpleType}))
                )))
            end
    else
        @eval begin 
            struct $ot{T <: XsdAnySimpleType} <: RestrictionType
                id::IDN
                value::T
                annot::ANNOTN
            end
        end

        for it in typs
            @eval begin
                $ot{$it}(id::IDN, s::AbstractString, annot::ANNOTN, nothing) = $ot(id, $it(s), annot)
                $ot{$it}(s::AbstractString) = $ot{$it}(nothing, s, nothing)
                # $ot{$it}(y::AbstractString, z::AbstractString) = $ot{$it}(ID(y), z)
                # $ot{$it}(f::Function, z::AbstractString) = $ot(nothing, f(z))
                # $ot{$it}(f::Function, id::IDN, z::AbstractString) = $ot(id, f(z))
                # $ot{$it}(f::Function, id::AbstractString, z::AbstractString) = $ot{$it}(ID(id), z)
                $ot{$it}(z::Function) = z()
            end
            # @eval println(methods($ot{$it}))
        end
            
        @eval begin
            $snm(reader::EzXML.StreamReader) = push(reader, 
            template(reader, $ot, (
                ("id", ID, nothing),
                ("value", Type{<:XsdSimpleType}, xsdrequired("value", Type{<:XsdSimpleType}))
                )))
                
        end        
    end
    @debug "creating end method" enm
    @eval $enm(reader::EzXML.StreamReader) = push(reader, stuffbelowlives!(reader, $ot))
end

for t in stypes
    nm, fixed, typ = t
    ot = Symbol("Xsd", uppercasefirst(string(nm)))
    if fixed
        @eval begin 
            struct $ot <: RestrictionType
                fixed::Bool
                id::IDN
                value::$typ
            end
        end
        @eval begin
            $ot(t::T) where {T<:XsdAnySimpleType} = $ot(false,nothing,t)
            $ot(s::AbstractString) = $ot(false,nothing,$typ(s))
            $ot(x::Bool, id::IDN, z::AbstractString) = $ot(x, id, $typ(z))    
            # $ot(id::IDN, s::AbstractString) = $ot(false,id,s)
            # $ot(y::AbstractString, z::AbstractString) = $ot(ID(y), z)
            # $ot(f::Function, fixed::Bool, id::IDN, s::AbstractString) = $ot(fixed,id,f(s))
            # $ot(f::Function, id::IDN, s::AbstractString) = $ot(f,false,id,s)
            # $ot(f::Function, id::AbstractString, z::AbstractString) = $ot(false, ID(id), z)
            # $ot(f::Function, s::AbstractString) = $ot(f,false,nothing,s)
            $ot(z::Function) = z()
        end
    else
        @eval begin 
            struct $ot <: RestrictionType
                id::IDN
                value::$typ
            end
        end
        @eval begin
            $ot(id::IDN, s::AbstractString) = $ot(id, $typ(s))
            $ot(s::AbstractString) = $ot(nothing, s)
            # $ot(y::AbstractString, z::AbstractString) = $ot(ID(y), z)
            # $ot(f::Function, z::AbstractString) = $ot(nothing, f(z))
            # $ot(f::Function, id::IDN, z::AbstractString) = $ot(id, f(z))
            # $ot(f::Function, id::AbstractString, z::AbstractString) = $ot(f, ID(id), z)
            $ot(z::Function) = z()
        end
    end
    # @eval println(methods($ot))
end


# All XsdSimpleTypes get instantiated with a string
# process(t::Type{<:XsdAnySimpleType}, v::AbstractString) = t(v)
# process(::Type{T}, v::T) where {T} = v
# process(t::Type{<:Real}, v::AbstractString) = parse(t,v)
# process(::Nothing,v) = nothing
# process(::Type{Any}, v::Function) = v() #this function throws the error about missing required values.
# process(::T, ::String) where {T<:XsdAnySimpleType} = println("BADASS PART 2!")


struct XsdExtension
    base::QName
    id::IDN
    content::Any
end

struct XsdRestriction
    base::QName
    id::IDN
    annot::Union{XsdAnnotation, Nothing}
    content::Vector{Any}
end
XsdRestriction(b::QName, id::IDN, content::Vector{T}) where {T<:RestrictionType} = XsdRestriction(b,id, nothing,content)
XsdRestriction(b::QName, id::IDN, v::T) where {T<:RestrictionType} = XsdRestriction(b, id, nothing, [v])
# XsdRestriction(s::AbstractString, id::IDN, v::Vector{T}) where {T<:RestrictionType} = XsdRestriction(QName(s), id, v)


struct XsdInclude
    schemaLocation::AnyURI
end
XsdInclude(u::AbstractString) = XsdInclude(AnyURI(u))


struct XsdImport
    namespace::Union{Nothing,String}
    schemaLocation::Union{Nothing,AnyURI}
end

struct XsdRedefine
    id::String
    schemaLocation::AnyURI
end
XsdRedefine(u::AbstractString) = XsdRedefine(AnyURI(u))


struct XsdComplexType
    tns::Union{AnyURI,Nothing}
    abstract::XsdBoolean
    block::Union{Nothing, XsdString}
    final::Union{Nothing, XsdString}
    id::IDN
    mixed::XsdBoolean
    name::Union{NCName,Nothing}
    # --- Calculated values
    prohibsub::Union{Tuple{String}, Tuple{}}
    finalsub::Union{Tuple{String}, Tuple{}}
    # --- Content
    annot::ANNOTN
    content::Any
end

"""
https://www.w3.org/TR/xmlschema-1/#cAttribute_Declarations
"""
struct XsdAttribute
    default::Union{XsdString,Nothing}
    fixed::Union{XsdString,Nothing}
    form::Union{QUAL, Nothing} # is used to set tns
    id::IDN
    name::NCName #required
    ref::Union{QName, Nothing}  
    type::Union{QName, Nothing} # ref to a type name or nothing. Nothing means content must contain a simpleType
    use::ATTUSE # default 'optional'
    t::Union{XsdSimpleType, XsdComplexType} # reference to actual type, after dereferencing of `type`, or to the type contained in `content`
    annot::ANNOTN
    tns::URIN #from schema or absent
    content::Union{XsdSimpleType, Nothing}
end

revdict(d::AbstractDict) = Dict(d[k]=>k for k in keys(d))

struct XsdList
    id::IDN
    itemtype::Union{QName, Nothing}
    annot::Union{XsdAnnotation, Nothing}
    content::Union{XsdSimpleType, Nothing}
end

struct XsdUnion
    id::IDN
    membertypes::Union{Vector{QName},Nothing}
    annot::Union{XsdAnnotation, Nothing}
    content::Vector{XsdSimpleType}
end


struct XsdAnonSimpleType <: XsdSimpleType
    id::Union{Nothing,ID}
    final::Union{Nothing,Bool}
    annot::Union{XsdAnnotation, Nothing}
    variety::Union{XsdRestriction, XsdList, XsdUnion}
end
XsdAnonSimpleType(id::AbstractString, variety::Union{XsdRestriction, XsdList, XsdUnion}) = XsdAnonSimpleType(ID(id),nothing,variety)
XsdAnonSimpleType(id::AbstractString, final, variety::Union{XsdRestriction, XsdList, XsdUnion}) = XsdAnonSimpleType(ID(id),final,variety)
XsdAnonSimpleType(variety::Union{XsdRestriction, XsdList, XsdUnion}) = XsdAnonSimpleType(nothing,nothing,variety)

struct XsdNamedSimpleType <: XsdSimpleType
    name::AbstractString
    id::Union{Nothing,ID}
    final::Union{Nothing,String} # Actually, it's a bit more complicated than this. See https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/datatypes.html#rf-defn
    annot::Union{XsdAnnotation, Nothing}
    variety::Union{XsdRestriction, XsdList, XsdUnion}
end
XsdNamedSimpleType(nm::AbstractString, v::Union{XsdRestriction, XsdList, XsdUnion}) = XsdNamedSimpleType(nm,nothing,nothing,v)
XsdNamedSimpleType(nm::AbstractString, id::Union{Nothing,ID}, v::Union{XsdRestriction, XsdList, XsdUnion}) = XsdNamedSimpleType(nm,id,nothing,v)
XsdNamedSimpleType(nm::AbstractString, id::AbstractString, v::Union{XsdRestriction, XsdList, XsdUnion}) = XsdNamedSimpleType(nm,ID(id),v)
XsdNamedSimpleType(nm::AbstractString, id::AbstractString, final::Union{Nothing,String}, v::Union{XsdRestriction, XsdList, XsdUnion}) = XsdNamedSimpleType(nm,ID(id),final,v)
XsdNamedSimpleType(nm::AbstractString, id::AbstractString, final::AbstractString, v::Union{XsdRestriction, XsdList, XsdUnion}) = XsdNamedSimpleType(nm,ID(id),final,v)

import Base: show
# show(io::IO, t::XsdNamedSimpleType) = t.name
# show(io::IO, t::XsdAnonSimpleType) = ""t.variety

(t::XsdSimpleType)(x::AbstractString) = build(x, t.variety)
# (t::XsdAnonType)(x::AbstractString) = build(x, t.variety)

struct XsdSimpleContent
    id::IDN
    annot::Union{XsdAnnotation, Nothing}
    content::Union{XsdRestriction, XsdExtension}
end

# May need to carry the target namespace as tns::Union{AnyURI,Nothing}
# May need to carry an explicit `scope` member, set during element construction, that may be global or limited to a complexType parent.
struct XsdElement
    tns::Union{AnyURI,Nothing} #1
    abstract::XsdBoolean
    block::Union{Nothing, XsdString}
    default::Union{Nothing, XsdString}
    final::Union{Nothing, XsdString} #5
    fixed::Union{Nothing,XsdString}
    form::Union{Nothing, QUAL}
    id::Union{Nothing,ID}
    maxoccurs::NNIUNB
    minoccurs::XsdNonNegativeInt # 10
    name::Union{NCName,Nothing} #password
    nillable::XsdBoolean #false
    ref::Union{Nothing, QName} #13
    substitutionGroup::Union{Nothing,QName}
    type::Union{QName, Nothing} # 15 if nothing, type is the enclosed content
    annot::ANNOTN
    typr::Union{Type{AST}, ST, XsdComplexType, Nothing} where {ST<:XsdSimpleType, AST <: XsdAnySimpleType} # this references either the type dereferenced from 'type' or the enclosed type defn in 'content'
    content::Union{T, XsdComplexType, Nothing} where {T<:XsdSimpleType}
end

"Create a new Element from an existing one, with new minOccurs and maxOccurs parameters."
XsdElement(e::XsdElement, minoccurs::XsdNonNegativeInt, maxoccurs::NNIUNB) = XsdElement(
    e.tns,e.abstract,e.block,e.default,e.final,e.fixed,e.form,e.id,
    maxoccurs,
    minoccurs,
    e.name,e.nillable,e.ref,e.substitutionGroup,e.type,e.annot,e.typr,e.content
    )

struct XsdAny
    # id = ID
    # maxOccurs = (nonNegativeInteger | unbounded)  : 1
    # minOccurs = nonNegativeInteger : 1
    # namespace = ((##any | ##other) | List of (anyURI | (##targetNamespace | ##local)) )  : ##any
    # processContents = (lax | skip | strict) : strict
    # {any attributes with non-schema namespace . . .}>
    # Content: (annotation?)

    id::IDN
    maxoccurs::NNIUNB
    minoccurs::XsdNonNegativeInt
    namespace::Union{NS_CONSTRAINT, Vector{Union{AnyURI, TNSLOCAL}}} #default = 'xsany'
    processContents::PROC_CONTENTS #required; default = strict
    annot::Union{XsdAnnotation,Nothing}
end


function makens(ns)
    n = nothing
    try
        n = NS_CONSTRAINT(ns)            
    catch
        try 
            n = TNSLOCAL(ns)
        catch
            n = AnyURI(ns)
        end
    end
end

XsdAny(id, maxos::AbstractString, minomino::AbstractString, ns::AbstractString, pc, annot) = XsdAny(id, isnumeric(first(maxos)) ? XsdNonNegativeInt(maxos) : UNBOUNDED(maxos),XsdNonNegativeInt(mino),makens(ns),pc,annot)
XsdAny(id, maxos::AbstractString, mino::AbstractString, ns, pc, annot) = XsdAny(id,isnumeric(maxos) ? XsdNonNegativeInt(maxos) : UNBOUNDED(maxos), XsdNonNegativeInt(mino), ns,pc,annot)
XsdAny(id, maxos, mino::AbstractString, ns, pc, annot) = XsdAny(id,maxos, XsdNonNegativeInt(mino), ns,pc,annot)
XsdAny(id, maxos, mino::AbstractString, ns::AbstractString, pc, annot) = XsdAny(id,maxos, XsdNonNegativeInt(mino), makens(ns), pc, annot)
XsdAny(id, maxos, mino, ns::AbstractString, pc, annot) = XsdAny(id, maxos, mino, makens(ns), pc, annot)


struct XsdAttributeGroup
    id::IDN
    ref::QName
    content::ANNOTN
end


struct XsdAll <: ModelGroupType
    id::IDN
    maxoccurs::XsdPositiveInt # must be 1
    minoccurs::XsdNonNegativeInt # may be 0 | 1
    annot::Union{XsdAnnotation,Nothing}
    elements::Vector{XsdElement}
end

XsdAll(id,maxo::AbstractString,mino::AbstractString,annot,elements) = XsdAll(
    id,
    XsdPositiveInt(1), # ignoring the input
    (0 <= parse(Int,mino) < 2) ? XsdNonNegativeInt(parse(Int,mino)) : error("minoccurs must equal 0|1."),
    annot,
    elements)
    
abstract type ModelGroupDefinition end

"""
    https://www.w3.org/TR/xmlschema-1/#element-group
"""
struct XsdGroup <: ModelGroupDefinition
    tns::URIN
    id::IDN
    maxoccurs::NNIUNB #default = 1
    minoccurs::XsdNonNegativeInt # default = 1
    name::NCName
    ref::Union{QName, Nothing}
    annot::ANNOTN
    contents::Any
end

struct XsdChoice <: ModelGroupType
    id::IDN
    maxoccurs::NNIUNB
    minoccurs::ZEROONE
    annot::Union{XsdAnnotation,Nothing}
    content::Any
end


struct XsdSequence <: ModelGroupType
    id::IDN
    maxoccurs::NNIUNB
    minoccurs::XsdNonNegativeInt
    annot::Union{XsdAnnotation,Nothing}
    content::Union{Nothing,Vector{Union{XsdElement, XsdGroup, XsdChoice, XsdSequence, XsdAny}}} # 0 or more
end    


mutable struct XsdSchema
    targetNamespace::Union{AnyURI,Nothing}
    ns::Dict{String,AnyURI}
    attributeFormDefault::QUAL
    blockdefault::Union{String,Nothing}
    elementformdefault::QUAL
    finaldefault::Union{String, Nothing}
    id::IDN
    version::Union{XsdToken, Nothing}
    xml_lang::Union{XsdLanguage,Nothing}   #TODO figure out how to support this
    inc::Union{Nothing,Vector{XsdInclude}}
    imp::Union{Nothing,Vector{XsdImport}}
    redefines::Union{Nothing,Vector{XsdRedefine}}
    annotations::Union{Nothing,Vector{XsdAnnotation}}
    simpletypes::Vector{XsdSimpleType}
    complextypes::Vector{XsdComplexType}
    groups::Union{Nothing,Vector{XsdGroup}}
    attributegroups::Union{Nothing, Vector{XsdAttributeGroup}}
    elements::Vector{XsdElement}
    attributes::Union{Nothing, Vector{XsdAttribute}}
    notations::Union{Nothing, Vector{XsdNotation}}
    typed::Dict{QName, Union{Type{<:XsdAnySimpleType}, <:XsdSimpleType, XsdComplexType}}
    elemd::Dict{QName, XsdElement}
end

