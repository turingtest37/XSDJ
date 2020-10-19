
using EzXML
using Dates
using URIParser
using Logging


include("utils.jl")
include("components.jl")


# This is what we are building! Maybe this needs to go into a function....
schema = nothing

const XSDNS = "http://www.w3.org/2001/XMLSchema"
const XMLNS = "http://www.w3.org/XML/1998/namespace"

# These entries will be merged with those found in the <schema>
DICT_DEFAULT_NS = Dict{String,AnyURI}("xs" => AnyURI(XSDNS), "xml" => AnyURI(XMLNS))

XLMNS_PFX = "xmlns"

# a dictionary of namespaces to Julia modules
nsmoddict = Dict{String,Module}(
    XSDNS => @__MODULE__,
    XMLNS => @__MODULE__
)

# stack for temporary storage
stack = Any[]

regns(ns::AbstractString, m::Module) = nsmoddict[ns] = m
"""Register one or more `namespace => Module` pairs so that callbacks may be executed in other modules.
    regns("http://www.myns.org/things" => MyModule)
"""
regns(p::Tuple{Pair,Vararg{Pair}}) = regns.(first.(p), last.(p))

# QUESTION : should we/how store anonymous simple types?
simpletypedict = Dict{QName,XsdNamedSimpleType}()

complextypedict = Dict{QName,XsdComplexType}()

# TODO expand this
basictypedict = Dict{String,Type}(
    "string" => String, 
    "int" => Int, 
    "token" => String, 
    "dateTime" => DateTime, 
    "number" => Float64,
    "boolean" => Bool,
    )

build(v, r::XsdEnumerationRestriction) = v in r ? v : error("XsdEnumerationRestriction: $(v) not in $(r)")

build(v, r::XsdMaxLengthRestriction) = length(v) <= r.maxlength ? v : error("XsdMaxLengthRestriction: $(length(v))")

function startlist!(reader::EzXML.StreamReader)
    @show nodename(reader)
end

function startunion!(reader::EzXML.StreamReader)
    @show nodename(reader)
end

function startrestriction!(reader::EzXML.StreamReader)
    global simpletypedict

    depthin = nodedepth(reader)
    @show reader.name depthin

    nm = barename(reader)
    ns = namespace(reader)

    base = nothing
    hasnodeattributes(reader) || error("'base' is a required attribute of 'restriction'")
    att = nodeattributes(reader)
    haskey(att, "base") || error("'base' is a required attribute of 'restriction'")
    @show att["base"]
    base = QName(nameparts(att["base"])...)

    restr = nothing

    # iterate ahead to next element
    if hasnodecontent(reader)
        # iterate past ignored nodes and white space at the same level
        while (nodedepth(reader) == depthin)
            iterate(reader)
        end
        @show reader.type nodedepth(reader)
        
        while (nodedepth(reader) > depthin)
            if reader.type == EzXML.READER_ELEMENT
                @show reader.name
                childnm = barename(reader)
                @show childnm
                if childnm == "enumeration"
                    restr = isnothing(restr) ? XsdEnumerationRestriction(base, String[]) : restr
                    if hasnodeattributes(reader)
                        a = nodeattributes(reader)
                        v = haskey(a, "value") ? a["value"] : error("'value' attribute is required on an enumeration restriction.")
                        !isnothing(v) && push!(restr.enumeration,v)
                    end
                elseif childnm == "maxLength"
                    if hasnodeattributes(reader)
                        a = nodeattributes(reader)
                        v = haskey(a, "value") ? a["value"] : error("'value' attribute is required on a maxLength restriction.")
                        restr = XsdMaxLengthRestriction(base, parse(Int, v))
                    end
                end    
            end
            iterate(reader)
            @show reader.type nodedepth(reader)
        end
    else
        println("WTF?? No node content?")
    end
    @show restr
    push!(stack, restr)
    if reader.type == EzXML.READER_END_ELEMENT
        fname = Symbol("end", lowercase(nm), "!")
        haskey(nsmoddict, ns) || error("Shouldn't happen.")
        @eval $nsmoddict[$ns], $fname($reader)
    end
    return nothing
end

endrestriction!(reader::EzXML.StreamReader) = println("end restriction $(nodename(reader))")

process(g::XsdGroup) = @show g # dereference g.ref

process(i::XsdImport) = @show i # perform download and processing of i.schemaLocation

function startdocumentation!(reader::EzXML.StreamReader)
    @show nodename(reader)
end

function enddocumentation!(reader::EzXML.StreamReader)
    @show nodename(reader)
end

function startsimplecontent!(reader::EzXML.StreamReader)
    @show nodename(reader)
end

function endsimplecontent!(reader::EzXML.StreamReader)
    @show nodename(reader)
end

function startcomplexcontent!(reader::EzXML.StreamReader)
    @show nodename(reader)
end

function endcomplexcontent!(reader::EzXML.StreamReader)
    @show nodename(reader)
end

function startextension!(reader::EzXML.StreamReader)
    @show nodename(reader)
end

function endextension!(reader::EzXML.StreamReader)
    @show nodename(reader)
end

function startattribute!(reader::EzXML.StreamReader)
    @show nodename(reader)
end

function endattribute!(reader::EzXML.StreamReader)
    @show nodename(reader)
end

function startschema!(reader::EzXML.StreamReader)
    depthin = nodedepth(reader)

    t = prefixns(reader)
    prefix, ns = isnothing(t) ? (nothing, nothing) : (first(t), last(t))
    @show prefix,ns
    !isnothing(prefix) && !isnothing(ns) && (DICT_DEFAULT_NS[prefix] = AnyURI(ns))

    global schema = XsdSchema(expandtree(reader))

    for (k,v) in pairs(nodeattributes(reader))
        pfx,nm = nameparts(k)
        if pfx == XLMNS_PFX
            schema.ns[nm] = AnyURI(v)
        end
    end
    return nothing
end

endschema!(reader::EzXML.StreamReader) = @show nodename(reader)

function startimport!(reader::EzXML.StreamReader)
    @show reader.name
    # create XsdImport object
    # maybe need to download the referenced schema. Should we do that here?
end

function startinclude!(reader::EzXML.StreamReader)
    @show reader.name
    # create XsdInclude object
    # maybe need to download the referenced schema. Should we do that here?
end

function extattr(reader::EzXML.StreamReader, names::AbstractVector{Symbol})
    hasnodeattributes(reader) || return nothing
    attr = nodeattributes(reader)
    # ?
end

function startsimpletype!(reader::EzXML.StreamReader)
    global schema

    depthin = nodedepth(reader)

    ename = QName(reader.name)
    @show ename

    # attributes : do this for every element
    name = nothing
    id = nothing
    final = nothing
    if hasnodeattributes(reader)
        attr = nodeattributes(reader)
        id = haskey(attr, "id") ? attr["id"] : nothing
        name = haskey(attr, "name") ? attr["name"] : nothing
        final = haskey(attr, "final") ? attr["final"] : nothing
    end

    # if hasnodecontent(reader)
    #     while (nodedepth(reader) == depthin)
    #         iterate(reader)
    #     end
    # end
    push!(stack, (ename, name, id, final))
    @show stack
    return nothing
end

function prefix4ns(n::Union{AnyURI,AbstractString})
    global schema
    d = revdict(schema.ns)
    return get(d, n, nothing)
end

function endsimpletype!(reader::EzXML.StreamReader)
    global schema
    tns = schema.tns # this is a URI
    ns = nspfx(reader)

    # extract the target namespace prefix by doing a reverse lookup
    rd = revdict(schema.ns)
    tnspfx = haskey(rd, tns) ? rd[tns] : nothing

    variety = pop!(stack)
    if variety isa XsdRestriction
        st = pop!(stack)
        if first(st) !== QName(nodename(reader))
            println("WHOA! Where is my simpleType???")
            push!(stack, st)
            return nothing
        end
        _, nm, id, final = st
        if isnothing(nm)
            v = XsdAnonSimpleType(id, final, variety)
        else
            v = XsdNamedSimpleType(QName(nm), id, final, variety)
            simpletypedict[QName(tnspfx,nm)] = v
        end
        push!(schema.simpleTypes, v)
    else
        push!(stack, variety)
    end
    return nothing
end


function startsequence!(reader::EzXML.StreamReader)
    @show nodename(reader)
end

function endsequence!(reader::EzXML.StreamReader)
    @show nodename(reader)
end

function startcomplextype!(reader::EzXML.StreamReader)
    @show nodename(reader)
    # use XSD Spec to fill this out    
end

function endcomplextype!(reader::EzXML.StreamReader)
    @show nodename(reader)
    # use XSD Spec to fill this out    
end

function startelement!(reader::EzXML.StreamReader)
    @show nodename(reader)
end

function endelement!(reader::EzXML.StreamReader)
    @show nodename(reader)
end

function startchoice!(reader::EzXML.StreamReader)
    @show nodename(reader)    
end

function endchoice!(reader::EzXML.StreamReader)
    @show nodename(reader)
end

function startany!(reader::EzXML.StreamReader)
    @show nodename(reader)
end

function endany!(reader::EzXML.StreamReader)
    @show nodename(reader)
end

function startannotation!(reader::EzXML.StreamReader)
    @show nodename(reader)
end

function endannotation!(reader::EzXML.StreamReader)
    @show nodename(reader)
end


function spindry!(reader::EzXML.StreamReader)
    local ns
    @show nsmoddict
    # try
        for ndtype in reader
            # @show ndtype
            t = nameparts(nodename(reader))
            nm = length(t) > 1 ? last(t) : first(t)

            if ndtype == EzXML.READER_ELEMENT
                fname = Symbol("start", lowercase(nm), "!")
                ns = namespace(reader)
                # @show fname
                # we may to have a backup plan for a default module if nothing is found in nsmoddict
                haskey(nsmoddict, ns) || continue
                @eval $nsmoddict[$ns], $fname($reader)
            elseif ndtype == EzXML.READER_END_ELEMENT
                ns = namespace(reader)
                fname = Symbol("end", lowercase(nm), "!")
                haskey(nsmoddict, ns) || continue
                @eval $nsmoddict[$ns], $fname($reader)
            end
        end
    # catch
    #     close(reader)
    # end
end
# end # module XSDJ

# using SOAP4Julia
# include("SOAP4Julia.jl")

# regns("http://schemas.xmlsoap.org/wsdl/" => SOAP4Julia,
# "http://schemas.xmlsoap.org/wsdl/soap/" => SOAP4Julia)

f = "resources/sonos.xsd"
reader = open(EzXML.StreamReader, f)
# for item in reader
#     @show item, reader.name
# end
spindry!(reader)
close(reader)
@show schema
