function nameparts(v)
    t = split(v,':')
    length(t) < 2 ? (nothing,t[1]) : (t[1],t[2])
end

# nspfx(node::EzXML.Node) = nodepath(node)[1:first(findlast(nodename(node), nodepath(node)))-1] 
nspfx(node::EzXML.Node) = first(nameparts(nodepath(node)))

function nspfx(reader::EzXML.StreamReader)
    reader.type == EzXML.READER_ELEMENT || reader.type == EzXML.READER_ATTRIBUTE || return nothing
    # @show nodeattributes(reader)
    # haskey(nodeattributes(reader), "name") || return nothing
    first(nameparts(nodename(reader)))
end

barename(reader::EzXML.StreamReader) = last(nameparts(nodename(reader)))

barename(n::EzXML.Node) = last(nameparts(nodepath(node)))

function prefixns(reader::EzXML.StreamReader)
    @debug "in prefixns"

    reader.type == EzXML.READER_ELEMENT || reader.type == EzXML.READER_ATTRIBUTE || return nothing
    pfx = ns = nothing    
    try
        ns = namespace(reader)
        !isnothing(ns) || return nothing
        @debug "ns" ns
        pfx = nspfx(reader)
        @debug "pfx" pfx
        # pfx = prefix4ns(ns)  # this doesn't work on <schema> because it has not been processed yet
    catch e
        @error e
    end
    (pfx, ns)
end

"Turns all multiple spaces into one space"
thin(s::AbstractString) = replace(s, r"\s+" => ' ')

"Return a function that when invoked with a type, raises an error with a custom message."
xsdrequired(nm::AbstractString, T::Type) = ()->error("Attribute '$(nm)' is required for $(T)")

# const NCREGEX = r"^[\w_][\w\d\.\-_\W]*"

# struct NCName <: XsdAnySimpleType
#     s::String
#     NCName(x::AbstractString) = !isnothing(match(NCREGEX, x)) ? new(string(x)) : error("'$(x)' is not an NCName. The string must conform to $(NCREGEX)")

# IDEA 
# ncnamewash(s) = code to enforce NCName rules or raise an error