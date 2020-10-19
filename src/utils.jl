function nameparts(v)
    t = split(v,':')
    length(t) < 2 ? (nothing,t[1]) : (t[1],t[2])
end

# nspfx(node::EzXML.Node) = nodepath(node)[1:first(findlast(nodename(node), nodepath(node)))-1] 
nspfx(node::EzXML.Node) = first(nameparts(nodepath(node)))

function nspfx(reader::EzXML.StreamReader)
    reader.type == EzXML.READER_ELEMENT || reader.type == EzXML.READER_ATTRIBUTE || return nothing
    @show nodeattributes(reader)
    haskey(nodeattributes(reader), "name") || return nothing
    first(nameparts(nodename(reader)))
end

barename(reader::EzXML.StreamReader) = last(nameparts(nodename(reader)))

barename(n::EzXML.Node) = last(nameparts(nodepath(node)))

function prefixns(reader::EzXML.StreamReader)
    println("in prefixns")

    reader.type == EzXML.READER_ELEMENT || reader.type == EzXML.READER_ATTRIBUTE || return nothing
    pfx = ns = nothing    
    try
        ns = namespace(reader)
        !isnothing(ns) || return nothing
        @show ns
        pfx = nspfx(reader)
        # pfx = prefix4ns(ns)  # this doesn't work on <schema> because it has not been processed yet
    catch        
    end
    (pfx, ns)
end
