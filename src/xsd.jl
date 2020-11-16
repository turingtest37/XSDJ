
# These entries will be merged with those found in the <schema>
DICT_DEFAULT_NS = Dict{String,AnyURI}("xs" => AnyURI(XSDNS), "xml" => AnyURI(XMLNS))

XLMNS_PFX = "xmlns"

# a dictionary of namespaces to Julia modules
nsmoddict = Dict{String,Module}(
    XSDNS => @__MODULE__,
    XMLNS => @__MODULE__
)

# stack for temporary storage
stack = nothing

"Put an object on the stack, including the current node depth of the reader."
push(r::EzXML.StreamReader, o) = !isnothing(o) && push!(stack, (nodedepth(r), o) )

"Pops an element off the stack, stripping off the node depth."
pop() = last(pop!(stack))

"Return the payload part of the `ith` element on the stack, popping the element off the stack."
function pop(i)
    r = last(stack[i])
    deleteat!(stack,i)
    r
end

"Return the payload part of the `ith` element on the stack, without popping it."
peek(i) = last(stack[i])

"Return the node depth of the ith element on the stack"
depth(i) = first(stack[i])

"Return the payload of the last element on the stack, without popping it."
peek() = last(stack[end])

"Returns the schema currently being constructed."
sch(reader::EzXML.StreamReader) = peekup(reader, XsdSchema)

function storetype(reader, v::Union{XsdComplexType, <:XsdSimpleType, <:XsdAnySimpleType}, nm::QName)
    local schema = sch(reader)
    schema.typed[nm] = v
end

function peekup(reader::EzXML.StreamReader, T::Type, levels=3)
    d = nodedepth(reader)
    slen = length(stack)
    # @debug "peekup stack depth:" slen
    slen > 0 || return nothing
    for i in slen:-1:1
        depth(i) < d || continue
        t = peek(i)
        # @debug "peekup peeked:" t
        if (t isa T) || (t isa Tuple && t[2] == T)
            # @debug "peekup We have a winner!" t
            return t
        end
    end
    return nothing
end

"Return all objects on the stack having node depths greater than the current depth,
without changing the reader."
function popbelow(r::EzXML.StreamReader)
    length(stack) < 1 && return nothing
    d = nodedepth(r)
    res = []
    (od,o) = pop!(stack)
    while od > d
        push!(res, o)
        (od,o) = pop!(stack)
    end
    push!(stack, (od,o))
    reverse!(res)
    return res
end

"Now supports single types or tuples"
nm_typ_args(s) = (length(s) < 2) ? (nothing,s,nothing) : length(s) < 3 ? (s[1],s[2],nothing) : (s[1],s[2],s[3:end])

function stuffbelowlives!(reader, T::Type, )

    @debug "stuffbelowlives! stack is:"
    # println.(stack)

    stuff = popbelow(reader)
    @debug "stuffbelowlives! got stuff" stuff 
    
    parent = pop()
    @debug "stuffbelowlives! popped" parent
    
    nm,typ,args = nm_typ_args(parent)    
    typ == T || error("Not to be outdone in the fuckups! $(typ) is not $(T)")
    
    # annot = nothing
    iobj = []
    for s in stuff
        @debug "stuffbelowlives! s from stuff" s
        if s isa Tuple
            snm,styp,sargs = nm_typ_args(s)
            @debug "stuffbelowlives! tuple parts" snm styp sargs
            if !isnothing(sargs)
                push!(iobj, _build(reader, styp, parent, sargs, nothing))
            else
                @warn "Tuple with no args? Hmmm." s
                push!(iobj, styp)
            end
        else
            push!(iobj,s)
        end
    end

    @debug "stuffbelowlives! Creating $T with" args iobj
    return _build(reader, T, parent, args, iobj)
end

function keepbrother!(reader)
    currdepth = nodedepth(reader)
    @debug "keepbrother! currdepth" currdepth

    slen = length(stack)
    slen > 0 || return nothing

    for i in slen:-1:1
        depth(i) == currdepth || continue # we only want tuples on the stack at the SAME level as us
        t = peek(i)
        if t isa Tuple
            @debug "keepbrother! peeked tuple:" t
            d = pop(i)
            @debug "keepbrother! popped:" d
            snm,styp,sargs = nm_typ_args(d)
            @debug "keepbrother! tuple parts" snm styp sargs
            if !isnothing(sargs)
                push(reader, _build(reader, styp, d, sargs, nothing))
            else
                @warn "Tuple with no args? Hmmm." d
                push(reader, d)
            end
        end
    end
    return nothing
end

function tnsprefix(schema)
    tns = schema.targetNamespace
    revdict(schema.ns)[tns]
end

regns(ns::AbstractString, m::Module) = nsmoddict[ns] = m

"""Register one or more `namespace => Module` pairs so that callbacks may be executed in other modules.
    regns("http://www.myns.org/things" => MyModule)
"""
regns(p::Tuple{Pair,Vararg{Pair}}) = regns.(first.(p), last.(p))

# process(t::Type{XsdEnumeration}, a, b::AbstractString) = t(a,b)
ensure(attr::Dict, k) = haskey(attr, k) || error("'$(k)' is a required attribute of $(reader.name)")

"Help the values along to their next life."
function normt(t::Type, v::AbstractString)
    if hasmethod(t, Tuple{typeof(v)})
        return t(v)
    elseif hasmethod(parse, Tuple{t,typeof(v)})
        return parse(t,v)
    elseif !isconcretetype(t)
        return v
    else
        error("dont know what to do with $t and $v")
    end
end
normt(t::Type, v::Bool) = v isa t ? v : t(v)
normt(t::Type, v::Int) = v isa t ? v : t(v)
normt(::Type{T}, v::T) where {T} = v
normt(::Type{NNIUNB}, v::AbstractString) = isnumeric(first(v)) ? XsdNonNegativeInt(v) : UNBOUNDED(v)
function normt(::Type{Union{NS_CONSTRAINT, Vector{Union{AnyURI, TNSLOCAL}}}}, v::AbstractString)
    try
        return NS_CONSTRAINT(v)
    catch
        try
            return TNSLOCAL(v)
        catch
            uris = split(v," ")
            return AnyURI.(uris)
        end
    end
end



scrub(w::AbstractString) = replace(chomp(strip(w)), ':'=>'_')
scrub(w) = w

""" Create a tuple containing all the data fields needed to create a concrete XSD type."""
function template(reader::EzXML.StreamReader, etype::Type, atts)
    ename = QName(reader.name)
    # @debug "template received atts $(atts) of type $(eltype(atts))"
    nm = (eltype(atts) == Any) ? getindex(atts,1) : getindex.(atts,1)
    typ = (eltype(atts) == Any) ? getindex(atts,2) : getindex.(atts,2)
    defv = (eltype(atts) == Any) ? getindex(atts,3) : getindex.(atts,3)
    # @debug "template parts:" nm typ defv
    vals = defv
    # Here we fetch the actual node attribute values as string (or function) 
    # and build a list that will be included in the result tuple
    if hasnodeattributes(reader)
        attr = nodeattributes(reader)
        # @debug "template: node attributes:" attr
        vals = get.(Ref(attr), nm, defv) #vals will be strings or a Function that handles (missing) required values
        # @debug "template: vals from attributes:" vals
    end
    # If a String value can be coerced into its final type, we do it here
    # @debug "vals typ" vals typ
    nvals = !isnothing(vals) ? map((v,t)->isnothing(v) ? nothing : normt(t,v), vals, typ) : []
    # @debug "template: nvals from vals:" nvals
    return length(nvals) > 0 ? (ename, etype, nvals...) : (ename, etype)
end

template(reader::EzXML.StreamReader, etype::Type{<:AbstractString}, s) = return (reader.name, etype, string(s))


# QUESTION : should we/how store anonymous simple types?
# simpletypedict = Dict{String,XsdNamedSimpleType}()

# complextypedict = Dict{QName,XsdComplexType}()

# TODO expand this
const basictypedict = Dict{String,Type{<:XsdAnySimpleType}}(
    "string" => XsdString, 
    "int" => XsdInt, 
    "token" => XsdToken, 
    "dateTime" => XsdDateTime, 
    "boolean" => XsdBoolean,
    "number" => XsdNumber,
    "anyURI" => AnyURI,
    "QName"  => QName,
    "NOTATION" => XsdNotation,
    "double" => XsdNumber,
    "decimal" => XsdNumber,
    "float" => XsdNumber,
    "hexBinary" => XsdHexBinary,
    "base64Binary" => XsdBase64Binary,
    "time" => XsdTime,
    "date" => XsdDate

    )

function startlist!(reader::EzXML.StreamReader)
    # @show nodename(reader)
end

function startunion!(reader::EzXML.StreamReader)
    # @show nodename(reader)
end

startanyattribute!(reader::EzXML.StreamReader) = push(reader,
    template(reader, XsdAnyAttribute, (
        ("id",                  ID,                             nothing),
        ("namespace",           Union{NS_CONSTRAINT,AnyURI},    "##any"),
        ("processContents",     PROC_CONTENTS,                  strict)
    )))

startattributegroup!(reader::EzXML.StreamReader) = push(reader,
    template(reader, XsdAttributeGroup, (
        ("id",            ID,       nothing),
        ("ref",           QName,    xsdrequired("ref", QName))
    )))

startxsdextension!(reader::EzXML.StreamReader) = push(reader,
    template(reader, XsdExtension, (
        ("base",    QName,      xsdrequired("base", QName)),
        ("id",      ID,         nothing)
    )))

"""Enumerations are always inside `restriction`"""
startenumeration!(reader::EzXML.StreamReader) = push(reader,
    template(reader, XsdEnumeration, (
        ("id", ID, nothing),
        ("value", Type{<:XsdSimpleType}, xsdrequired("value", Type{<:XsdSimpleType}))
    )))

startmaxlength!(reader::EzXML.StreamReader) = push(reader, 
    template(reader, XsdMaxLength, (
        ("fixed", Bool, false),
        ("id", ID, nothing),
        ("value", XsdNonNegativeInt, xsdrequired("value", XsdNonNegativeInt))
    )))

startrestriction!(reader::EzXML.StreamReader) = push(reader,
    template(reader, XsdRestriction, (
        ("base", QName, xsdrequired("base", QName)),
        ("id", ID, nothing)
    )))

function endrestriction!(reader::EzXML.StreamReader)
    @debug "endrestriction!"
    push(reader, stuffbelowlives!(reader, XsdRestriction))
    return nothing
end

startsimplecontent!(reader::EzXML.StreamReader) = push(reader,
    template(reader, XsdSimpleContent, (
        ("id",  ID,  nothing)
    )))


function endsimplecontent!(reader::EzXML.StreamReader)
    # @show nodename(reader)
end

function startcomplexcontent!(reader::EzXML.StreamReader)
    # @show nodename(reader)
end

function endcomplexcontent!(reader::EzXML.StreamReader)
    # @show nodename(reader)
end

function startextension!(reader::EzXML.StreamReader)
    # @show nodename(reader)
end

function endextension!(reader::EzXML.StreamReader)
    # @show nodename(reader)
end

function startattribute!(reader::EzXML.StreamReader)
    

# there may be no end attribute

#How to set the type member:
# If "type" appears as an attribute, use its QName value
# If no "type" attribute is present, this implies a child simpleType MUST be present:
# in endattribute!, instantiate the simpleType below and set it to the content member
# If type is absent and no child is found, error.
# type::Union{QName, Nothing}



    # set tns according to:
    """
    If form is present and its ·actual value· is qualified, 
    or if form is absent and the ·actual value· of attributeFormDefault 
    on the <schema> ancestor is qualified, then the 
    ·actual value· of the targetNamespace [attribute] 
    of the parent <schema> element information item, 
    or ·absent· if there is none, otherwise ·absent·.
    """
end

function endattribute!(reader::EzXML.StreamReader)
    # @show nodename(reader)
end

function startschema!(reader::EzXML.StreamReader)
    @debug "startschema! depth=" nodedepth(reader)

    # extract and store <schema> namespace
    prefix, ns = prefixns(reader)
    # @show prefix,ns
    !isnothing(prefix) && !isnothing(ns) && (DICT_DEFAULT_NS[prefix] = AnyURI(ns))

    nsd = copy(DICT_DEFAULT_NS)
    for (k, v) in pairs(nodeattributes(reader))
        pfx, nm = nameparts(k)
        if pfx == XLMNS_PFX
            nsd[nm] = AnyURI(v)
        end
    end

    # These basic XSD types must be reference-able by elements inside the schema, using the actual tns prefix.
    typed = Dict{QName, Union{Type{<:XsdAnySimpleType}, <:XsdSimpleType, XsdComplexType}}()
    for (k,v) in pairs(basictypedict)
        n = QName(prefix,k)
        @debug "startschema! Adding n=>v to the type dictionary" n v
        typed[n] = v
    end
    @debug "startschema! Initial type dict" typed

    atts = (
        ("targetNamespace",         AnyURI,                 nothing),
        ("ns",                      Dict{String,AnyURI},    nsd),
        ("attributeFormDefault",    QUAL,                   unqualified), # (qualified | unqualified) : unqualified
        ("blockDefault",            String,                 nothing), # (#all | List of (extension | restriction | substitution))  : ''
        ("elementFormDefault",      QUAL,                   unqualified), # (qualified | unqualified) : unqualified
        ("finalDefault",            String,                 nothing), # (#all | List of (extension | restriction | list | union))  : ''
        ("id",                      ID,                     nothing),
        ("version",                 XsdToken,               nothing),
        ("xml:lang",                XsdLanguage,            nothing)
    )

    t = template(reader, XsdSchema, atts)
    nm,typ,args = nm_typ_args(t)

    schema = XsdSchema(args...,
    XsdInclude[],
    XsdImport[],
    XsdRedefine[],
    XsdAnnotation[],
    XsdSimpleType[],
    XsdComplexType[],
    XsdGroup[],
    XsdAttributeGroup[],
    XsdElement[],
    XsdAttribute[],
    XsdNotation[],
    typed, #typed
    Dict{QName, XsdElement}() #elemd
    )
    push(reader, schema)
end

function endschema!(reader::EzXML.StreamReader)
    @debug "endschema!"
    # global schema

    stuff = popbelow(reader)
    # peek the schema template to fill it in`, but leave it on the stack
    local schema = peek()

    # inc::Union{Nothing,Vector{XsdInclude}}
    # imp::Union{Nothing,Vector{XsdImport}}
    # redefines::Union{Nothing,Vector{XsdRedefine}}
    # annotations::Union{Nothing,Vector{XsdAnnotation}}
    # simpleTypes::Vector{XsdSimpleType}
    # complexTypes::Vector{XsdComplexType}
    # groups::Union{Nothing,Vector{XsdGroup}}
    # attributeGroups::Union{Nothing, Vector{XsdAttributeGroup}}
    # elements::Vector{XsdElement}
    # attributes::Union{Nothing, Vector{XsdAttribute}}
    # notations::Union{Nothing, Vector{XsdNotation}}

    for s in stuff
        s isa XsdElement && push!(schema.elements, s)
        s isa XsdSimpleType && push!(schema.simpletypes, s)
        s isa XsdComplexType && push!(schema.complextypes, s)
        s isa XsdAttribute && push!(schema.attributes, s)
        s isa XsdInclude && push!(schema.inc, s)
        s isa XsdImport && push!(schema.imp, s)
        s isa XsdRedefine && push!(schema.redefines, s)
        s isa XsdAnnotation && push!(schema.annotations, s)
        s isa XsdGroup && push!(schema.groups, s)
        s isa XsdAttributeGroup && push!(schema.attributegroups, s)
        s isa XsdNotation && push!(schema.notations, s)
    end
    # Maybe here is where we process includes and imports ??????????
    return nothing
end

function startimport!(reader::EzXML.StreamReader)
    # @show reader.name
    # create XsdImport object
    # maybe need to download the referenced schema. Should we do that here?
end

function startinclude!(reader::EzXML.StreamReader)
    # @show reader.name
    # create XsdInclude object
    # maybe need to download the referenced schema. Should we do that here?
end

function startsimpletype!(reader::EzXML.StreamReader)
    @debug "startsimpletype!"
    atts = (
        ("name", String, nothing),
        ("id", ID, nothing),
        ("final", String, nothing)
    )
    push(reader, template(reader, XsdSimpleType, atts))
end

function endsimpletype!(reader::EzXML.StreamReader)
    @debug "endsimpletype!"
    push(reader, stuffbelowlives!(reader, XsdSimpleType))
end

function startsequence!(reader::EzXML.StreamReader)
    @debug "startsequence!"

    # id::Union{ID,Nothing}
    # maxoccurs::Union{XsdNonNegativeInt, UNBOUNDED}
    # minoccurs::XsdNonNegativeInt
    # annot::Union{XsdAnnotation,Nothing}
    # content::Vector{Any} #Maybe force a subtype of a Xsd... type?
    atts = (
        ("id",          ID,                                     nothing),
        ("maxOccurs",   Union{XsdNonNegativeInt,UNBOUNDED},     XsdNonNegativeInt(1)),
        ("minOccurs",   XsdNonNegativeInt,                      XsdNonNegativeInt(1))
    )
    push(reader, template(reader, XsdSequence, atts))
end

function endsequence!(reader::EzXML.StreamReader)
    @debug "endsequence!"
    push(reader, stuffbelowlives!(reader, XsdSequence))
    return nothing
end

# function endsequence!(reader::EzXML.StreamReader)
#     @debug "endsequence!"

#     #stuff can be a lot of different things, including uninstantiated things....
#     stuff = popbelow(reader)
#     t = pop() # this should be the XsdSequence
#     !isnothing(t) || error("we have landed hard on the ice and our knee hurts.")
#     nm,typ,args = nm_typ_args(t)
#     typ == XsdSequence || error("Ut-the fuck? You fuckin' wit me? Ut-the fuck is this? $(typ)")

#     annot = nothing
#     iobj = Any[]
#     for o in stuff
#         # @debug "endsequence! stuff is" o
#         if o isa XsdAnnotation
#             annot = o
#         elseif typeof(o) <: Union{XsdElement, XsdGroup, XsdChoice, XsdSequence, XsdAny}
#             push!(iobj, o)
#         else
#             # Some elements below may not be instantiated yet. Do that now
#             onm,otyp,oargs = nm_typ_args(o)
#             # @debug "endsequence! Got interesting stuff:" onm otyp oargs
#             r = _build(reader, otyp, oargs)
#             # @debug "endsequence! built:" r

#             # This stuff may include element templates that lack annotation, type and content info. How to include that in a general manner?
#             push!(iobj, r)
#             # error("You're messing with the wrong sheriff, son. What's a '$(o)'?")
#         end
#     end
#     push(reader, XsdSequence(args..., annot, iobj))
#     return nothing
# end

function startcomplextype!(reader::EzXML.StreamReader)
    @debug "startcomplextype!"

    #__________ATTRIBUTES
    # tns::Union{AnyURI,Nothing} #1
    # abstract::XsdBoolean
    # block::Union{Nothing, XsdString}
    # final::Union{Nothing, XsdString}
    # id::Union{ID, Nothing} #5
    # mixed::XsdBoolean
    # name::Union{XsdString,Nothing} #7
    #__________CONTENT
    # annot::Union{XsdAnnotation, Nothing}
    # content::
    #Need to get the tns
    local schema = peekup(reader, XsdSchema)

    atts = (
    ("tns",         AnyURI,         schema.targetNamespace),
    ("abstract",    XsdBoolean,     XsdBoolean(false)),
    ("block",       XsdString,      nothing),
    ("final",       XsdString,      nothing),
    ("id",          ID,             nothing),
    ("mixed",       XsdBoolean,     XsdBoolean(false)),
    ("name",        NCName,         nothing),
    )

    push(reader, template(reader, XsdComplexType, atts))

end

function endcomplextype!(reader::EzXML.StreamReader)
    @debug "endcomplextype!"
    push(reader, stuffbelowlives!(reader, XsdComplexType))
    return nothing
end

"** This might be the template for an end! function"
# function endcomplextype!(reader::EzXML.StreamReader)
#     @debug "endcomplextype!"

#     schema = peekup(reader, XsdSchema)
#     pfx = tnsprefix(schema)
#     tns = schema.targetNamespace

#     stuff = popbelow(reader)
#     # @debug "endcomplextype! stuff" stuff

#     nm,typ,args = nm_typ_args(pop())
#     # @debug "endcomplextype! nm,typ,args" nm typ args
#     typ == XsdComplexType || error("Won't be fooled again.")
        
#     annot = nothing
#     iobj = []
#     for s in stuff
#         if s isa XsdAnnotation
#             annot = s
#         else
#             push!(iobj,s)
#         end
#     end
#     # @debug "endcomplextype! Creating a $typ with " args iobj
#     o = typ(args..., annot, iobj)
#     name = args[7]
#     if !isnothing(name)
#         # register this as a named type with the schema
#         name = QName(pfx, name)
#         schema.typed[name] = typeof(o)
#     end        

#     push(reader, o)
# end


function startelement!(reader::EzXML.StreamReader)
    @debug "startelement!"

    # We need to know whether this is a top-level element (name is required)
    # or a child of <complexType> or <group>. Look up into the stack to decide.

    t = peekup(reader, XsdComplexType)
    t = isnothing(t) ? peekup(reader, XsdGroup) : t
    # @debug "startelement! Found XsdComplexType on the stack:" t

    if !isnothing(t) # we are inside a complex type definition. Use reduced attribute set
        @debug "We are trapped inside a complex type definition! Use your powers!"
        # xs:schema
        # XsdSchema
        # ("targetNamespace", AnyURI, nothing),
        # ("ns", Dict{String,AnyURI}, nsd),
        # ("attributeFormDefault", QUAL, unqualified), # (qualified | unqualified) : unqualified
        # ("blockDefault", String, nothing), # (#all | List of (extension | restriction | substitution))  : ''
        # ("elementFormDefault", QUAL, unqualified), # (qualified | unqualified) : unqualified
        # ("finalDefault", String, nothing), # (#all | List of (extension | restriction | list | union))  : ''
        # ("id", ID, nothing),
        # ("version", XsdToken, nothing),
        # ("lang", XsdLanguage, nothing)

        # Use more complicated rules to set the Target Namespace

        tns = nothing
        schema = peekup(reader, XsdSchema)
        # @debug "startelement! Found XsdSchema on the stack:" schema
        # elementFormDefault
        efd = schema.elementformdefault #

        attr = nodeattributes(reader)
        if haskey(attr, "form") && QUAL(attr["form"]) == qualified || efd == qualified
            tns = schema.targetNamespace
        end

        atts = (
            ("tns",         AnyURI,     tns),
            ("abstract",    XsdBoolean, XsdBoolean(false)),
            ("block",       XsdString,  nothing),
            ("default",     XsdString,  nothing),
            ("final",       XsdString,  nothing),
            ("fixed",       XsdString,  nothing),
            ("form",        QUAL,       nothing),
            ("id",          XsdString,  nothing),
            ("maxOccurs",   NNIUNB,             XsdNonNegativeInt(1)),
            ("minOccurs",   XsdNonNegativeInt,  XsdNonNegativeInt(1)),
            ("name",        NCName,     nothing),
            ("nillable",    XsdBoolean, XsdBoolean(false)),
            ("ref",         QName,      nothing),
            ("substitutionGroup",XsdString,nothing),
            ("type",        QName,      nothing),
        )

    else
        @debug "startelement! We are a top-level element."
        # We are a top-level element; use the schema's Target Namespace
        local schema = peekup(reader, XsdSchema)
        # @debug "startelement! Found XsdSchema on the stack:" schema
        tns = schema.targetNamespace

        atts = (
            ("tns",         AnyURI,                     tns),#1
            ("abstract",    XsdBoolean,                 XsdBoolean(false)),
            ("block",       XsdString,                  nothing),
            ("default",     XsdString,                  nothing),
            ("final",       XsdString,                  nothing),#5
            ("fixed",       XsdString,                  nothing),
            ("form",        XsdString,                  nothing),
            ("id",          XsdString,                  nothing),
            ("maxOccurs",   NNIUNB,                     XsdNonNegativeInt(1)),
            ("minOccurs",   XsdNonNegativeInt,          XsdNonNegativeInt(1)),#10
            ("name",        NCName,                     xsdrequired("name",NCName)),
            ("nillable",    XsdBoolean,                 XsdBoolean(false)),
            ("ref",         QName,                      nothing),
            ("substitutionGroup",XsdString,             nothing),
            ("type",        QName,                      nothing),#15
        )
    end
    push(reader, template(reader, XsdElement, atts))
    return nothing
end

function endelement!(reader::EzXML.StreamReader)
    @debug "endelement!"
    push(reader, stuffbelowlives!(reader, XsdElement))
end
    # -- attributes above --
    # -- content below --
    # annotation::Union{Nothing, XsdAnnotation}
    # t::Type{Union{XsdSimpleType,XsdComplexType}} # this references either the type dereferenced from 'type' or the enclosed type defn in 'content'
    # content::Union{XsdSimpleType, XsdComplexType, Nothing}

    # We always need schema
    # schema = peekup(reader, XsdSchema)
    # pfx = tnsprefix(schema)

    # Extract contents below
    # stuff = popbelow(reader)
    # @debug "popped stuff:" stuff

    # Extract the attributes needed to make an Element
    # nm,typ,args = nm_typ_args(pop())
    # @debug "endelement nm,typ,args" nm typ args
    # nm == QName(reader.name) || error("Found non-element tuple with name: $nm")
    # name = QName(pfx, args[11])
    # @debug "endelement! created name" name

    # Need the parent element to know whether we are top-level or inside a complexType
    # parent = peekup(reader, XsdComplexType)
    # parent = isnothing(parent) ? peekup(reader, XsdGroup) : parent

    # The `type` and `substitutionGroup` attributes may be used to determine type if no children elements are present
    # typattr = args[15]
    # substgroupattr = args[14]

    # annot = nothing
    # typr = nothing # this is `t` in the component
    # content = nothing
    # e = nothing # the result element

    # @debug "endelement! Gentlemen, we have calculated typr as :" typr

    # Get the value of the `type` attribute out of the tuple:
# ----- attributes
1   # tns::Union{AnyURI,Nothing} #1
    # abstract::XsdBoolean
    # block::Union{Nothing, XsdString}
    # default::Union{Nothing, XsdString}
5   # final::Union{Nothing, XsdString} #5
    # fixed::Union{Nothing,XsdString}
    # form::Union{Nothing, QUAL}
    # id::Union{Nothing,ID}
    # maxoccurs::NNIUNB
10    # minoccurs::XsdNonNegativeInt # 10
    # name::Union{XsdString,Nothing} #password
    # nillable::XsdBoolean #false
13    # ref::Union{Nothing, QName}
    # substitutionGroup::Union{Nothing,QName}
15    # type::Union{QName, Nothing} # 15 if nothing, type is the enclosed content
# ----- content
    # annot::Union{Nothing, XsdAnnotation}
    # t::Union{T,XsdComplexType} where {T<:XsdSimpleType} # this references either the type dereferenced from 'type' or the enclosed type defn in 'content'
    # content::Union{T, XsdComplexType, Nothing} where {T<:XsdSimpleType}

    # @debug "Creating $typ with:" args annot typr content
#     e = typ(args..., annot, typr, content)
#     @debug "I created this beautiful thing for you, dear. I call it 'e':. And now I will push it onto the stack for you to retrieve later, after my passing." e
#     push(reader, e)
#     # store the element reference in the schema's element dictionary
#     pfx = tnsprefix(schema)
#     ename = QName(pfx, e.name)
#     @debug "Storing element in elemd as " ename
#     schema.elemd[ename] = e
#     return nothing
# end

function startchoice!(reader::EzXML.StreamReader)
    # ------ ATTRIBS -----
    # id::IDN
    # maxoccurs::NNIUNB
    # minoccurs::XsdNonNegativeInt
    #----- CONTENT -----
    # annot::Union{XsdAnnotation,Nothing}
    # content::Any
    push(reader, template(reader, XsdChoice, (
        ("id", ID, nothing),
        ("maxOccurs", NNIUNB, XsdNonNegativeInt(1)),
        ("minOccurs", ZEROONE, ZEROONE(1))
    )))
end

function endchoice!(reader::EzXML.StreamReader)
    @debug "endchoice!"

    schema = peekup(reader, XsdSchema)
    pfx = tnsprefix(schema)
    tns = schema.targetNamespace

    stuff = popbelow(reader)
    @debug "endchoice! stuff" stuff

    nm,typ,args = nm_typ_args(pop())
    @debug "endchoice! nm,typ,args" nm typ args
    typ == XsdChoice || error("Won't be fooled again. $typ ????")
        
    annot = nothing
    iobj = []
    for s in stuff
        if s isa XsdAnnotation
            annot = s
        else
            push!(iobj,s)
        end
    end
    @debug "endchoice! Creating a $typ with:" args annot iobj
    o = typ(args..., annot, iobj)
    push(reader, o)
end

"""

    startany!

# id::IDN
# maxoccurs::NNIUNB
# minoccurs::XsdNonNegativeInt
# namespace::Union{NS_CONSTRAINT, Vector{Union{AnyURI, TNSLOCAL}}} #default = 'nsany'
# processContents::PROC_CONTENTS #required; default = strict
# annot::Union{XsdAnnotation,Nothing}
"""
startany!(reader::EzXML.StreamReader) = push(reader,
    template(reader, XsdAny, (
        ("id",              ID,                                                     nothing),
        ("maxOccurs",       NNIUNB,                                                 XsdNonNegativeInt(1)),
        ("minOccurs",       XsdNonNegativeInt,                                      XsdNonNegativeInt(1)),
        ("namespace",       Union{NS_CONSTRAINT,Vector{Union{AnyURI,TNSLOCAL}}},    nsany),
        ("processContents", PROC_CONTENTS,                                          strict)
    )))

function endany!(reader::EzXML.StreamReader)
    @debug "endany!"
    push(reader, stuffbelowlives!(reader, XsdAny))
    return nothing
end

startannotation!(reader::EzXML.StreamReader) = push(reader,
    template(reader, XsdAnnotation, (
        ("id", ID, nothing),
    )))

function endannotation!(reader::EzXML.StreamReader)
    @debug "endannotation!"
    push(reader, stuffbelowlives!(reader, XsdAnnotation))
    return nothing
end

function startdocumentation!(reader::EzXML.StreamReader)
    @debug "startdocumentation!"
    atts = (
        ("source", AnyURI, nothing),
        ("xml_lang", XsdLanguage, nothing),
    )
    push(reader, template(reader, XsdDocumentation, atts))
    return nothing
end

function enddocumentation!(reader::EzXML.StreamReader)
    @debug "enddocumentation!"
    push(reader, stuffbelowlives!(reader, XsdDocumentation))
    return nothing
end

function text!(reader::EzXML.StreamReader)
    @debug "text!"
    s = (strip ∘ chomp)(nodecontent(reader))
    @debug "text! s:" s
    length(s) > 0 ? push(reader, template(reader, typeof(s), s)) : nothing
end

function cdata!(reader::EzXML.StreamReader)
    @debug "cdata!"
    s = nodecontent(reader)
    @debug "cdata! s:" s
    push(reader,s)
end

function dispatch(reader::EzXML.StreamReader, fname)
    mod = @__MODULE__
    if nodetype(reader) == EzXML.READER_ELEMENT
        ns = namespace(reader)
        haskey(nsmoddict, ns) || error("No module for $(ns)")
        mod = nsmoddict[ns]
    end
    @debug "dispatching to" mod fname
    @eval $mod, $fname($reader)
end

function spindry!(reader::EzXML.StreamReader)
    global stack = Any[]

    for ndtype in reader
        t = nameparts(nodename(reader))
        _, nm = t
        @debug "spindry! node name type" nm reader.type
    
        if reader.type == EzXML.READER_ELEMENT
            # @show fname
            # we may to have a backup plan for a default module if nothing is found in nsmoddict
            #TODO put the following into a dispatch function
            dispatch(reader, Symbol("start", lowercase(nm), "!"))
        elseif reader.type == EzXML.READER_END_ELEMENT
            dispatch(reader, Symbol("end", lowercase(nm), "!"))
            keepbrother!(reader)
        elseif reader.type == EzXML.READER_CDATA
            dispatch(reader, Symbol("cdata!"))
        elseif reader.type == EzXML.READER_TEXT
            #name is '#text' so don't use that
            dispatch(reader, Symbol("text!"))
        end
    end
    pop()
end
# end # module XSDJ

# using SOAP4Julia
# include("SOAP4Julia.jl")

# regns("http://schemas.xmlsoap.org/wsdl/" => SOAP4Julia,
# "http://schemas.xmlsoap.org/wsdl/soap/" => SOAP4Julia)

