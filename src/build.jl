
function _build(reader, T::Type{XsdDocumentation}, parent::Tuple, args, content)
    @debug "_build(XsdDocumentation)" args content
    src,lang = args
    T(args...,content)
end

function _build(reader, T::Type{XsdAnnotation}, parent::Tuple, args, content)
    @debug "_build(XsdAnnotation)" args, content
    T(args..., content)
end

function _build(reader, T::Type{XsdMaxLength}, parent::Tuple, args, _)
    # pnm, ptyp, pargs = nm_typ_args(parent)
    # @debug "_build(XsdMaxLength) from parent:" pnm ptyp pargs
    # basenm = pargs[1]
    # @debug "_build(XsdMaxLength) basenm" basenm
    # local schema = sch(reader)
    # pfx = prefixns(reader)
    # basetyp = schema.typed[basenm]
    # @debug "_build(XsdMaxLength) base type for restriction:" basetyp
    
    @debug "_build(XsdMaxLength)" args
    T(args...)
end


"""
    _build(XsdAny)

# --- ATTRIBS -----
# id::IDN
# maxoccurs::NNIUNB
# minoccurs::XsdNonNegativeInt
# namespace::Union{NS_CONSTRAINT, Vector{Union{AnyURI, TNSLOCAL}}} #default = 'nsany'
# processContents::PROC_CONTENTS #required; default = strict
----- CONTENT ----
# annot::Union{XsdAnnotation,Nothing}
"""
function _build(reader, T::Type{XsdAny}, parent::Tuple, args, content)
    @debug "_build(XsdAny)" args content
    annot, c = wheatchaff(content)
    T(args..., annot)
end

_build(reader, T::Type{<:AbstractString}, parent::Tuple, args, _) = string(args...)

#TODO Consider moving to named tuples for atts in start...! methods. Not sure it is worth it,
function _build(reader::EzXML.StreamReader, T::Type{XsdElement}, parent::Tuple, args::Tuple, content)
    @debug "build(XsdElement)" parent args content
    # args are attributes
    # assume `type` references an actual type, searchable in schema
    local schema = sch(reader)
    pfx = tnsprefix(schema)

    typattr = args[15]
    substgroupattr = args[14]

    # e = nothing
    annot, c = wheatchaff(content)
    c = isnothing(c) ? nothing : first(c)

    name = args[11] #THIS SUCKS (integer indexing)
    @debug "_build(XsdElement) name $(typeof(name))" name typeof(name)


    refattr = args[13]
    # This element might simply refer to another element.
    # Look up the element that this points to
    @debug "build refattr" refattr
    if !isnothing(refattr)
        haskey(schema.elemd, refattr) || error("No type found for $(refattr)")
        elemr = get(schema.elemd, refattr, nothing)
        @debug "build - Found element for $(refattr)" elemr
        !isnothing(elemr) || error("No element found for ref $(refattr)")
        @debug "build Found element reference. Running with it... elemr=" elemr
        maxoccurs,minoccurs = args[9],args[10]

        """https://www.w3.org/TR/xmlschema-1/#cElement_Declarations"""
        e = XsdElement(elemr, minoccurs, maxoccurs) # USE THE ACTUAL VALUES OF `minOccurs` and `maxOccurs` from the local definition
        # don't forget to register this element!
        if !isnothing(name)
            qn = QName(pfx, name)
            @debug "_build(XsdElement) registering with" qn
            schema.elemd[qn] = e
        end
    
        return e
    end
    # no ref attribute
    # might have a `type` attribute  
    typr = c
    @debug "build typattr:" typattr
    if (!isnothing(typattr))
        typr = haskey(schema.typed, typattr) ? schema.typed[typattr] : error("can't find entry for key $typattr in schema dict $(schema.typed)")
        @debug "build found type:" typr
    end
    @debug "_build(XsdElement) building with " T args annot typr c
    o = T(args..., annot, typr, c)
    @debug "_build(XsdElement) built" o
    # register this element
    if !isnothing(name)
        qn = QName(pfx, name)
        @debug "_build(XsdElement) registering with" qn
        schema.elemd[qn] = o
    end
    o
end

function _build(reader, T::Type{XsdRestriction}, parent::Tuple, args, content)
    @debug "_build(XsdRestriction)" args content

    # # turn stuff into instantiated types
    # annot = nothing
    # iobj = Any[]
    # for s in stuff
    #     # @debug "endrestriction! Raw stuff:" s

    #     snm,styp,sother = nm_typ_args(s)
    #     # @debug "endrestriction! Contained stuff:" snm styp sother
    #     # @debug "endrestriction! $(styp) has value field type:" fieldtype(styp, :value)
    #     if styp isa XsdAnnotation
    #         annot = sother
    #     else
    #     # THIS LINE SHOWS THE POWER OF JULIA'S TYPE SYSTEM
    #     o = isconcretetype(fieldtype(styp, :value)) ? styp(sother...) : styp{basetyp}(sother...)
    #     # o = t2obj(s)
    #     # @debug "endrestriction! Created this to go back on stack:" o
    #     push!(iobj, o)
    #     end
    # end
    annot, c = wheatchaff(content)
    T(args..., annot, c)
end

function _build(reader, T::Type{XsdEnumeration}, parent::Tuple, args, content)
    # need base from parent XsdRestriction
    @debug "_build(XsdEnumeration)" reader T parent args content
    pnm, ptyp, pargs = nm_typ_args(parent)

    @debug "_build(XsdEnumeration) from parent" pnm ptyp pargs
    local schema = sch(reader)

    if ptyp == XsdRestriction
        base = pargs[1]
    else
        r = peekup(reader, XsdRestriction)
        rnm,rtyp,rargs = nm_typ_args(r)
        base = rargs[1]
    end
    @debug "_build(XsdEnumeration) base" base
    innert = schema.typed[base]
    @debug "_build(XsdEnumeration) innert" innert

    annot, c = wheatchaff(content)
    return T{innert}(args..., annot, c)
end

function wheatchaff(content)
    isnothing(content) && return (nothing,nothing)

    a = filter(v->isa(v, XsdAnnotation), content)
    a = isempty(a) ? nothing : first(a)

    c = filter(v->!isa(v, XsdAnnotation), content)
    c = isempty(c) ? nothing : c

    (a,c)
end

function _build(reader, T::Type{XsdSequence}, parent::Tuple, args::Tuple, content)
    @debug "_build(XsdSequence)" parent args content
    annot, c = wheatchaff(content)
    @debug "_build(XsdSequence) creating XsdSequence with" args annot c
    T(args..., annot, c) #Only 0|1 annotation
end

function _build(reader, T::Type{XsdSimpleType}, parent::Tuple, args::Tuple, content)
    local schema = sch(reader)
    tnspfx = tnsprefix(schema)

    annot, variety = wheatchaff(content)
    variety = first(variety)
    @debug "_build(XsdSimpleType) variety:" variety

    v = nothing
    name,id,final = args
    # @debug "endsimpletype! Creating type from:" typ attargs annot variety
    # if variety isa XsdRestriction
    if !isnothing(name)
        v = XsdNamedSimpleType(name, id, final, annot, variety)
        schema.typed[QName(tnspfx,name)] = v
    else
        v = XsdAnonSimpleType(id, final, annot, variety)
    end
    return v
end

"""
See [https://www.w3.org/TR/xmlschema-1/#Complex_Type_Definitions]
"""
function _build(reader, T::Type{XsdComplexType}, parent::Tuple, args::Tuple, content)
    @debug "_build(XsdComplexType)" parent args content

    # atts = (
    #    1 ("tns",         AnyURI,         schema.targetNamespace),
    #    2 ("abstract",    XsdBoolean,     XsdBoolean(false)),
    #    3 ("block",       XsdString,      nothing),
    #    4 ("final",       XsdString,      nothing),
    #    5 ("id",          ID,             nothing),
    #    6 ("mixed",       XsdBoolean,     XsdBoolean(false)),
    #    7 ("name",        NCName,         nothing),
    #     )
    
    local schema = sch(reader)

    ebv = nothing #effective `block` value
    block = args[3]
    bdef = schema.blockdefault
    ebv = !isnothing(block) ? block : !isnothing(bdef) ? bdef : ""
    prohibitedsubs = ebv == "#all" ? ("restriction","extension") : (ebv == "") ? () : ebv

    efv = nothing #effective `final` value
    final = args[4]
    fdef = schema.finaldefault
    efv = !isnothing(final) ? final : !isnothing(fdef) ? fdef : ""
    finalsubs = efv == "#all" ? ("restriction","extension") : (efv == "") ? () : efv

    annot, c = wheatchaff(content)
    @debug "_build(XsdComplexType) creating XsdComplexType with" args annot c

    o = T(args..., prohibitedsubs, finalsubs, annot, c) #Only 0|1 annotation

    name = args[7]
    @debug "_build(XsdComplex) name" name
    if !isnothing(name)
        pfx = tnsprefix(schema)
         # register this as a named type with the schema
        name = QName(pfx, name)
        schema.typed[name] = o
    end        
    o
end
