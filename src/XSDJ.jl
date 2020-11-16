module XSDJ

using EzXML
using Dates
using URIParser
using Logging
using TimeZones

const XSDNS = "http://www.w3.org/2001/XMLSchema"
const XMLNS = "http://www.w3.org/XML/1998/namespace"

include("datatypes.jl")
include("utils.jl")
include("components.jl")
include("build.jl")
include("xsd.jl")

export spindry!

end # module
