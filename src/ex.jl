using EzXML
using XSDJ


f = "resources/sonos.xsd"
reader = open(EzXML.StreamReader, f)
schema = spindry!(reader);
close(reader)
@show schema

