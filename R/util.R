# helper functions go here

urn = function(n = 1){
    replicate(n, uriref(uuid::UUIDgenerate(), base = "uuid:urn:"))
}
