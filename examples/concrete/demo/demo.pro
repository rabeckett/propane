define pfx1 = 1.0.0.0/24
define pfx2 = 1.0.1.0/24
define pfx3 = 2.0.0.0/24
define pfx4 = 2.0.1.0/24

define local = pfx1 or pfx2
define peer = {CORE, IDAT}

define transit(X,Y) = enter({X,Y}) & exit({X,Y})

define notransit = {
	true => not transit(peer, peer)
}

define basic_routing = {
	pfx1 => end(A), 
	pfx2 => end(B),
	pfx3 => end(E),
	pfx4 => end(F),
	true => end(out) & exit(IDAT >> CORE),
}

define locality = {
	local => internal
}

define main = basic_routing & locality & notransit