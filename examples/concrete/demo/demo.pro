define pfx1 = 1.0.0.0/24
define pfx2 = 1.0.1.0/24
define pfx3 = 2.0.0.0/24
define pfx4 = 2.0.1.0/24
  
define main = basic_routing & locality & notransit & block_internal


define basic_routing = {
	pfx1 => end(A), 
	pfx2 => end(B),
	pfx3 => end(E),
	pfx4 => end(F),
	true => end(out) & exit(IDAT >> CORE),
}

# Don't route private address space from external peers
define private = 
  10.0.0.0/[8..32] or 
  172.16.0.0/[12..32] or 
  192.168.0.0/[16..32] or 
  169.254.0.0/[16..32] or 
  240.0.0.0/[4..32] or 
  192.0.0.0/[24..32]

define block_internal = {
	private => not enter(out)
}

# Ensure pfx1 and pfx2 do not leak outside the data center
define local = pfx1 or pfx2

define locality = {
	local => internal
}

# Prevent transit traffic between peers
define peer = {CORE, IDAT}

define transit(X,Y) = enter({X,Y}) & exit({X,Y})

define notransit = {
	true => not transit(peer, peer)
}
