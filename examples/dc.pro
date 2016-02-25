define PL1 = 1.0.0.0/24
define PL2 = 1.0.1.0/24
define PG1 = 2.0.0.0/24
define PG2 = 2.0.1.0/24

define transit(X) =  enter(X) and exit(X)

define no_transit = {
	true => not transit(Back1 or Back2)
}

define ownership = {
	PG1 => end(A),
	PG2 => end(B),
	PL1 => end(E),
	PL2 => end(F)
}

define routing = {
	PG1 or PG2 => any,
	PL1 or PL2 => always(in),
	true => exit(Back1) >> exit(Back2)
}

define main = { 
	true => exit(Back1) >> exit(Back2)
} /* no_transit and ownership and routing */


control {
	aggregate(1.0.0.0/16, in -> out)
}