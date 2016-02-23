
define PG1 = 1.0.0.0/24
define PG2 = 1.0.1.0/24
define PL1 = 2.0.0.0/24
define PL2 = 2.0.1.0/24


define transit(X) = 
	enter(X) and exit(X)

define NoTransit = {
	true => not transit(Back1 or Back2)
}

define Ownership = {
	PL1 => end(A),
	PL2 => end(B),
	PG1 => end(E),
	PG2 => end(F)
}

define Routing = {
	PL1 or PL2 => always(in),
	PG1 or PG2 => any,
	true => exit(Back1) >> exit(Back2)
}

define main = no_transit and routing and ownership
