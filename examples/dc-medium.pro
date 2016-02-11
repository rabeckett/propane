define PG1 = 1.0.0.0/24
define PG2 = 1.0.1.0/24
define PL1 = 2.0.0.0/24
define PL2 = 2.0.1.1/24
define PAGG  = 1.0.0.0/16

define routing = {
	PL1 or PL2 => always(in),
	PG1 or PG2 => any,
	true => exit(out)
}

define ownership = {
	PG1 => end(A),
	PG2 => end(B),
	PL1 => end(E),
	PL2 => end(F),
	true => any
}


define main = ownership and routing

control {
	aggregate(PAGG, in -> out)
}