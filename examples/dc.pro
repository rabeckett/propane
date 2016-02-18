define PG1 = 1.0.0.0/24
define PG2 = 1.0.1.0/24
define PL1 = 2.0.0.0/24
define PL2 = 2.0.1.0/24

/* TODO: bug where this is marked as unused even
   when it is used in the control constraints */
define PAGG = 1.0.0.0/16

define ownership = {
	PG1 => end(A),
	PG2 => end(B),
	PL1 => end(E),
	PL2 => end(F),
}

define routing = {
	PL1 or PL2 => always(in),
}

define main = ownership and routing

control {
	aggregate(PAGG, in -> out)
}
