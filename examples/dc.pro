
define PG1 = 1.0.0.0/24
define PG2 = 1.0.1.0/24
define PL1 = 2.0.0.0/24
define PL2 = 2.0.1.0/24

define PAGG = 1.0.0.0/16

define main = {
	PG1 => end(A) and internal,
	PG2 => end(B) and internal,
	PL1 => end(E) and internal,
	PL2 => end(F) and internal,
	true => end(out)
}

control {
	aggregate(PAGG, in -> out)
}

