
define PG1 = 1.0.0.0/24
define PG2 = 1.0.1.0/24
define PL1 = 2.0.0.0/24
define _PL2 = 2.0.1.0/24
define _PAGG = 1.0.0.0/16

define main = {
	PG1 => originate(A) and internal,
	PG2 => originate(B) and internal,
	PL1 => originate(E) and internal,
	true => exit(out)
}

/*
control {
	aggregate(PAGG, in -> out)
}
*/
