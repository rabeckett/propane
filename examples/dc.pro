/*
define PG1 = 1.0.0.0/24
define PG2 = 1.0.1.0/24
define PL1 = 2.0.0.0/24
define PL2 = 2.0.1.0/24
define _PAGG = 1.0.0.0/16
*/

define main = {
	true => originate(A) and always(in)
}

/*
control {
	aggregate(PAGG, in -> out)
}
*/
