define PG1 = 1.1.1.0/32
define PG2 = 1.1.1.1/32
define PG = PG1 or PG2

define PL1 = 2.2.2.0/32
define PL2 = 2.2.2.1/32
define PL = PL1 or PL2

define PAGG  = 1.1.1.1/31

define routing = {
    PG => any,
    PL => always(in)
}

define ownership = {
	PG1 => end(A),
    PG2 => end(B),
    PL1 => end(E),
    PL2 => end(F)
}

define main = routing and ownership

control {
    aggregate(PAGG, in -> out)
}