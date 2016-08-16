define Routing = {
    true => any
}

define Ownership = {
    10.0.0.0/32 => end(A),
    10.0.0.1/32	=> end(B),
}

define main = Routing and Ownership

control {
    aggregate(10.0.0.0/31, in -> out)
}