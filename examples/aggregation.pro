control {
    aggregate(10.0.0.0/31, in -> out)
}

task Routing {
    true => any
}

task Ownership {
    10.0.0.0/32 => end(A),
    10.0.0.1/32	=> end(B),
}

Routing and Ownership