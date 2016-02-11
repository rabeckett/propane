﻿control {
    aggregate(10.0.0.0/31, A -> X),
	tag(8075:420, 1.2.0.0/16, B -> X),
	maxroutes(100, in -> out),
	longest_path(100),
}

task Routing {
    10.10.0.0/32 => through(M) >> avoid(N) >> any,
    10.10.0.1/32 => any,
    true 		 => any,
}

task Ownership {
    10.10.0.0/32 => end(A),
    10.10.0.1/32 => end(B),
    true 		 => end(A),
}

Routing and Ownership