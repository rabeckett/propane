define LP1 = 1.0.0.0/24
define LP2 = 1.0.1.0/24
define GP1 = 2.0.0.0/24
define GP2 = 2.0.1.0/24

define Peer = {Peer1, Peer2}

define destination = {
	GP1 => end(A), 
	GP2 => end(B),
	LP1 => end(E),
	LP2 => end(F),
	true => exit(Peer1) >> exit(Peer2),
}

define locality = {
	LP1 or LP2 => internal
}

define transit(X,Y) = enter(X) and exit(Y)

define notransit = {
	true => not transit(Peer,Peer) 
}

define main = destination and locality and notransit