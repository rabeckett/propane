define Peer = {Sprint, Level3}

define transit(X,Y) = (enter(X) & exit(Y)) + (enter(Y) & exit(X))

define notransit = { 
	true => not transit(Peer,Peer) 
}

define preferences = { 
	true => exit(R1) >> exit(R2) >> exit(Peer) 
}

define ownership = {
	172.4.1.0/24 => any
}

define main = preferences & ownership & notransit
