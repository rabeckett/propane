define Peer = {Sprint, Level3}

define transit(X,Y) = (enter(X) and exit(Y)) or (enter(Y) and exit(X))

define notransit = { 
	true => not transit(Peer,Peer) 
}

define preferences = { 
	true => exit(R1) >> exit(R2) >> exit(Peer) 
}

define ownership = {
	16.4.0.0/16 => end(Princeton),
	true 		=> drop
}

define main = preferences and ownership and notransit
