# Give meaningful names to (groups of) ASes
define Princeton = as500
define Peer = {Sprint, Level3}

# Transit traffic between X and Y 
# both enters and leaves through X or Y
define transit(X,Y) = enter(X+Y) & exit(X+Y)

# Always prevent transit between Peers
define notransit = { 
	true => not transit(Peer,Peer) 
}

# Prefer to leave through R1 over R2 over a Peer
define preferences = { 
	true => exit(R1 >> R2 >> Peer) 
}

# Ensure traffic for the prefix 172.4.1.0/24 ends up at Princeton
# This can prevent certain types of route hijacks
define ownership = {
	172.4.1.0/24 => end(Princeton)
}

define main = preferences & ownership & notransit
