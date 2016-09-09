define P1 = 1.0.0.0/24
define P2 = 2.0.0.0/24
define P3 = 3.0.0.0/24
define P4 = 4.0.0.0/24

define destination = {
  P1 => end(A),
  P2 => end(B),
  P3 => end(C),
  P4 => end(D),
  true => end(out) and (exit(Peer1) >> exit(Peer2) >> exit(DC2))
}

define transit(X,Y) = (enter(X) and exit(Y)) or (enter(Y) and exit(X))

define notransit = {
  true => not transit(Peer1, Peer2)
}

define main = destination and notransit