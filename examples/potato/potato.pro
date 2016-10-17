define main = {
     172.0.0.0/24 => (through(A) >> through(B)) & end(Peer),
     1.2.3.4/24 => (through(A) >> through(B)) & end(E),
     true => drop
}