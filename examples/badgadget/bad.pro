
define main = {
	1.1.1.1 => (path(A,B,D) >> path(A,D)) + (path(B,C,D) >> path(B,D)) + (path(C,A,D) >> path(C,D)),
	true => drop
}