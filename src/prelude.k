define sqrt(numeric v) returning numeric {
	_caos { SETV $_return(w) SQRT $v(r); }
}
