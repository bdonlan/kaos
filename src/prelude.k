define sqrt(numeric v) returning numeric {
	_caos { SETV $_return(w) SQRT $v(r); }
}

define iterator(agent) enum(numeric family, numeric genus, numeric species) {
  agent targ;
  _caos {
## XXX: is this use of .loop correct?
    .loop {
    .targ > $targ {
      ENUM $family(r) $genus(r) $species(r);
    };
    .kaos { _yield(targ); };
    };
    NEXT;
  };
}
define unid(agent a) returning numeric {
  _caos { .targ < $a { SETV $_return(w) UNID; }; };
}

define iterator() reps(numeric n) {
  _caos {
    REPS $n(r);
    .loop { .kaos { _yield(); }; };
    REPE;
  };
}

define norn() returning agent {
  _caos { SETA $_return(w) NORN; };
}

define set norn(agent norn) {
  _caos { NORN $norn(r); }
}

define newsimple(numeric family, numeric genus, numeric species, string spritefile, numeric firstframe, numeric noframes, numeric plane) returning agent {
        _caos {
                .targ > $_return {
                        NEW: SIMP $family(r) $genus(r) $species(r) $spritefile(r) $firstframe(r) $noframes(r) $plane(r);
                };
        }
}

# need better folder :(
define attributes(agent a) returning numeric {
        _caos {
                .targ < $a {
                        SETV $_return(w) ATTR;
                };
        };
}

define set attributes(numeric attr, agent a) {
        _caos {
                .targ < $a {
                        ATTR $attr(r);
                };
        };
}

define move(agent a, numeric x, numeric y) {
        _caos {
                .targ < $a {
                        MVTO $x(r) $y(r);
                };
        };
}

define set dcor(numeric display, agent a) {
        _caos {
                .targ < $a {
                        DCOR $display(r);
                };
        };
}

define set perm(numeric value, agent a) {
        _caos {
                .targ < $a {
                        PERM $value(r);
                };
        };
}


