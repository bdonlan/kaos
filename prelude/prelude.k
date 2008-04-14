/*  Kaos - A compiler for creatures scripts
 *  Copyright (C) 2005-2008  Bryan Donlan
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version, or the MIT licence (below).
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *  The MIT licence:
 *
 *  Permission is hereby granted, free of charge, to any person obtaining a copy
 *  of this software and associated documentation files (the "Software"), to deal
 *  in the Software without restriction, including without limitation the rights
 *  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 *  copies of the Software, and to permit persons to whom the Software is
 *  furnished to do so, subject to the following conditions:
 *  
 *  The above copyright notice and this permission notice shall be included in
 *  all copies or substantial portions of the Software.
 *  
 *  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 *  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 *  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 *  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 *  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 *  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 *  THE SOFTWARE.
 */

define sqrt(numeric v) returning numeric {
	_caos { .inline let $return = SQRT $v(r); }
}

define iterator(agent) enum(numeric family, numeric genus, numeric species) {
  agent targ;
  _caos {
// XXX: is this use of .loop correct?
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
  _caos { .targ < $a { .inline let $return = UNID; }; };
}

define iterator() reps(numeric n) {
  _caos {
    REPS $n(r);
    .loop { .kaos { _yield(); }; };
    REPE;
  };
}

define norn() returning agent {
  _caos { .inline let $return = NORN; };
}

define set norn(agent norn) {
  _caos { NORN $norn(r); }
}

define newsimple(numeric family, numeric genus, numeric species, string spritefile, numeric firstframe, numeric noframes, numeric plane) returning agent {
        _caos {
                .targ > $return {
                        NEW: SIMP $family(r) $genus(r) $species(r) $spritefile(r) $firstframe(r) $noframes(r) $plane(r);
                };
        }
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

define kill(agent a) {
	_caos {
		KILL $a(r);
		.targ zap;
	};
}

define windowleft() returning numeric {
	_caos { .inline let $return = WNDL; };
}

define windowright() returning numeric {
	_caos { .inline let $return = WNDR; };
}

define pointer() returning agent {
	_caos { .static let $return = PNTR; };
}

define null() returning agent {
	_caos { .static let $return = NULL; };
}

define stop() {
	_caos { STOP; };
}

define plane(agent a) returning numeric {
	_caos {
		.targ < $a {
			.inline let $return = PLNE;
		};
	}
}

define x(agent a) returning numeric {
	_caos {
		.targ < $a {
			.inline let $return = POSL;
		};
	}
}

define y(agent a) returning numeric {
	_caos {
		.targ < $a {
			.inline let $return = POST;
		};
	}
}

define centrex(agent a) returning numeric {
	_caos {
		.targ < $a {
			.inline let $return = POSX;
		};
	}
}

define newcompound(numeric family, numeric genus, numeric species, string spritefile, numeric firstframe, numeric noframes, numeric plane) returning agent {
	_caos {
		.targ > $return {
			NEW: COMP $family(r) $genus(r) $species(r) $spritefile(r) $firstframe(r) $noframes(r) $plane(r);
		};
	}
}

define set tick(numeric ticktime, agent a) {
	_caos {
		.targ < $a {
			TICK $ticktime(r);
		};
	}
}

define set pose(numeric pose, agent a) {
	_caos {
		.targ < $a {
			POSE $pose(r);
		};
	}
}

define killpart(agent a, numeric partno) {
	_caos {
		.targ < $a {
			PAT: KILL $partno(r);
		};
	}
}

// TODO: ugh
define changepart(agent a, numeric partno) {
	_caos {
		PART $partno(r);
	}
}

define newtextpart(agent a, numeric partno, string spritefile, numeric firstframe, numeric x, numeric y, numeric plane, string fontspritefile) {
	_caos {
		.targ < $a {
			PAT: FIXD $partno(r) $spritefile(r) $firstframe(r) $x(r) $y(r) $plane(r) $fontspritefile(r);
		};
	}
}

// TODO: better params
define textformat(agent a, numeric leftmargin, numeric topmargin, numeric rightmargin, numeric bottommargin, numeric linespacing, numeric charspacing, numeric justification) {
	_caos {
		.targ < $a {
			FRMT $leftmargin(r) $topmargin(r) $rightmargin(r) $bottommargin(r) $linespacing(r) $charspacing(r) $justification(r);
		};
	}
}

define set parttext(string text, agent a) {
	_caos {
		.targ < $a {
			PTXT $text(r);
		};
	}
}

define nopages(agent a) returning numeric {
	_caos {
		.targ < $a {
			.inline let $return = NPGS;
		};
	}
}

define moveto(agent a, numeric x, numeric y) {
	_caos {
		.targ < $a {
			MVTO $x(r) $y(r);
		};
	}
}

define floatto(agent a, agent target) {
	_caos {
		.targ < $a {
			FREL $target(r);
		};
	}
}

define owner() returning agent {
	_caos {
		.static let $return = OWNR;
	};
}

define set inputmask(numeric mask, agent a) {
	_caos {
		.targ < $a {
			IMSK $mask(r);
		};
	}
}

define set plane(numeric val, agent a) {
	_caos {
		.targ < $a {
			PLNE $val(r);
		};
	}
}

define parttext(agent a) returning string {
	_caos {
		.targ < $a {
			.inline let $return = PTXT;
		};
	}
}

define floatat(agent a, numeric x, numeric y) {
	_caos {
		.targ < $a {
			FLTO $x(r) $y(r);
		};
	}
}

define newtextinputpart(agent a, numeric partno, string spritefile, numeric firstframe, numeric x, numeric y, numeric plane, numeric msgno, string fontspritefile) {
	_caos {
		.targ < $a {
			PAT: TEXT $partno(r) $spritefile(r) $firstframe(r) $x(r) $y(r) $plane(r) $msgno(r) $fontspritefile(r);
		};
	}
}

define setfocus(agent a) {
	_caos {
		.targ < $a {
			FCUS;
		};
	}
}

define randomagent(numeric family, numeric genus, numeric species) returning agent {
	_caos {
		.targ > $return {
			RTAR $family(r) $genus(r) $species(r);
		};
	}
}

define shoutorder(agent a, string order) {
	_caos {
		.targ < $a {
			ORDR SHOU $order(r);
		};
	}
}

