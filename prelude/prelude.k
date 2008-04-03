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
	_caos { .inline let $_return = SQRT $v(r); }
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
  _caos { .targ < $a { .inline let $_return = UNID; }; };
}

define iterator() reps(numeric n) {
  _caos {
    REPS $n(r);
    .loop { .kaos { _yield(); }; };
    REPE;
  };
}

define norn() returning agent {
  _caos { .inline let $_return = NORN; };
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

// need better folder :(
define attributes(agent a) returning numeric {
        _caos {
                .targ < $a {
                        .inline let $_return = ATTR;
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

define kill(agent a) {
	_caos {
		.targ zap;
		KILL $a(r);
	};
}
