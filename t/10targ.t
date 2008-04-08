#!/usr/bin/perl
use strict;
use warnings;

use Test::Kaos;
plan tests => 5;

test_output("Production and folding from targ", q{
	kill(newsimple(1,2,3, "foo", 4, 5, 6));
}, q{
	NEW: SIMP 1 2 3 "foo" 4 5 6
	KILL TARG
	RSCR
});

test_output("Multiple production from targ", q{
	agent x = newsimple(1,2,3, "foo", 4, 5, 6);
	agent y = newsimple(1,2,3, "foo", 4, 5, 6);
	kill(x);
	kill(y);
}, q{
	NEW: SIMP 1 2 3 "foo" 4 5 6
	SETA $1 TARG
	NEW: SIMP 1 2 3 "foo" 4 5 6
	SETA $2 TARG
	KILL $1
	KILL $2
	RSCR
});

test_output("Setting targ (and folding within)", q{
	print(null.unid);
}, q{
	TARG NULL
	OUTV UNID
	RSCR
});

test_output("Setting targ (multiple contexts; partial folding)", q{
	numeric a = null.unid;
	numeric b = norn.unid;
	print(a);
	print(b);
}, q{
	TARG NULL
	SETV $1 UNID
	TARG NORN
	OUTV $1
	OUTV UNID
	RSCR
});

test_output("Setting targ (invalidation; no folding)", q{
	numeric a = norn.unid;
	kill(norn);
	print (a);
}, q{
	TARG NORN
	SETV $1 UNID
	KILL NORN
	OUTV $1
	RSCR
});

