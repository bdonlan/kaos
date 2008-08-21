#!/usr/bin/perl
use strict;
use warnings;

use Test::Kaos;
plan tests => 1;

test_output("doif divergence", qq{
install {
  numeric x;
  x = 1;
  if (1 == 1) {
  } else {
    x = __touch(1);
    print(x);
  }
}
}, q{
  SETV $1 1
  DOIF 1 EQ 1
  ELSE
  SETV $2 1
  OUTV $2
  ENDI
});
