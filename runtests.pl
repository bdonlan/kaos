#!/usr/bin/perl

use strict;
use warnings;

use Test::Harness;
use POSIX qw(getcwd);

$Test::Harness::switches .= " -I".getcwd()."/t/lib";

runtests(glob("t/*.t"));
