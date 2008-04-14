package Test::Kaos;

require Exporter;
use Test::More;

our @ISA = qw(Exporter);
our @EXPORT = qw(test_syntax test_error test_output plan);

BEGIN {
	eval {
		require IPC::Run;
		import IPC::Run qw(run timeout);
	};
	if ($@) {
		import Test::More skip_all => "Failed to import IPC::Run";
		exit 0;
	}
}

sub runkaos {
	my ($in, $out, $err, $args) = @_;
	$args = [] unless defined $args;
	return run ["dist/build/kaos/kaos", "-o", "-", "-", @$args], $in, $out, $err, timeout(10);
}

sub test_syntax {
	my ($desc, $code) = @_;
	my $in = $code;
	my $out = '';
	my $err = '';
	ok(runkaos(\$in, \$out, \$err), $desc);
}

sub prep_re {
	my $re = shift;
	my $out;
	my %seen;
	$re =~ s/^\s*//s;
	$re =~ s/\s*$//s;
	while ($re =~ s/^([^\$\s]*)(?:\$(\d+)|(\s+)|$)//s) {
		my $pre = quotemeta($1);
		my $n   = $2;
		my $ws  = $3;
		$pre =~ s/\s+/\\s+/g;
		$out .= $pre;
		if (defined $n) {
			$out .= ($seen{$n}++ ? "\\$n" : '(VA\d\d)');
		} elsif (defined ($ws)) {
			$out .= q{\s+};
		} else {
			last;
		}
	}
	return $out;
}

sub hashp_str {
	my @l = split /\n/, $_[0];
	return join("\n", map { "# $_" } @l)."\n";
}

sub test_output {
	my ($desc, $code, $re) = @_;
	$re = prep_re($re) unless ref $re;
	my $in = $code;
	my $out = '';
	my $err = '';
	my $res = runkaos(\$in, \$out, \$err);
	if (!$res) {
		ok(0, "$desc - kaos error");
		print hashp_str($err);
	}
	$out = "\n$out\n";
	if($out =~ /$re/si) {
		ok(1, $desc);
	} else {
		ok(0, $desc);
		print "# Expected:\n";
		print hashp_str($_[2]);
		print "# Got:\n";
		print hashp_str($out);
	}
}

1;
