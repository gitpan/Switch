use Carp;
use Switch;
my $OFFSET = 0;

%a = (1=>1);

while (<>)
{
	chomp;
	print "$_ matched: \n";
	switch;

		# SELF
		#BAD: case $_ { print "case at line ", __LINE__+$OFFSET, "\n" }
		case ($_) { print "case at line ", __LINE__+$OFFSET, "\n" }

		# ARRAY
		case ([10,5,1]) { print "case at line ", __LINE__+$OFFSET, "\n" }
		case [10,5,1] { print "case at line ", __LINE__+$OFFSET, "\n" }

		# VARS
		case %a { print "case at line ", __LINE__+$OFFSET, "\n" }
		case (%a) { print "case at line ", __LINE__+$OFFSET, "\n" }
		#BAD: case @a { print "case at line ", __LINE__+$OFFSET, "\n" }
		case (@a) { print "case at line ", __LINE__+$OFFSET, "\n" }

		# NUMERIC
		case (1) { print "case at line ", __LINE__+$OFFSET, "\n" }
		case 1 { print "case at line ", __LINE__+$OFFSET, "\n" }

		# STRING
		case ('a') { print "case at line ", __LINE__+$OFFSET, "\n" }
		case 'a' { print "case at line ", __LINE__+$OFFSET, "\n" }

		# PATTERN

		case qr/a/i { print "case at line ", __LINE__+$OFFSET, "\n" }
		case qr{a}i { print "case at line ", __LINE__+$OFFSET, "\n" }
		case /a/i { print "case at line ", __LINE__+$OFFSET, "\n" }
		case (/a/i) { print "case at line ", __LINE__+$OFFSET, "\n" }
		case (qr/a/i) { print "case at line ", __LINE__+$OFFSET, "\n" }
		# HASH
		case {} { print "case at line ", __LINE__+$OFFSET, "\n" }
		case ({}) { print "case at line ", __LINE__+$OFFSET, "\n" }
		case {1=>1, a=>1, 2=>0} { print "case at line ", __LINE__+$OFFSET, "\n" }

		# SUB/BLOCK
		case (sub {eval{local $^W;$_[0]==2}}) { print "case at line ", __LINE__+$OFFSET, "\n" }
		case {$_[0]==2} { print "case at line ", __LINE__+$OFFSET, "\n" }
		case {0} { print "case at line ", __LINE__+$OFFSET, "\n" }
		case {1} { print "case at line ", __LINE__+$OFFSET, "\n" }
}
