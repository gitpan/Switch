package Switch;

use strict;
use vars qw($VERSION);
use Carp;

$VERSION = '1.00';


# LOAD FILTERING MODULE...
use Filter::Util::Call;

my $offset = 0;
sub __();

# CATCH ATTEMPTS TO CALL case OUTSIDE THE SCOPE OF ANY switch

$::_SWITCH = sub { croak "no switch statement active for case" };

sub import
{
	filter_add({}) unless @_>1 && $_[1] ne '__';
	my $pkg = caller;
	no strict 'refs';
	for ( qw( on_defined on_exists ) )
	{
		*{"${pkg}::$_"} = \&$_;
	}
	*{"${pkg}::__"} = \&__ if grep /__/, @_;
	$offset = (caller)[2]+1;
	1;
}

sub unimport
{	
	filter_del()
}

sub filter
{
	my($self) = @_ ;

	my $status = 1;
	$status = filter_read(10_000);
	return $status if $status<0;
	$_ = filter_blocks($_);
	$_ = "# line $offset\n" . $_ if $offset; undef $offset;
	return $status;
}

use Text::Balanced ':ALL';

sub is_block
{
	local $SIG{__WARN__}=sub{die$@};
	local $^W=1;
	my $ishash = defined  eval 'my $hr='.$_[0];
	undef $@;
	return !$ishash;
}

sub filter_blocks
{
	my $source = shift;
	pos $source = 0;
	my $text = "";
	component: while (pos $source < length $source)
	{
		#print STDERR "<<<", substr($source,pos $source,20), ">>>\n";
		if ($source =~ m/(\G\s*use\s+Switch\b)/gc)
		{
			#print STDERR " <<use switch>>\n";
			$text .= q{use Switch 'noimport'};
			next component;
		}
		my @pos = Text::Balanced::_match_quotelike(\$source,qr/\s*/,1,1);
		if (defined $pos[0])
		{
			#print STDERR " <<quotelike>>\n";
			$text .= substr($source,$pos[2],$pos[18]-$pos[2]);
			next component;
		}
		@pos = Text::Balanced::_match_variable(\$source,qr/\s*/);
		if (defined $pos[0])
		{
			#print STDERR " <<variable:",pos($source),">>\n";
			$text .= substr($source,$pos[0],$pos[4]-$pos[0]);
			next component;
		}

		if ($source =~ m/\G(\n*)(\s*)switch/gc)
		{
			#print STDERR " <<switch>>\n";
			$text .= $1.'local $::_SWITCH;'.$2.'Switch::switch';
			$source =~ m/\G(\s*[(]?\s*)(?=%|qw)/gc 
			   and $text .= $1. '\\';
			$source =~ m/\G(\s*[(]?\s*)(?=\/)/gc 
			   and $text .= $1 . 'qr';
			$source =~ m/\G(\s*[(]?\s*)m\b/gc 
			   and $text .= $1 . 'qr';
			@pos = Text::Balanced::_match_codeblock(\$source,qr/\s*/,qr/\{/,qr/\}/,qr/\{/,qr/\}/,undef);
			if (@pos)
			{
				my $code = substr($source,$pos[0],$pos[4]-$pos[0]);
				$text .= " sub" if is_block $code;
				$text .= " " . filter_blocks($code);
				next component;
			}
		}
		elsif ($source =~ m/\G(\s*)(case)(?!\s*=>)/gc)
		{
			#print STDERR " <<case>>\n";
			$text .= $1."if (Switch::case";
			@pos = Text::Balanced::_match_codeblock(\$source,qr/\s*/,qr/\{/,qr/\}/,qr/\{/,qr/\}/,undef);
			if (@pos)
			{
				my $code = substr($source,$pos[0],$pos[4]-$pos[0]);
				$text .= " sub" if is_block $code;
				$text .= " " . filter_blocks($code) . ")";
				next component;
			}
			@pos = Text::Balanced::_match_codeblock(\$source,qr/\s*/,qr/[[(]/,qr/[])]/,qr/[[({]/,qr/[])}]/,undef);
			if (@pos)
			{
				my $code = filter_blocks(substr($source,$pos[0],$pos[4]-$pos[0]));
				$code =~ s {^\s*[(]\s*%}   { ( \\\%}	||
				$code =~ s {^\s*[(]\s*m\b} { ( qr}	||
				$code =~ s {^\s*[(]\s*/}   { ( qr/}	||
				$code =~ s {^\s*[(]\s*qw}  { ( \\qw};
				$text .= " $code)";
				next component;
			}
			@pos = Text::Balanced::_match_quotelike(\$source,qr/\s*/,1,1);
			if (@pos)
			{
				my $code = substr($source,$pos[2],$pos[18]-$pos[2]);
				#print STDERR "[[[$code]]]\n";
				$code = filter_blocks($code);
				$code =~ s {^\s*m}  { qr}	||
				$code =~ s {^\s*/}  { qr/}	||
				$code =~ s {^\s*qw} { \\qw};
				$text .= " $code)";
				next component;
			}
			if ($source =~ m/\G\s*(([^\$\@{])[^\$\@{]*)(?=\s*{)/gc)
			{
				my $code = filter_blocks($1);
				$text .= ' \\' if $2 eq '%';
				$text .= " $code)";
				next component;
			}

			my @caller = caller 1;
			die "Bad case statement: $@ near ", $caller[1], " line ",
			      (substr($source,0,pos $source)=~tr/\n/\n/)+1+$caller[2],
			      "\n";
		}

		#print STDERR " <<other>>\n";
		$source =~ m/\G(\s*(\w+|#.*\n|\W))/gc;
		$text .= $1;
	}
	$text;
}



sub in
{
	my ($x,$y) = @_;
	my @numy;
	for my $nextx ( @$x )
	{
		my $numx = ref($nextx) || (~$nextx&$nextx) eq 0;
		for my $j ( 0..$#$y )
		{
			my $nexty = $y->[$j];
			push @numy, ref($nexty) || (~$nexty&$nexty) eq 0
				if @numy <= $j;
			return 1 if $numx && $numy[$j] && $nextx==$nexty
			         || $nextx eq $nexty;
			
		}
	}
	return "";
}

sub on_exists
{
	my $ref = @_==1 && ref($_[0]) eq 'HASH' ? $_[0] : { @_ };
	[ keys %$ref ]
}

sub on_defined
{
	my $ref = @_==1 && ref($_[0]) eq 'HASH' ? $_[0] : { @_ };
	[ grep { defined $ref->{$_} } keys %$ref ]
}

sub switch(;$)
{
	my ($s_val) = @_ ? $_[0] : $_;
	my $s_ref = ref $s_val;
	
	if ($s_ref eq 'CODE')
	{
		$::_SWITCH =
		      sub { my $c_val = $_[0];
			    return $s_val == $c_val  if ref $c_val eq 'CODE';
			    return $s_val->(@$c_val) if ref $c_val eq 'ARRAY';
			    return $s_val->($c_val);
			  };
	}
	elsif ($s_ref eq "" && (~$s_val&$s_val) eq 0)	# NUMERIC SCALAR
	{
		$::_SWITCH =
		      sub { my $c_val = $_[0];
			    my $c_ref = ref $c_val;
			    return $s_val == $c_val 	if $c_ref eq ""
							&& (~$c_val&$c_val) eq 0;
			    return $s_val eq $c_val 	if $c_ref eq "";
			    return in([$s_val],$c_val)	if $c_ref eq 'ARRAY';
			    return $c_val->($s_val)	if $c_ref eq 'CODE';
			    return $c_val->call($s_val)	if $c_ref eq 'Switch';
			    return scalar $s_val=~/$c_val/
							if $c_ref eq 'Regexp';
			    return scalar $c_val->{$s_val}
							if $c_ref eq 'HASH';
		            return;	
			  };
	}
	elsif ($s_ref eq "")				# STRING SCALAR
	{
		$::_SWITCH =
		      sub { my $c_val = $_[0];
			    my $c_ref = ref $c_val;
			    return $s_val eq $c_val 	if $c_ref eq "";
			    return in([$s_val],$c_val)	if $c_ref eq 'ARRAY';
			    return $c_val->($s_val)	if $c_ref eq 'CODE';
			    return $c_val->call($s_val)	if $c_ref eq 'Switch';
			    return scalar $s_val=~/$c_val/
							if $c_ref eq 'Regexp';
			    return scalar $c_val->{$s_val}
							if $c_ref eq 'HASH';
		            return;	
			  };
	}
	elsif ($s_ref eq 'ARRAY')
	{
		$::_SWITCH =
		      sub { my $c_val = $_[0];
			    my $c_ref = ref $c_val;
			    return in($s_val,[$c_val]) 	if $c_ref eq "";
			    return in($s_val,$c_val)	if $c_ref eq 'ARRAY';
			    return $c_val->(@$s_val)	if $c_ref eq 'CODE';
			    return $c_val->call(@$s_val)
							if $c_ref eq 'Switch';
			    return scalar grep {$_=~/$c_val/} @$s_val
							if $c_ref eq 'Regexp';
			    return scalar grep {$c_val->{$_}} @$s_val
							if $c_ref eq 'HASH';
		            return;	
			  };
	}
	elsif ($s_ref eq 'Regexp')
	{
		$::_SWITCH =
		      sub { my $c_val = $_[0];
			    my $c_ref = ref $c_val;
			    return $c_val=~/s_val/ 	if $c_ref eq "";
			    return scalar grep {$_=~/s_val/} @$c_val
							if $c_ref eq 'ARRAY';
			    return $c_val->($s_val)	if $c_ref eq 'CODE';
			    return $c_val->call($s_val)	if $c_ref eq 'Switch';
			    return $s_val eq $c_val	if $c_ref eq 'Regexp';
			    return grep {$_=~/$s_val/ && $c_val->{$_}} keys %$c_val
							if $c_ref eq 'HASH';
		            return;	
			  };
	}
	elsif ($s_ref eq 'HASH')
	{
		$::_SWITCH =
		      sub { my $c_val = $_[0];
			    my $c_ref = ref $c_val;
			    return $s_val->{$c_val} 	if $c_ref eq "";
			    return scalar grep {$s_val->{$_}} @$c_val
							if $c_ref eq 'ARRAY';
			    return $c_val->($s_val)	if $c_ref eq 'CODE';
			    return $c_val->call($s_val)	if $c_ref eq 'Switch';
			    return grep {$_=~/$c_val/ && $s_val->{"$_"}} keys %$s_val
							if $c_ref eq 'Regexp';
			    return $s_val==$c_val	if $c_ref eq 'HASH';
		            return;	
			  };
	}
	elsif ($s_ref eq 'Switch')
	{
		$::_SWITCH =
		      sub { my $c_val = $_[0];
			    return $s_val == $c_val  if ref $c_val eq 'Switch';
			    return $s_val->call(@$c_val)
						     if ref $c_val eq 'ARRAY';
			    return $s_val->call($c_val);
			  };
	}
	else
	{
		croak "Cannot switch on $s_ref";
	}
	return 1;
}

sub case($) { $::_SWITCH->(@_); }

# IMPLEMENT __

my $placeholder = bless { arity=>1, impl=>sub{$_[1+$_[0]]} };

sub __() { $placeholder }

sub __arg($)
{
	my $index = $_[0]+1;
	bless { arity=>0, impl=>sub{$_[$index]} };
}

sub hosub(&@)
{
	# WRITE THIS
}

sub call
{
	my ($self,@args) = @_;
	return $self->{impl}->(0,@args);
}

sub meta_bop(&)
{
	my ($op) = @_;
	sub
	{
		my ($left, $right, $reversed) = @_;
		($right,$left) = @_ if $reversed;

		my $rop = ref $right eq 'Switch'
			? $right
			: bless { arity=>0, impl=>sub{$right} };

		my $lop = ref $left eq 'Switch'
			? $left
			: bless { arity=>0, impl=>sub{$left} };

		my $arity = $lop->{arity} + $rop->{arity};

		return bless {
				arity => $arity,
				impl  => sub { my $start = shift;
					       return $op->($lop->{impl}->($start,@_),
						            $rop->{impl}->($start+$lop->{arity},@_));
					     }
			     };
	};
}

sub meta_uop(&)
{
	my ($op) = @_;
	sub
	{
		my ($left) = @_;

		my $lop = ref $left eq 'Switch'
			? $left
			: bless { arity=>0, impl=>sub{$left} };

		my $arity = $lop->{arity};

		return bless {
				arity => $arity,
				impl  => sub { $op->($lop->{impl}->(@_)) }
			     };
	};
}


use overload
	"+"	=> 	meta_bop {$_[0] + $_[1]},
	"-"	=> 	meta_bop {$_[0] - $_[1]},  
	"*"	=>  	meta_bop {$_[0] * $_[1]},
	"/"	=>  	meta_bop {$_[0] / $_[1]},
	"%"	=>  	meta_bop {$_[0] % $_[1]},
	"**"	=>  	meta_bop {$_[0] ** $_[1]},
	"<<"	=>  	meta_bop {$_[0] << $_[1]},
	">>"	=>  	meta_bop {$_[0] >> $_[1]},
	"x"	=>  	meta_bop {$_[0] x $_[1]},
	"."	=>  	meta_bop {$_[0] . $_[1]},
	"<"	=>  	meta_bop {$_[0] < $_[1]},
	"<="	=>  	meta_bop {$_[0] <= $_[1]},
	">"	=>  	meta_bop {$_[0] > $_[1]},
	">="	=>  	meta_bop {$_[0] >= $_[1]},
	"=="	=>  	meta_bop {$_[0] == $_[1]},
	"!="	=>  	meta_bop {$_[0] != $_[1]},
	"<=>"	=>  	meta_bop {$_[0] <=> $_[1]},
	"lt"	=>  	meta_bop {$_[0] lt $_[1]},
	"le"	=> 	meta_bop {$_[0] le $_[1]},
	"gt"	=> 	meta_bop {$_[0] gt $_[1]},
	"ge"	=> 	meta_bop {$_[0] ge $_[1]},
	"eq"	=> 	meta_bop {$_[0] eq $_[1]},
	"ne"	=> 	meta_bop {$_[0] ne $_[1]},
	"cmp"	=> 	meta_bop {$_[0] cmp $_[1]},
	"\&"	=> 	meta_bop {$_[0] & $_[1]},
	"^"	=> 	meta_bop {$_[0] ^ $_[1]},
	"|"	=>	meta_bop {$_[0] | $_[1]},
	"atan2"	=>	meta_bop {atan2 $_[0], $_[1]},

	"neg"	=>	meta_uop {-$_[0]},
	"!"	=>	meta_uop {!$_[0]},
	"~"	=>	meta_uop {~$_[0]},
	"cos"	=>	meta_uop {cos $_[0]},
	"sin"	=>	meta_uop {sin $_[0]},
	"exp"	=>	meta_uop {exp $_[0]},
	"abs"	=>	meta_uop {abs $_[0]},
	"log"	=>	meta_uop {log $_[0]},
	"sqrt"  =>	meta_uop {sqrt $_[0]},
	"bool"  =>	sub { croak "Can't use && or || in expression containing __" },

	#	"&()"	=>	sub { $_[0]->{impl} },

	#	"||"	=>	meta_bop {$_[0] || $_[1]},
	#	"&&"	=>	meta_bop {$_[0] && $_[1]},
	# fallback => 1,
	;
1;

__END__


=head1 NAME

Switch - A switch statement for Perl

=head1 VERSION

This document describes version 1.00 of Switch,
released August  7, 2000.

=head1 SYNOPSIS

	use Switch;

	switch ($val);

		case 1		{ print "number 1" }
		case "a"	{ print "string a" }
		case [1..10,42]	{ print "number in list" }
		case (@array)	{ print "number in list" }
		case /\w+/	{ print "pattern" }
		case qr/\w+/	{ print "pattern" }
		case (%hash)	{ print "entry in hash" }
		case (\%hash)	{ print "entry in hash" }
		case (\&sub)	{ print "arg to subroutine" }
		else		{ print "previous case not true" }

=head1 BACKGROUND

In seeking to devise a "Swiss Army" case mechanism suitable for Perl,
it is useful to generalize this notion of distributed conditional
testing as far as possible. Specifically, the concept of "matching"
between the switch value and the various case values need not be
restricted to numeric (or string or referential) equality, as it is in other 
languages. Indeed, as Table 1 illustrates, Perl
offers at least sixteen different ways in which two values could
generate a match.

	Table 1: Matching a switch value ($s) with a case value ($c)

        Switch  Case    Type of Match Implied   Matching Code
        Value   Value   
        ======  =====   =====================   =============

        number  same    numeric or referential  match if $s == $c;
        or ref          equality

        other   other   string equality         match if $s eq $c;
        non-ref non-ref
        scalar  scalar

        string  regexp  pattern match           match if $s =~ /$c/;

        array   scalar  array entry existence   match if 0<=$c && $c<@$s;
        ref             array entry definition  match if defined $s->[$c];
                        array entry truth       match if $s->[$c];

        array   array   array intersection      match if intersects(@$s, @$c);
        ref     ref     (apply this table to
                         all pairs of elements
                         $s->[$i] and
                         $c->[$j])

        array   regexp  array grep              match if grep /$c/, @$s;
        ref     

        hash    scalar  hash entry existence    match if exists $s->{$c};
        ref             hash entry definition   match if defined $s->{$c};
                        hash entry truth        match if $s->{$c};

        hash    regexp  hash grep               match if grep /$c/, keys %$s;
        ref     

        sub     scalar  return value defn       match if defined $s->($c);
        ref             return value truth      match if $s->($c);

        sub     array   return value defn       match if defined $s->(@$c);
        ref     ref     return value truth      match if $s->(@$c);


In reality, Table 1 covers 29 alternatives, because only the equality and
intersection tests are commutative; in all other cases, the roles of
the C<$s> and C<$c> variables could be reversed to produce a
different test. For example, instead of testing a single hash for
the existence of a series of keys (C<match if exists $s-E<gt>{$c}>),
one could test for the existence of a single key in a series of hashes
(C<match if exists $c-E<gt>{$s}>).

As L<perltodo> observes, a Perl case mechanism must support all these
"ways to do it".


=head1 DESCRIPTION

The Switch.pm module implements a generalized case mechanism that covers
the numerous possible combinations of switch and case values described above.

The module augments the standard Perl syntax with two new control
statements: C<switch> and C<case>. The C<switch> statement takes a
single optional scalar argument of any type (including a subroutine
reference specified as a simple block). If the argument is a variable,
it must be enclosed in parentheses; otherwise the parentheses are
optional. If the argument is omitted, C<$_> is used instead. Regardless
of the origin or type of argument, C<switch> stores its value as the
current switch value in the (localized) control variable C<${::SWITCH}>.

The C<case> statement takes a single scalar argument (again, in mandatory
parentheses if it's a variable) and selects the
appropriate type of matching between that argument and the current value
of C<${^::SWITCH}>. The type of matching used is determined by the
respective types of the C<${::SWITCH}> value and the C<case> argument, as
specified in Table 1. If the match is successful, the block associated
with the C<case> statement is executed.

In all other respects, the C<case> statement is semantically identical
to an C<if> statement. For example, it can be followed by an C<else>
clause, and can be used as a postfix statement qualifier. 

Together these two new statements provide a fully generalized case
mechanism:

        use switch;

        # AND LATER...

        %special = ( woohoo => 1,  d'oh => 1 );

        while (<>) {
                switch ($_);

                case %special  { print "homer\n"; next }       # if $special{$_}
                case /a-z/i    { print "alpha\n"; next }       # if $_ =~ /a-z/i
                case [1..9]    { print "small num\n"; next }   # if $_ in [1..9]

                case { $_[0] >= 10 } {    		       # if $_ >= 10
                        my $age = <>;
                        switch { $_[0] < $age };

                        case 20  { print "teens\n"; next }     # if 20 < $age
                        case 30  { print "twenties\n"; next }  # if 30 < $age
                        else     { print "history\n"; next }
                }

                print "must be punctuation\n" case /\W/;       # if $_ ~= /\W/
        }

Note that C<switch>es can be nested within C<case> (or any other) blocks,
and a series of case statements can try different types of matches
-- hash membership, pattern match, array intersection, simple equality,
etc. -- against the same switch value.

The use of intersection tests against an array reference is particularly
useful for aggregating integral cases:

        sub classify_digit
        {
                switch ($_[0]);  case 0            { return 'zero' }
                                 case [2,4,6,8]    { return 'even' }
                                 case [1,3,4,7,9]  { return 'odd' }
                                 case /[A-F]/i     { return 'hex' }
        }


=head2 Higher-order Operations

One situation in which C<switch> and C<case> do not provide a good
substitute for a cascaded C<if>, is where a switch value needs to
be tested against a series of conditions. For example:

        sub beverage {
                switch shift;

                case { $_[0] < 10 }  { return 'milk' }
                case { $_[0] < 20 }  { return 'coke' }
                case { $_[0] < 30 }  { return 'beer' }
                case { $_[0] < 40 }  { return 'wine' }
                case { $_[0] < 50 }  { return 'malt' }
                case { $_[0] < 60 }  { return 'Moet' }
                else                 { return 'milk' }
        }

The need to specify each condition as an implicit subroutine block is
tiresome. To overcome this, when importing Switch.pm, users may elect to
also import a special "placeholder" subroutine named C<__> [sic]. This
subroutine converts (almost) any expression in which it appears to a
reference to a higher-order function. That is, the expression:

        use switch '__';

        __ < 2 + __

is equivalent to:

        sub { $_[0] < 2 + $_[1] }

With C<__>, the previous ugly case statements can be rewritten:

        case  __ < 10  { return 'milk' }
        case  __ < 20  { return 'coke' }
        case  __ < 30  { return 'beer' }
        case  __ < 40  { return 'wine' }
        case  __ < 50  { return 'malt' }
        case  __ < 60  { return 'Moet' }
        else           { return 'milk' }

The C<__> subroutine makes extensive use of operator overloading to
perform its magic. All operations involving __ are overloaded to
produce an anonymous subroutine that implements a lazy version
of the original operation.

The only problem is that operator overloading does not allow the
boolean operators C<&&> and C<||> to be overloaded. So a case statement
like this:

        case  0 <= __ && __ < 10  { return 'digit' }  

doesn't act as expected, because when it is
executed, it constructs two higher order subroutines
and then treats the two resulting references as arguments to C<&&>:

        sub { 0 <= $_[0] } && sub { $_[0] < 10 }

This boolean expression is inevitably true, since both references are
non-false. Fortunately, the overloaded C<'bool'> operator catches this
situation and flags it as a error. 

=head1 AUTHOR

Damian Conway (damian@conway.org)

=head1 BUGS

There are undoubtedly serious bugs lurking somewhere in code this funky :-)
Bug reports and other feedback are most welcome.

=head1 COPYRIGHT

Copyright (c) 1997-2000, Damian Conway. All Rights Reserved.
This module is free software. It may be used, redistributed
and/or modified under the terms of the Perl Artistic License
  (see http://www.perl.com/perl/misc/Artistic.html)
