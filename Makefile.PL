
use ExtUtils::MakeMaker;
WriteMakefile(
		NAME	=> q[Switch],
		VERSION => q[2.09],
		PREREQ_PM => { Filter::Util::Call => 0, Text::Balanced     => 0 },

	     );
