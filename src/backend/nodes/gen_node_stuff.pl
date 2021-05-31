#!/usr/bin/perl
#----------------------------------------------------------------------
#
# TODO
#
# src/backend/nodes/gen_node_stuff.pl
#
#----------------------------------------------------------------------

use strict;
use warnings;

use experimental 'smartmatch';

my @node_types = ();

my $in_struct = undef;
my $subline;
my $is_node_struct;

while (my $line = <ARGV>)
{
	chomp $line;
	next if $line =~ /^\s*$/ || $line =~ m!^\s*/\*.*\*/\s*$!;

	if ($in_struct)
	{
		$subline++;

		if ($subline == 1)
		{
			$is_node_struct = 0;
			next if $line eq '{';
			die;
		}
		elsif ($subline == 2)
		{
			if ($line =~ /^\s*NodeTag\s+type;/ ||
				$line =~ /^\s*Expr\s+/ ||
				$line =~ /^\s*Plan\s+/ ||
				$line =~ /^\s*Scan\s+/ ||
				$line =~ /^\s*Join\s+/ ||
				$line =~ /^\s*Sort\s+/ ||
				$line =~ /^\s*Path\s+/ ||
				$line =~ /^\s*JoinPath\s+/ ||
				$line =~ /^\s*SortPath\s+/ ||
				$line =~ /^\s*PlanState\s+/ ||
				$line =~ /^\s*ScanState\s+/ ||
				$line =~ /^\s*JoinState\s+/ ||
				$line =~ /^\s*PartitionPruneStep\s+/ ||
				$line =~ /^\s*CreateStmt\s+/ ||
				$line =~ /^\s*MemoryContextData\s+/)
			{
				$is_node_struct = 1;
				next;
			}
		}

		if ($line =~ /^\}\s*$in_struct;$/)
		{
			push @node_types, $in_struct if $is_node_struct;
			$in_struct = undef;
		}
		elsif ($line =~ /^\};$/ && !$is_node_struct)
		{
			$in_struct = undef;
		}
	}
	else
	{
		if ($line =~ /^typedef struct (\w+)(\s*\/\*.*)?$/)
		{
			$in_struct = $1;
			$subline = 0;
		}
		elsif ($line =~ /^typedef (\w+) (\w+);$/ && $1 ~~ @node_types)
		{
			push @node_types, $2;
		}
	}
}

if ($in_struct)
{
	print STDERR "runaway \"$in_struct\"\n";
	exit 1;
}

push @node_types, 'IntList', 'OidList'; # aliases for List
push @node_types, 'Integer', 'Float', 'String', 'BitString', 'Null'; # aliases for Value
push @node_types, 'AppendState', 'SpecialJoinInfo', 'TIDBitmap', 'PlannerInfo', 'IndexOptInfo'; # FIXME

open my $nt, '>', 'nodetags.h' or die;

print $nt "typedef enum NodeTag\n";
print $nt "{\n";
print $nt "\tT_Invalid = 0,\n";

foreach my $n (@node_types)
{
	print $nt "\tT_${n},\n";
}

print $nt "} NodeTag;\n";

close $nt;
