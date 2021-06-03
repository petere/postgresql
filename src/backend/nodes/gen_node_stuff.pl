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

use File::Basename;

my @node_types = ();

my $in_struct = undef;
my $subline;
my $is_node_struct;
my $supertype;
my $supertype_field;
my @my_fields;
my %my_field_types;

my %all_node_types;

my @no_copy;

# FIXME: long allowed?
my @scalar_types = qw{
	bits32 bool char double int int8 int16 int32 int64 long uint8 uint16 uint32 uint64
	AclMode AttrNumber Cost Index Oid Size StrategyNumber SubTransactionId TimeLineID XLogRecPtr
};
push @scalar_types, 'struct CustomScanMethods*';  # TODO: doc

my @abstract = qw{BufferHeapTupleTableSlot HeapTupleTableSlot JoinPath MemoryContextData MinimalTupleTableSlot PartitionPruneStep VirtualTupleTableSlot};

# pathnodes.h exceptions
push @no_copy, qw(RelOptInfo IndexOptInfo Path PlannerGlobal EquivalenceClass EquivalenceMember ForeignKeyOptInfo GroupingSetData IncrementalSortPath IndexClause MinMaxAggInfo PathTarget PlannerInfo PlannerParamItem ParamPathInfo RollupData RowIdentityVarInfo StatisticExtInfo);
push @scalar_types, qw(EquivalenceClass* EquivalenceMember* QualCost Selectivity);

push @no_copy, qw(CallContext InlineCodeBlock);
push @no_copy, qw(AppendState);  # FIXME
push @no_copy, qw(Expr TIDBitmap);


foreach my $infile (@ARGV)
{
	open my $ifh, '<', $infile or die;

while (my $line = <$ifh>)
{
	chomp $line;
	$line =~ s!/\*.*$!!;
	$line =~ s/\s*$//;
	next if $line eq '';
	next if $line =~ m!^\s*\*.*$!;  # XXX starts with *
	next if $line =~ /^#(define|ifdef|endif)/;

	if ($in_struct)
	{
		$subline++;

		if ($subline == 1)
		{
			$is_node_struct = 0;
			$supertype = undef;
			next if $line eq '{';
			die;
		}
		elsif ($subline == 2)
		{
			if ($line =~ /^\s*NodeTag\s+type;/)
			{
				$is_node_struct = 1;
				next;
			}
			elsif ($line =~ /\s*(\w+)\s+(\w+);/ && $1 ~~ @node_types)
			{
				$is_node_struct = 1;
				$supertype = $1;
				$supertype_field = $2;
				next;
			}
		}

		if ($line =~ /^\}\s*$in_struct;$/)
		{
			if ($is_node_struct)
			{
				push @node_types, $in_struct;
				my @f = @my_fields;
				my %ft = %my_field_types;
				if ($supertype)
				{
					my @superfields;
					foreach my $sf (@{$all_node_types{$supertype}->{fields}})
					{
						my $fn = "${supertype_field}.$sf";
						push @superfields, $fn;
						$ft{$fn} = $all_node_types{$supertype}->{field_types}{$sf};
					}
					unshift @f, @superfields;
				}
				#warn "$in_struct has no fields\n" unless scalar(@f);
				$all_node_types{$in_struct}->{fields} = \@f;
				$all_node_types{$in_struct}->{field_types} = \%ft;

				if (basename($infile) eq 'execnodes.h' ||
					basename($infile) eq 'trigger.h' ||
					basename($infile) eq 'event_trigger.h' ||
					basename($infile) eq 'amapi.h' ||
					basename($infile) eq 'tableam.h' ||
					basename($infile) eq 'tsmapi.h' ||
					basename($infile) eq 'fdwapi.h' ||
					basename($infile) eq 'tuptable.h' ||
					basename($infile) eq 'replnodes.h' ||
					basename($infile) eq 'supportnodes.h' ||
					$infile =~ /\.c$/ ||
					($supertype && ($supertype eq 'Path' || $supertype eq 'JoinPath'))
				)
				{
					push @no_copy, $in_struct;
				}
			}
			$in_struct = undef;
			@my_fields = ();
			%my_field_types = ();
		}
		elsif ($line =~ /^\};$/ && !$is_node_struct)
		{
			$in_struct = undef;
		}
		elsif ($line =~ /^\s*(.+)\s*\b(\w+)(\[\w+\])?;/)
		{
			if ($is_node_struct)
			{
				my $type = $1;
				my $name = $2;
				my $array_size = $3;

				$type =~ s/^const\s*//;
				$type =~ s/\s*$//;
				$type =~ s/\s+\*$/*/;
				die if $type eq '';
				$type = $type . $array_size if $array_size;
				push @my_fields, $name;
				$my_field_types{$name} = $type;
			}
		}
		else
		{
			if ($is_node_struct)
			{
				#warn "$infile:$.: could not parse \"$line\"\n";
			}
		}
	}
	else
	{
		if ($line =~ /^typedef struct (\w+)(\s*\/\*.*)?$/ && $1 ne 'Node')
		{
			$in_struct = $1;
			$subline = 0;
		}
		elsif ($line =~ /^typedef (\w+) (\w+);$/ && $1 ~~ @node_types)
		{
			my $alias_of = $1;
			my $n = $2;

			push @node_types, $n;
			my @f = @{$all_node_types{$alias_of}->{fields}};
			my %ft = %{$all_node_types{$alias_of}->{field_types}};
			$all_node_types{$n}->{fields} = \@f;
			$all_node_types{$n}->{field_types} = \%ft;
		}
		elsif ($line =~ /^typedef enum (\w+)(\s*\/\*.*)?$/)
		{
			push @scalar_types, $1;
		}
	}
}

if ($in_struct)
{
	print STDERR "runaway \"$in_struct\"\n";
	exit 1;
}

	close $ifh;
} # for each file

push @no_copy, 'MemoryContextData';
push @node_types, 'Value';
push @node_types, 'IntList', 'OidList'; # aliases for List
push @no_copy, 'List', 'IntList', 'OidList';
push @node_types, 'Integer', 'Float', 'String', 'BitString', 'Null'; # aliases for Value
push @no_copy, 'Integer', 'Float', 'String', 'BitString', 'Null'; # aliases for Value
push @node_types, 'AppendState', 'SpecialJoinInfo', 'TIDBitmap', 'PlannerInfo', 'IndexOptInfo'; # FIXME


# nodetags.h

# FIXME: only overwrite nodetags.h if it actually changed

open my $nt, '>', 'nodetags.h' or die;

my $i = 0;

print $nt "typedef enum NodeTag\n";
print $nt "{\n";
print $nt "\tT_Invalid = $i,\n";
$i++;

foreach my $n (@node_types)
{
	next if $n ~~ @abstract;
	print $nt "\tT_${n} = $i,\n";
	$i++;
}

print $nt "} NodeTag;\n";

close $nt;


# copyfuncs.c

open my $cf, '>', 'copyfuncs2.c' or die;

print $cf q!
#include "postgres.h"

#include "miscadmin.h"
#include "nodes/extensible.h"
#include "nodes/pathnodes.h"
#include "nodes/plannodes.h"
#include "utils/datum.h"
#include "utils/rel.h"


/*
 * Macros to simplify copying of different kinds of fields.  Use these
 * wherever possible to reduce the chance for silly typos.  Note that these
 * hard-wire the convention that the local variables in a Copy routine are
 * named 'newnode' and 'from'.
 */

/* Copy a simple scalar field (int, float, bool, enum, etc) */
#define COPY_SCALAR_FIELD(fldname) \
	(newnode->fldname = from->fldname)

/* Copy a field that is a pointer to some kind of Node or Node tree */
#define COPY_NODE_FIELD(fldname) \
	(newnode->fldname = copyObjectImpl(from->fldname))

/* Copy a field that is a pointer to a Bitmapset */
#define COPY_BITMAPSET_FIELD(fldname) \
	(newnode->fldname = bms_copy(from->fldname))

/* Copy a field that is a pointer to a C string, or perhaps NULL */
#define COPY_STRING_FIELD(fldname) \
	(newnode->fldname = from->fldname ? pstrdup(from->fldname) : (char *) NULL)

/* Copy a field that is a pointer to a simple palloc'd object of size sz */
#define COPY_POINTER_FIELD(fldname, sz) \
	do { \
		Size	_size = (sz); \
		if (_size > 0) \
		{ \
			newnode->fldname = palloc(_size); \
			memcpy(newnode->fldname, from->fldname, _size); \
		} \
	} while (0)

/* Copy a parse location field (for Copy, this is same as scalar case) */
#define COPY_LOCATION_FIELD(fldname) \
	(newnode->fldname = from->fldname)

!;

foreach my $n (@node_types)
{
	next if $n ~~ @abstract;
	next if grep { $_ eq $n } @no_copy;

	print $cf "
static $n *
_copy${n}(const $n *from)
{
\t${n} *newnode = makeNode($n);

";

	if ($n eq 'Value')
	{
		# See also A_Const when changing this code! */
		print $cf q!	COPY_SCALAR_FIELD(type);
	switch (from->type)
	{
		case T_Integer:
			COPY_SCALAR_FIELD(val.ival);
			break;
		case T_Float:
		case T_String:
		case T_BitString:
			COPY_STRING_FIELD(val.str);
			break;
		case T_Null:
			/* nothing to do */
			break;
		default:
			elog(ERROR, "unrecognized node type: %d",
				 (int) from->type);
			break;
	}
!;
	}
	elsif ($n eq 'A_Const')
	{
		# This part must duplicate Value
		print $cf q!	COPY_SCALAR_FIELD(val.type);
	switch (from->val.type)
	{
		case T_Integer:
			COPY_SCALAR_FIELD(val.val.ival);
			break;
		case T_Float:
		case T_String:
		case T_BitString:
			COPY_STRING_FIELD(val.val.str);
			break;
		case T_Null:
			/* nothing to do */
			break;
		default:
			elog(ERROR, "unrecognized node type: %d",
				 (int) from->val.type);
			break;
	}

	COPY_LOCATION_FIELD(location);
!;
	}
	# FIXME: ExtensibleNode
	else
	{
		my $last_array_size_field;

		foreach my $f (@{$all_node_types{$n}->{fields}})
		{
			my $t = $all_node_types{$n}->{field_types}{$f};
			if ($t eq 'char*')
			{
				print $cf "\tCOPY_STRING_FIELD($f);\n";
			}
			elsif ($t eq 'Bitmapset*' || $t eq 'Relids')
			{
				print $cf "\tCOPY_BITMAPSET_FIELD($f);\n";
			}
			elsif ($t eq 'int' && $f =~ 'location$')
			{
				print $cf "\tCOPY_LOCATION_FIELD($f);\n";
			}
			elsif (grep {$_ eq $t} @scalar_types)
			{
				print $cf "\tCOPY_SCALAR_FIELD($f);\n";
				$last_array_size_field = "from->$f";
			}
			elsif ($t =~ /(\w+)\*/ && $1 ~~ @scalar_types)
			{
				print $cf "\tCOPY_POINTER_FIELD($f, $last_array_size_field * sizeof($1));\n";
			}
			elsif ($t eq 'Datum' && $f eq 'constvalue') {
				print $cf q?
	if (from->constbyval || from->constisnull)
	{
		/*
		 * passed by value so just copy the datum. Also, don't try to copy
		 * struct when value is null!
		 */
		newnode->constvalue = from->constvalue;
	}
	else
	{
		/*
		 * passed by reference.  We need a palloc'd copy.
		 */
		newnode->constvalue = datumCopy(from->constvalue,
										from->constbyval,
										from->constlen);
	}

?;
			}
			elsif ($f =~ /_cache/)
			{
				# XXX hack for RestrictInfo.scansel_cache
				print $cf "\t/* skip: $f */\n";
			}
			#elsif ($t =~ /(\w+)\*/)
			elsif ($t =~ /(\w+)\*/ && ($1 ~~ @node_types || $1 eq 'Node' || $1 eq 'Expr'))
			{
				print $cf "\tCOPY_NODE_FIELD($f);\n";
				$last_array_size_field = "list_length(from->$f)" if $t eq 'List*';
			}
			elsif ($t =~ /\w+\[/)
			{
				# COPY_SCALAR_FIELD might work for these, but let's not assume that
				print $cf "\tmemcpy(newnode->$f, from->$f, sizeof(newnode->$f));\n";
			}
			else
			{
				print $cf "\tabort();\t/* TODO: ($t) $f */\n";
			}
		}
	}

	print $cf "
\treturn newnode;
}
";
}

print $cf "
void *
copyObjectImpl(const void *from)
{
\tvoid\t   *retval;

\tif (from == NULL)
\t\treturn NULL;

\t/* Guard against stack overflow due to overly complex expressions */
\tcheck_stack_depth();

\tswitch (nodeTag(from))
\t{
";

foreach my $n (@node_types)
{
	next if $n ~~ @abstract;
	next if grep { $_ eq $n } @no_copy;
	next if $n eq 'Value';

	print $cf "
\t\tcase T_${n}:
\t\t\tretval = _copy${n}(from);
\t\t\tbreak;";
}

print $cf "
\t\tcase T_Integer:
\t\tcase T_Float:
\t\tcase T_String:
\t\tcase T_BitString:
\t\tcase T_Null:
\t\t\tretval = _copyValue(from);
\t\t\tbreak;";

print $cf "
\t\tcase T_List:
\t\t\tretval = list_copy_deep(from);
\t\t\tbreak;
\t\tcase T_IntList:
\t\tcase T_OidList:
\t\t\tretval = list_copy(from);
\t\t\tbreak;";

print $cf "
\t\tdefault:
\t\t\telog(ERROR, \"unrecognized node type: %d\", (int) nodeTag(from));
\t\t\tretval = 0;\t\t\t/* keep compiler quiet */
\t\t\tbreak;
\t}

\treturn retval;
}
";

close $cf;
