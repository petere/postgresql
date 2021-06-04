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
my %my_field_attrs;

my %all_node_types;

my @no_copy;
my @no_read_write;

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

my @enum_types;

push @no_copy, qw(CallContext InlineCodeBlock);
push @no_copy, qw(AppendState);  # FIXME
push @no_copy, qw(Expr TIDBitmap);

push @no_read_write, qw(AccessPriv AlterTableCmd CallContext CreateOpClassItem FunctionParameter InferClause InlineCodeBlock ObjectWithArgs OnConflictClause PartitionCmd RoleSpec VacuumRelation);
push @no_read_write, qw(AppendState);  # FIXME
push @no_read_write, qw(TIDBitmap);


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

		if ($line =~ /^\}\s*$in_struct;$/ || $line =~ /^\};$/)
		{
			if ($is_node_struct)
			{
				push @node_types, $in_struct;
				my @f = @my_fields;
				my %ft = %my_field_types;
				my %fa = %my_field_attrs;
				if ($supertype)
				{
					my @superfields;
					foreach my $sf (@{$all_node_types{$supertype}->{fields}})
					{
						my $fn = "${supertype_field}.$sf";
						push @superfields, $fn;
						$ft{$fn} = $all_node_types{$supertype}->{field_types}{$sf};
						$fa{$fn} = $all_node_types{$supertype}->{field_attrs}{$sf};
					}
					unshift @f, @superfields;
				}
				#warn "$in_struct has no fields\n" unless scalar(@f);
				$all_node_types{$in_struct}->{fields} = \@f;
				$all_node_types{$in_struct}->{field_types} = \%ft;
				$all_node_types{$in_struct}->{field_attrs} = \%fa;

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
					$infile =~ /\.c$/
				)
				{
					push @no_copy, $in_struct;
					push @no_read_write, $in_struct;
				}

				if ($supertype && ($supertype eq 'Path' || $supertype eq 'JoinPath'))
				{
					push @no_copy, $in_struct;
				}
			}
			$in_struct = undef;
			@my_fields = ();
			%my_field_types = ();
			%my_field_attrs = ();
		}
		elsif ($line =~ /^\s*(.+)\s*\b(\w+)(\[\w+\])?\s*(?:pg_node_attr\(([\w ]*)\))?;/)
		{
			if ($is_node_struct)
			{
				my $type = $1;
				my $name = $2;
				my $array_size = $3;
				my $attr = $4;

				$type =~ s/^const\s*//;
				$type =~ s/\s*$//;
				$type =~ s/\s+\*$/*/;
				die if $type eq '';
				$type = $type . $array_size if $array_size;
				push @my_fields, $name;
				$my_field_types{$name} = $type;
				$my_field_attrs{$name} = $attr;
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
		if ($line =~ /^(?:typedef )?struct (\w+)(\s*\/\*.*)?$/ && $1 ne 'Node')
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
			my %fa = %{$all_node_types{$alias_of}->{field_attrs}};
			$all_node_types{$n}->{fields} = \@f;
			$all_node_types{$n}->{field_types} = \%ft;
			$all_node_types{$n}->{field_attrs} = \%fa;
		}
		elsif ($line =~ /^typedef enum (\w+)(\s*\/\*.*)?$/)
		{
			push @enum_types, $1;
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
push @node_types, 'TIDBitmap'; # FIXME

my @value_nodes = qw(Integer Float String BitString Null);


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


# copyfuncs.c, equalfuncs.c

open my $cf, '>', 'copyfuncs.inc1.c' or die;
open my $ef, '>', 'equalfuncs.inc1.c' or die;
open my $cf2, '>', 'copyfuncs.inc2.c' or die;
open my $ef2, '>', 'equalfuncs.inc2.c' or die;

foreach my $n (@node_types)
{
	next if $n ~~ @abstract;
	next if grep { $_ eq $n } @no_copy;
	next if $n eq 'Value';

	print $cf2 "
\t\tcase T_${n}:
\t\t\tretval = _copy${n}(from);
\t\t\tbreak;";

	print $ef2 "
\t\tcase T_${n}:
\t\t\tretval = _equal${n}(a, b);
\t\t\tbreak;";

	next if $n eq 'A_Const' || $n eq 'ExtensibleNode';

	print $cf "
static $n *
_copy${n}(const $n *from)
{
\t${n} *newnode = makeNode($n);

";

	print $ef "
static bool
_equal${n}(const $n *a, const $n *b)
{
";

	{
		my $last_array_size_field;

		foreach my $f (@{$all_node_types{$n}->{fields}})
		{
			my $t = $all_node_types{$n}->{field_types}{$f};
			my $a = $all_node_types{$n}->{field_attrs}{$f};
			my $equal_ignore = (defined($a) && $a =~ /\bequal_ignore\b/);
			if ($t eq 'char*')
			{
				print $cf "\tCOPY_STRING_FIELD($f);\n";
				print $ef "\tCOMPARE_STRING_FIELD($f);\n" unless $equal_ignore;
			}
			elsif ($t eq 'Bitmapset*' || $t eq 'Relids')
			{
				print $cf "\tCOPY_BITMAPSET_FIELD($f);\n";
				print $ef "\tCOMPARE_BITMAPSET_FIELD($f);\n" unless $equal_ignore;
			}
			elsif ($t eq 'int' && $f =~ 'location$')
			{
				print $cf "\tCOPY_LOCATION_FIELD($f);\n";
				print $ef "\tCOMPARE_LOCATION_FIELD($f);\n" unless $equal_ignore;
			}
			elsif ($t ~~ @scalar_types || $t ~~ @enum_types)
			{
				print $cf "\tCOPY_SCALAR_FIELD($f);\n";
				if (defined($a) && $a =~ /\bequal_ignore_if_zero\b/)
				{
					print $ef "\tif (a->$f != b->$f && a->$f != 0 && b->$f != 0)\n\t\treturn false;\n";
				}
				else
				{
					print $ef "\tCOMPARE_SCALAR_FIELD($f);\n" unless $equal_ignore || $t eq 'CoercionForm';
				}
				$last_array_size_field = "from->$f";
			}
			elsif ($t =~ /(\w+)\*/ && $1 ~~ @scalar_types)
			{
				my $tt = $1;
				print $cf "\tCOPY_POINTER_FIELD($f, $last_array_size_field * sizeof($tt));\n";
				(my $l2 = $last_array_size_field) =~ s/from/a/;
				print $ef "\tCOMPARE_POINTER_FIELD($f, $l2 * sizeof($tt));\n" unless $equal_ignore;
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
				print $ef q{
	/*
	 * We treat all NULL constants of the same type as equal. Someday this
	 * might need to change?  But datumIsEqual doesn't work on nulls, so...
	 */
	if (!a->constisnull && !b->constisnull &&
		!datumIsEqual(a->constvalue, b->constvalue,
					  a->constbyval, a->constlen))
		return false;
};
			}
			elsif ($f =~ /_cache/)
			{
				# XXX hack for RestrictInfo.scansel_cache
				print $cf "\t/* skip: $f */\n";
			}
			elsif ($t =~ /(\w+)\*/ && ($1 ~~ @node_types || $1 eq 'Node' || $1 eq 'Expr'))
			{
				print $cf "\tCOPY_NODE_FIELD($f);\n";
				print $ef "\tCOMPARE_NODE_FIELD($f);\n" unless $equal_ignore;
				$last_array_size_field = "list_length(from->$f)" if $t eq 'List*';
			}
			elsif ($t =~ /\w+\[/)
			{
				# COPY_SCALAR_FIELD might work for these, but let's not assume that
				print $cf "\tmemcpy(newnode->$f, from->$f, sizeof(newnode->$f));\n";
				print $ef "\tCOMPARE_POINTER_FIELD($f, sizeof(a->$f));\n" unless $equal_ignore;
			}
			else
			{
				print $cf "\tabort();\t/* TODO: ($t) $f */\n";
				print $ef "\tabort();\t/* TODO: ($t) $f */\n" unless $equal_ignore;
			}
		}
	}

	print $cf "
\treturn newnode;
}
";
	print $ef "
\treturn true;
}
";
}

close $cf;
close $ef;
close $cf2;
close $ef2;


# outfuncs.c, readfuncs.c

open my $of, '>', 'outfuncs.inc1.c' or die;
open my $rf, '>', 'readfuncs.inc1.c' or die;
open my $of2, '>', 'outfuncs.inc2.c' or die;
open my $rf2, '>', 'readfuncs.inc2.c' or die;

my %name_map = (
	'ARRAYEXPR' => 'ARRAY',
	'CASEEXPR' => 'CASE',
	'CASEWHEN' => 'WHEN',
	'COALESCEEXPR' => 'COALESCE',
	'COLLATEEXPR' => 'COLLATE',
	'DECLARECURSORSTMT' => 'DECLARECURSOR',
	'MINMAXEXPR' => 'MINMAX',
	'NOTIFYSTMT' => 'NOTIFY',
	'RANGETBLENTRY' => 'RTE',
	'ROWCOMPAREEXPR' => 'ROWCOMPARE',
	'ROWEXPR' => 'ROW',
);

my @custom_readwrite = qw(A_Const A_Expr BoolExpr Const Constraint ExtensibleNode Query RangeTblEntry Value);

foreach my $n (@node_types)
{
	next if $n ~~ @abstract;
	next if grep { $_ eq $n } @no_read_write;
	next if $n eq 'List' || $n eq 'IntList' || $n eq 'OidList';
	next if $n ~~ @value_nodes;
	next if $n eq 'Expr';

	if ($n =~ /Stmt$/)
	{
		my @keep = qw(AlterStatsStmt CreateForeignTableStmt CreateStatsStmt CreateStmt DeclareCursorStmt ImportForeignSchemaStmt IndexStmt NotifyStmt PlannedStmt PLAssignStmt RawStmt ReturnStmt SelectStmt SetOperationStmt);
		next unless $n ~~ @keep;
	}

	my $N = uc $n;
	$N =~ s/_//g;
	if ($name_map{$N})
	{
		$N = $name_map{$N};
	}

	my $no_read = 0;
	if ($n eq 'A_Star' || $n eq 'A_Const' || $n eq 'A_Expr' || $n eq 'Constraint' || $n =~ /Path$/ || $n eq 'ForeignKeyCacheInfo' || $n eq 'ForeignKeyOptInfo' || $n eq 'PathTarget' || $n eq 'Value')
	{
		$no_read = 1;
	}

	print $of2 "\t\t\tcase T_${n}:\n".
	  "\t\t\t\t_out${n}(str, obj);\n".
	  "\t\t\t\tbreak;\n";

	print $rf2 "\telse if (MATCH(\"$N\", " . length($N) . "))\n".
	  "\t\treturn_value = _read${n}();\n" unless $no_read;

	next if $n ~~ @custom_readwrite;

	print $of "
static void
_out${n}(StringInfo str, const $n *node)
{
\tWRITE_NODE_TYPE(\"$N\");

";

	print $rf "
pg_attribute_unused()
static $n *
_read${n}(void)
{
\tREAD_LOCALS($n);

" unless $no_read;

	my $last_array_size_field;

	foreach my $f (@{$all_node_types{$n}->{fields}})
	{
		my $t = $all_node_types{$n}->{field_types}{$f};
		my $a = $all_node_types{$n}->{field_attrs}{$f} || '';
		my $readwrite_ignore = (defined($a) && $a =~ /\breadwrite_ignore\b/);
		next if $readwrite_ignore;

		# XXX Previously, for subtyping, only the leaf field name is
		# used. Ponder whether we want to keep it that way.

		if ($t eq 'bool')
		{
			print $of "\tWRITE_BOOL_FIELD($f);\n";
			print $rf "\tREAD_BOOL_FIELD($f);\n" unless $no_read;
		}
		elsif ($t eq 'int' && $f =~ 'location$')
		{
			print $of "\tWRITE_LOCATION_FIELD($f);\n";
			print $rf "\tREAD_LOCATION_FIELD($f);\n" unless $no_read;
		}
		elsif ($t eq 'int' || $t eq 'int32' || $t eq 'AttrNumber' || $t eq 'StrategyNumber')
		{
			print $of "\tWRITE_INT_FIELD($f);\n";
			print $rf "\tREAD_INT_FIELD($f);\n" unless $no_read;
			$last_array_size_field = "node->$f" if $t eq 'int';
		}
		elsif ($t eq 'uint32' || $t eq 'bits32' || $t eq 'AclMode' || $t eq 'BlockNumber' || $t eq 'Index' || $t eq 'SubTransactionId')
		{
			print $of "\tWRITE_UINT_FIELD($f);\n";
			print $rf "\tREAD_UINT_FIELD($f);\n" unless $no_read;
		}
		elsif ($t eq 'uint64')
		{
			print $of "\tWRITE_UINT64_FIELD($f);\n";
			print $rf "\tREAD_UINT64_FIELD($f);\n" unless $no_read;
		}
		elsif ($t eq 'Oid')
		{
			print $of "\tWRITE_OID_FIELD($f);\n";
			print $rf "\tREAD_OID_FIELD($f);\n" unless $no_read;
		}
		elsif ($t eq 'long')
		{
			print $of "\tWRITE_LONG_FIELD($f);\n";
			print $rf "\tREAD_LONG_FIELD($f);\n" unless $no_read;
		}
		elsif ($t eq 'char')
		{
			print $of "\tWRITE_CHAR_FIELD($f);\n";
			print $rf "\tREAD_CHAR_FIELD($f);\n" unless $no_read;
		}
		elsif ($t eq 'double')
		{
			# XXX Currently, double is only used in plan and path
			# nodes for number-of-rows values, which are printed as
			# whole numbers.  If this no longer holds, maybe change
			# those to a specialized type.
			# FIXME: allvisfrac
			print $of "\tWRITE_FLOAT_FIELD($f, \"%.0f\");\n";
			print $rf "\tREAD_FLOAT_FIELD($f);\n" unless $no_read;
		}
		elsif ($t eq 'Cost')
		{
			print $of "\tWRITE_FLOAT_FIELD($f, \"%.2f\");\n";
			print $rf "\tREAD_FLOAT_FIELD($f);\n" unless $no_read;
		}
		elsif ($t eq 'QualCost')
		{
			print $of "\tWRITE_FLOAT_FIELD($f.startup, \"%.2f\");\n";
			print $of "\tWRITE_FLOAT_FIELD($f.per_tuple, \"%.2f\");\n";
			print $rf "\tREAD_FLOAT_FIELD($f.startup);\n" unless $no_read;
			print $rf "\tREAD_FLOAT_FIELD($f.per_tuple);\n" unless $no_read;
		}
		elsif ($t eq 'Selectivity')
		{
			print $of "\tWRITE_FLOAT_FIELD($f, \"%.4f\");\n";
			print $rf "\tREAD_FLOAT_FIELD($f);\n" unless $no_read;
		}
		elsif ($t eq 'char*')
		{
			print $of "\tWRITE_STRING_FIELD($f);\n";
			print $rf "\tREAD_STRING_FIELD($f);\n" unless $no_read;
		}
		elsif ($t eq 'Bitmapset*' || $t eq 'Relids')
		{
			print $of "\tWRITE_BITMAPSET_FIELD($f);\n";
			print $rf "\tREAD_BITMAPSET_FIELD($f);\n" unless $no_read;
		}
		elsif ($t ~~ @enum_types)
		{
			print $of "\tWRITE_ENUM_FIELD($f, $t);\n";
			print $rf "\tREAD_ENUM_FIELD($f, $t);\n" unless $no_read;
		}
		elsif ($t =~ /(\w+)(\*|\[)/ && $1 ~~ @scalar_types)
		{
			warn "$t $n.$f" unless $last_array_size_field;
			my $tt = uc $1;
			print $of "\tWRITE_${tt}_ARRAY($f, $last_array_size_field);\n";
			(my $l2 = $last_array_size_field) =~ s/node/local_node/;
			print $rf "\tREAD_${tt}_ARRAY($f, $l2);\n" unless $no_read;
		}
		elsif ($t eq 'RelOptInfo*' && $a eq 'path_hack1')
		{
			print $of "\tappendStringInfoString(str, \" :parent_relids \");\n".
			  "\toutBitmapset(str, node->$f->relids);\n";
		}
		elsif ($t eq 'PathTarget*' && $a eq 'path_hack2')
		{
			(my $f2 = $f) =~ s/pathtarget/parent/;
			print $of "\tif (node->$f != node->$f2->reltarget)\n".
			  "\t\tWRITE_NODE_FIELD($f);\n";
		}
		elsif ($t eq 'ParamPathInfo*' && $a eq 'path_hack3')
		{
			print $of "\tif (node->$f)\n".
			  "\t\toutBitmapset(str, node->$f->ppi_req_outer);\n".
			  "\telse\n".
			  "\t\toutBitmapset(str, NULL);\n";
		}
		elsif ($t =~ /(\w+)\*/ && ($1 ~~ @node_types || $1 eq 'Node' || $1 eq 'Expr'))
		{
			unless ($1 ~~ @no_read_write)
			{
				print $of "\tWRITE_NODE_FIELD($f);\n";
				print $rf "\tREAD_NODE_FIELD($f);\n" unless $no_read;
			}
			else
			{
				#print STDERR "*** skipping $t\n";
			}
			$last_array_size_field = "list_length(node->$f)" if $t eq 'List*';
		}
		elsif ($t eq 'struct CustomPathMethods*' ||	$t eq 'struct CustomScanMethods*')
		{
			print $of q{
	appendStringInfoString(str, " :methods ");
	outToken(str, node->methods->CustomName);
};
			print $rf q!
	{
		/* Lookup CustomScanMethods by CustomName */
		char	   *custom_name;
		const CustomScanMethods *methods;
		token = pg_strtok(&length); /* skip methods: */
		token = pg_strtok(&length); /* CustomName */
		custom_name = nullable_string(token, length);
		methods = GetCustomScanMethods(custom_name, false);
		local_node->methods = methods;
	}
! unless $no_read;
		}
		elsif ($t eq 'ParamListInfo' || $t =~ /PartitionBoundInfoData/ || $t eq 'PartitionDirectory' || $t eq 'PartitionScheme' || $t eq 'void*' || $t =~ /\*\*$/)
		{
			# ignore
		}
		else
		{
			print $of "\tabort();\t/* TODO: ($t) $f */\n";
			print $rf "\tabort();\t/* TODO: ($t) $f */\n" unless $no_read;
		}
	}

	print $of "}
";
	print $rf "
\tREAD_DONE();
}
" unless $no_read;
}

close $of;
close $rf;
close $of2;
close $rf2;
