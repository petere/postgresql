#-------------------------------------------------------------------------
#
# Makefile--
#    Makefile for backend/nodes
#
# IDENTIFICATION
#    src/backend/nodes/Makefile
#
#-------------------------------------------------------------------------

subdir = src/backend/nodes
top_builddir = ../../..
include $(top_builddir)/src/Makefile.global

OBJS = \
	bitmapset.o \
	copyfuncs2.o \
	equalfuncs.o \
	extensible.o \
	list.o \
	makefuncs.o \
	nodeFuncs.o \
	nodes.o \
	outfuncs.o \
	params.o \
	print.o \
	read.o \
	readfuncs.o \
	tidbitmap.o \
	value.o

include $(top_srcdir)/src/backend/common.mk

nodefiles = \
	nodes/nodes.h \
	nodes/execnodes.h \
	nodes/plannodes.h \
	nodes/primnodes.h \
	nodes/pathnodes.h \
	nodes/memnodes.h \
	nodes/pg_list.h \
	nodes/extensible.h \
	nodes/parsenodes.h \
	nodes/replnodes.h \
	commands/trigger.h \
	commands/event_trigger.h \
	nodes/tidbitmap.h \
	foreign/fdwapi.h \
	access/amapi.h \
	access/tableam.h \
	access/tsmapi.h \
	utils/rel.h \
	nodes/supportnodes.h \
	executor/tuptable.h \
	nodes/lockoptions.h \
	access/sdir.h

node-stuff:
	$(PERL) gen_node_stuff.pl $(addprefix ../../include/,$(nodefiles)) ../utils/mmgr/aset.c ../utils/mmgr/generation.c ../utils/mmgr/slab.c ../executor/nodeWindowAgg.c
	cd $(top_srcdir)/src/include/nodes && rm -f nodetags.h && ln -s ../../backend/nodes/nodetags.h .
