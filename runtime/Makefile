# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub

DESTDIR=

PACKAGES := fmt,camlp5,pa_ppx.base
TARGET_PACKAGE = pa_ppx_stream_seq_runtime

ML = pa_ppx_stream_seq_runtime.ml
TOOLSML = mk_meta.ml
MLI := pa_ppx_stream_seq_runtime.mli
ALLSRC=$(ML) $(MLI) $(TOOLSML)
ALLPACKAGES=$(PACKAGES)
TARGET_INSTALL_FILES=$(FILES_TARGET_INSTALL_FILES)

all: $(CMO) $(CMX) META
	$(MAKE) DESTDIR=$(WD)/$(TOP)/local-install/ install

doc: $(CMT) $(CMTI)

install:: default_install

clean::
	rm -rf META mk_meta mk_meta.ml
