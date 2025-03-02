WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub

PACKAGES := $(PACKAGES),pa_ppx_stream_seq,ounit2,pa_ppx.utils,pa_ppx_stream_seq.link,pa_ppx.testutils,pp-binary-ints,bos

ML= \
	pa_ppx_stream_seq_test.ml \

ALLSRC=$(ML)
ALLPACKAGES=$(PACKAGES)

TESTS=pa_ppx_stream_seq_test.exe

all: $(TESTS)

test:: all
	mkdir -p _build && ./pa_ppx_stream_seq_test.exe

pa_ppx_stream_seq_test.exe: pa_ppx_stream_seq_test.cmo
	$(LAUNCH) $(OCAMLFIND) ocamlc -linkall -linkpkg $(OCAMLCFLAGS) -package $(PACKAGES) -linkpkg -linkall -o $@ $<

clean::
	rm -rf *.cm* _build *.exe

