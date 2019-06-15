######
#
# A simple Makefile
#
######

PATSCC=$(TEMPTORY)/bin/tempacc
PATSOPT=$(TEMPTORY)/bin/tempopt
MYATSCC=$(TEMPTORY)/bin/mytempacc

######

all::

######

# all:: \
# test_symbol
# test_symbol: \
# test_symbol_dats
# test_symbol_dats: \
# test_symbol.dats; $(MYATSCC) $<
# regress:: test_symbol_dats; ./test_symbol_dats
# cleanall:: ; $(RMF) test_symbol_dats

######

# all:: \
# test_filepath
# test_filepath: \
# test_filepath_dats
# test_filepath_dats: \
# test_filepath.dats; $(MYATSCC) $<
# regress:: test_filepath_dats; ./test_filepath_dats
# cleanall:: ; $(RMF) test_filepath_dats

######

all:: \
xats
xats: \
xats_dats
xats_dats: \
xats.dats; \
$(PATSCC) -DATS_MEMALLOC_LIBC -I./.. $< -o $@
regress:: xats_dats; ./xats_dats
cleanall:: ; $(RMF) xats_dats

######

testall:: all
testall:: regress
testall:: cleanall

######

RMF=rm -f

######

cleanall:: ; $(RMF) *_dats.c
cleanall:: ; $(RMF) *~

######

###### end of [Makefile] ######
