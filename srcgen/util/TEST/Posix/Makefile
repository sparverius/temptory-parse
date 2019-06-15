######
#
# A simple Makefile
#
######

TEMPCC=$(TEMPTORY)/bin/tempacc
TEMPOPT=$(TEMPTORY)/bin/tempopt
MYTEMPACC=$(TEMPTORY)/bin/mytempacc

######

all::

######

all:: \
test_cblist

test_cblist: \
test_cblist_dats
test_cblist_dats: \
test_cblist.dats; $(MYTEMPACC) $<
regress:: \
test_cblist_dats; ./test_cblist_dats
cleanall:: ; $(RMF) test_cblist_dats

######

testall:: all
testall:: regress
testall:: cleanall

######

RMF=rm -f

######

###### end of [Makefile] ######
