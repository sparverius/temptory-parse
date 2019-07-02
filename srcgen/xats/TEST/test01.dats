abstflt Foo

sortdef foo = tflt -> tflt

absprop gseq
typedef cs = string
typedef c0 = char


#extern fun{pf:prop}{xs:tflt;x0:tflt} gseq_rforall(xs): bool
#extern fun{x0:tflt} gseq_rforall$test : x0 -> bool //(x0): bool
#extern fun{} string0_rforall(cs): bool
#extern fun{} string0_rforall$test(c0): bool

impltmp
gseq_rforall<gseq><cs,c0>(cs) =
  string0_rforall<>(cs)
  where
    impltmp
    string0_rforall$test<>(c0) =
      gseq_rforall$test<c0>(c0)
  end
