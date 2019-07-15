typedef SHR(a:type) = a // for commenting purpose
typedef NSH(a:type) = a // for commenting purpose

(* ****** ****** *)

abstype
fprecision_prop (a1:t0p, a2:t0p)
propdef
fprecision (a1:t0p, a2:t0p) = fprecision_prop (a1, a2)
praxi
fprecision_float (): fprecision (float, float)
praxi
fprecision_double (): fprecision (double, double)
praxi
fprecision_ldouble (): fprecision (ldouble, ldouble)
