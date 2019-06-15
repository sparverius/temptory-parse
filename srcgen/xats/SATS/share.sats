#define List0 list1_0
#define List0_vt list1_0_vt
#define Option optn1
#define Option_vt optn1_vt

#staload "libats/SATS/stdio.sats"

typedef print_type(x:tflt) = x -> void
typedef prerr_type(x:tflt) = x -> void
typedef fprint_type(x:tflt) = (FILEref, x) -> void
