// Generated by cpp11: do not edit by hand
// clang-format off


#include "cpp11/declarations.hpp"
#include <R_ext/Visibility.h>

// align_rings.cpp
cpp11::integers find_splits_c(cpp11::doubles lengths, int n);
extern "C" SEXP _transformr_find_splits_c(SEXP lengths, SEXP n) {
  BEGIN_CPP11
    return cpp11::as_sexp(find_splits_c(cpp11::as_cpp<cpp11::decay_t<cpp11::doubles>>(lengths), cpp11::as_cpp<cpp11::decay_t<int>>(n)));
  END_CPP11
}
// align_rings.cpp
cpp11::writable::list insert_points_c(cpp11::doubles x, cpp11::doubles y, cpp11::integers splits, int n);
extern "C" SEXP _transformr_insert_points_c(SEXP x, SEXP y, SEXP splits, SEXP n) {
  BEGIN_CPP11
    return cpp11::as_sexp(insert_points_c(cpp11::as_cpp<cpp11::decay_t<cpp11::doubles>>(x), cpp11::as_cpp<cpp11::decay_t<cpp11::doubles>>(y), cpp11::as_cpp<cpp11::decay_t<cpp11::integers>>(splits), cpp11::as_cpp<cpp11::decay_t<int>>(n)));
  END_CPP11
}
// align_rings.cpp
int rotate_c(cpp11::doubles poly_x, cpp11::doubles poly_y, cpp11::doubles ref_x, cpp11::doubles ref_y);
extern "C" SEXP _transformr_rotate_c(SEXP poly_x, SEXP poly_y, SEXP ref_x, SEXP ref_y) {
  BEGIN_CPP11
    return cpp11::as_sexp(rotate_c(cpp11::as_cpp<cpp11::decay_t<cpp11::doubles>>(poly_x), cpp11::as_cpp<cpp11::decay_t<cpp11::doubles>>(poly_y), cpp11::as_cpp<cpp11::decay_t<cpp11::doubles>>(ref_x), cpp11::as_cpp<cpp11::decay_t<cpp11::doubles>>(ref_y)));
  END_CPP11
}
// fill_down.cpp
cpp11::integers fill_down(cpp11::logicals nas);
extern "C" SEXP _transformr_fill_down(SEXP nas) {
  BEGIN_CPP11
    return cpp11::as_sexp(fill_down(cpp11::as_cpp<cpp11::decay_t<cpp11::logicals>>(nas)));
  END_CPP11
}
// sf_packing.cpp
cpp11::writable::list unpack_sf(cpp11::list sf, cpp11::strings type);
extern "C" SEXP _transformr_unpack_sf(SEXP sf, SEXP type) {
  BEGIN_CPP11
    return cpp11::as_sexp(unpack_sf(cpp11::as_cpp<cpp11::decay_t<cpp11::list>>(sf), cpp11::as_cpp<cpp11::decay_t<cpp11::strings>>(type)));
  END_CPP11
}
// sf_packing.cpp
cpp11::writable::list repack_sf(cpp11::data_frame df, cpp11::strings type, int n_frames);
extern "C" SEXP _transformr_repack_sf(SEXP df, SEXP type, SEXP n_frames) {
  BEGIN_CPP11
    return cpp11::as_sexp(repack_sf(cpp11::as_cpp<cpp11::decay_t<cpp11::data_frame>>(df), cpp11::as_cpp<cpp11::decay_t<cpp11::strings>>(type), cpp11::as_cpp<cpp11::decay_t<int>>(n_frames)));
  END_CPP11
}

extern "C" {
static const R_CallMethodDef CallEntries[] = {
    {"_transformr_fill_down",       (DL_FUNC) &_transformr_fill_down,       1},
    {"_transformr_find_splits_c",   (DL_FUNC) &_transformr_find_splits_c,   2},
    {"_transformr_insert_points_c", (DL_FUNC) &_transformr_insert_points_c, 4},
    {"_transformr_repack_sf",       (DL_FUNC) &_transformr_repack_sf,       3},
    {"_transformr_rotate_c",        (DL_FUNC) &_transformr_rotate_c,        4},
    {"_transformr_unpack_sf",       (DL_FUNC) &_transformr_unpack_sf,       2},
    {NULL, NULL, 0}
};
}

extern "C" attribute_visible void R_init_transformr(DllInfo* dll){
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}