#include <Rcpp.h>
using namespace Rcpp;

// allTuplesSorted(NA_integer_, matrix(3L), 10L) evaluates to TRUE.
//' An internal helper function to check that each tuple is sorted in 
//' increasing order; only used for tuples of size > 2.
//' 
//' @param pos1 An integer vector.
//' @param internal_pos An integer matrix.
//' @param posm An integer vector.
//' 
//' @keywords internal
//' 
//' @return TRUE if each tuple is sorted in strictly increasing order, 
//' FALSE otherwise.
//' 
//' @details 
//' .allTuplesSorted is adapted from http://stackoverflow.com/a/7601857.
//' Strict inequalities are required.
//' 
//' @keywords internal
//' 
// [[Rcpp::export(".allTuplesSortedCpp")]]
bool allTuplesSorted(IntegerVector pos1, IntegerMatrix internal_pos,  
                     IntegerVector posm) {
  int nrow = internal_pos.nrow(), ncol = internal_pos.ncol();
  for (int i = 0; i < nrow; i++) {
    if (pos1[i] >= internal_pos(i, 0)) {
      return false;
    }
    for (int j = 0; j < ncol - 1; j++) {
      if (internal_pos(i, j) >= internal_pos(i, j + 1)) {
        return false;
      }
    }
    if (internal_pos(i, ncol - 1) >= posm[i]) {
      return false;
    }
  }
  return true;
}
