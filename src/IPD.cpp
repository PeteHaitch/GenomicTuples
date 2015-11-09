#include <Rcpp.h>
using namespace Rcpp;

//' An internal function to compute the IPD of a GTuples; ; only used for 
//' tuples of size > 2.
//' 
//' @param pos1 An integer vector.
//' @param internal_pos An integer matrix.
//' @param posm An integer vector.
//' 
//' @keywords internal
//'
//' @return An integer matrix with the same number of rows as internal_pos and 
//' number of columns equal to size - 1.
//' 
//' @note This function silently coerces numeric matrices to integer matrices
//' and does integer subtraction. \emph{This will give unexpected results but
//' it's not a problem for me since I only use it on integer matrices.}
// [[Rcpp::export(".IPDCpp")]]
IntegerMatrix IPD(IntegerVector pos1, IntegerMatrix internal_pos, 
                  IntegerVector posm) {
  int nrow = internal_pos.nrow(), ncol = internal_pos.ncol();
  IntegerMatrix D(nrow, ncol + 1);
  for (int i = 0; i < nrow; i++) {
    D(i, 0) = internal_pos(i, 0) - pos1[i];
    for (int j = 0; j < ncol - 1; j++) {
      D(i, j + 1) = internal_pos(i, j + 1) - internal_pos(i, j);
    }
    D(i, ncol) = posm[i] - internal_pos(i, ncol - 1);
  }
  return D;
}
