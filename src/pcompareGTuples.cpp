#include <Rcpp.h>
using namespace Rcpp;

//' An internal function used to compare GTuples.
//' 
//' @details 
//' The tuples should have already been converted to integer representations,
//' namely an integer vector for the difference in chromosome, an integer 
//' vector for the difference in strand and an integer matrix for the 
//' difference in  positions.
//' 
//' @param int_seqnames An integer vector of length n. An integer 
//' representation of the difference in seqnames of each tuple.
//' @param int_strand An integer vector of length n. An integer representation 
//' of the difference in strand of each tuple.
//' @param int_pos An integer matrix with n rows. Each row represents the 
//' difference in positions of each tuple. 
//' 
//' @return An integer vector where each element is the comparison of a pair
//' of tuples. If the first tuple in the pair is "<" than the second tuple then 
//' the return value for that element is < 0, if the first tuple in the pair is 
//' "==" the second tuple then the return value is 0, and if the first tuple is 
//' ">" that the second tuple then the return value is > 0.
//' 
//' @keywords internal
//'
//' 
// [[Rcpp::export(".pcompareGTuplesCpp")]]
IntegerVector pcompareGTuples(IntegerVector int_seqnames, 
                              IntegerVector int_strand, 
                              IntegerMatrix int_pos) {
  int n = int_seqnames.size();
  IntegerVector val(n);
  int nc = int_pos.ncol();
  
  for (int i = 0; i < n; i++) {
    if (int_seqnames[i] != 0) {
      val[i] = int_seqnames[i];
      continue;
    }
    if (int_strand[i] != 0) {
      val[i] = int_strand[i];
      continue;
    }
    // Have to do it in this weird order otherwise can't "continue" the outer 
    // loop
    val[i] = int_pos(i, nc - 1); 
    for (int j = 0; j < (nc - 1); j++) {
            if (int_pos(i, j) != 0) {
        val[i] = int_pos(i, j);
        break;
      }
    }
  }
  return val;
}
