#include <Rcpp.h>
using namespace Rcpp;

//' An internal helper function used in finding duplicate genomic tuples.
//'
//' The tuples should have already been converted to integer representations,
//' namely an integer vector for the seqnames, an integer vector 
//' for the strand and an integer matrix for the  positions.
//' 
//' @param int_seqnames An integer vector of length n. An integer 
//' representation of the seqnames of each m-tuple.
//' @param int_strand An integer vector of length n. An integer representation 
//' of the strand of each m-tuple.
//' @param int_pos An integer matrix with n rows. Each row represents the 
//' difference in positions of each tuple.
//' 
//' @return A logical vector. TRUE if the corresponding tuple is a candidate 
//' duplicate and FALSE otherwise. 
//' \emph{NOTE:} TRUE does not mean that it is a duplicate, merely that it is a 
//' candidate. Further checking is required of these candidates, e.g. using the 
//' \code{\link[base]{duplicated.array}} method.
//' 
// [[Rcpp::export(".candidateDuplicateGTuples")]]
LogicalVector candidateDuplicateGTuples(IntegerVector int_seqnames, 
                                        IntegerVector int_strand, 
                                        IntegerMatrix int_pos) {
  
  int nr = int_pos.nrow();
  int nc = int_pos.ncol();
  LogicalVector val(nr);
  IntegerVector hash(nr);
  
  // Compute a hash of each tuple, which is just the sum of the tuples integer
  // representation.
  // TODO: Find a better hash function that results in fewer collisions and 
  // thus reduces the number of tuples that need to be explicitly compared with 
  // base::duplicated.array
  for (int i = 0; i < nr; i++){
    int row_total = int_seqnames[i] + int_strand[i];
    for (int j = 0; j < nc; j++){
      row_total += int_pos(i, j);
    }
    hash[i] = row_total;
  }
  
  // Check for duplicates in hash
  // rev(duplicated(rev(hash))) is equivalent to 
  // duplicated(hash, fromLast = TRUE)
  val = duplicated(hash) | rev(duplicated(rev(hash)));
  
  // WARNING: Still need to check candidate duplicates to see if they are truly
  // duplicate m-tuples. This is currently done outside of this function.
  
  return val;
}
