#include <Rcpp.h>
using namespace Rcpp;

//' An internal helper function used in finding duplicate genomic tuples.
//'
//' The tuples should have already been converted to integer representations,
//' namely an integer vector for the seqnames, an integer vector 
//' for the strand and an integer matrix for the  positions.
//' 
//' @note \strong{WARNING}: This function only works for tuples up to size 18 
//' (seqnames + strand + pos1, ..., pos18). It uses a naive hash function of 
//' each tuple to identify **candidate** tuples, which much then be checked 
//' using another method to confirm that they are indeed duplicates. The hash
//' function is the inner product of the tuple (in integer representation) with 
//' a vector of prime numbers. The choice of prime numbers is arbitrary but 
//' seems to work well in practice (certainly much better than using )
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
// [[Rcpp::export(".tmp")]]
LogicalVector tmp(IntegerVector int_seqnames, 
IntegerVector int_strand, 
IntegerMatrix int_pos) {
  
  // use (upto) 20 prime numbers (seems to work well, although 
  // no serious justification for it) 
  // TODO: Check for integer overflow
  NumericVector primes = NumericVector::create(3433.0, 3449.0, 3457.0, 3461.0, 
  3463.0, 3467.0, 3469.0, 3491.0, 3499.0, 3511.0, 3517.0, 3527.0, 3529.0, 
  3533.0, 3539.0, 3541.0, 3547.0, 3557.0, 3559.0, 3571.0);
  
  int nr = int_pos.nrow();
  int nc = int_pos.ncol();
  LogicalVector val(nr);
  NumericVector hash(nr);
  
  // Compute a hash of each tuple, which is just the sum of the tuples integer
  // representation.
  // TODO: Find a better hash function that results in fewer collisions and 
  // thus reduces the number of tuples that need to be explicitly compared with 
  // base::duplicated.array
  for (int i = 0; i < nr; i++) {
    int row_total = int_seqnames[i] * primes[0] + int_strand[i] * primes[1];
    for (int j = 0; j < nc; j++) {
      row_total += int_pos(i, j) * primes[j + 2];
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
