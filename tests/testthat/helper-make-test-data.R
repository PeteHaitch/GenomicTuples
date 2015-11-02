### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### GTuples objects used in tests
###

seqinfo <- Seqinfo(paste0("chr", 1:3), c(1000, 2000, 1500), NA, "mock1")
# Empty GTuples object
gt0 <- GTuples(seqinfo = seqinfo)
# 1-tuples
gt1 <- GTuples(seqnames = Rle(c("chr1", "chr2", "chr1", "chr3"),
                              c(1, 3, 2, 4)),
               tuples = matrix(c(1:10), ncol = 1),
               strand = Rle(strand(c("-", "+", "*", "+", "-")),
                            c(1, 2, 2, 3, 2)),
               score = 1:10, seqinfo = seqinfo)
# 2-tuples
gt2 <- GTuples(seqnames = Rle(c("chr1", "chr2", "chr1", "chr3"),
                              c(1, 3, 2, 4)),
               tuples = matrix(c(1:10, 2:11), ncol = 2),
               strand = Rle(strand(c("-", "+", "*", "+", "-")),
                            c(1, 2, 2, 3, 2)),
               score = 1:10, seqinfo = seqinfo)
# 3-tuples
gt3 <- GTuples(seqnames = Rle(c("chr1", "chr2", "chr1", "chr3"),
                              c(1, 3, 2, 4)),
               tuples = matrix(c(1:10, 2:11, 3:12), ncol = 3),
               strand = Rle(strand(c("-", "+", "*", "+", "-")),
                            c(1, 2, 2, 3, 2)),
               score = 1:10, seqinfo = seqinfo)

# Construct a set of 3-tuples with "interesting" overlaps
q3 <- c(GTuples('chr1', matrix(as.integer(c(1, 1, 1, 3, 3, 5, 7, 9, 7)), 
                               ncol = 3), strand = '*'),
        GTuples('chr1', matrix(as.integer(c(1, 1, 1, 3, 3, 5, 7, 9, 7)), 
                               ncol = 3), strand = '+'),
        GTuples('chr1', matrix(as.integer(c(1, 1, 1, 3, 3, 5, 7, 9, 7)), 
                               ncol = 3), strand = '-'))
# 4-tuples
gt4 <- GTuples(seqnames = Rle(c("chr1", "chr2", "chr1", "chr3"),
                              c(1, 3, 2, 4)),
               tuples = matrix(c(1:10, 2:11, 3:12, 4:13), ncol = 4),
               strand = Rle(strand(c("-", "+", "*", "+", "-")),
                            c(1, 2, 2, 3, 2)),
               score = 1:10, seqinfo = seqinfo)
# Construct a set of 4-tuples with "interesting" overlaps
q4 <- c(GTuples('chr1', matrix(as.integer(c(1, 1, 1, 1, 3, 3, 3, 7, 
                                            7, 7, 9, 9, 9, 11, 11, 11)), 
                               ncol = 4), strand = '*'),
        GTuples('chr1', matrix(as.integer(c(1, 1, 1, 1, 3, 3, 3, 7, 
                                            7, 7, 9, 9, 9, 11, 11, 11)), 
                               ncol = 4), strand = '+'),
        GTuples('chr1', matrix(as.integer(c(1, 1, 1, 1, 3, 3, 3, 7, 
                                            7, 7, 9, 9, 9, 11, 11, 11)), 
                               ncol = 4), strand = '-'))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### GTuplesList objects used in tests
###

gtl0 <- GTuplesList(A = gt0, B = gt0)
gtl1 <- GTuplesList(A = gt1[1:5], B = gt1[6:10])
gtl2 <- GTuplesList(A = gt2[1:5], B = gt2[6:10])
gtl3 <- GTuplesList(A = gt3[1:5], B = gt3[6:10])
gtl4 <- GTuplesList(A = gt4[1:5], B = gt4[6:10])

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The above GTuples and GTuplesList objects coerced to GRanges
###
### Used in tests of findOverlaps-based functions
gr0 <- as(gt0, "GRanges")
gr1 <- as(gt1, "GRanges")
gr2 <- as(gt2, "GRanges")
gr3 <- as(gt3, "GRanges")
q3_gr <- as(q3, "GRanges")
gr4 <- as(gt4, "GRanges")
q4_gr <- as(q4, "GRanges")

grl0 <- as(gtl0, "GRangesList")
grl1 <- as(gtl1, "GRangesList")
grl2 <- as(gtl2, "GRangesList")
grl3 <- as(gtl3, "GRangesList")
grl4 <- as(gtl4, "GRangesList")
