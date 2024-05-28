#include <Rcpp.h>
#include <unordered_map>
#include <string>

//' Generate Unique Pairs Up To a Number
//'
//' This function generates all unique pairs of integers up to a given number.
//'
//' @param x An integer specifying the upper limit for pairs.
//' @param oneIndexed A logical indicating whether the pairs should be one-indexed. Default is TRUE.
//' 
//' @return A list of unique pairs of integers up to the specified number.
//' @export
//' @keywords iteration
//' @examples
//' # Generate unique pairs up to 3 (one-indexed)
//' getUniquePairsUpTo(3)
//' # Generate unique pairs up to 3 (zero-indexed)
//' getUniquePairsUpTo(3, oneIndexed = FALSE)
// [[Rcpp::export]]
std::vector<std::vector<int>> getUniquePairsUpTo(int x, bool oneIndexed = true) {

    if (x <= 1) return std::vector<std::vector<int>>();
    std::vector<std::vector<int>> uniquePairList (x * (x - 1) / 2);

    int index = 0;
    for (int i = oneIndexed; i < x - 1 + oneIndexed; i++)  {
        for (int j = i + 1; j < x + oneIndexed; j++) {
            uniquePairList[index] = {i, j};
            index++;
        }
    }

    return uniquePairList;
}
