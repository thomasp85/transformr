#include <Rcpp.h>

using namespace Rcpp;

//[[Rcpp::export]]
IntegerVector fill_down(LogicalVector nas) {
  int i;
  int last_na = 0;
  IntegerVector indexes(nas.size());

  for (i = 0; i < nas.size(); ++i) {
    if (!nas[i]) {
      last_na = i + 1;
    }
    indexes[i] = last_na;
  }
  for (i = 0; i < nas.size(); ++i) {
    if (indexes[i] != 0) break;
    indexes[i] = last_na;
  }

  return indexes;
}
