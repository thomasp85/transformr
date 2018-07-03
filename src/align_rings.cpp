#include <Rcpp.h>
using namespace Rcpp;

//[[Rcpp::export]]
IntegerVector find_splits(NumericVector lengths, int n) {
  double total = 0;
  int i,j;
  IntegerVector splits(lengths.size(), 0);

  for (i = 0; i < lengths.size() - 1; ++i) {
    total += lengths[i];
    if (total / (n + 1) > lengths[i + 1]) break;
  }
  if (i == lengths.size() - 1) total += lengths[i];

  int n_remain = n;
  j = 0;
  while (n_remain > 0) {
    if (j >= splits.size()) {
      splits[0] += n_remain;
      break;
    }
    int split = std::round(n * lengths[j] / total);
    if (split == 0) split = 1;
    split = split < n_remain ? split : n_remain;
    splits[j] = split;
    n_remain -= split;
    j++;
  }

  return splits;
}

//[[Rcpp::export]]
List insert_points(NumericVector x, NumericVector y, IntegerVector splits, int n) {
  std::vector<double> new_x, new_y;
  int i, j, next_i;
  double x_seg, y_seg;

  for (i = 0; i < x.size(); ++i) {
    new_x.push_back(x[i]);
    new_y.push_back(y[i]);
    if (splits[i] == 0) continue;
    next_i = i == x.size() - 1 ? 0 : i + 1;
    x_seg = (x[next_i] - x[i]) / (splits[i] + 1);
    y_seg = (y[next_i] - y[i]) / (splits[i] + 1);
    for (j = 1; j <= splits[i]; ++j) {
      new_x.push_back(x[i] + x_seg * j);
      new_y.push_back(y[i] + y_seg * j);
    }
  }

  return List::create(
    Named("x") = new_x,
    Named("y") = new_y
  );
}

//[[Rcpp::export]]
int rotate(NumericVector poly_x, NumericVector poly_y, NumericVector ref_x,
          NumericVector ref_y) {
  double best_ss = std::numeric_limits<double>::infinity();
  int i, j, j_off, best_i = 0;
  int n = poly_x.size();
  double ss,x,y;

  for (i = 0; i < n; ++i) {
    ss = 0;
    for (j = 0; j < n; ++j) {
      j_off = (i + j) % n;
      x = ref_x[j] - poly_x[j_off];
      y = ref_y[j] - poly_y[j_off];
      ss += x*x + y*y;
    }

    if (ss < best_ss) {
      best_ss = ss;
      best_i = i;
    }
  }
  return best_i + 1;
}
