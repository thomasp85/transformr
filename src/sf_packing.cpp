#include <Rcpp.h>
using namespace Rcpp;

CharacterVector MULTIPOINTCLASS = CharacterVector::create("XY", "MULTIPOINT", "sfg");
CharacterVector MULTILINESTRINGCLASS = CharacterVector::create("XY", "MULTILINESTRING", "sfg");
CharacterVector MULTIPOLYGONCLASS = CharacterVector::create("XY", "MULTIPOLYGON", "sfg");

Shelter<SEXP> shelter;

//[[Rcpp::export]]
List unpack_sf(List sf, CharacterVector type) {
  List res(sf.size());
  int i;
  for (i = 0; i < sf.size(); ++i) {
    if (type[i] == "POINT") {
      NumericVector coord = sf[i];
      DataFrame new_coord = DataFrame::create(
        _["x"] = coord[0],
        _["y"] = coord[1]
      );
      res[i] = new_coord;
    } else if (type[i] == "MULTIPOINT") {
      NumericMatrix coord = sf[i];
      DataFrame new_coord = DataFrame::create(
        _["x"] = coord(_, 0),
        _["y"] = coord(_, 1)
      );
      res[i] = new_coord;
    } else if (type[i] == "LINESTRING") {
      NumericMatrix coord = sf[i];
      DataFrame new_coord = DataFrame::create(
        _["x"] = coord(_, 0),
        _["y"] = coord(_, 1)
      );
      res[i] = List::create(List::create(new_coord));
    } else if (type[i] == "MULTILINESTRING") {
      List coord_list = sf[i];
      List new_coord_list(coord_list.size());
      for (int j = 0; j < coord_list.size(); ++j) {
        NumericMatrix coord = coord_list[j];
        DataFrame new_coord = DataFrame::create(
          _["x"] = coord(_, 0),
          _["y"] = coord(_, 1)
        );
        new_coord_list[j] = List::create(new_coord);
      }
      res[i] = new_coord_list;
    } else if (type[i] == "POLYGON") {
      List coord_list = sf[i];
      List new_coord_list(coord_list.size());
      for (int j = 0; j < coord_list.size(); ++j) {
        NumericMatrix coord = coord_list[j];
        coord = coord(Range(0, coord.nrow() - 2), _);
        DataFrame new_coord = DataFrame::create(
          _["x"] = coord(_, 0),
          _["y"] = coord(_, 1)
        );
        new_coord_list[j] = new_coord;
      }
      res[i] = List::create(new_coord_list);
    } else if (type[i] == "MULTIPOLYGON") {
      List coord_list = sf[i];
      List new_coord_list(coord_list.size());
      for (int j = 0; j < coord_list.size(); ++j) {
        List polygon = coord_list[j];
        List new_polygon(polygon.size());
        for (int k = 0; k < polygon.size(); ++k) {
          NumericMatrix coord = polygon[k];
          coord = coord(Range(0, coord.nrow() - 2), _);
          DataFrame new_coord = DataFrame::create(
            _["x"] = coord(_, 0),
            _["y"] = coord(_, 1)
          );
          new_polygon[k] = new_coord;
        }
        new_coord_list[j] = new_polygon;
      }
      res[i] = new_coord_list;
    } else {
      stop("Unknown geometry type");
    }
  }
  return res;
}
NumericMatrix make_point(NumericVector &x, NumericVector &y, std::vector< std::vector<int> > &start) {
  int first = start[0][0];
  int last = start.back()[0];
  int size = last - first;
  NumericMatrix res(size, 2);

  res(_, 0) = x[Range(first, last - 1)];
  res(_, 1) = y[Range(first, last - 1)];
  res.attr("class") = MULTIPOINTCLASS;

  return res;
}
List make_path(NumericVector &x, NumericVector &y, std::vector< std::vector<int> > &start) {
  std::vector<NumericMatrix> res;
  int i, j;
  for (i = 0; i < start.size() - 1; ++i) {
    for (j = 0; j < start[i].size(); ++j) {
      int first = start[i][j];
      int last = j == start[i].size() - 1 ? start[i + 1][0] : start[i][j + 1] - 1;
      NumericMatrix path(last - first, 2);
      path(_, 0) = x[Range(first, last - 1)];
      path(_, 1) = y[Range(first, last - 1)];
      res.push_back(path);
    }
  }
  List res_final = wrap(res);
  res_final.attr("class") = MULTILINESTRINGCLASS;

  return res_final;
}
List make_polygon(NumericVector &x, NumericVector &y, std::vector< std::vector<int> > &start) {
  std::vector<List> res;
  std::vector<NumericMatrix> poly;

  int i, j, k;
  for (i = 0; i < start.size() - 1; ++i) {
    for (j = 0; j < start[i].size(); ++j) {
      int first = start[i][j];
      int last = j == start[i].size() - 1 ? start[i + 1][0] : start[i][j + 1] - 1;
      NumericMatrix polygon(last - first + 1, 2);
      for (k = 0; k + first < last; ++k) {
        polygon(k, 0) = x[k + first];
        polygon(k, 1) = y[k + first];
      }
      polygon(k, 0) = polygon(0, 0);
      polygon(k, 1) = polygon(0, 1);
      poly.push_back(polygon);
    }
    List poly_list = wrap(poly);
    res.push_back(poly_list);
    poly.clear();
  }

  List res_final = wrap(res);
  res_final.attr("class") = MULTIPOLYGONCLASS;

  return res_final;
}
//[[Rcpp::export]]
List repack_sf(DataFrame df, CharacterVector type, int n_frames) {
  List res(type.size() * n_frames);
  IntegerVector id = df["sf_id"];
  IntegerVector geo_id = df["id"];
  IntegerVector frame = df[".frame"];
  NumericVector x = df["x"];
  NumericVector y = df["y"];
  int i;
  int k = 0;
  int current_id = id[0];
  int current_geo_id = geo_id[0];
  int current_frame = frame[0];
  bool last;
  std::string current_type = as<std::string>(type[current_id - 1]);
  std::vector< std::vector<int> > start_ind_all;
  std::vector<int> start_ind;
  start_ind.push_back(0);

  for (i = 1; i <= df.nrow(); ++i) {
    last = i == df.nrow();
    if (!last && current_id == id[i] && current_frame == frame[i]) {
      if (current_geo_id != geo_id[i]) {
        start_ind_all.push_back(start_ind);
        start_ind = {i};
        current_geo_id = geo_id[i];
      } else if (R_IsNA(x[i])) {
        start_ind.push_back(++i);
      }
    } else {
      start_ind_all.push_back(start_ind);
      start_ind = {i};
      start_ind_all.push_back(start_ind);
      if (current_type == "POINT" || current_type == "MULTIPOINT") {
        res[k++] = make_point(x, y, start_ind_all);
      } else if (current_type == "LINESTRING" || current_type == "MULTILINESTRING") {
        res[k++] = make_path(x, y, start_ind_all);
      } else if (current_type == "POLYGON" || current_type == "MULTIPOLYGON") {
        res[k++] = make_polygon(x, y, start_ind_all);
      }
      if (!last) {
        current_id = id[i];
        current_frame = frame[i];
        current_type = as<std::string>(type[current_id - 1]);
        current_geo_id = geo_id[i];
        start_ind_all.clear();
        start_ind = {i};
      }
    }
  }
  return res;
}
