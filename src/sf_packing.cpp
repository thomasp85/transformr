#include <cpp11/strings.hpp>
#include <cpp11/list.hpp>
#include <cpp11/doubles.hpp>
#include <cpp11/integers.hpp>
#include <cpp11/data_frame.hpp>
#include <cpp11/matrix.hpp>

#include <vector>

using namespace cpp11::literals;

cpp11::writable::strings MULTIPOINTCLASS = {"XY", "MULTIPOINT", "sfg"};
cpp11::writable::strings MULTILINESTRINGCLASS = {"XY", "MULTILINESTRING", "sfg"};
cpp11::writable::strings MULTIPOLYGONCLASS = {"XY", "MULTIPOLYGON", "sfg"};

[[cpp11::register]]
cpp11::writable::list unpack_sf(cpp11::list sf, cpp11::strings type) {
  cpp11::writable::list res(sf.size());
  int i;
  for (i = 0; i < sf.size(); ++i) {
    if (type[i] == "POINT") {
      cpp11::doubles coord = sf[i];
      cpp11::writable::data_frame new_coord({
        "x"_nm = coord[0],
        "y"_nm = coord[1]
      });
      res[i] = new_coord;
    } else if (type[i] == "MULTIPOINT") {
      cpp11::doubles_matrix<> coord = sf[i];
      cpp11::doubles coord_vec = coord.data();
      cpp11::writable::doubles x(coord_vec.begin(), coord_vec.begin() + coord.nrow());
      cpp11::writable::doubles y(coord_vec.begin() + coord.nrow(), coord_vec.end());
      cpp11::writable::data_frame new_coord({
        "x"_nm = x,
        "y"_nm = y
      });
      res[i] = new_coord;
    } else if (type[i] == "LINESTRING") {
      cpp11::doubles_matrix<> coord = sf[i];
      cpp11::doubles coord_vec = coord.data();
      cpp11::writable::doubles x(coord_vec.begin(), coord_vec.begin() + coord.nrow());
      cpp11::writable::doubles y(coord_vec.begin() + coord.nrow(), coord_vec.end());
      cpp11::writable::data_frame new_coord({
        "x"_nm = x,
        "y"_nm = y
      });
      cpp11::writable::list wrapper, wrapper2;
      wrapper.push_back(new_coord);
      wrapper2.push_back(wrapper);
      res[i] = wrapper2;
    } else if (type[i] == "MULTILINESTRING") {
      cpp11::list coord_list = sf[i];
      cpp11::writable::list new_coord_list(coord_list.size());
      for (int j = 0; j < coord_list.size(); ++j) {
        cpp11::doubles_matrix<> coord = coord_list[j];
        cpp11::doubles coord_vec = coord.data();
        cpp11::writable::doubles x(coord_vec.begin(), coord_vec.begin() + coord.nrow());
        cpp11::writable::doubles y(coord_vec.begin() + coord.nrow(), coord_vec.end());
        cpp11::writable::data_frame new_coord({
          "x"_nm = x,
          "y"_nm = y
        });
        cpp11::writable::list wrapper;
        wrapper.push_back(new_coord);
        new_coord_list[j] = wrapper;
      }
      res[i] = new_coord_list;
    } else if (type[i] == "POLYGON") {
      cpp11::list coord_list = sf[i];
      cpp11::writable::list new_coord_list(coord_list.size());
      for (int j = 0; j < coord_list.size(); ++j) {
        cpp11::doubles_matrix<> coord = coord_list[j];
        cpp11::doubles coord_vec = coord.data();
        cpp11::writable::doubles x(coord_vec.begin(), coord_vec.begin() + (coord.nrow() - 1));
        cpp11::writable::doubles y(coord_vec.begin() + coord.nrow(), coord_vec.end() + -1);
        cpp11::writable::data_frame new_coord({
          "x"_nm = x,
          "y"_nm = y
        });
        new_coord_list[j] = new_coord;
      }
      cpp11::writable::list wrapper;
      wrapper.push_back(new_coord_list);
      res[i] = wrapper;
    } else if (type[i] == "MULTIPOLYGON") {
      cpp11::list coord_list = sf[i];
      cpp11::writable::list new_coord_list(coord_list.size());
      for (int j = 0; j < coord_list.size(); ++j) {
        cpp11::list polygon = coord_list[j];
        cpp11::writable::list new_polygon(polygon.size());
        for (int k = 0; k < polygon.size(); ++k) {
          cpp11::doubles_matrix<> coord = polygon[k];
          cpp11::doubles coord_vec = coord.data();
          cpp11::writable::doubles x(coord_vec.begin(), coord_vec.begin() + (coord.nrow() - 1));
          cpp11::writable::doubles y(coord_vec.begin() + coord.nrow(), coord_vec.end() + -1);
          cpp11::writable::data_frame new_coord({
            "x"_nm = x,
            "y"_nm = y
          });
          new_polygon[k] = new_coord;
        }
        new_coord_list[j] = new_polygon;
      }
      res[i] = new_coord_list;
    } else {
      cpp11::stop("Unknown geometry type");
    }
  }
  return res;
}
cpp11::doubles make_point(cpp11::doubles &x, cpp11::doubles &y, std::vector< std::vector<int> > &start) {
  int first = start[0][0];
  int last = start.back()[0];
  int size = last - first;
  cpp11::writable::doubles_matrix<> res(size, 2);

  int row = 0;
  for (int i = first; i < last; i++) {
    res(row, 0) = x[i];
    res(row, 1) = y[i];
    row++;
  }
  cpp11::writable::doubles res2 = res.data();
  res2.attr("class") = MULTIPOINTCLASS;

  return res2;
}
cpp11::writable::list make_path(cpp11::doubles &x, cpp11::doubles &y, std::vector< std::vector<int> > &start) {
  cpp11::writable::list res;
  int i, j;
  for (i = 0; i < start.size() - 1; ++i) {
    for (j = 0; j < start[i].size(); ++j) {
      int first = start[i][j];
      int last = j == start[i].size() - 1 ? start[i + 1][0] : start[i][j + 1] - 1;
      cpp11::writable::doubles_matrix<> path(last - first, 2);
      int row = 0;
      for (int k = first; k < last; k++) {
        path(row, 0) = x[k];
        path(row, 1) = y[k];
        row++;
      }
      res.push_back(path);
    }
  }
  res.attr("class") = MULTILINESTRINGCLASS;

  return res;
}
cpp11::writable::list make_polygon(cpp11::doubles &x, cpp11::doubles &y, std::vector< std::vector<int> > &start) {
  cpp11::writable::list res;

  int i, j, k;
  for (i = 0; i < start.size() - 1; ++i) {
    cpp11::writable::list poly;
    for (j = 0; j < start[i].size(); ++j) {
      int first = start[i][j];
      int last = j == start[i].size() - 1 ? start[i + 1][0] : start[i][j + 1] - 1;
      cpp11::writable::doubles_matrix<> polygon(last - first + 1, 2);
      int row = 0;
      for (k = first; k < last; ++k) {
        polygon(row, 0) = x[k];
        polygon(row, 1) = y[k];
        row++;
      }
      polygon(row, 0) = x[first];
      polygon(row, 1) = y[first];
      poly.push_back(polygon);
    }
    res.push_back(poly);
  }
  res.attr("class") = MULTIPOLYGONCLASS;

  return res;
}
[[cpp11::register]]
cpp11::writable::list repack_sf(cpp11::data_frame df, cpp11::strings type, int n_frames) {
  cpp11::writable::list res(type.size() * n_frames);
  cpp11::integers id = df["sf_id"];
  cpp11::integers geo_id = df["id"];
  cpp11::integers frame = df[".frame"];
  cpp11::doubles x = df["x"];
  cpp11::doubles y = df["y"];
  int i;
  int k = 0;
  int current_id = id[0];
  int current_geo_id = geo_id[0];
  int current_frame = frame[0];
  bool last;
  std::string current_type = type[current_id - 1];
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
        current_type = type[current_id - 1];
        current_geo_id = geo_id[i];
        start_ind_all.clear();
        start_ind = {i};
      }
    }
  }
  return res;
}
