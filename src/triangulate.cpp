#include <Rcpp.h>
#include "earcut.h"
#include <algorithm>
#include <set>
#include <iterator>
using namespace Rcpp;

// The number type to use for tessellation
using Coord = double;

// The index type. Defaults to uint32_t, but you can also pass uint16_t if you know that your
// data won't have more than 65536 vertices.
using N = uint32_t;

// Create array
using Point = std::pair<Coord, Coord>;

struct tri{
  std::vector<int> points;
  std::vector<int> neighbors;
  double area;
  bool included;
  bool operator<(const tri& rhs) const { return area < rhs.area; };
};
struct mesh{
  std::set<int> triangels;
  std::set<int> neighbors;
  double area;
  bool operator<(const mesh& rhs) const { return area < rhs.area; };
};

double tri_area(Point A, Point B, Point C) {
  double a2 = (B.first - A.first) * (B.first - A.first) + (B.second - A.second) * (B.second - A.second);
  double b2 = (C.first - B.first) * (C.first - B.first) + (C.second - B.second) * (C.second - B.second);
  double c2 = (A.first - C.first) * (A.first - C.first) + (A.second - C.second) * (A.second - C.second);

  return std::sqrt((a2 + b2 + c2) * (a2 + b2 + c2) - 2 * (a2 * a2 + b2 * b2 + c2 * c2)) / 4;
}


//[[Rcpp::export]]
NumericMatrix cut_polygon(NumericMatrix polygon, int n) {
  std::vector< std::vector<Point> > poly;
  std::vector<Point> ring;
  std::vector<Point> all_points;
  int i,j;

  for (i = 0; i < polygon.nrow(); ++i) {
    Point corner;
    corner.first = polygon(i, 0);
    corner.second = polygon(i, 1);

    if (R_IsNA(corner.first)) {
      poly.push_back(ring);
      ring = std::vector<Point>();
    } else {
      ring.push_back(corner);
      all_points.push_back(corner);
    }
  }
  poly.push_back(ring);

  std::vector<N> triangle_ind = mapbox::earcut<N>(poly);
  std::vector<tri> triangles;
  std::map<std::pair<int, int>, std::vector<int> > shared;
  tri triangle;
  for (i = 0; i < triangle_ind.size(); ++i) {
    triangle.points.push_back(triangle_ind[i]);
    if (triangle.points.size() == 3) {
      triangle.area = tri_area(all_points[triangle.points[0]],
                               all_points[triangle.points[1]],
                               all_points[triangle.points[2]]);
      triangles.push_back(triangle);
      std::pair<int, int> side1(triangle.points[0], triangle.points[1]);
      std::pair<int, int> side2(triangle.points[1], triangle.points[2]);
      std::pair<int, int> side3(triangle.points[0], triangle.points[2]);
      if (side1.first > side1.second) {
        side1.first = triangle.points[1];
        side1.second = triangle.points[0];
      }
      if (side2.first > side2.second) {
        side2.first = triangle.points[2];
        side2.second = triangle.points[1];
      }
      if (side3.first > side3.second) {
        side3.first = triangle.points[2];
        side3.second = triangle.points[0];
      }
      shared[side1].push_back(triangles.size() - 1);
      shared[side2].push_back(triangles.size() - 1);
      shared[side3].push_back(triangles.size() - 1);
      triangle = tri();
    }
  }

  std::map<std::pair<int, int>, std::vector<int> >::iterator it;
  for (it = shared.begin(); it != shared.end(); it++) {
    std::vector<int> tri_ind = it->second;
    for (i = 0; i < tri_ind.size(); ++i) {
      for (j = 0; j < tri_ind.size(); ++j) {
        if (i == j) continue;
        triangles[tri_ind[i]].neighbors.push_back(tri_ind[j]);
      }
    }
  }
  std::vector<mesh> meshes;
  for (i = 0; i < triangles.size(); ++i) {
    mesh m;
    m.triangels.insert(i);
    m.neighbors.insert(triangles[i].neighbors.begin(), triangles[i].neighbors.end());
    m.area = triangles[i].area;
    meshes.push_back(m);
  }
  std::sort(meshes.begin(), meshes.end());

  while (meshes.size() > n) {
    int tri_ind = *(meshes[0].neighbors.begin());
    for (i = 1; i < meshes.size(); ++i) {
      std::set<int>::iterator it = meshes[i].triangels.find(tri_ind);
      if (it != meshes[i].triangels.end()) {
        meshes[0].area += meshes[i].area;
        meshes[0].triangels.insert(meshes[i].triangels.begin(), meshes[i].triangels.end());
        meshes[0].neighbors.insert(meshes[i].neighbors.begin(), meshes[i].neighbors.end());
        std::set<int> prunned_neighbors;
        std::set_difference(meshes[0].neighbors.begin(), meshes[0].neighbors.end(),
                            meshes[0].triangels.begin(), meshes[0].triangels.end(),
                            std::inserter(prunned_neighbors, prunned_neighbors.end()));
        meshes[0].neighbors = prunned_neighbors;
        meshes.erase(meshes.begin() + i);
        std::sort(meshes.begin(), meshes.end());
        break;
      }
    }
  }

  NumericMatrix meshmat(triangles.size() * 3, 3);

  int k = 0;
  std::set<int>::iterator it_tri;
  for (i = 0; i < meshes.size(); ++i) {
    for (it_tri = meshes[i].triangels.begin(); it_tri != meshes[i].triangels.end(); it_tri++) {
      tri triangle = triangles[*it_tri];
      for (j = 0; j < 3; ++j) {
        meshmat(k, 0) = all_points[triangle.points[j]].first;
        meshmat(k, 1) = all_points[triangle.points[j]].second;
        meshmat(k, 2) = i;
        ++k;
      }
    }
  }

  return meshmat;
}
