#ifndef _POLYGONSOUPHEADER_
#include "PolygonSoup.h"
#endif

#include <CGAL/IO/io.h>
#include <locale>  // tolower

std::string toLower(std::string s) {
  for(char& c : s) {
    c = std::tolower(c);
  }
  return s;
}

std::pair<std::vector<std::vector<int>>, bool> list_to_faces2(
    const Rcpp::List L) {
  const size_t nfaces = L.size();
  std::vector<std::vector<int>> faces;
  faces.reserve(nfaces);
  bool triangle = true;
  for(size_t i = 0; i < nfaces; i++) {
    Rcpp::IntegerVector face_rcpp = Rcpp::as<Rcpp::IntegerVector>(L(i));
    std::vector<int> face(face_rcpp.begin(), face_rcpp.end());
//     std::transform(
//       face.begin(), face.end(), face.begin(),
// 	  std::bind(std::minus<int>(), std::placeholders::_1, 1)
//     );
    faces.emplace_back(face);
    triangle = triangle && (face.size() == 3);
  }
  return std::make_pair(faces, triangle);
}

// [[Rcpp::export]]
Rcpp::List readFile(const std::string filename) {
  const std::string ext = toLower(filename.substr(filename.length() - 3, 3));
  std::ifstream infile;
  infile.open(filename);
  const bool binary = CGAL::IO::is_binary(infile);
  std::vector<Point3> points;
  std::vector<std::vector<int>> faces;
  bool ok = false;
  if(ext == "ply") {
    ok = CGAL::IO::read_PLY(
      infile, points, faces,
      CGAL::parameters::use_binary_mode(binary)
    );
    if(!ok && !binary) {
      ok = CGAL::IO::read_PLY(
        infile, points, faces,
        CGAL::parameters::use_binary_mode(true)
      );
    }
  } else if(ext == "stl") {
    ok = CGAL::IO::read_STL(
      filename, points, faces,
      CGAL::parameters::use_binary_mode(binary)
    );
  } else if(ext == "obj") {
    ok = CGAL::IO::read_OBJ(infile, points, faces);
  } else if(ext == "off") {
    ok = CGAL::IO::read_OFF(infile, points, faces);
  } else {
    Rcpp::stop("Unknown file extension.");
  }
  infile.close();
  Rcpp::List out;
  if(ok) {
    const size_t npoints = points.size();
    Rcpp::NumericMatrix Vertices(3, npoints);
    for(size_t i = 0; i < npoints; i++) {
      const Point3 point_i = points[i];
      Rcpp::NumericVector col_i =
          Rcpp::NumericVector::create(point_i.x(), point_i.y(), point_i.z());
      Vertices(Rcpp::_, i) = col_i;
    }
    const size_t nfaces = faces.size();
    Rcpp::List Faces(nfaces);
    for(size_t i = 0; i < nfaces; i++) {
      const std::vector<int> face_i = faces[i];
      Rcpp::IntegerVector col_i(face_i.begin(), face_i.end());
      Faces(i) = col_i + 1;
    }
    out["vertices"] = Rcpp::transpose(Vertices);
    out["faces"] = Faces;
  } else {
    Rcpp::stop("Reading failure.");
  }
  return out;
}

// [[Rcpp::export]]
void writeFile(const std::string filename,
               const bool binary,
               const int precision,
               const Rcpp::NumericMatrix Vertices,
               const Rcpp::List Faces) {
  const std::vector<Point3> points = matrix_to_points3<Point3>(Vertices);
  const std::pair<std::vector<std::vector<int>>, bool> faces =
      list_to_faces2(Faces);
  const std::string ext = toLower(filename.substr(filename.length() - 3, 3));
  bool ok = false;
  if(ext == "ply") {
    ok = CGAL::IO::write_PLY(
      filename, points, faces.first,
      CGAL::parameters::use_binary_mode(binary).stream_precision(precision)
    );
  } else if(ext == "stl") {
    if(!faces.second) {
      Rcpp::stop("STL files only accept triangular faces.");
    }
    ok = CGAL::IO::write_STL(
      filename, points, faces.first,
      CGAL::parameters::use_binary_mode(binary).stream_precision(precision)
    );
  } else if(ext == "obj") {
    ok = CGAL::IO::write_OBJ(
      filename, points, faces.first,
      CGAL::parameters::stream_precision(precision)
    );
  } else if(ext == "off") {
    ok = CGAL::IO::write_OFF(
      filename, points, faces.first,
      CGAL::parameters::stream_precision(precision)
    );
  } else {
    Rcpp::stop("Unknown file extension.");
  }
  if(!ok) {
    Rcpp::stop("Failed to write file.");
  }
}
