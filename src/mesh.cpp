#ifndef _POLYGONSOUPHEADER_
#include "PolygonSoup.h"
#endif

// [[Rcpp::export]]
Rcpp::List SurfEMesh(const Rcpp::List rmesh,
                     const bool isTriangle,
                     const bool triangulate,
                     const bool clean,
                     const bool normals) {
  Message("\u2014 Processing mesh...");
  EMesh3 mesh = makeSurfMesh<EMesh3, EPoint3>(rmesh, clean, false);
  const bool really_triangulate = !isTriangle && triangulate;
  Rcpp::DataFrame Edges0;
  Rcpp::NumericMatrix Normals0;
  if(really_triangulate) {
    Edges0 = getEdges<EK, EMesh3, EPoint3>(mesh);
    if(normals) {
      Normals0 = getEKNormals(mesh);
    }
    Message("Triangulation.");
    const bool success = PMP::triangulate_faces(mesh);
    if(!success) {
      Rcpp::stop("Triangulation has failed.");
    }
    if(CGAL::is_closed(mesh)) {
      if(!PMP::is_outward_oriented(mesh)) {
        PMP::reverse_face_orientations(mesh);
      }
      const bool bv = PMP::does_bound_a_volume(mesh);
      std::string msg2;
      if(bv) {
        msg2 = "The mesh bounds a volume.";
      } else {
        msg2 = "The mesh does not bound a volume - reorienting.";
        PMP::orient_to_bound_a_volume(mesh);
      }
      Message(msg2);
    }
  }
  Message("... done.\n");
  Rcpp::List routmesh = RSurfEKMesh(mesh, normals);
  if(really_triangulate) {
    routmesh["edges0"] = Edges0;
    if(normals) {
      routmesh["normals0"] = Normals0;
    }
  }
  return routmesh;
}

// [[Rcpp::export]]
Rcpp::List SurfQMesh(const Rcpp::List rmesh,
                     const bool isTriangle,
                     const bool triangulate,
                     const bool clean,
                     const bool normals) {
  Message("\u2014 Processing mesh...");
  QMesh3 mesh = makeSurfQMesh(rmesh, clean, false);
  const bool really_triangulate = !isTriangle && triangulate;
  Rcpp::DataFrame Edges0;
  Rcpp::NumericMatrix Normals0;
  if(really_triangulate) {
    Edges0 = getEdges<QK, QMesh3, QPoint3>(mesh);
    if(normals) {
      Normals0 = getQNormals(mesh);
    }
    Message("Triangulation.");
    const bool success = PMP::triangulate_faces(mesh);
    if(!success) {
      Rcpp::stop("Triangulation has failed.");
    }
    if(CGAL::is_closed(mesh)) {
      if(!PMP::is_outward_oriented(mesh)) {
        PMP::reverse_face_orientations(mesh);
      }
      const bool bv = PMP::does_bound_a_volume(mesh);
      std::string msg2;
      if(bv) {
        msg2 = "The mesh bounds a volume.";
      } else {
        msg2 = "The mesh does not bound a volume - reorienting.";
        PMP::orient_to_bound_a_volume(mesh);
      }
      Message(msg2);
    }
  }
  Message("... done.\n");
  Rcpp::List routmesh = RSurfQMesh(mesh, normals);
  if(really_triangulate) {
    routmesh["edges0"] = Edges0;
    if(normals) {
      routmesh["normals0"] = Normals0;
    }
  }
  return routmesh;
}
