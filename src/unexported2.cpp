#ifndef _POLYGONSOUPHEADER_
#include "PolygonSoup.h"
#endif

Rcpp::NumericMatrix getVertices_EK(EMesh3 mesh) {
  const size_t nvertices = mesh.number_of_vertices();
  Rcpp::NumericMatrix Vertices(3, nvertices);
  {
    size_t i = 0;
    for(EMesh3::Vertex_index vd : mesh.vertices()) {
      Rcpp::NumericVector col_i(3);
      const EPoint3 vertex = mesh.point(vd);
      col_i(0) = CGAL::to_double<EK::FT>(vertex.x());
      col_i(1) = CGAL::to_double<EK::FT>(vertex.y());
      col_i(2) = CGAL::to_double<EK::FT>(vertex.z());
      Vertices(Rcpp::_, i) = col_i;
      i++;
    }
  }
  return Vertices;
}

Rcpp::CharacterMatrix getVertices_QK(QMesh3 mesh) {
  const size_t nvertices = mesh.number_of_vertices();
  Rcpp::CharacterMatrix Vertices(3, nvertices);
  {
    size_t i = 0;
    for(QMesh3::Vertex_index vd : mesh.vertices()) {
      Rcpp::CharacterVector col_i(3);
      const QPoint3 vertex = mesh.point(vd);
      col_i(0) = q2str(vertex.x());
      col_i(1) = q2str(vertex.y());
      col_i(2) = q2str(vertex.z());
      Vertices(Rcpp::_, i) = col_i;
      i++;
    }
  }
  return Vertices;
}

template <typename KernelT, typename MeshT, typename PointT>
Rcpp::DataFrame getEdges(MeshT mesh) {
  const size_t nedges = mesh.number_of_edges();
  Rcpp::IntegerVector I1(nedges);
  Rcpp::IntegerVector I2(nedges);
  Rcpp::NumericVector Length(nedges);
  Rcpp::NumericVector Angle(nedges);
  Rcpp::LogicalVector Exterior(nedges);
  Rcpp::LogicalVector Coplanar(nedges);
  {
    size_t i = 0;
    for(typename MeshT::Edge_index ed : mesh.edges()) {
      typename MeshT::Vertex_index s = source(ed, mesh);
      typename MeshT::Vertex_index t = target(ed, mesh);
      I1(i) = (int)s + 1;
      I2(i) = (int)t + 1;
      std::vector<PointT> points(4);
      points[0] = mesh.point(s);
      points[1] = mesh.point(t);
      typename MeshT::Halfedge_index h0 = mesh.halfedge(ed, 0);
      points[2] = mesh.point(mesh.target(mesh.next(h0)));
      typename MeshT::Halfedge_index h1 = mesh.halfedge(ed, 1);
      points[3] = mesh.point(mesh.target(mesh.next(h1)));
      typename KernelT::FT angle = CGAL::abs(CGAL::approximate_dihedral_angle(
          points[0], points[1], points[2], points[3]));
      Angle(i) = CGAL::to_double(angle);
      Exterior(i) = angle < 179.0 || angle > 181.0;
      Coplanar(i) = CGAL::coplanar(points[0], points[1], points[2], points[3]);
      typename KernelT::FT el = PMP::edge_length(h0, mesh);
      Length(i) = CGAL::to_double(el);
      i++;
    }
  }
  Rcpp::DataFrame Edges = Rcpp::DataFrame::create(
    Rcpp::Named("i1")       = I1,
    Rcpp::Named("i2")       = I2,
    Rcpp::Named("length")   = Length,
    Rcpp::Named("angle")    = Angle,
    Rcpp::Named("exterior") = Exterior,
    Rcpp::Named("coplanar") = Coplanar
  );
  return Edges;
}

template Rcpp::DataFrame getEdges<EK, EMesh3, EPoint3>(EMesh3);
template Rcpp::DataFrame getEdges<QK, QMesh3, QPoint3>(QMesh3);

template <typename MeshT>
Rcpp::List getFaces(MeshT mesh) {
  const size_t nfaces = mesh.number_of_faces();
  Rcpp::List Faces(nfaces);
  {
    size_t i = 0;
    for(typename MeshT::Face_index fd : mesh.faces()) {
      Rcpp::IntegerVector col_i;
      for(typename MeshT::Vertex_index vd :
          vertices_around_face(mesh.halfedge(fd), mesh)) {
        col_i.push_back(vd + 1);
      }
      Faces(i) = col_i;
      i++;
    }
  }
  return Faces;
}

template <typename MeshT>
Rcpp::IntegerMatrix getTFaces(MeshT mesh) {
  const size_t nfaces = mesh.number_of_faces();
  Rcpp::IntegerMatrix Faces(3, nfaces);
  {
    size_t i = 0;
    for(typename MeshT::Face_index fd : mesh.faces()) {
      Rcpp::IntegerVector col_i;
      for(typename MeshT::Vertex_index vd :
          vertices_around_face(mesh.halfedge(fd), mesh)) {
        col_i.push_back(vd + 1);
      }
      Faces(Rcpp::_, i) = col_i;
      i++;
    }
  }
  return Faces;
}

Rcpp::NumericMatrix getEKNormals(EMesh3 mesh) {
  const size_t nvertices = mesh.number_of_vertices();
  Rcpp::NumericMatrix Normals(3, nvertices);
  auto vnormals = mesh.add_property_map<EMesh3::Vertex_index, EVector3>(
                          "v:normals", CGAL::NULL_VECTOR)
                      .first;
  auto fnormals = mesh.add_property_map<EMesh3::Face_index, EVector3>(
                          "f:normals", CGAL::NULL_VECTOR)
                      .first;
  PMP::compute_normals(mesh, vnormals, fnormals);
  {
    size_t i = 0;
    for(EMesh3::Vertex_index vd : vertices(mesh)) {
      Rcpp::NumericVector col_i(3);
      const EVector3 normal = vnormals[vd];
      col_i(0) = CGAL::to_double<EK::FT>(normal.x());
      col_i(1) = CGAL::to_double<EK::FT>(normal.y());
      col_i(2) = CGAL::to_double<EK::FT>(normal.z());
      Normals(Rcpp::_, i) = col_i;
      i++;
    }
  }
  return Normals;
}

Rcpp::NumericMatrix getQNormals(QMesh3 mesh) {
  const size_t nvertices = mesh.number_of_vertices();
  Rcpp::NumericMatrix Normals(3, nvertices);
  auto vnormals = mesh.add_property_map<QMesh3::Vertex_index, QVector3>(
                          "v:normals", CGAL::NULL_VECTOR)
                      .first;
  auto fnormals = mesh.add_property_map<QMesh3::Face_index, QVector3>(
                          "f:normals", CGAL::NULL_VECTOR)
                      .first;
  PMP::compute_normals(mesh, vnormals, fnormals);
  {
    size_t i = 0;
    for(QMesh3::Vertex_index vd : vertices(mesh)) {
      Rcpp::NumericVector col_i(3);
      const QVector3 normal = vnormals[vd];
      col_i(0) = normal.x().to_double();
      col_i(1) = normal.y().to_double();
      col_i(2) = normal.z().to_double();
      Normals(Rcpp::_, i) = col_i;
      i++;
    }
  }
  return Normals;
}

Rcpp::List RSurfEKMesh(EMesh3 mesh, const bool normals) {
  Rcpp::DataFrame Edges = getEdges<EK, EMesh3, EPoint3>(mesh);
  Rcpp::NumericMatrix Vertices = getVertices_EK(mesh);
  Rcpp::List Faces = getFaces<EMesh3>(mesh);
  Rcpp::List out = Rcpp::List::create(Rcpp::Named("vertices") = Vertices,
                                      Rcpp::Named("edges") = Edges,
                                      Rcpp::Named("faces") = Faces);
  if(normals) {
    Rcpp::NumericMatrix Normals = getEKNormals(mesh);
    out["normals"] = Normals;
  }
  return out;
}

Rcpp::List RSurfQMesh(QMesh3 mesh, const bool normals) {
  Rcpp::DataFrame Edges = getEdges<QK, QMesh3, QPoint3>(mesh);
  Rcpp::CharacterMatrix Vertices = getVertices_QK(mesh);
  Rcpp::List Faces = getFaces<QMesh3>(mesh);
  Rcpp::List out = Rcpp::List::create(Rcpp::Named("vertices") = Vertices,
                                      Rcpp::Named("edges") = Edges,
                                      Rcpp::Named("faces") = Faces);
  if(normals) {
    Rcpp::NumericMatrix Normals = getQNormals(mesh);
    out["normals"] = Normals;
  }
  return out;
}

Rcpp::List RSurfTEKMesh(EMesh3 mesh, const bool normals) {
  Rcpp::DataFrame Edges = getEdges<EK, EMesh3, EPoint3>(mesh);
  Rcpp::NumericMatrix Vertices = getVertices_EK(mesh);
  Rcpp::IntegerMatrix Faces = getTFaces<EMesh3>(mesh);
  Rcpp::List out = Rcpp::List::create(Rcpp::Named("vertices") = Vertices,
                                      Rcpp::Named("edges") = Edges,
                                      Rcpp::Named("faces") = Faces);
  if(normals) {
    Rcpp::NumericMatrix Normals = getEKNormals(mesh);
    out["normals"] = Normals;
  }
  return out;
}

Rcpp::List RSurfTQMesh(QMesh3 mesh, const bool normals) {
  Rcpp::DataFrame Edges = getEdges<QK, QMesh3, QPoint3>(mesh);
  Rcpp::CharacterMatrix Vertices = getVertices_QK(mesh);
  Rcpp::IntegerMatrix Faces = getTFaces<QMesh3>(mesh);
  Rcpp::List out = Rcpp::List::create(Rcpp::Named("vertices") = Vertices,
                                      Rcpp::Named("edges") = Edges,
                                      Rcpp::Named("faces") = Faces);
  if(normals) {
    Rcpp::NumericMatrix Normals = getQNormals(mesh);
    out["normals"] = Normals;
  }
  return out;
}
