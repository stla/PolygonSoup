#ifndef _POLYGONSOUPHEADER_
#define _POLYGONSOUPHEADER_
#endif

#include <Rcpp.h>
#define CGAL_EIGEN3_ENABLED 1

#include <CGAL/Exact_predicates_inexact_constructions_kernel.h>
#include <CGAL/Exact_predicates_exact_constructions_kernel.h>
#include <CGAL/Kernel/global_functions.h>
#include <CGAL/Surface_mesh.h>
#include <CGAL/Vector_3.h>

#include <CGAL/utility.h>

#include <CGAL/Polygon_mesh_processing/compute_normal.h>
#include <CGAL/Polygon_mesh_processing/orient_polygon_soup.h>
#include <CGAL/Polygon_mesh_processing/orientation.h>
#include <CGAL/Polygon_mesh_processing/polygon_soup_to_polygon_mesh.h>
#include <CGAL/Polygon_mesh_processing/repair_polygon_soup.h>

#include <CGAL/Polygon_mesh_processing/triangulate_faces.h>

#include <CGAL/Polygon_mesh_processing/measure.h>

//#include <CGAL/Polygon_mesh_processing/corefinement.h>

#include <CGAL/number_utils.h>

#include <CGAL/Cartesian.h>

#include <CGAL/Polygon_mesh_processing/IO/polygon_mesh_io.h>

#include <CGAL/Gmpq.h>
#include <CGAL/Gmpz.h>
#include "gmp.h"

// to remove degenerate faces:
// #include <CGAL/Polygon_mesh_processing/shape_predicates.h>

//#include <CGAL/property_map.h>
//#include <boost/iterator/transform_iterator.hpp>

typedef CGAL::Exact_predicates_inexact_constructions_kernel K;
typedef CGAL::Exact_predicates_exact_constructions_kernel EK;

typedef K::Point_3 Point3;
typedef EK::Point_3 EPoint3;

typedef CGAL::Surface_mesh<EPoint3> EMesh3;

typedef EK::Vector_3 EVector3;

typedef CGAL::Cartesian<CGAL::Gmpq> QK;
typedef CGAL::Surface_mesh<QK::Point_3> QMesh3;
typedef QK::Point_3 QPoint3;
typedef QK::Vector_3 QVector3;

// -------------------------------------------------------------------------- //
namespace PMP = CGAL::Polygon_mesh_processing;

// -------------------------------------------------------------------------- //

// Rcpp::NumericMatrix points3_to_matrix(std::vector<Point3>);

template <typename PointT>
std::vector<PointT> matrix_to_points3(const Rcpp::NumericMatrix);

template <typename MeshT, typename PointT>
MeshT makeSurfMesh(const Rcpp::List, const bool, const bool);

template <typename MeshT, typename PointT>
MeshT makeSurfTMesh(const Rcpp::List, const bool, const bool);

QMesh3 makeSurfQMesh(const Rcpp::List, const bool, const bool);
QMesh3 makeSurfTQMesh(const Rcpp::List, const bool, const bool);

template <typename KernelT, typename MeshT, typename PointT>
Rcpp::DataFrame getEdges(MeshT);

Rcpp::NumericMatrix getEKNormals(EMesh3);
Rcpp::NumericMatrix getQNormals(QMesh3);

Rcpp::List RSurfEKMesh(EMesh3, const bool);
Rcpp::List RSurfQMesh(QMesh3, const bool);
Rcpp::List RSurfTEKMesh(EMesh3, const bool);
Rcpp::List RSurfTQMesh(QMesh3, const bool);

std::string q2str(CGAL::Gmpq);

void Message(std::string);

//template <typename MeshT>
//MeshT removeDegenerateFaces(MeshT);
