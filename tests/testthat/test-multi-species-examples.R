
# Tutorial: http://r-pkgs.had.co.nz/tests.html
# And example see: https://github.com/ss3sim/ss3sim/tree/master/tests/testthat
context("Testing multispecies examples")

# Eastern Bering Sea -- 3 species (50 knots)
test_that("Eastern Bering Sea 3-species is working ", {
  skip_on_travis()
  # Prepping
  test_path = file.path(multispecies_example_path,"EBS_3species")
  load( file.path(test_path,"opt.RData") )
  load( file.path(test_path,"Record.RData") )
  load( file.path(test_path,"parhat.RData") )
  attach(Record)
  on.exit( detach(Record) )
  # Run model
  load( file.path(test_path,"Data_Geostat.RData") )
  Extrapolation_List = make_extrapolation_info( Region=Region, strata.limits=strata.limits )
  Spatial_List = make_spatial_info( grid_size_km=grid_size_km, n_x=n_x, Method=Method, Lon=Data_Geostat[,'Lon'], Lat=Data_Geostat[,'Lat'], Extrapolation_List=Extrapolation_List, randomseed=Kmeans_Config[["randomseed"]], nstart=Kmeans_Config[["nstart"]], iter.max=Kmeans_Config[["iter.max"]], DirPath=test_path )
  Data_Geostat = cbind( Data_Geostat, "knot_i"=Spatial_List$knot_i )
  TmbData = VAST::Data_Fn("Version"=Version_VAST, "OverdispersionConfig"==rep(VesselConfig[2],2), "FieldConfig"=FieldConfig, "RhoConfig"=RhoConfig, "ObsModel"=ObsModel, "c_i"=Data_Geostat[,'spp']-1, "b_i"=Data_Geostat[,'Catch_KG'], "a_i"=Data_Geostat[,'AreaSwept_km2'], "v_i"=as.numeric(factor(paste(Data_Geostat[,'Vessel'],Data_Geostat[,'Year'])))-1, "s_i"=Data_Geostat[,'knot_i']-1, "t_i"=Data_Geostat[,'Year'], "a_xl"=Spatial_List$a_xl, "MeshList"=Spatial_List$MeshList, "GridList"=Spatial_List$GridList, "Method"=Spatial_List$Method )
  TmbList = VAST::Build_TMB_Fn("TmbData"=TmbData, "RunDir"=test_path, "Version"=Version_VAST, "RhoConfig"=RhoConfig, "loc_x"=Spatial_List$loc_x)
  on.exit( dyn.unload(paste0(test_path,"/",TMB::dynlib(Version_VAST))), add=TRUE )
  Opt = TMBhelper::Optimize( obj=TmbList[["Obj"]], getsd=FALSE, lower=TmbList[["Lower"]], upper=TmbList[["Upper"]] )
  # Comparisons
  expect_equal( abs(Opt$par), abs(opt$par), tolerance=1e-3 )
  expect_equal( Opt$objective, opt$objective, tolerance=1e-3 )
})

# Eastern Bering Sea -- 3 species (100 knots)
test_that("Eastern Bering Sea 5-species is working ", {
  skip_on_travis()
  # Prepping
  test_path = file.path(multispecies_example_path,"EBS_5species")
  load( file.path(test_path,"opt.RData") )
  load( file.path(test_path,"Record.RData") )
  load( file.path(test_path,"parhat.RData") )
  attach(Record)
  on.exit( detach(Record) )
  # Run model
  load( file.path(test_path,"Data_Geostat.RData") )
  Extrapolation_List = make_extrapolation_info( Region=Region, strata.limits=strata.limits )
  Spatial_List = make_spatial_info( grid_size_km=grid_size_km, n_x=n_x, Method=Method, Lon=Data_Geostat[,'Lon'], Lat=Data_Geostat[,'Lat'], Extrapolation_List=Extrapolation_List, randomseed=Kmeans_Config[["randomseed"]], nstart=Kmeans_Config[["nstart"]], iter.max=Kmeans_Config[["iter.max"]], DirPath=test_path )
  Data_Geostat = cbind( Data_Geostat, "knot_i"=Spatial_List$knot_i )
  TmbData = VAST::Data_Fn("Version"=Version_VAST, "OverdispersionConfig"==rep(VesselConfig[2],2), "FieldConfig"=FieldConfig, "RhoConfig"=RhoConfig, "ObsModel"=ObsModel, "c_i"=Data_Geostat[,'spp']-1, "b_i"=Data_Geostat[,'Catch_KG'], "a_i"=Data_Geostat[,'AreaSwept_km2'], "v_i"=as.numeric(factor(paste(Data_Geostat[,'Vessel'],Data_Geostat[,'Year'])))-1, "s_i"=Data_Geostat[,'knot_i']-1, "t_i"=Data_Geostat[,'Year'], "a_xl"=Spatial_List$a_xl, "MeshList"=Spatial_List$MeshList, "GridList"=Spatial_List$GridList, "Method"=Spatial_List$Method )
  TmbList = VAST::Build_TMB_Fn("TmbData"=TmbData, "RunDir"=test_path, "Version"=Version_VAST, "RhoConfig"=RhoConfig, "loc_x"=Spatial_List$loc_x)
  on.exit( dyn.unload(paste0(test_path,"/",TMB::dynlib(Version_VAST))), add=TRUE )
  Opt = TMBhelper::Optimize( obj=TmbList[["Obj"]], getsd=FALSE, lower=TmbList[["Lower"]], upper=TmbList[["Upper"]] )
  # Comparisons
  expect_equal( abs(Opt$par), abs(opt$par), tolerance=1e-3 )
  expect_equal( Opt$objective, opt$objective, tolerance=1e-3 )
})

