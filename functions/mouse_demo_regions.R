# library(cocoframer)
# library(purrr)
# library(rgl)
# 
# structures <- c("root","BST", "MEA", "AOB", "In", "st","MOB", "IIn")
# mesh_list <- map(structures, ccf_2017_mesh)
# 
# names(mesh_list) <- structures
# 
# plot_ccf_meshes(mesh_list,
#                 fg_structure = c("BST","MEA", "AOB", "In", "st", "MOB","IIn"),
#                 bg_structure = "root",
#                 style = "cartoon")
# 
# rgl.snapshot("demo_circuit.png")

# library(cocoframer)
# library(purrr)
# library(rgl)
# 
# structures <- c("root","BST", "MEA", "AOB", "In", "st","MOB", "IIn")
# mesh_list <- map(structures, ccf_2017_mesh)
# 
# names(mesh_list) <- structures
# 
# plot_ccf_meshes(mesh_list,
#                 fg_structure = c("BST","MEA", "AOB", "In", "st", "MOB","IIn"),
#                 bg_structure = "root",
#                 style = "cartoon")
# 
# rgl.snapshot("mouse_cluster_1.png")