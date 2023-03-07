import bpy
# import bmesh
node_positions = [((0, 0), -0.05), ((0, 1), -0.06), ((0, 2), -0.1), ((0, 3), -0.13), ((0, 4), -0.13), ((0, 5), -0.16), ((0, 6), -0.14), ((0, 7), -0.11), ((0, 8), -0.1), ((1, 0), -0.06), ((1, 1), -0.09), ((1, 2), -0.13), ((1, 3), -0.16), ((1, 4), -0.19), ((1, 5), -0.21), ((1, 6), -0.21), ((1, 7), -0.19), ((1, 8), -0.17), ((2, 0), -0.1), ((2, 1), -0.13), ((2, 2), -0.17), ((2, 3), -0.21), ((2, 4), -0.25), ((2, 5), -0.28), ((2, 6), -0.3), ((2, 7), -0.32), ((2, 8), -0.32), ((3, 0), -0.13), ((3, 1), -0.16), ((3, 2), -0.21), ((3, 3), -0.27), ((3, 4), -0.32), ((3, 5), -0.38), ((3, 6), -0.42), ((3, 7), -0.46), ((3, 8), -0.48), ((4, 0), -0.13), ((4, 1), -0.19), ((4, 2), -0.25), ((4, 3), -0.32), ((4, 4), -0.4), ((4, 5), -0.48), ((4, 6), -0.55), ((4, 7), -0.61), ((4, 8), -0.67), ((5, 0), -0.16), ((5, 1), -0.21), ((5, 2), -0.28), ((5, 3), -0.38), ((5, 4), -0.48), ((5, 5), -0.59), ((5, 6), -0.69), ((5, 7), -0.77), ((5, 8), -0.82), ((6, 0), -0.14), ((6, 1), -0.21), ((6, 2), -0.3), ((6, 3), -0.42), ((6, 4), -0.55), ((6, 5), -0.69), ((6, 6), -0.84), ((6, 7), -0.94), ((6, 8), -1.03), ((7, 0), -0.11), ((7, 1), -0.19), ((7, 2), -0.32), ((7, 3), -0.46), ((7, 4), -0.61), ((7, 5), -0.77), ((7, 6), -0.94), ((7, 7), -1.13), ((7, 8), -1.25), ((8, 0), -0.1), ((8, 1), -0.17), ((8, 2), -0.32), ((8, 3), -0.48), ((8, 4), -0.67), ((8, 5), -0.82), ((8, 6), -1.03), ((8, 7), -1.25), ((8, 8), -1.6)]
# Get the selected plane
# plane = bpy.data.objects["Plane"].select_set(True)

# Change size of plane to be 8 * 10
# context = bpy.context
# obj = context.object
# plane = bpy.context.scene.objects[0]
# me = obj.data
bpy.data.objects["Plane"].scale[0] = 8
bpy.data.objects["Plane"].scale[1] = 8
bpy.ops.object.mode_set(mode="EDIT")
bpy.ops.mesh.select_all(action='SELECT')
# New bmesh
# bm = bmesh.new()
# bm = bmesh.from_edit_mesh(me)
# load the mesh
# bm.from_mesh(me)

bpy.ops.mesh.subdivide(number_cuts=7)

# bmesh.ops.subdivide_edges(bm,cuts=7)
# bmesh.update_edit_mesh(me)
bpy.ops.object.mode_set(mode='OBJECT')
# bpy.context.active_object.data.scale[0] = 8
# bpy.context.active_object.data.scale[1] = 8
# bpy.ops.transform.resize(8,8,0)
selectedVerts = [v for v in bpy.context.active_object.data.vertices if v.select]
for v in range(len(selectedVerts)):
    new_location = selectedVerts[v].co
    # print(new_location)
    # new_location[0] = new_location[0]
    # new_location[2] = new_location[2] + node_positions[v][1]
    new_location[2] = node_positions[v][1]
    selectedVerts[v].co = new_location
    # print(f"{selectedVerts[v].index}, {node_positions[v]}")
# for vert in selectedVerts:
bpy.ops.object.mode_set(mode="EDIT")

bpy.ops.mesh.select_all(action='DESELECT')
bpy.ops.wm.save_as_mainfile(filepath="/home/orindale/Programing/terra_struct_gen/maps/map_1_size_8.blend")
# bpy.ops.object.mode_set(mode="OBJECT")

# Write back to the mesh

# bm.to_mesh(obj.data)
# me.update()
# Split the selected plane 7 times to create the correct number of nodes 
# to be varied.
# bpy.ops.mesh.subdivide(number_cuts=7)
# Get the data of the plane
# me = obj.data
# Get the vertices of the plane
# verts = me.verts

# De-select everything
# for ob in bpy.context.selected_objects:
#     ob.select_set(False)

# use node_positions to vary nodes as indicated.