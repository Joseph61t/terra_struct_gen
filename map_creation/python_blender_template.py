import bpy
# import bmesh
node_positions = <node_list>
# Get the selected plane
plane = bpy.data.objects["Plane"]

# Change size of plane to be <size> * 10
# context = bpy.context
# obj = context.object
# plane = bpy.context.scene.objects[0]
# me = obj.data
# bpy.data.objects["Plane"].scale[0] = <size>/2
plane.scale[0] = <size>/2
# bpy.data.objects["Plane"].scale[1] = <size>/2
plane.scale[1] = <size>/2
# world_matrix = plane.matrix_world
bpy.ops.object.mode_set(mode="EDIT")
bpy.ops.mesh.select_all(action='SELECT')
# New bmesh
# bm = bmesh.new()
# bm = bmesh.from_edit_mesh(me)
# load the mesh
# bm.from_mesh(me)

bpy.ops.mesh.subdivide(number_cuts=<times_to_be_split>)

# bmesh.ops.subdivide_edges(bm,cuts=<times_to_be_split>)
# bmesh.update_edit_mesh(me)
bpy.ops.object.mode_set(mode='OBJECT')
# bpy.context.active_object.data.scale[0] = <size>
# bpy.context.active_object.data.scale[1] = <size>
# bpy.ops.transform.resize(<size>,<size>,0)
selectedVerts = [v for v in bpy.context.active_object.data.vertices if v.select]
for v in range(len(selectedVerts)):
    new_location = selectedVerts[v].co #* world_matrix
    x = ((new_location[0] * <size>)/2) + <size>/2
    y = ((new_location[1] * <size>)/2) + <size>/2
    # print(f"{int(x)},{int(y)}")
    # print(new_location)
    # print(f"|----- new value {node_positions[v][1]} -----|")
    new_location[2] = node_positions[f"{int(x)},{int(y)}"]
    # print(f"{new_location}\n")
    selectedVerts[v].co = new_location
    # print(selectedVerts[v].co)
    # print(f"{selectedVerts[v].index}, {node_positions[v]}")
# for v in plane.data.vertices:
#     world = world_matrix * v.co
#     print(world)

bpy.ops.object.mode_set(mode="EDIT")

bpy.ops.mesh.select_all(action='DESELECT')
bpy.ops.wm.save_as_mainfile(filepath="<name>")
# bpy.ops.object.mode_set(mode="OBJECT")

# Write back to the mesh

# bm.to_mesh(obj.data)
# me.update()
# Split the selected plane <times_to_be_split> times to create the correct number of nodes 
# to be varied.
# bpy.ops.mesh.subdivide(number_cuts=<times_to_be_split>)
# Get the data of the plane
# me = obj.data
# Get the vertices of the plane
# verts = me.verts

# De-select everything
# for ob in bpy.context.selected_objects:
#     ob.select_set(False)

# use node_positions to vary nodes as indicated.