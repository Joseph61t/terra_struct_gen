import bpy
# Nodes, and associated data
node_positions = <node_list>

# Get the selected plane
plane = bpy.data.objects["Plane"]

# Change size of plane
plane.scale[0] = <size>/2
plane.scale[1] = <size>/2

bpy.ops.object.mode_set(mode="EDIT")
bpy.ops.mesh.select_all(action='SELECT')

bpy.ops.mesh.subdivide(number_cuts=<times_to_be_split>)
bpy.ops.object.mode_set(mode='OBJECT')

selectedVerts = [v for v in bpy.context.active_object.data.vertices if v.select]
for v in range(len(selectedVerts)):
    new_location = selectedVerts[v].co #* world_matrix
    x = ((new_location[0] * <size>)/2) + <size>/2
    y = ((new_location[1] * <size>)/2) + <size>/2
    # print(f"{int(x)},{int(y)}")
    # print(new_location)
    # print(f"|----- new value {node_positions[v][1]} -----|")
    new_location[2] = node_positions[f"{int(x)},{int(y)}"][0]
    # print(f"{new_location}\n")
    selectedVerts[v].co = new_location
    # print(selectedVerts[v].co)
    # print(f"{selectedVerts[v].index}, {node_positions[v]}")

bpy.ops.object.mode_set(mode="EDIT")

bpy.ops.mesh.select_all(action='DESELECT')
bpy.ops.wm.save_as_mainfile(filepath="<name>")