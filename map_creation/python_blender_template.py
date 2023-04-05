import bpy
# Nodes, and associated data
vertice_positions = <node_list>

# structure_dictionary = <structures>
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
original_vertice_count = len(selectedVerts)
# {'structure_value':[(vertice_number,vertice_position,height)...]}
to_be_extruded = dict()
for v in range(len(selectedVerts)):
    new_location = selectedVerts[v].co #* world_matrix
    x = ((new_location[0] * <size>)/2) + <size>/2
    y = ((new_location[1] * <size>)/2) + <size>/2
    # print(f"{int(x)},{int(y)}")
    # print(f"|----- new value {vertice_positions[v][1]} -----|")
    height = vertice_positions[f"{int(x)},{int(y)}"][0]
    new_location[2] = height
    selectedVerts[v].co = new_location
    # print(selectedVerts[v].co)
    # print(f"{selectedVerts[v].index}, {vertice_positions[v]}")
    struct_value = vertice_positions[f"{int(x)},{int(y)}"][1]
    if struct_value != 0:
        if str(struct_value) in to_be_extruded:
            to_be_extruded[str(struct_value)].append((v,f"{x},{y}",height))
        else:
            to_be_extruded[str(struct_value)] = [(v,f"{x},{y}",height)]

# bpy.ops.object.mode_set(mode="EDIT")
# bpy.ops.mesh.select_all(action='DESELECT')
# bpy.ops.object.mode_set(mode='OBJECT')

for _struct,vertices in to_be_extruded.items():
    bpy.ops.object.mode_set(mode="EDIT")
    bpy.ops.mesh.select_all(action='DESELECT')
    bpy.ops.object.mode_set(mode='OBJECT')
    structure_height = None
    for vert in vertices:
        bpy.context.active_object.data.vertices[vert[0]].select = True
        
        if structure_height == None or vert[2] > structure_height:
            structure_height = vert[2]
    new_struct_height = structure_height + 0.5
    bpy.ops.object.mode_set(mode="EDIT")
    bpy.ops.mesh.extrude_context()
    bpy.ops.mesh.select_all(action='DESELECT')
    bpy.ops.object.mode_set(mode='OBJECT')
    veritices_to_move = bpy.context.active_object.data.vertices[-len(vertices):]
    for v in veritices_to_move:
        v.select = True
    for v in range(len(veritices_to_move)):
        new_location = veritices_to_move[v].co
        new_location[2] = new_struct_height

bpy.ops.object.mode_set(mode="EDIT")

bpy.ops.mesh.select_all(action='DESELECT')
bpy.ops.object.mode_set(mode="OBJECT")
bpy.ops.wm.save_as_mainfile(filepath="<name>")
