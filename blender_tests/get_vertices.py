import bpy
from math import sqrt, floor
# size = 32

bpy.ops.object.mode_set(mode="EDIT")
bpy.ops.mesh.select_all(action='SELECT')
bpy.ops.object.mode_set(mode='OBJECT')
selectedVerts = [v for v in bpy.context.active_object.data.vertices if v.select]
size = floor(sqrt(len(selectedVerts))) - 1
# print(size)
for v in range(len(selectedVerts)):
    new_location = selectedVerts[v].co
    x = ((new_location[0] * size)/2) + size/2
    y = ((new_location[1] * size)/2) + size/2
    height = new_location[2]
    print(f"number {v} (x,y):({x},{y}) -> height:{height}")

# blender /home/orindale/Programing/terra_struct_gen/blender_tests/test_extrude.blend --background --python /home/orindale/Programing/terra_struct_gen/blender_tests/get_vertices.py