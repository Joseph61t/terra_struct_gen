import bpy
# from math import floor, sqrt

# bpy.ops.mesh.extrude_context()
# size = floor(sqrt(len(bpy.context.active_object.data.vertices))) - 1
# bpy.ops.object.mode_set(mode="EDIT")

bpy.ops.object.mode_set(mode = 'OBJECT')
veritices_to_move = bpy.context.active_object.data.vertices[-4:]
for v in veritices_to_move:
    v.select = True

height = max([ve.co[2] for ve in veritices_to_move]) + 1
for v in range(len(veritices_to_move)):
    new_location = veritices_to_move[v].co
    new_location[2] = height

bpy.ops.object.mode_set(mode="EDIT")


bpy.ops.wm.save_as_mainfile(filepath="/home/orindale/Programing/terra_struct_gen/blender_tests/test_extrude.blend")

    # veritices_to_move[v].select

# blender /home/orindale/Programing/terra_struct_gen/blender_tests/test_extrude.blend --background --python /home/orindale/Programing/terra_struct_gen/blender_tests/extrude_newest_vertices.py