import os
import argparse
# import math

def read_file(file):
    f = open(file,'r')
    map_data_string = f.read()
    map_data_list = map_data_string[1:len(map_data_string)-3].splitlines()
    # for line in map_data_list:
    #     n_line = line[:-1].replace('[','').replace(']','').strip()
    #     n_line = n_line

    proccessed_map_data_list = [line[:-1].replace('{','').replace('}','').strip().split(',') for line in map_data_list]
    map_data_points = {}
    for line in proccessed_map_data_list:
        map_data_points[f"{int(line[0].strip())},{int(line[1].strip())}"] = float(line[2].strip())
    # map_data_points = f"({int(line[0].strip())},{int(line[1].strip())}):{float(line[2].strip())}" for line in proccessed_map_data_list]
    return map_data_points

def create_blender_python(size,map_data):
    # splits = str(int(math.log(size,2)+2))
    splits = str(size-1)
    # print(splits)
    map_data_points = read_file(map_data)
    f = open("/home/orindale/Programing/terra_struct_gen/map_creation/python_blender_template.py", 'r')
    py_blender = f.read()
    f.close()
    # Make name for generated map.
    name = f"/home/orindale/Programing/terra_struct_gen/maps/map_size_{size}.blend"
    if not os.path.isfile(name):
        pass
    else:
        same_size = 1
        while True:
            name = f"/home/orindale/Programing/terra_struct_gen/maps/map_{same_size}_size_{size}.blend"
            if not os.path.isfile(name):
                break
            else:
                same_size += 1

    py_blender = py_blender.replace("<node_list>",str(map_data_points))
    py_blender = py_blender.replace("<times_to_be_split>",splits)
    py_blender = py_blender.replace("<size>",str(size))
    py_blender = py_blender.replace("<name>",name)
    file_name = f"py_blender_{size}.py"
    f = open(f"/home/orindale/Programing/terra_struct_gen/map_creation/blend_files/{file_name}",'w')
    f.write(py_blender)
    f.close()
    # open blender file just made in blender with no GUI
    # import py_blender_<size>.py into blender, and run
    os.system(f"blender /home/orindale/Programing/terra_struct_gen/map_creation/starting_map.blend --background --python /home/orindale/Programing/terra_struct_gen/map_creation/blend_files/{file_name}")



def main():
    parser = argparse.ArgumentParser(description="""Process data from generate_map.erl, and use it to create a 
    python file to be imported into blender for creating a three dimensional map.""")
    parser.add_argument("--map_size",type=int, required=True, help="The size of the map being generated")
    parser.add_argument("--map_path",type=str, required=True, help="The path to the data for the map being generated")
    args = parser.parse_args()
    create_blender_python(args.map_size,args.map_path)
                        #   4,"/home/orindale/Programing/terra_struct_gen/map_creation/maps_datas/Size_4(1)")

main()