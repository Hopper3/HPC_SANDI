import os
import numpy as np
import nibabel as nib
import time
from concurrent.futures import ProcessPoolExecutor
# step 1, creat 5-dimensions data array to save all data
def create_file_paths_matrix(base_path, categories):
    # Initialize a dictionary
    file_paths = {category: [] for category in categories}
    for root, dirs, files in os.walk(base_path):
        for file in files:
            for category in categories:
                if category in file and file.endswith('.nii.gz'):
                    file_paths[category].append(os.path.join(root, file))

    max_length = max(len(paths) for paths in file_paths.values())
    matrix = np.full((max_length, len(categories)), None)
    for col_index, category in enumerate(categories):
        for row_index, file_path in enumerate(file_paths[category]):
            matrix[row_index, col_index] = file_path
    
    return matrix

def load_data(file_paths):
    # load .nii.gz files
    image_data = []
    for file_path in file_paths:
        img = nib.load(file_path)
        data = img.get_fdata()
        image_data.append(data)
    return image_data

def extract_regions_data(label_data):
    # get regions data
    unique_values = np.unique(label_data[label_data != 0])
    regions_data = []

    for region_value in unique_values:
        region_data = np.zeros_like(label_data)
        region_data[label_data == region_value] = region_value
        regions_data.append(region_data)

    return regions_data

def process_labels(file_paths):
    label_data = load_data(file_paths)
    all_regions_data = []
    for label_data_entry in label_data:
        regions_data = extract_regions_data(label_data_entry)
        all_regions_data.append(regions_data)
    all_regions_data_3d = np.array(all_regions_data)
    return all_regions_data_3d

def main(num_cores):
    categories = ['fa', 'ad', 'rd', 'md', 'De', 'Din', 'extra', 'fneurite', 'fsoma', 'Rsoma', 'label']
    path = 'H:/ISMRM'
    paths_matrix = create_file_paths_matrix(path, categories)
    label_file_paths = paths_matrix[:, 10]  

    start_time = time.time()
    with ProcessPoolExecutor(max_workers=num_cores) as executor:
        all_regions_data_3d = process_labels(label_file_paths)
    end_time = time.time()
    
    print(f"Time taken with {num_cores} cores: {end_time - start_time} seconds")
    print(all_regions_data_3d.shape)

if __name__ == '__main__':
    for num_cores in [1, 2, 4, 8, 16, 32]:
        main(num_cores)
