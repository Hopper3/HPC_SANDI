import numpy as np
import nibabel as nib
import os
from multiprocessing import Pool
import time

# 设定数据所在的路径
path_AD = 'H:/ISMRM/AD'
path_B6 = 'H:/ISMRM/B6'

# 定义一个函数来加载一个目录下的所有NIfTI文件
def load_nifti_files(folder_path):
    nifti_files = {}
    for file_name in os.listdir(folder_path):
        if file_name.endswith('.nii.gz'):
            full_path = os.path.join(folder_path, file_name)
            # 使用nibabel加载NIfTI文件
            img = nib.load(full_path)
            # 将加载的NIfTI文件存储到字典中，键是文件名，值是对应的NIfTI对象
            nifti_files[file_name] = img
    return nifti_files

# 定义一个函数来加载AD和B6下的FA、AD、MD和RD图像
def load_dti_data(path):
    dti_data = {}
    dti_data['fa'] = load_nifti_files(os.path.join(path, 'fa'))
    dti_data['ad'] = load_nifti_files(os.path.join(path, 'ad'))
    dti_data['md'] = load_nifti_files(os.path.join(path, 'md'))
    dti_data['rd'] = load_nifti_files(os.path.join(path, 'rd'))
    return dti_data

# 创建一个字典，存储AD和B6下的DTI数据
AD_DTI = {}
B6_DTI = {}

# 加载AD和B6下的DTI数据
AD_DTI = load_dti_data(path_AD)
print(AD_DTI)
B6_DTI = load_dti_data(path_B6)

# 定义一个函数来提取特定标签下的数值
def extract_label_values(label, dti_data):
    label_values = {}
    for dti_type, dti_files in dti_data.items():
        label_values[dti_type] = []
        for label_num in range(1, 167):  # 166个区域
            label_file = f'label{label_num}.nii.gz'
            if label_file in dti_files:
                label_img = dti_files[label_file].get_fdata()
                label_value = label_img[label == label_num]
                label_values[dti_type].append(label_value)
    return label_values

# 提取AD下的标签数据
AD_label_values = extract_label_values(label_img, AD_DTI)

# 举例输出
print("Example - FA values for label 1:", AD_label_values['fa'][0])
