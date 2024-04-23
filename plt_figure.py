import matplotlib.pyplot as plt

cores = [1, 2, 4, 8, 16, 32]
times = [45.25013709068298, 45.467966079711914, 42.270431995391846,
         41.92313861846924, 39.67076921463013, 41.9321186542511]

speedup = [times[0] / time for time in times]
cores_power_of_two = [2**i for i in range(6)]
plt.figure(figsize=(10, 6))
plt.plot(cores_power_of_two, speedup, marker='o', linestyle='-', color='b')
plt.xscale('log', base=2)  # 使用base参数设置x轴为对数坐标，底数为2
plt.xlabel('Number of Cores (log scale base 2)')
plt.ylabel('Speedup')
plt.title('Strong Scaling: Speedup vs Number of Cores (log scale, base 2)')
plt.grid(True, which="both", ls="--")  # 对数网格
plt.xticks(cores_power_of_two, labels=[f'$2^{i}$' for i in range(6)])  # 标记x轴为2的幂次形式
plt.yticks([round(s, 2) for s in speedup])
plt.show()

