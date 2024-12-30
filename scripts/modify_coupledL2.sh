#!/bin/bash

# 定义文件路径
file="./coupledL2/src/main/scala/coupledL2/CoupledL2.scala"

# 检查是否存在 "lazy val prefetcher"
if grep -q "lazy val prefetcher" "$file"; then
    echo "文件中已存在 'lazy val prefetcher'，无需替换。"
else
    # 如果不存在，则替换 "val prefetcher" 为 "lazy val prefetcher"
    sed -i 's/val prefetcher/lazy val prefetcher/' "$file"
    echo "替换完成，将 'val prefetcher' 替换为 'lazy val prefetcher'。"
fi
