#!/bin/bash

# 检查特定变量是否是 lazy val，如果不是则替换为 lazy val
# replace_as_lazy <val> <file>
replace_as_lazy() {
    if ! grep -q "val $1" "$2"; then
        echo "  [ERROR] 文件中未找到 'val $1'，请检查文件内容。"
        exit 1
    fi

    if grep -q "lazy val $1" "$2"; then
        echo "  文件中已存在 'lazy val $1'，无需替换。"
    else
        sed -i "s/val $1/lazy val $1/" "$2"
        echo "  替换完成，将 'val $1' 替换为 'lazy val $1'。"
    fi
}

# =================================================
echo "1. 为 TLCPL2-AsL1 替换 lazy val prefetcher" # 解决输入
replace_as_lazy "prefetcher" "./coupledL2/src/main/scala/coupledL2/CoupledL2.scala"

echo "2. 为 TL2CHIL2 替换 lazy val mshrCtl" # 验证卡死
replace_as_lazy "mshrCtl" "./coupledL2/src/main/scala/coupledL2/tl2chi/Slice.scala"

echo "3. 为 TL2CHIL2 替换 lazy val Slice"
# 因为要覆盖的 Slice() 在 val slices 里，所以不方便直接 override slices（否则还需要将 slice 连线重写一遍）
# 所以我们将原来 slices 中实例化 Slice() 部分抽象成一个函数 createSlice，
# 然后在 TL2CHIL2-FV 中再 override 这个函数

# 尝试了 sed 的方案，正则表达式太复杂了，还是采用 git patch 吧
cd coupledL2
patch_file="../scripts/coupledL2.diff"

# 检查补丁是否可以应用
git apply --check "$patch_file" &> /dev/null
conflict=$?

# 检查补丁是否已经被用过(是否可以反向应用)
git apply --reverse --check $patch_file &> /dev/null
applied=$?

# 条件判断逻辑
if [ $conflict -eq 0 ] && [ $applied -ne 0 ]; then
  echo "  补丁可以应用，正在应用..."
  git apply "$patch_file"
elif [ $conflict -ne 0 ] && [ $applied -eq 0 ]; then
  echo "  补丁已应用，无需重复应用。"
elif [ $conflict -ne 0 ] && [ $applied -ne 0 ]; then
  echo "  [ERROR] 补丁无法应用，与当前代码存在冲突！"
  exit 1
else
  echo "  不应该 apply 和 reverse 都成功，疑问？"
fi

cd ..