# OpenLLC

## Compile source code

```
make init
make verify-chi
```

## Update to lastest L2/L3

```
# clear all changes in CPL2
cd coupledL2 && git reset --hard HEAD && cd ..

# fetch lastest OpenXiangShan/OpenLLC
git fetch xiangshan master

# [operate at cpl2fv branch]
# rebase fv commits above master commits
git rebase xiangshan/master
make init
```