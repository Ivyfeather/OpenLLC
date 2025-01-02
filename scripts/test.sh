
conflict=$(git apply --reverse --check ../scripts/coupledL2.diff)
echo $conflict