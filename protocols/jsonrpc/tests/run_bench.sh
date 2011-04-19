echo -n "Commit:"
git --no-pager show -s --format="%H" HEAD

echo "Yojson:"
for i in $(seq 1 10); do
  ./yojson < latest.json | tail -n 1
done
echo
echo "Sojson:"
for i in $(seq 1 10); do
  ./sojson < latest.json | tail -n 1
done
