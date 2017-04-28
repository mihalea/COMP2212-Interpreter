for i in {1..10}
do
    echo "---------------- Test $i ----------------"
    ./mysplinterpreter ../Programs/prog$i.ol < ../Problems/prog$i.in
    echo "-----------------------------------------"
done
