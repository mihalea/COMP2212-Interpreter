begin
    var out = {};
    var set1 = INPUT0 intersect INPUT1;
    for x in set1 {
        out = out add "a" ^ x;
    };
    print out;
end
