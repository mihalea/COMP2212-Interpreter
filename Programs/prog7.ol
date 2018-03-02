begin
    var inters1 = INPUT0 intersect INPUT1;
    var union2 = INPUT1 union INPUT2;
    var res1 = inters1 union INPUT2;
    var res2 = INPUT0 intersect union2;

    print res1;
    print res2;
end
