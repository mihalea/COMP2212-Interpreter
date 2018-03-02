begin
    var out = {};
    var myunion = INPUT0 union INPUT1;
    var myintersect = INPUT0 intersect INPUT1;
    for x in myunion {
        for y in myintersect {
            out = out add x ^ y;
        };
    };
    print out;
end
