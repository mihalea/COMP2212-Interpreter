begin
    var s1 = {};
    var s2 = {};
    for x in INPUT0 {
        s1 = s1 add "a" ^ x;
    };
    for x in INPUT1 {
        s2 = s2 add "b" ^ x;
    };
    var out = s1 union s2;
    print out;
end
