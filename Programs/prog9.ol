begin
    var set1 = {};
    var set2 = {};
    for x in INPUT0 {
        set1 = set1 add "a" ^ x;
    };
    for x in INPUT1 {
        var i = 0;
        var y = x;
        for i to K+1 {
            y ^= "b";
            set2 = set2 add y;
        };
    };
    var out = set1 union set2;
    print out;
end
