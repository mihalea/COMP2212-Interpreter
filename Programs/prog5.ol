begin
  var w = {"a", "b", "c"};
  var out = {};

  for x in INPUT0 {
    for i in w {
      for j in w {
        out = out add x ^ i ^ j;
      };
    };
  };

  print out;
end
