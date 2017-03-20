begin
  var w = {"a", "b", "c"};

  for x in INPUT0 {
    for i in w {
      for j in w {
        print x ^ i ^ j;
      };
    };
  };
end
