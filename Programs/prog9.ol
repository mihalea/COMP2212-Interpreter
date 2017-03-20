begin
  var x = {"a", "b"};
  var out = {};

  for w in INPUT0 {
    for i in x {
      out = out add w ^ i;
    };
  };

  for w in INPUT1 {
    for i in x {
      out = out add w ^ i;
    };
  };

  print out;
end
