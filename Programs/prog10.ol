begin
  var a = ":";
  var b = ":";
  var i = 0;
  var out = {};
  for i to K {
    for w in INPUT0 {
      out = out add w ^ a;
    };

    for w in INPUT1 {
      out = out add w ^ b;
    };

    a ^= "a";
    b ^= "b";
  };

  print out;
end
