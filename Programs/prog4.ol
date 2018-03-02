begin
  var a = ":";
  var i = 0;
  var out = {};
  for i to K {
    for w in INPUT0 {
      out = out add w ^ a;
    };
    a ^= "a";
  };
  print out;
end
