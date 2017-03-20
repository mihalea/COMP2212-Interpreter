begin
  var a = ":";
  var i = 0;
  for i to K {
    for w in INPUT0 {
      print w ^ a;
    };
    a ^= "a";
  };
end
