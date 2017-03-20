begin
  var out = {};
  for w in INPUT0 {
    out = out add w ^ "a";
  };

  for w in INPUT1 {
    out = out add w ^ "b";
  };

  print out;
end
