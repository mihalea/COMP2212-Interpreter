begin
  var set1 = INPUT0 union INPUT1;
  var set2 = INPUT0 union INPUT2;

  var out = {};
  for x in set1 {
    out = out add x ^ "a";
  };
  print out;

  out = {};
  for x in set2 {
    out = out add x ^ "b";
  };
  print out;
end
