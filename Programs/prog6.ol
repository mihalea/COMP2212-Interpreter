begin
  var s = INPUT0 union INPUT1;
  var out = {};
  for x in s{
    out = out add x ^ "a";
  };
  print out;
end
