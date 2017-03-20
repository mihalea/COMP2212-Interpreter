begin
  var set1 = INPUT0 union INPUT1;
  var set2 = INPUT0 union INPUT2;

  print "SETA";
  for x in set1 {
    print x ^ "a";
  };

  print "SETB";
  for x in set2 {
    print x ^ "b";
  };
end
