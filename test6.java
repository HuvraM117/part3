var a = 10;
var b = 20;

function bmethod() {
  var b = 30;
  return a + b;
}

function cmethod() {
  var a = 40;
  return bmethod() + a + b;
}

function main () {
  var b = 5;
  return cmethod() + a + b;
}
Test 7: Boolean parameters and return values. This code should return true.

function minmax(a, b, min) {
  if (min && a < b || !min && a > b)
    return true;
  else
    return false;
}

function main() {
  return (minmax(10, 100, true) && minmax(5, 3, false));
}