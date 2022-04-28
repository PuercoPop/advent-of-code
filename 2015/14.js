const rs = {
  Vixen: [8, 8, 53],
  Blitzen: [13, 4, 49],
  Rudolph: [20, 7, 132],
  Cupid: [12, 4, 43],
  Donner: [9, 5, 38],
  Dasher: [10, 4, 37],
  Comet: [3, 36, 76],
  Prancer: [9, 12, 97],
  Dancer: [37, 1, 36],
  // Examples
  // Comet: [14, 10, 127],
};

function distance(r, at) {
  const [d, active, rest] = r;
  let tick = 0;
  let status = "active";
  let rem = active;
  let traveled = 0;

  while (tick <= at) {
    if (status === "active") {
      traveled += d;
    }
    rem -= 1;
    if (rem == 0) {
      if ((status = status === "active")) {
        status = "resting";
        rem = rest;
      } else {
        status = "active";
        rem = active;
      }
    }
    tick++;
  }
  return traveled;
}

distances = Object.fromEntries(
  Object.entries(rs).map(([r, attrs]) => [r, distance(attrs, 2503)])
);
console.log(distances);
