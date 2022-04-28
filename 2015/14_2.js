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
};

const example = {
  Comet: [14, 10, 127],
  Dancer: [16, 11, 162],
};

function makeReindeer(name, attrs) {
  return {
    name,
    speed: attrs[0],
    activeTime: attrs[1],
    restTime: attrs[2],
    score: 0,
    traveled: 0,
    status: "active",
    rem: attrs[1],
  };
}

function tick(r) {
  let n = { ...r };
  if (n.status === "active") {
    n.traveled += n.speed;
  }
  n.rem -= 1;
  if (n.rem === 0) {
    n.status = n.status === "active" ? "resting" : "active";
    n.rem = n.status === "active" ? n.activeTime : n.restTime;
  }
  return n;
}

function pickBy(reindeers, attribute) {
  return Object.values(reindeers).reduce((x, y) =>
    x[attribute] > y[attribute] ? x : y
  );
}

function solve2(rs, at) {
  let reindeers = {};
  Object.entries(rs).map(
    ([name, attrs]) => (reindeers[name] = makeReindeer(name, attrs))
  );
  for (let t = 0; t < at; t++) {
    // t === 0 && console.log(Object.keys(reindeers));
    // t === 1 && console.log(reindeers);
    Object.entries(reindeers).map(
      ([name, reindeer]) => (reindeers[name] = tick(reindeer))
    );
    let r = pickBy(reindeers, "traveled");
    let leaders = Object.values(reindeers).filter(
      (x) => x.traveled === r.traveled
    );
    leaders.map((r) => {
      reindeers[r.name] = { ...r, score: r.score + 1 };
    });
  }
  // return reindeers;
  return pickBy(reindeers, "score");
}

console.log("Day 2: ", solve2(rs, 2503));
// console.log("Day 2: ", solve2(example, 1000));
