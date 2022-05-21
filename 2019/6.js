const fs = require("fs");

const input = fs.readFileSync("./6.input", "utf8");
const graph = {}
const reachable = {}
let from, to;

input.split("\n").map((line) => {
  if (!line) return;
  [_, from, to] = /(\w+)\)(\w+)/.exec(line);
  graph[from] = graph.hasOwnProperty(from) ? graph[from].concat(to) : [to]
});

for(const homePlanet in graph) {
  let next = graph[homePlanet].slice();
  reachable[homePlanet]=next.slice();

  while(next.length > 0) {
    let currPlanet = next.pop();
    let orbitingPlanets = graph[currPlanet];
    if (orbitingPlanets) {
      next = [...next, ...orbitingPlanets];
      reachable[homePlanet] = [...reachable[homePlanet], ...orbitingPlanets]
    }
  }
}
const answer = Object.values(reachable)
  .map(planets => new Set(planets).size)
  .reduce((x, y) => x + y);
console.log(answer)
