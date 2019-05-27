window.onload = function() {
  document.body.appendChild(grid);
};

let size = 20;

let turn = 0;
let lastClicked;

let grid = clickableGrid(size, function(el, row, col) {
  console.log("You clicked on element:", el);
  console.log("You clicked on row:", row);
  console.log("You clicked on col:", col);

  if (isEven(turn)) { el.innerHTML = 'X'; } else {
    el.innerHTML = 'O';
  }
  el.className = 'clicked';

  if (lastClicked) lastClicked.className = '';
  lastClicked = el;
  turn++;
});


function clickableGrid(size, callback) {
  let grid = document.createElement('table');
  grid.className = 'grid';

  for (let r = 0; r < size; ++r) {
    let tr = grid.appendChild(document.createElement('tr'));
    for (let c = 0; c < size; ++c) {
      let cell = tr.appendChild(document.createElement('td'));
      cell.innerHTML = " ";
      cell.addEventListener('click', (function(el, r, c) {
        return function() {
          callback(el, r, c);
        };
      })(cell, r, c), false);
    };
  }
  return grid;
}

function isEven(n) {
  return (n % 2 == 0);
}
