const size = 3;
let turn = 0;

let lastClicked;

const isEven = (n) => (n % 2 == 0);

let selectCell = (grid, row, col) => {
  let cells = grid.getElementsByTagName('td');
  let cellIndex = col + row * size;
  let cell = cells[cellIndex];

  if (cell !== lastClicked) {
    if (lastClicked)
      lastClicked.classList
        .replace('clicked', 'normal');
    lastClicked = cell;

    if (isEven(turn)) { cell.innerHTML = 'X'; }
    else { cell.innerHTML = 'O'; }
    cell.classList.toggle('clicked');

    turn++;
  }
};

let generateGrid = (size) => {
  var grid = document.createElement('table');
  grid.classList.toggle('grid');

  for (let r = 0; r < size; ++r) {
    let tr = grid
      .appendChild(document.createElement('tr'));
    for (let c = 0; c < size; ++c) {
      let cell = tr
        .appendChild(document.createElement('td'));
      cell.onclick = () => {
        console.log("Clicked on cell:", { r, c });
        selectCell(grid, r, c);
      };
    };
  };
  return grid;
};

let fillGrid = (grid) => {
  let cells = grid.getElementsByTagName('td');
  for (let [r, rv] of Object.entries(testData)) {
    for (let [c, cv] of Object.entries(rv)) {
      let cellIndex = Number(c) + Number(r) * size;
      cells[cellIndex].innerHTML = cv;
    };
  }
};

window.onload = () => {
  let grid = generateGrid(size);
  document.body.appendChild(grid);

  fillGrid(grid);
  selectCell(grid, 2, 2);
};

const testData = {
  0: {
    0: "",
    1: "X",
    2: "",
  },
  1: {
    0: "",
    1: "O",
    2: "",
  },
  2: {
    0: "",
    1: "X",
    2: "",
  }
};



