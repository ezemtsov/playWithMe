//--------------------------------------------------
// Game model

class Game {
  static get labels() {
    return ["X", "O"];
  }
  constructor() {
    this.size = 20;
    this.history = [];
    this.lastMove = undefined;
    this._turn = 1;
  }
  get turn() {
    this._turn = 1 - this._turn;
    return Game.labels[this._turn];
  }
  updateState(data) {
    this.size = data.params.size;
    this.history = data.history;
  }
  selectCell(row, col) {
    console.log("Clicked on:", row, col);
    drawSelection(this, row, col);
  }
  replayHistory() {
    this.history.forEach(v =>
      this.selectCell(v.row, v.col));
  }
};

//--------------------------------------------------
// Visual functions

// Perform selection when cell is chosen
function drawSelection(game, row, col) {
  let cells = document.body.getElementsByTagName('td');
  let cellIndex = row * game.size + col;

  let cell = cells[cellIndex];

  if (game.lastMove)
    game.lastMove.classList
      .replace('clicked', 'normal');
  game.lastMove = cell;

  cell.innerHTML = game.turn;
  cell.classList.toggle('clicked');

}

// Initialize game field
function drawGrid(game) {
  var grid = document.createElement('table');
  grid.classList.toggle('grid');
  for (let r = 0; r < game.size; ++r) {
    let tr = grid
      .appendChild(document.createElement('tr'));
    for (let c = 0; c < game.size; ++c) {
      let cell = tr
        .appendChild(document.createElement('td'));
      cell.onclick = () => game.selectCell(r, c);
    };
  };
  document.body.appendChild(grid);
};

//--------------------------------------------------
// "main" function

window.onload = () => {
  let game = new Game;
  game.updateState(testData);
  drawGrid(game);
  game.replayHistory();

  game.selectCell(2, 2);

};

//--------------------------------------------------
// test data

const testData = {
  params: {
    size: 10
  },
  history: [
    { row: 0, col: 1 },
    { row: 1, col: 1 },
    { row: 2, col: 1 }]
};

//--------------------------------------------------
// Helper functions

const isEven = (n) => (n % 2 == 0);
