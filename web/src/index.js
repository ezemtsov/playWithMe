//--------------------------------------------------
// Game model

class Game {
  static get labels() {
    return ["X", "O"];
  }
  constructor() {
    this.size = 3;
    this.history = [];
    this.lastMove = undefined;
    this._turn = 1;
  }
  get turn() {
    this._turn = 1 - this._turn;
    return Game.labels[this._turn];
  }
  selectCell(row, col) {
    if (this.history.some(
      v => v.row == row && v.col == col)) {
      console.log("Cell is occupied");
    } else {
      console.log("Clicked on:", row, col);
      this.history.push({ row: row, col: col });
    }
  }
  fetchState(data) {
    this.size = data.params.size;
    this.history = data.history;
  }
  replayHistory() {
    this.history.forEach(v =>
      drawSelection(this, v.row, v.col));
  }
};

//--------------------------------------------------
// Visual functions

// Perform selection when cell is chosen
function drawSelection(game, row, col) {
  let cells = document.body.getElementsByTagName('td');
  let cellIndex = row * game.size + col;

  let cell = cells[cellIndex];
  console.log(cell, row, col);
  if (game.lastMove)
    game.lastMove.classList
      .replace('clicked', 'normal');
  game.lastMove = cell;

  cell.innerHTML = game.turn;
  cell.classList.toggle('clicked');
}

// Initialize game field
function drawGrid(game, socket) {
  var grid = document.createElement('table');
  grid.classList.toggle('grid');
  for (let r = 0; r < game.size; ++r) {
    let tr = grid
      .appendChild(document.createElement('tr'));
    for (let c = 0; c < game.size; ++c) {
      let cell = tr
        .appendChild(document.createElement('td'));
      cell.onclick = () => sendMove(socket, r, c);
    };
  };
  document.body.appendChild(grid);
};

//--------------------------------------------------
// Network functions

function connect(game) {
  var socket = new WebSocket('ws://127.0.0.1:9160');
  socket.onopen = function(event) {
    console.log('Connected to: ' + event.currentTarget.url);
    sendMove(socket, 0, 0);
  };
  socket.onerror = function(error) {
    console.log('WebSocket Error: ' + error);
  };
  socket.onmessage = function(event) {
    let msg = (event.data);

    if (msg) {
      try {
        let move = JSON.parse(msg);
        drawSelection(game, move.row, move.col);
      } catch (e) { console.log(msg); }
    }
  };
  return socket;
};

function sendMove(socket, row, col) {
  let message = {
    row: row,
    col: col
  };
  socket.send(JSON.stringify(message));
};
//--------------------------------------------------
// "main" function

window.onload = () => {
  let game = new Game;
  let socket = connect(game);
  drawGrid(game, socket);
  // for testing only: game.fetchState(testData);
  // for testing only: game.replayHistory();
};

//--------------------------------------------------
// test data

const test_msg = {
  player: "host",
  move: {
    row: 0,
    col: 0
  }
};

const testData = {
  params: {
    size: 20
  },
  history: []
  // { row: 0, col: 1 },
  // { row: 1, col: 1 },
  // { row: 2, col: 1 }]
};

//--------------------------------------------------
// Helper functions

const isEven = (n) => (n % 2 == 0);
