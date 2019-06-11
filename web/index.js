//--------------------------------------------------
// Game model
class Game {
  constructor() {
    this.size = 20;
    this.wait = true;
    this.socket = undefined;
    this.history = [];
    this._turn = 1;
  }
  get turn() {
    this._turn = 1 - this._turn;
    return Game.labels[this._turn];
  }
  selectCell(row, col) {
    if (this.history.some(
      v => v.coord.row == row && v.coord.col == col)) {
      console.log("Cell is occupied");
    } else {
      console.log("Clicked on:", row, col);
      this.history.push({ coord: { row: row, col: col } });
      sendMove(this.socket, row, col);
    }
  }
  replayHistory() {
    this.history.forEach(move => {
      drawSelection(this, move);
    });
  }
  connect() {
    let myGame = this;
    let socket = new WebSocket('ws://127.0.0.1:9160');
    socket.onopen = function(event) {
      console.log('Connected to: ' + event.currentTarget.url);
      sendMove(socket, -1, -1);
    };
    socket.onerror = function(error) {
      console.log('WebSocket Error: ' + error);
    };
    socket.onmessage = function(event) {
      let msg = (event.data);

      let ctrlMsg = JSON.parse(msg);
      switch (ctrlMsg.mType) {
        case "User":
          switch (ctrlMsg.mValue.tag) {
            case "Connected":
              requestHistory(socket);
              drawSnackbar(ctrlMsg.mValue.contents + " connected");
              break;
            case "Disconnected":
              drawSnackbar(ctrlMsg.mValue.contents + " disconnected");
              break;
            case "Move":
              drawSelection(myGame, ctrlMsg.mValue.contents);
              break;
            case "Win":
              drawSnackbar(ctrlMsg.mValue.contents + " won!");
              break;
          }
        case "Game":
          switch (ctrlMsg.mValue.tag) {
            case "History":
              console.log('Recieved history');
              myGame.history = ctrlMsg.mValue.contents;
              myGame.replayHistory();
              break;
            case "Clean":
              myGame.history = [];
              myGame.lastMove = undefined;
              cleanGrid();
              drawSnackbar("New game started");
          }
      };
    };
    this.socket = socket;
  };
}

//--------------------------------------------------
// Visual functions


// Perform selection when cell is chosen
function drawSelection(game, move) {
  let cells = document.body.getElementsByTagName('td');
  let cellIndex = move.coord.row * game.size + move.coord.col;
  let cell = cells[cellIndex];

  if (game.lastMove)
    game.lastMove.classList
      .replace('clicked', 'normal');
  game.lastMove = cell;

  cell.innerHTML = move.value;
  cell.classList.toggle('clicked');
}

// Network
function sendMove(socket, row, col) {
  let message = {
    type: 'Move',
    value: {
      row: row,
      col: col
    }
  };
  socket.send(JSON.stringify(message));
};

function requestHistory(socket) {
  let message = {
    type: 'Command',
    value: 'GetHistory'
  };
  socket.send(JSON.stringify(message));
};

// Initialize game field
function drawGrid(game) {
  let grid = document.createElement('table');
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
  document.getElementById('myContent').appendChild(grid);
};

function cleanGrid() {
  let cells = document.body.getElementsByTagName('td');
  cells = Array.from(cells);
  cells.forEach(cell => cell.innerHTML = '');
}


function drawSnackbar(text) {
  'use strict';
  let snackbarContainer = document.querySelector('#my-snackbar');
  let data = {
    message: text,
    timeout: 2000,
    actionHandler: function(event) { },
    actionText: 'Close'
  };
  snackbarContainer.MaterialSnackbar.showSnackbar(data);
};

function requestCleanup(socket) {
  let message = {
    type: 'Command',
    value: 'CleanGrid'
  };
  socket.send(JSON.stringify(message));
}

function initInterface(socket) {
  let cleanupButton = document.getElementById('cleanupButton');
  cleanupButton.onclick = () => requestCleanup(socket);
}

//--------------------------------------------------
// "main" function
window.onload = () => {
  let game = new Game;
  drawGrid(game);
  game.connect();
  initInterface(game.socket);
};
