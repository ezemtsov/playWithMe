//--------------------------------------------------
// Game model
class Game {
  constructor() {
    this.size = 30;
    this.wait = true;
    this.socket = null;
    this.history = [];
    this.players = [];
    this.selectedToken = "X";
  }
  get lastMove() {
    return this.history[this.history.length - 1];
  }
  get session() {
    let pathname = window.location.pathname;
    let session = () => {
      if (pathname == '/') {
        return null;
      } else {
        return pathname.slice(1);
      }
    };
    return session;
  }
  rememberPlayer(player) {
    this.players.push(player);
    addToPlayerList(player);
  }
  forgetPlayer(player) {
    this.players = this.players.filter(e => e != player);
    removeFromPlayerList(player);
  }
  selectCell(row, col) {
    let move = {
      coord: {
        row: row,
        col: col
      },
      value: this.selectedToken
    };

    if (this.history.some(
      v => v.coord.row == row && v.coord.col == col)) {
      console.log('Cell is occupied');
    } else {
      console.log('Clicked on:', row, col);
      unfocusCell(this.lastMove);
      this.history.push(move);
      sendMessage(
        this.socket,
        this.session(),
        msgMove(row, col, this.selectedToken));
    }
  }
  replayHistory() {
    //Redraw all the moves from history
    this.history.forEach(move => {
      fillCell(move);
    });
    focusCell(this.lastMove);

    // Pick a correct token.
    // It's being selected based on available tokens
    // and list of connected players.
    let playersN = this.players.length;
    // tokens are defined once as global alias to token class elements
    let tokensN = Array.from(tokens).length;
    let playerToken = (playersN - 1) % tokensN;
    switchToToken(this, tokens[playerToken]);
  }
  connect(name) {
    let game = this;
    let socket = new WebSocket('ws://34.68.64.169:8080');
    //let socket = new WebSocket('ws://0.0.0.0:8080');

    socket.onopen = function(event) {
      console.log('Connected to: ' + event.currentTarget.url);

      sendMessage(socket, game.session(), msgConnect(name));
      sendMessage(socket, game.session(), mstGetHistory(name));
    };
    socket.onerror = function(error) {
      console.log('WebSocket Error: ' + error);
    };
    socket.onmessage = function(event) {
      let msg = (event.data);
      let ctrlMsg = JSON.parse(msg);
      let data = ctrlMsg.data;
      switch (ctrlMsg.message) {
        case 'Connected':
          drawGrid(game.size, (r, c) => game.selectCell(r, c));
          drawSnackbar(data.Player + ' connected');
          game.rememberPlayer(data.Player);
          break;
        case 'Disconnected':
          drawSnackbar(data.Player + ' disconnected');
          game.forgetPlayer(data.Player);
          break;
        case 'Move':
          fillCell(data.Cell);
          focusCell(data.Cell);
          break;
        case 'Win':
          drawSnackbar(data.Player + ' won!');
          break;
        case 'SetSession':
          window.history.pushState(null, null, data.SessionId);
          break;
        case 'SetHistory':
          console.log('Recieved history');
          game.players = data.History.players;
          game.history = data.History.moves;
          game.replayHistory();
          refillPlayerList(game.players);
          break;
        case 'Clean':
          game.history = [];
          cleanGrid();
          drawSnackbar('New game started');
      }
    };
    this.socket = socket;
  };
};


//--------------------------------------------------
// NETWORK FUNCTIONS

// INPUT
// Rewrite handler function

// OUTPUT

function sendMessage(socket, session, msg) {
  let message = [session, msg];
  socket.send(JSON.stringify(message));
}

function msgMove(row, col, v) {
  return {
    method: 'PostMove',
    resource: {
      Cell: {
        coord: { row: row, col: col },
        value: v
      }
    }
  };
};

function msgConnect(name) {
  return {
    method: 'Connect',
    resource: {
      Player: name
    }
  };
}

function mstGetHistory() {
  return { method: 'GetHistory' };
};

function msgCleanHistory() {
  return { method: 'CleanHistory' };
}

//--------------------------------------------------
// GRID FUNCTIONS


function drawGrid(size, clickHandler) {
  let grid = document.createElement('div');
  grid.classList.toggle('grid');
  for (let r = 0; r < size; ++r) {
    let row = document.createElement('div');
    grid.appendChild(row);
    for (let c = 0; c < size; ++c) {
      let cell = row.appendChild(document.createElement('div'));
      cell.id = cellId(r, c);
      cell.classList.toggle('cell');
      cell.onclick = () => clickHandler(r, c);
    };
  };
  document.getElementById('grid').appendChild(grid);
};

function cleanGrid() {
  let cells = document.body.getElementsByClassName('cell');
  cells = Array.from(cells);
  cells.forEach(cell => cell.innerHTML = '');
}

function fillCell(move) {
  if (move) {
    let coord = move.coord;
    let value = move.value;
    let cell = document.getElementById(
      cellId(coord.row, coord.col));

    if (cell) {
      cell.innerHTML = value;
    }
  }
}

function focusCell(move) {
  if (move) {
    let coord = move.coord;
    let cell = document.getElementById(
      cellId(coord.row, coord.col));

    if (cell) {
      cell.classList.toggle('grid-cell_clicked');
    }
  }
}

function unfocusCell(move) {
  if (move) {
    let coord = move.coord;
    let cell = document.getElementById(
      cellId(coord.row, coord.col));

    if (cell) {
      cell.classList
        .replace('grid-cell_clicked', 'grid-cell_normal');
    }
  }
}

//--------------------------------------------------
// INTERFACE FUNCTIONS

function refillPlayerList(players) {
  let ul = document.querySelector('.player-list-item');
  while (ul.firstChild) {
    ul.removeChild(ul.firstChild);
  }
  players.forEach(player => addToPlayerList(player));
}

function addToPlayerList(player) {
  let ul = document.querySelector('.player-list-item');
  let li = document.createElement('li');
  li.classList.add('mdl-list__item');
  li.id = player;
  li.appendChild(document.createTextNode(player));
  ul.appendChild(li);
}

function removeFromPlayerList(player) {
  let li = document.getElementById(player);
  li.parentNode.removeChild(li);
}

function drawSnackbar(text) {
  'use strict';
  let snackbarContainer = document.querySelector('#my-snackbar');
  let data = {
    message: text,
    timeout: 2000,
    actionHandler: function() { },
    actionText: 'Close'
  };
  snackbarContainer.MaterialSnackbar.showSnackbar(data);
};

function switchToToken(game, token) {
  //tokens are defined once as global alias to token class elemets)
  for (let t of tokens) {
    t.classList.remove('mdl-button--colored');
  }
  token.classList.add('mdl-button--colored');
  game.selectedToken = token.innerText;
}

function initInterface(game) {
  let cleanupButton = document.getElementById('cleanupButton');
  cleanupButton.onclick = () =>
    sendMessage(game.socket, game.session, msgCleanHistory());

  let dialog = document.querySelector('dialog');
  dialogPolyfill.registerDialog(dialog);
  let nameInput = dialog.querySelector('.playerName');
  let newGameButton = dialog.querySelector('.newGame');

  //tokens are defined once as global alias to token class elemets)
  for (let t of tokens)
    t.onclick = () => switchToToken(game, t);

  nameInput.addEventListener('keyup', function(event) {
    if (event.keyCode === 13) {
      event.preventDefault();
      newGameButton.click();
    }
    if (nameInput.value != '') {
      newGameButton.removeAttribute('disabled');
    } else {
      newGameButton.setAttribute('disabled', null);
    }
  });

  newGameButton.addEventListener('click', function() {
    let name = nameInput.value;
    game.connect(name);
    dialog.close();
  });

  dialog.showModal();
}

//--------------------------------------------------
// "main" function
window.onload = () => {
  let game = new Game;
  initInterface(game);
};

let tokens = document.getElementsByClassName('token');

const cellId = (r, c) => {
  return "cell-" + r + ":" + c;
};
