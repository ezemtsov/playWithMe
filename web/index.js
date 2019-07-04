//--------------------------------------------------
// Game model
class Game {
  constructor() {
    this.size = 30;
    this.wait = true;
    this.socket = null;
    this.history = [];
    this.players = [];
    this.me = {
      id: null,
      name: null,
      token: {
        code: null,
        color: null
      }
    };
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
  updatePlayer(player) {
    updatePlayerList(player);
  }
  setToken(token) {
    // Save seleced token
    this.me.token = token;
    // Toggle selector to relevant token
    toggleTokenSelector(this.me.token);
    // Let others know that we have switched
    sendMessage(this.socket, msgUpdatePlayer(this.me));
  }
  selectCell(x, y) {
    if (this.history.some(
      v => v.coord.x == x && v.coord.y == y)) {
      console.log('Cell is occupied');
    } else {
      console.log('Clicked on:', x, y);
      sendMessage(this.socket, msgMove(x, y, this.me.token));
    }
  }
  replayHistory() {
    //Redraw all the moves from history
    this.history.forEach(move => fillCell(move));
    focusCell(this.lastMove);
    //Fill player list
    refillPlayerList(this.players);

    // // Set default token
    // let playersN = this.players.length;
    // // tokens are defined once as global alias to token class elements
    // let tokensN = Array.from(tokens).length;
    // let playerTokenId = (playersN - 1) % tokensN;

  }
  connect() {
    let game = this;
    let socket = new WebSocket('wss://tatrix.org/public/games/play-with-me/server');
    //let socket = new WebSocket('ws://34.68.64.169:8080');
    //let socket = new WebSocket('ws://0.0.0.0:8080');

    socket.onopen = function(event) {
      console.log('Connected to: ' + event.currentTarget.url);

      sendMessage(socket, msgConnect(game.me.name, "NewGame", game.session()));
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
          drawSnackbar(data.Player.name + ' connected');
          game.rememberPlayer(data.Player);
          break;
        case 'Disconnected':
          drawSnackbar(data.Player.name + ' disconnected');
          game.forgetPlayer(data.Player);
          break;
        case 'SetCell':
          unfocusCell(game.lastMove);
          game.history.push(data.Cell);
          fillCell(data.Cell);
          focusCell(data.Cell);
          break;
        case 'Win':
          drawSnackbar(data.Player.name + ' won!');
          break;
        case 'SetSession':
          console.log('Recieved session data');
          game.players = data.SessionData.players;
          game.history = data.SessionData.history;
          game.replayHistory();

          game.me = data.SessionData.me;
          toggleTokenSelector(game.me.token);

          window.history.pushState(null, null, data.SessionData.session);
          break;
        case 'Clean':
          game.history = [];
          cleanGrid();
          drawSnackbar('New game started');
          break;
        case 'UpdatePlayer':
          game.updatePlayer(data.Player);
          break;
      }
    };
    this.socket = socket;
  };
};

//--------------------------------------------------
// NETWORK FUNCTIONS

// OUTPUT

function sendMessage(socket, msg) {
  socket.send(JSON.stringify(msg));
}

function msgMove(row, col, token) {
  return {
    method: 'PostMove',
    resource: {
      Cell: {
        coord: {
          x: row,
          y: col
        },
        value: token
      }
    }
  };
}

function msgConnect(name, mode, session) {
  return {
    method: 'Connect',
    resource: {
      ConnectionData: {
        playerName: name,
        mode: mode,
        session: session
      }
    }
  };
}

function msgCleanHistory() {
  return { method: 'CleanHistory' };
}

function msgUpdatePlayer(player) {
  return {
    method: "UpdatePlayer",
    resource: {
      Player: player
    }
  };
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
      cellId(coord.x, coord.y));

    if (cell) {
      cell.innerHTML = value.code;
    }
  }
}

function focusCell(move) {
  if (move) {
    let coord = move.coord;
    let cell = document.getElementById(
      cellId(coord.x, coord.y));

    if (cell) {
      cell.classList.toggle('grid-cell_clicked');
    }
  }
}

function unfocusCell(move) {
  if (move) {
    let coord = move.coord;
    let cell = document.getElementById(
      cellId(coord.x, coord.y));

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
  li.id = player.id;
  li.innerText = (player.name + '\t(' + player.token.code + ')');

  ul.appendChild(li);
}

function removeFromPlayerList(player) {
  let li = document.getElementById(player.id);
  li.parentNode.removeChild(li);
}

function updatePlayerList(player) {
  let li = document.getElementById(player.id);
  li.innerText = (player.name + '\t(' + player.token.code + ')');
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

function toggleTokenSelector(token) {
  //tokens are defined once as global alias to token class elemets)
  for (let t of tokens) {
    t.classList.remove('mdl-button--colored');
  }
  let tokenSelector = document.getElementById("token-" + token.code);
  if (tokenSelector) {
    tokenSelector.classList.add('mdl-button--colored');
  }
}

function initInterface(game) {
  let cleanupButton = document.getElementById('cleanupButton');
  cleanupButton.onclick = () =>
    sendMessage(game.socket, msgCleanHistory());

  let dialog = document.querySelector('dialog');
  dialogPolyfill.registerDialog(dialog);
  let nameInput = dialog.querySelector('.playerName');
  let newGameButton = dialog.querySelector('.newGame');

  //tokens are defined once as global alias to token class elemets)
  for (let t of tokens) {
    t.onclick = () => {
      if (!t.classList.contains('mdl-button--colored')) {
        game.setToken({
          code: t.innerText,
          color: "black"
        });
      }
    };
  }

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
    game.me.name = name;
    game.connect();
    drawGrid(game.size, (r, c) => game.selectCell(r, c));
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
