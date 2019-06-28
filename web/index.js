//--------------------------------------------------
// Game model
class Game {
  constructor() {
    this.size = 30;
    this.wait = true;
    this.socket = null;
    this.history = [];
    this.players = [];
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
    if (this.history.some(
      v => v.coord.row == row && v.coord.col == col)) {
      console.log('Cell is occupied');
    } else {
      console.log('Clicked on:', row, col);
      this.history.push({ coord: { row: row, col: col } });
      sendMessage(this.socket, this.session(), msgMove(row, col));
    }
  }
  replayHistory() {
    this.history.forEach(move => {
      drawSelection(this, move);
    });
  }
  connect(name) {
    let myGame = this;
    let socket = new WebSocket('ws://34.68.64.169:8080');
    //let socket = new WebSocket('ws://0.0.0.0:8080');

    socket.onopen = function(event) {
      console.log('Connected to: ' + event.currentTarget.url);
      // console.log('onOpen', myGame.session(), msgJoinSession(name));
      sendMessage(socket, myGame.session(), msgJoinSession(name));
    };
    socket.onerror = function(error) {
      console.log('WebSocket Error: ' + error);
    };
    socket.onmessage = function(event) {
      let msg = (event.data);

      let ctrlMsg = JSON.parse(msg);
      switch (ctrlMsg.mType) {
        case 'User':
          switch (ctrlMsg.mValue.tag) {
            case 'Connected':
              drawSnackbar(ctrlMsg.mValue.contents + ' connected');
              sendMessage(myGame.socket, myGame.session(), msgRequestHistory());
              myGame.rememberPlayer(ctrlMsg.mValue.contents);
              break;
            case 'Disconnected':
              drawSnackbar(ctrlMsg.mValue.contents + ' disconnected');
              myGame.forgetPlayer(ctrlMsg.mValue.contents);
              break;
            case 'Move':
              drawSelection(myGame, ctrlMsg.mValue.contents);
              break;
            case 'Win':
              drawSnackbar(ctrlMsg.mValue.contents + ' won!');
              break;
          }
        case 'Game':
          switch (ctrlMsg.mValue.tag) {
            case 'NewSession':
              window.history.pushState(null, null, ctrlMsg.mValue.contents);
              break;
            case 'History':
              let history = ctrlMsg.mValue.contents[0];
              let players = ctrlMsg.mValue.contents[1];
              myGame.players = players;
              myGame.history = history;
              myGame.replayHistory();
              refillPlayerList(myGame.players);
              break;
            case 'Clean':
              myGame.history = [];
              myGame.lastMove = null;
              cleanGrid();
              drawSnackbar('New game started');
          }
      };
    };
    this.socket = socket;
  };
}


//--------------------------------------------------
// NETWORK FUNCTIONS

function sendMessage(socket, session, msg) {
  let message = [session, msg];
  socket.send(JSON.stringify(message));
}

function msgMove(row, col) {
  return {
    tag: 'Post',
    contents: {
      tag: 'Move',
      contents: {
        row: row,
        col: col
      }
    }
  };
};

function msgJoinSession(name) {
  return {
    tag: 'Connect',
    contents: {
      tag: 'Player',
      contents: name
    }
  };
}

function msgRequestHistory() {
  return {
    tag: 'Get',
    contents: {
      tag: 'History'
    }
  };
}


function msgCleanHistory() {
  return {
    tag: 'Delete',
    contents: {
      tag: 'History'
    }
  };
}


//--------------------------------------------------
// GRID FUNCTIONS


function drawGrid(game) {
  let grid = document.createElement('div');
  grid.classList.toggle('grid');
  for (let r = 0; r < game.size; ++r) {
    let row = document.createElement('div');
    grid.appendChild(row);
    for (let c = 0; c < game.size; ++c) {
      let cell = row.appendChild(document.createElement('div'));
      cell.id = cellId(r, c);
      cell.classList.toggle('cell');
      cell.onclick = () => game.selectCell(r, c);
    };
  };
  document.getElementById('myContent').appendChild(grid);
};

function cleanGrid() {
  let cells = document.body.getElementsByClassName('cell');
  cells = Array.from(cells);
  cells.forEach(cell => cell.innerHTML = '');
}

function drawSelection(game, move) {
  let cell = document.getElementById(cellId(move.coord.row, move.coord.col));

  if (game.lastMove)
    game.lastMove.classList
      .replace('grid-cell_clicked', 'grid-cell_normal');
  game.lastMove = cell;

  cell.innerHTML = move.value;
  cell.classList.toggle('grid-cell_clicked');
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
    actionHandler: function(event) { },
    actionText: 'Close'
  };
  snackbarContainer.MaterialSnackbar.showSnackbar(data);
};


function initInterface(game) {
  let cleanupButton = document.getElementById('cleanupButton');
  cleanupButton.onclick = () => sendMessage(game.socket, game.session, msgCleanHistory());

  let dialog = document.querySelector('dialog');
  dialogPolyfill.registerDialog(dialog);
  let nameInput = dialog.querySelector('.playerName');
  let newGameButton = dialog.querySelector('.newGame');

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

  drawGrid(game);
  initInterface(game);
};


const cellId = (r, c) => {
  return "cell-" + r + ":" + c;
};
