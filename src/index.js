import './main.css';
import { Main } from './Main.elm';

var storedState = localStorage.getItem('elm-elmo-save');
var startingState = storedState ? JSON.parse(storedState) : null;
var node = document.getElementById('root');
var elmo = Main.embed(node, startingState);

elmo.ports.setStorage.subscribe(function(state) {
  localStorage.setItem('elm-elmo-save', JSON.stringify(state));
});
