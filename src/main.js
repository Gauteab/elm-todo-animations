import { Elm } from "./Main.elm";

const storedState = localStorage.getItem("elm-todo-save");

const startingState = storedState ? JSON.parse(storedState) : null;

const app = Elm.Main.init({ flags: startingState });

app.ports.setStorage.subscribe((state) => {
  localStorage.setItem("elm-todo-save", JSON.stringify(state));
});
