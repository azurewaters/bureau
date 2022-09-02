//  This is where we tell Elm to take over the <main> element.
import { Elm } from "./Main.elm"

//  Initialise everything
const main = Elm.Main.init({
  node: document.getElementsByTagName("main")[0],
})
