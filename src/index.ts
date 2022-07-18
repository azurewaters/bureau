//  This is where we tell Elm to take over the <main> element.
import { FirebaseConnector } from "./FirebaseConnector"
import { Elm } from "./Main.elm"

//  Initialise everything
const main = Elm.Main.init({
  node: document.getElementsByTagName("main")[0],
})

// let fbConnector = new FirebaseConnector(main, {
//   apiKey: "AIzaSyAHdguGvP4fKrnTky5RPEr4JpV66-X2aAE",
//   authDomain: "bureau-1b322.firebaseapp.com",
//   projectId: "bureau-1b322",
//   storageBucket: "bureau-1b322.appspot.com",
//   messagingSenderId: "68963419040",
//   appId: "1:68963419040:web:4055d09f1af9110355dac5",
// })

//  Subscribe to the ports
// main.ports.signUpAUser.subscribe(fbConnector.signUpAUser.bind(fbConnector))
// main.ports.signInAUser.subscribe(fbConnector.signInAUser.bind(fbConnector))
// main.ports.uploadAFile.subscribe(fbConnector.uploadAFile.bind(fbConnector))
