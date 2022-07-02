//  This is where we tell Elm to take over the <main> element.
import { Elm } from "./Main.elm"
import { initializeApp } from "firebase/app"
import { getAuth, signInWithEmailAndPassword, Unsubscribe } from "firebase/auth"
import {
  getFirestore,
  addDoc,
  collection,
  getDocs,
  QueryDocumentSnapshot,
  serverTimestamp,
  query,
  onSnapshot,
} from "firebase/firestore"
import { getStorage, ref, uploadBytesResumable } from "firebase/storage"
import { Report, reportConverter } from "./Report"

// Initialize Firebase
const firebaseConfig = {
  apiKey: "AIzaSyAHdguGvP4fKrnTky5RPEr4JpV66-X2aAE",
  authDomain: "bureau-1b322.firebaseapp.com",
  projectId: "bureau-1b322",
  storageBucket: "bureau-1b322.appspot.com",
  messagingSenderId: "68963419040",
  appId: "1:68963419040:web:4055d09f1af9110355dac5",
}
const fb = initializeApp(firebaseConfig)
const fbAuth = getAuth()

//  Initialise everything
const main = Elm.Main.init({
  node: document.getElementsByTagName("main")[0],
  flags: {
    fbAuth: fbAuth,
  },
})

//  Connect up the ports
main.ports.registerAUser.subscribe(registerAUser)
main.ports.signInAUser.subscribe(signInAUser)

//  Port handlers
function signInAUser(credentials: { email: string; password: string }) {
  signInWithEmailAndPassword(fbAuth, credentials.email, credentials.password)
    .then((authenticatedUser) => {
      //  Get the current user
      if (fbAuth.currentUser) {
        main.ports.credentialsVerified.send(fbAuth.currentUser.uid)
      }
    })
    .catch((error) => {
      main.ports.fbError.send(error)
    })
}

function registerAUser(details: { email: string; password: string }) {
  console.log(details)
  throw new Error("Function not implemented.")
}
