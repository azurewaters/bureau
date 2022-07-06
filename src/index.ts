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
import { createClient } from "@supabase/supabase-js"

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

//  Initialise Supabase
const supabase = createClient(
  "https://bgwgivatowayfodanvqf.supabase.co",
  "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6ImJnd2dpdmF0b3dheWZvZGFudnFmIiwicm9sZSI6ImFub24iLCJpYXQiOjE2NTY2NTkxNDEsImV4cCI6MTk3MjIzNTE0MX0.AfP9p5wZsXZkSbXwcTGAMwELeB1HtX1Q0iiAvWr5Glw"
)

supabase.auth.onAuthStateChange((event, session) => {
  switch (event) {
    case "SIGNED_IN":
      if (session && session.user) {
        main.ports.userSignedIn.send({
          token: session.access_token,
          userId: session.user.id,
        })
      }
      break

    case "TOKEN_REFRESHED":
      if (session) {
        main.ports.tokenRefreshed.send(session.access_token)
      }
      break

    default:
      break
  }
})

//  Initialise everything
const main = Elm.Main.init({
  node: document.getElementsByTagName("main")[0],
  flags: {
    fbAuth: fbAuth,
  },
})

//  Connect up the ports
main.ports.signUpAUser.subscribe(signUpAUser)
main.ports.signInAUser.subscribe(signInAUser)

//  Port handlers
// function signInAUser(credentials: { email: string; password: string }) {
//   signInWithEmailAndPassword(fbAuth, credentials.email, credentials.password)
//     .then((authenticatedUser) => {
//       //  Get the current user
//       if (fbAuth.currentUser) {
//         main.ports.credentialsVerified.send(fbAuth.currentUser.uid)
//       }
//     })
//     .catch((error) => {
//       main.ports.fbError.send(error)
//     })
// }

async function signUpAUser(details: { email: string; password: string }) {
  const { user, session, error } = await supabase.auth.signUp({
    email: details.email,
    password: details.password,
  })
  console.log(user, session, error)
}

async function signInAUser(credentials: { email: string; password: string }) {
  const { user, session, error } = await supabase.auth.signIn({
    email: credentials.email,
    password: credentials.password,
  })
}
