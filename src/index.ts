//  This is where we tell Elm to take over the <main> element.
import { Elm } from "./Main.elm"
import { initializeApp } from "firebase/app"
import { getAuth, signInWithEmailAndPassword, createUserWithEmailAndPassword } from "firebase/auth"
import { getStorage, ref, uploadBytesResumable } from "firebase/storage"

//  Initialise everything
const main = Elm.Main.init({ node: document.getElementsByTagName('main')[0] })

const firebaseConfig = {
  apiKey: "AIzaSyAHdguGvP4fKrnTky5RPEr4JpV66-X2aAE",
  authDomain: "bureau-1b322.firebaseapp.com",
  projectId: "bureau-1b322",
  storageBucket: "bureau-1b322.appspot.com",
  messagingSenderId: "68963419040",
  appId: "1:68963419040:web:4055d09f1af9110355dac5"
};

//  Test comment
// Initialize Firebase
const fb = initializeApp(firebaseConfig)
const fbAuth = getAuth()
const fbStorage = getStorage()

//  Connect up the ports
main.ports.signInAUser.subscribe(signInAUser)
main.ports.registerAUser.subscribe(registerAUser)
main.ports.processAFile.subscribe(processAFile)

//  Port handlers
function signInAUser(credentials: { email: string, password: string }) {
  signInWithEmailAndPassword(fbAuth, credentials.email, credentials.password)
    .then((authenticatedUser) => {
      main.ports.credentialsVerified.send(true)
    })
    .catch((error) => {
      main.ports.fbError.send(error)
    })
}

function registerAUser(details: { email: string, password: string }) {
  console.log(details)
  throw new Error("Function not implemented.")
}

function processAFile(fileToProcess: { id: Number, file: File }) {
  //  This is where we upload a file to Firebase Storage and then record its path in the database.
  if (fbAuth.currentUser) {
    let userId = fbAuth.currentUser.uid
    let randomNumber = Math.floor(Math.random() * 100000)
    let filenameToUploadAs = `${randomNumber} - ${fileToProcess.file.name}`
    let uploadFileRef = ref(fbStorage, `${userId}/${filenameToUploadAs}`)
    let uploadTask = uploadBytesResumable(uploadFileRef, fileToProcess.file)
    uploadTask.on('state_changed', (snapshot) => {
      //  This is where we let Elm know about the upload progress
      main.ports.uploadProgress.send({ id: fileToProcess.id, progress: snapshot.bytesTransferred / snapshot.totalBytes })
    }, (error) => {
      //  This is where we let Elm know about any errors while uploading the file
      main.ports.uploadError.send({ id: fileToProcess.id })  //  Not yet letting Elm know what error occurred
    }, () => {
      //  This is where we let Elm know about the upload completion
      main.ports.uploadComplete.send({ id: fileToProcess.id, path: uploadFileRef.fullPath })
    })
  } else {
    //  We shouldn't ever get here
    throw new Error("No user logged in.")
  }
}
