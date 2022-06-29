//  This is where we tell Elm to take over the <main> element.
import { Elm } from "./Main.elm"
import { initializeApp } from "firebase/app"
import { getAuth, signInWithEmailAndPassword } from "firebase/auth"
import {
  getFirestore,
  addDoc,
  collection,
  getDocs,
  QueryDocumentSnapshot,
  serverTimestamp,
} from "firebase/firestore"
import { getStorage, ref, uploadBytesResumable } from "firebase/storage"
import { Report, reportConverter } from "./Report"

import * as pdfjs from "pdfjs-dist"

//  Set up pdfjs
pdfjs.GlobalWorkerOptions.workerSrc =
  "https://cdn.jsdelivr.net/npm/pdfjs-dist@2.14.305/build/pdf.worker.min.js"

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
const fbStore = getFirestore()
const fbStorage = getStorage()

//  Initialise everything
const main = Elm.Main.init({
  node: document.getElementsByTagName("main")[0],
  flags: {
    pdfjs: pdfjs,
    fbAuth: fbAuth,
    fbStore: fbStore,
    fbStorage: fbStorage,
  },
})

//  Connect up the ports
main.ports.signInAUser.subscribe(signInAUser)
main.ports.registerAUser.subscribe(registerAUser)
// main.ports.processAFile.subscribe(processAFile)
main.ports.fetchTheUsersReports.subscribe(fetchTheUsersReports)

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

function processAFile(fileToProcess: { id: Number; file: File }) {
  //  This is where we upload a file to Firebase Storage and then record its path in the database.
  if (fbAuth.currentUser) {
    let userId = fbAuth.currentUser.uid
    let randomNumber = Math.floor(Math.random() * 100000)
    let filenameToUploadAs = `${randomNumber} - ${fileToProcess.file.name}`
    let reportRef = ref(fbStorage, `${userId}/${filenameToUploadAs}`)
    let uploadTask = uploadBytesResumable(reportRef, fileToProcess.file)
    uploadTask.on(
      "state_changed",
      (snapshot) => {
        //  This is where we let Elm know about the upload progress
        main.ports.uploadProgress.send({
          id: fileToProcess.id,
          progress: snapshot.bytesTransferred / snapshot.totalBytes,
        })
      },
      (error) => {
        //  This is where we let Elm know about any errors while uploading the file
        main.ports.uploadError.send({ id: fileToProcess.id }) //  Not yet letting Elm know what error occurred
      },
      async () => {
        //  Upload has completed

        //  Now, record the details of the file in the database
        await addDoc(collection(fbStore, userId), {
          name: fileToProcess.file.name,
          size: fileToProcess.file.size,
          mime: fileToProcess.file.type,
          uploadedOn: serverTimestamp(),
        })

        //  Next, we let Elm know about the upload completion
        main.ports.uploadComplete.send({
          id: fileToProcess.id,
        })
      }
    )
  } else {
    //  We shouldn't ever get here
    throw new Error("No user logged in.")
  }
}

async function fetchTheUsersReports(userId: string) {
  //  If the userId is valid
  //  Fetch all the files belonging to the user from firestore
  if (userId) {
    let reports: any[] = []
    const querySnapshot = await getDocs(
      collection(fbStore, `${userId}`).withConverter(reportConverter)
    )
    querySnapshot.forEach((storedFile: QueryDocumentSnapshot<Report>) => {
      let sF: any = storedFile.data()
      sF.uploadedOn =
        sF.uploadedOn.seconds * 1000 + sF.uploadedOn.nanoseconds / 1000000 //  Because Elm can use this
      reports.push(sF)
    })
    main.ports.fetchedUsersReports.send(reports)
  }
}
