import { FirebaseApp, initializeApp } from "firebase/app"
import {
  Auth,
  createUserWithEmailAndPassword,
  getAuth,
  signInWithEmailAndPassword,
  Unsubscribe,
  User,
  UserCredential,
} from "firebase/auth"
import {
  addDoc,
  collection,
  DocumentData,
  Firestore,
  getFirestore,
  onSnapshot,
  Query,
  query,
  serverTimestamp,
} from "firebase/firestore"
import {
  FirebaseStorage,
  getStorage,
  ref,
  StorageError,
  uploadBytesResumable,
} from "firebase/storage"
import { Report, reportConverter } from "./Report"

export class FirebaseConnector {
  #elmApp: any
  #fb: FirebaseApp
  #fbAuth: Auth
  #fbStore: Firestore
  #fbStorage: FirebaseStorage
  #reportsListenerUnsubscriber?: Unsubscribe

  #authStateChanged(user: User | null) {
    if (user) {
      //  Let Elm know that we've signed in
      this.#elmApp.ports.userSignedIn.send(user.uid)

      //  Start listening for reports belonging to this user
      this.#listenForReports()
    } else {
      //  Stop listening for reports
      if (this && this.#reportsListenerUnsubscriber) {
        this.#reportsListenerUnsubscriber()
      }
    }
  }

  #listenForReports() {
    if (this.#fbAuth.currentUser) {
      //  Set up the query
      const q: Query<DocumentData> = query(
        collection(
          this.#fbStore,
          `${this.#fbAuth.currentUser.uid}`
        ).withConverter(reportConverter)
      )

      //  Listen for changes to the reports collection for this user
      this.#reportsListenerUnsubscriber = onSnapshot(q, (snapshot) => {
        snapshot.docChanges().forEach((change) => {
          switch (change.type) {
            case "added":
              const addedReport = change.doc.data()
              addedReport.id = change.doc.id
              this.#elmApp.ports.reportAdded.send(addedReport)
              break

            case "modified":
              const modifiedReport: any = change.doc.data()
              modifiedReport.id = change.doc.id
              this.#elmApp.ports.reportModified.send(modifiedReport)
              break

            case "removed":
              const removedReport: string = change.doc.id
              this.#elmApp.ports.reportRemoved.send(removedReport)
              break

            default:
              break
          }
        })
      })
    }
  }

  constructor(elmApp: any, firebaseConfig: any) {
    this.#fb = initializeApp(firebaseConfig)
    this.#fbAuth = getAuth(this.#fb)
    this.#fbStore = getFirestore(this.#fb)
    this.#fbStorage = getStorage(this.#fb)
    this.#elmApp = elmApp

    this.#fbAuth.onAuthStateChanged(this.#authStateChanged.bind(this))
  }

  signUpAUser(credentials: { email: string; password: string }) {
    createUserWithEmailAndPassword(
      this.#fbAuth,
      credentials.email,
      credentials.password
    )
  }

  signInAUser(credentials: { email: string; password: string }) {
    console.log(this)
    //  This is where we sign in a user
    signInWithEmailAndPassword(
      this.#fbAuth,
      credentials.email,
      credentials.password
    )
      .then((userCredential: UserCredential) => {
        this.#elmApp.ports.userSignedIn.send(userCredential.user.uid)
      })
      .catch((error) => console.log(error))
  }

  uploadAFile(fileDetails: { id: number; file: File }) {
    //  This is where we upload the file if all the properties we need have been sent in
    let randomNumber = Math.floor(Math.random() * 100000)
    let uploadFileRef = ref(
      this.#fbStorage,
      `${this.#fbAuth.currentUser?.uid}/${randomNumber} - ${
        fileDetails.file.name
      }`
    )

    //  Now, upload the file
    let uploadTask = uploadBytesResumable(uploadFileRef, fileDetails.file)
    uploadTask.on(
      "state_changed",
      (snapshot) => {
        this.#elmApp.ports.fileUploadProgressed.send({
          id: fileDetails.id,
          progress: (snapshot.bytesTransferred / snapshot.totalBytes) * 100,
        })
      },
      (error: StorageError) => {
        this.#elmApp.ports.fileUploadErrored.send(fileDetails.id)
      },
      async () => {
        //  Now, record the details of the file in the database
        if (this.#fbAuth.currentUser) {
          await addDoc(
            collection(this.#fbStore, this.#fbAuth.currentUser.uid),
            {
              name: fileDetails.file.name,
              size: fileDetails.file.size,
              mime: fileDetails.file.type,
              uploadedOn: serverTimestamp(),
            }
          )

          this.#elmApp.ports.fileUploadCompleted.send(fileDetails.id)
        }
      }
    )
  }
}
