import { FirebaseApp, initializeApp } from "firebase/app"
import {
  Auth,
  createUserWithEmailAndPassword,
  getAuth,
  signInWithEmailAndPassword,
  Unsubscribe,
  User,
} from "firebase/auth"
import {
  collection,
  DocumentData,
  Firestore,
  getFirestore,
  onSnapshot,
  Query,
  query,
} from "firebase/firestore"

class Recorder {
  #elmApp: any
  #fb: FirebaseApp
  #fbAuth: Auth
  #fbStore: Firestore
  #reportsListenerUnsubscriber?: Unsubscribe

  constructor(elmApp: any, firebaseConfig: any) {
    this.#fb = initializeApp(firebaseConfig)
    this.#fbAuth = getAuth()
    this.#fbStore = getFirestore()
    this.#elmApp = elmApp

    this.#fbAuth.onAuthStateChanged(this.#authStateChanged)
  }

  #authStateChanged(user: User | null) {
    if (user) {
      //  Let Elm know that we've signed in
      this.#elmApp.ports.userSignedIn.send(user.uid)

      //  Start listening for reports belonging to this user
      this.#listenForReports()
    } else {
      //  Stop listening for reports
      if (this.#reportsListenerUnsubscriber) {
        this.#reportsListenerUnsubscriber()
      }
    }
  }

  #listenForReports() {
    if (this.#fbAuth.currentUser) {
      //  Set up the query
      const q: Query<DocumentData> = query(
        collection(this.#fbStore, `${this.#fbAuth.currentUser.uid}`)
      )

      //  Listen for changes to the reports collection for this user
      this.#reportsListenerUnsubscriber = onSnapshot(q, (snapshot) => {
        snapshot.docChanges().forEach((change) => {
          switch (change.type) {
            case "added":
              const addedReport: any = change.doc.data()
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

  signUpAUser(email: string, password: string) {
    createUserWithEmailAndPassword(this.#fbAuth, email, password)
  }

  signInAUser(email: string, password: string) {
    //  This is where we sign in a user
    signInWithEmailAndPassword(this.#fbAuth, email, password)
  }
}
