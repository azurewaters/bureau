import { Auth } from "firebase/auth"
import {
  collection,
  Firestore,
  getFirestore,
  onSnapshot,
  query,
} from "firebase/firestore"
import { reportConverter } from "./Report"

customElements.define(
  "database-connector",
  class extends HTMLElement {
    fbAuth?: Auth

    //  Private fields
    #fbStore?: Firestore

    #reportsListenerUnsubscriber?: () => void

    static get observedAttributes() {
      return ["fbAuth"]
    }

    constructor() {
      super()
      this.#fbStore = getFirestore()
    }

    connectedCallback() {
      console.log("connectedCallback")
      this.#listenForReports()
    }

    attributeChangedCallback() {
      this.#listenForReports()
    }

    disconnectedCallback() {
      if (this.#reportsListenerUnsubscriber) {
        this.#reportsListenerUnsubscriber()
      }
    }

    #listenForReports() {
      if (this.fbAuth && this.fbAuth.currentUser && this.#fbStore) {
        //  Set up the query
        const q = query(
          collection(
            this.#fbStore,
            `${this.fbAuth.currentUser.uid}`
          ).withConverter(reportConverter)
        )

        //  Listen for changes to the reports collection for this user
        this.#reportsListenerUnsubscriber = onSnapshot(q, (snapshot) => {
          snapshot.docChanges().forEach((change) => {
            switch (change.type) {
              case "added":
                const addedReport: any = change.doc.data()
                addedReport.id = change.doc.id

                this.dispatchEvent(
                  new CustomEvent("reportAdded", {
                    detail: { report: addedReport },
                  })
                )
                break

              case "modified":
                const modifiedReport: any = change.doc.data()
                modifiedReport.id = change.doc.id

                this.dispatchEvent(
                  new CustomEvent("reportModified", {
                    detail: { report: modifiedReport },
                  })
                )
                break

              case "removed":
                this.dispatchEvent(
                  new CustomEvent("reportRemoved", {
                    detail: { id: change.doc.id },
                  })
                )
                break

              default:
                break
            }
          })
        })
      }
    }
  }
)
