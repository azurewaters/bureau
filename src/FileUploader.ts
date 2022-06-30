import { FASTElement, customElement, observable } from "@microsoft/fast-element"
import { Auth, User } from "firebase/auth"
import {
  Firestore,
  addDoc,
  collection,
  serverTimestamp,
} from "firebase/firestore"
import {
  FirebaseStorage,
  ref,
  StorageError,
  uploadBytesResumable,
} from "firebase/storage"

@customElement("file-uploader")
export class FileUploader extends FASTElement {
  @observable fbAuth?: Auth //  Could have used @attr but since it is not a simple primitive that will be reflected in the attribute of the custom element, we are using observable
  @observable fbStore?: Firestore
  @observable fbStorage?: FirebaseStorage
  @observable fileId?: number
  @observable file?: File

  constructor() {
    super()
  }

  connectedCallback() {
    super.connectedCallback()
    this.uploadTheFile()
  }

  uploadTheFile() {
    //  This is where we upload the file if all the properties we need have been sent in
    if (
      this.fbAuth?.currentUser?.uid &&
      this.fbStore &&
      this.fbStorage &&
      this.file
    ) {
      let randomNumber = Math.floor(Math.random() * 100000)
      let uploadFileRef = ref(
        this.fbStorage,
        `${this.fbAuth?.currentUser?.uid}/${randomNumber} - ${this.file.name}`
      )

      //  Now, upload the file
      let uploadTask = uploadBytesResumable(uploadFileRef, this.file)
      uploadTask.on(
        "state_changed",
        (snapshot) => {
          let detail = {
            id: this.fileId,
            progress: (snapshot.bytesTransferred / snapshot.totalBytes) * 100,
          }
          console.log(detail)
          this.dispatchEvent(
            new CustomEvent("fileUploadProgressed", {
              detail: detail,
            })
          ) //  Let Elm know about the progress
        },
        (error: StorageError) => {
          this.dispatchEvent(
            new CustomEvent("fileUploadErrored", {
              detail: { id: this.fileId },
            })
          ) //  Let Elm know about the error
        },
        async () => {
          //  Now, record the details of the file in the database
          if (
            this.fbAuth &&
            this.fbAuth.currentUser &&
            this.fbStore &&
            this.file
          ) {
            await addDoc(
              collection(this.fbStore, this.fbAuth.currentUser.uid),
              {
                name: this.file.name,
                size: this.file.size,
                mime: this.file.type,
                uploadedOn: serverTimestamp(),
              }
            )
          }

          this.dispatchEvent(
            new CustomEvent("fileUploadCompleted", {
              detail: { id: this.fileId },
            })
          ) //  Let Elm know that the upload has completed
        }
      )
    }
  }
}
