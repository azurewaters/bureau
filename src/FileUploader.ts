import { FASTElement, customElement, observable } from "@microsoft/fast-element"
import { Auth, User } from "firebase/auth"
import {
  FirebaseStorage,
  ref,
  StorageError,
  uploadBytesResumable,
} from "firebase/storage"

@customElement("file-uploader")
export class FileUploader extends FASTElement {
  @observable auth?: Auth //  Could have used @attr but since it is not a simple primitive that will be reflected in the attribute of the custom element, we are using observable
  @observable storage?: FirebaseStorage
  @observable fileId?: number
  @observable file?: File

  constructor() {
    super()
    console.log("constructor")
  }

  connectedCallback() {
    super.connectedCallback()
    console.log("connected")
  }

  authChanged() {
    console.log("authChanged")
    this.uploadTheFile()
  }

  storageChanged() {
    console.log("storageChanged")
    this.uploadTheFile()
  }

  fileIdChanged() {
    console.log("fileIdChanged")
    this.uploadTheFile()
  }

  fileChanged() {
    console.log("fileChanged")
    this.uploadTheFile()
  }

  uploadTheFile() {
    //  This is where we upload the file if all the properties we need have been sent in
    if (this.auth?.currentUser?.uid && this.storage && this.file) {
      let randomNumber = Math.floor(Math.random() * 100000)
      let uploadFileRef = ref(
        this.storage,
        `${this.auth?.currentUser?.uid}/${randomNumber} - ${this.file.name}`
      )

      //  Now, upload the file
      let uploadTask = uploadBytesResumable(uploadFileRef, this.file)
      uploadTask.on(
        "state_changed",
        (snapshot) => {
          this.dispatchEvent(
            new CustomEvent("fileUploadProgress", {
              detail: {
                id: this.fileId,
                progress: snapshot.bytesTransferred / snapshot.totalBytes,
              },
            })
          ) //  Let Elm know about the progress
        },
        (error: StorageError) => {
          this.dispatchEvent(
            new CustomEvent("fileUploadError", { detail: { id: this.fileId } })
          ) //  Let Elm know about the error
        },
        () => {
          this.dispatchEvent(new CustomEvent("fileUploadComplete")) //  Let Elm know that the upload has completed
        }
      )
    }
  }
}
