import { Auth, User } from "firebase/auth"
import {
  FirebaseStorage,
  ref,
  StorageError,
  uploadBytesResumable,
} from "firebase/storage"

customElements.define(
  "file-uploader",
  class FileUploader extends HTMLElement {
    //  Properties
    private _user?: User
    private _auth?: Auth
    private _storage?: FirebaseStorage
    private _fileId?: number
    private _file?: File

    constructor() {
      super()
    }

    connectedCallback() {
      console.log("File Uploader - connectedCallback")
      this._uploadFile()
    }

    disconnectedCallback() {
      console.log("File Uploader - disconnectedCallback")
    }

    set user(value: User) {
      this._user = value
      this._uploadFile()
    }

    set auth(value: Auth) {
      this._auth = value
      this._uploadFile()
    }

    set storage(value: FirebaseStorage) {
      this._storage = value
      this._uploadFile()
    }

    set fileId(value: number) {
      this._fileId = value
      this._uploadFile()
    }

    set file(value: File) {
      this._file = value
      this._uploadFile()
    }

    private _uploadFile(): void {
      //  This is where we upload a file to Firebase Storage and then record its path in the database.
      if (this._user && this._auth && this._file && this._storage) {
        let randomNumber = Math.floor(Math.random() * 100000)
        let uploadFileRef = ref(
          this._storage,
          `${this._user.uid}/${randomNumber} - ${this._file.name}`
        )

        //  Now, upload the file
        let uploadTask = uploadBytesResumable(uploadFileRef, this._file)
        uploadTask.on(
          "state_changed",
          (snapshot) => {
            //  This is where we let Elm know about the upload progress
            this.dispatchEvent(
              new CustomEvent("fileUploadProgress", {
                detail: {
                  id: this._fileId,
                  progress: snapshot.bytesTransferred / snapshot.totalBytes,
                },
              })
            )
          },
          (error: StorageError) => {
            //  This is where we let Elm know about any errors that happened while uploading the file
            this.dispatchEvent(
              new CustomEvent("fileUploadError", {
                detail: {
                  id: this._fileId,
                  errorCode: error.code,
                },
              })
            )
          },
          () => {
            //  This is where we let Elm know about the upload completion
            this.dispatchEvent(
              new CustomEvent("fileUploadComplete", {
                detail: { id: this._fileId },
              })
            )
          }
        )
      }
    }
  }
)
