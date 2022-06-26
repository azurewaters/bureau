import { FieldValue } from "firebase/firestore"

export class Report {
  id: string = ""
  name: string = ""
  size: number = 0
  mime: string = ""
  uploadedOn: FieldValue

  constructor(
    id: string,
    name: string,
    size: number,
    mime: string,
    uploadedOn: FieldValue
  ) {
    this.name = name
    this.size = size
    this.mime = mime
    this.uploadedOn = uploadedOn
  }
}

export const reportConverter = {
  toFirestore: (uploadedFile: Report) => {
    return uploadedFile
  },
  fromFirestore: (snapshot: any, options: any) => {
    return new Report(
      snapshot.id,
      snapshot.data().name,
      snapshot.data().size,
      snapshot.data().mime,
      snapshot.data().uploadedOn
    )
  },
}
