import { FieldValue, QueryDocumentSnapshot } from "firebase/firestore"

export class Report {
  id: string = ""
  name: string = ""
  size: number = 0
  mime: string = ""
  uploadedOn: FieldValue | number

  constructor(
    id: string,
    name: string,
    size: number,
    mime: string,
    uploadedOn: FieldValue | number
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
  fromFirestore: (snapshot: QueryDocumentSnapshot, options: any) => {
    return new Report(
      snapshot.id, //  This bit is not working, for some reason
      snapshot.data().name,
      snapshot.data().size,
      snapshot.data().mime,
      snapshot.data().uploadedOn.seconds * 1000 +
        snapshot.data().uploadedOn.nanoseconds / 1000000
    )
  },
}
