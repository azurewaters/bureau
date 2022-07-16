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
    //  Since the uploadedOn is supposed to be a timestamp, it may not be populated when a new report is added
    //  So, we set the value to 0 in such a case
    let uploadedOn = snapshot.data().uploadedOn
      ? snapshot.data().uploadedOn.seconds * 1000 +
        snapshot.data().uploadedOn.nanoseconds / 1000000
      : 0

    //  Read the report's values and
    return new Report(
      snapshot.id, //  This bit is not working, for some reason
      snapshot.data().name,
      snapshot.data().size,
      snapshot.data().mime,
      uploadedOn
    )
  },
}
