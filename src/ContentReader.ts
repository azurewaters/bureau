import * as pdfjs from "pdfjs-dist"

export class ContentFinder {
  //  Private fields -- begin with #
  #pdfjs: any

  constructor() {
    //  Set up pdfjs
    pdfjs.GlobalWorkerOptions.workerSrc =
      "https://cdn.jsdelivr.net/npm/pdfjs-dist@2.14.305/build/pdf.worker.min.js"
  }

  async #getTextInPDF(): Promise<void> {
    console.log("Going to read text")
    if (this.#pdfjs) {
      let fileContents: Uint8Array = await this.#readFile(this.#file)

      //  Now, use pdfjs to extract the text from the file
      let pdf: pdfjs.PDFDocumentProxy = await pdfjs.getDocument({
        data: fileContents,
      }).promise
      let numberOfPages: number = pdf.numPages

      //  Read the text in each page
      let promisesGettingPageText = []
      for (let pageNumber = 1; pageNumber <= numberOfPages; pageNumber++) {
        promisesGettingPageText.push(this.#getTextInPage(pdf, pageNumber))
      }

      let textInPDF = (await Promise.all(promisesGettingPageText))
        .map((text: string) => text)
        .join(" ")

      //  Send the text to the parent component by throwing an event
    }
  }

  async #getTextInPage(pdf: any, pageNumber: number): Promise<string> {
    let page: any = await pdf.getPage(pageNumber)
    let tokenisedText = await page.getTextContent()
    let text = tokenisedText.items.map((token: any) => token.str).join("")
    return text
  }

  #readFile(file: File): Promise<Uint8Array> {
    return new Promise((resolve, reject) => {
      let reader = new FileReader()
      reader.onload = () => {
        resolve(new Uint8Array(reader.result as ArrayBuffer))
      }
      reader.readAsArrayBuffer(file)
    })
  }

  getText({ fileId: number, file: File })
}
