import * as pdfjs from "pdfjs-dist"

//  This is the web component that gets the file's text
//  whether or not the file is a PDF containing text, a PDF containing images, or just an image
customElements.define(
  "text-getter",
  class TextGetter extends HTMLElement {
    //  Private fields -- begin with #
    #pdfjs: any
    #fileId?: number
    #file?: File

    constructor() {
      super()
      //  Set up pdfjs
      pdfjs.GlobalWorkerOptions.workerSrc =
        "https://cdn.jsdelivr.net/npm/pdfjs-dist@2.14.305/build/pdf.worker.min.js"

      console.log("constructor - TextGetter")
    }

    connectedCallback() {
      this.#getTextInPDF()
    }

    attributeChangedCallback(name: string, oldValue: string, newValue: string) {
      this.#getTextInPDF()
    }

    async #getTextInPDF(): Promise<void> {
      console.log("Going to read text")
      if (this.#pdfjs && this.#fileId && this.#file) {
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
        let event = new CustomEvent("fileTextRead", {
          detail: { text: textInPDF },
        })
        this.dispatchEvent(event)
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
  }
)
