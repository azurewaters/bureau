//  This is the web component that gets the file's text
//  whether or not the file is a PDF containing text, a PDF containing images, or just an image
customElements.define(
  "text-getter",
  class TextGetter extends HTMLElement {
    private _pdfjs: any
    private _fileId?: number
    private _file?: File

    constructor() {
      super()
      console.log("constructor")
    }

    connectedCallback() {
      this._getTextInPDF(this._pdfjs, this._fileId, this._file)
    }

    disconnectedCallback() {
      console.log("Text Getter - disconnectedCallback")
    }

    set pdfjs(value: any) {
      this._pdfjs = value
      this._getTextInPDF(this._pdfjs, this._fileId, this._file)
    }

    set fileId(value: number) {
      this._fileId = value
      this._getTextInPDF(this._pdfjs, this._fileId, this._file)
    }

    set file(value: File) {
      this._file = value
      this._getTextInPDF(this._pdfjs, this._fileId, this._file)
    }

    private async _getTextInPDF(
      pdfjs: any,
      fileId?: number,
      file?: File
    ): Promise<void> {
      console.log("Going to read text")
      if (pdfjs && fileId && file) {
        let fileContents: Uint8Array = await this._readFile(file)

        //  Now, use pdfjs to extract the text from the file
        let pdf: any = await pdfjs.getDocument({
          data: fileContents,
        }).promise
        let numberOfPages: number = pdf.numPages

        //  Read the text in each page
        let promisesGettingPageText = []
        for (let pageNumber = 1; pageNumber <= numberOfPages; pageNumber++) {
          promisesGettingPageText.push(this._getTextInPage(pdf, pageNumber))
        }

        let textInPDF = (await Promise.all(promisesGettingPageText))
          .map((text: string) => text)
          .join(" ")

        //  Send the text to the parent component by throwing an event
        let event = new CustomEvent("gotText", {
          detail: { text: textInPDF },
        })
        this.dispatchEvent(event)
      }
    }

    private async _getTextInPage(
      pdf: any,
      pageNumber: number
    ): Promise<string> {
      let page: any = await pdf.getPage(pageNumber)
      let tokenisedText = await page.getTextContent()
      let text = tokenisedText.items.map((token: any) => token.str).join("")
      return text
    }

    private _readFile(file: File): Promise<Uint8Array> {
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
