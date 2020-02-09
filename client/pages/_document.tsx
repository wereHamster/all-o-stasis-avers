import Document, { Head, Main, NextScript } from "next/document";

export default class extends Document {
  render() {
    return (
      <html>
        <Head>
          <meta name="viewport" content="width=device-width, initial-scale=1" />

          <link rel="stylesheet" href="/static/app.css" />
          <link rel="stylesheet" href="https://unpkg.com/react-day-picker/lib/style.css" />
          <link rel="stylesheet" href="/static/UI-Icon-master/icon.css" />
          <link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Advent+Pro" />
        </Head>
        <body>
          <Main />
          <NextScript />
        </body>
      </html>
    );
  }
}
