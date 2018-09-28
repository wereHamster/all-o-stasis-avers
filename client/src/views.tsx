import * as React from "react";

import { App } from "./app";
import logo from "!!file-loader!../assets/logo.svg";

import { Site } from "./Views/Components/Site";

export function loadingView() {
  return (
    <div style={{ minHeight: "100vh", display: "flex", justifyContent: "center", alignItems: "center" }}>
      <img src={logo} style={{ display: "block", maxWidth: "50vw" }} />
    </div>
  );
}

export function notFoundView(app: App) {
  return (
    <Site app={app}>
      <div className="login">
        <div className="logo">Ooops..</div>
        <div className="about">
          Not Found <a href="/">back</a>
        </div>
      </div>
    </Site>
  );
}
