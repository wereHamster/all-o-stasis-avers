import * as React from "react";

import { Site } from "./Views/Components/Site";
import { Loader } from "./Components/Loader";

export function loadingView() {
  return (
    <div style={{ minHeight: "100vh", display: "flex", justifyContent: "center", alignItems: "center" }}>
      <Loader />
    </div>
  );
}

export function notFoundView() {
  return (
    <Site>
      <div className="login">
        <div className="logo">Ooops..</div>
        <div className="about">
          Not Found <a href="/">back</a>
        </div>
      </div>
    </Site>
  );
}
