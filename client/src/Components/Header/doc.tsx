import * as React from "react";

import { markdown, ReactSpecimen } from "@catalog/core";

import { Header } from "./Header";
import { anonymousApp, setterApp, adminApp } from "../../Catalog/App";

export default () => markdown`
# Anonymous

${(
  <ReactSpecimen noSource>
    <Header app={anonymousApp} />
  </ReactSpecimen>
)}

# Setter

${(
  <ReactSpecimen noSource>
    <Header app={setterApp} />
  </ReactSpecimen>
)}

# Admin

${(
  <ReactSpecimen noSource>
    <Header app={adminApp} />
  </ReactSpecimen>
)}
`;
