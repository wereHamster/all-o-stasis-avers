import * as React from "react";

import { markdown, ReactSpecimen } from "@catalog/core";

import { NavBar } from "./NavBar";
import { anonymousApp, setterApp, adminApp } from "../../Catalog/App";

export default () => markdown`
# Anonymous

${(
  <ReactSpecimen noSource>
    <NavBar app={anonymousApp} />
  </ReactSpecimen>
)}

# Setter

${(
  <ReactSpecimen noSource>
    <NavBar app={setterApp} />
  </ReactSpecimen>
)}

# Admin

${(
  <ReactSpecimen noSource>
    <NavBar app={adminApp} />
  </ReactSpecimen>
)}
`;
