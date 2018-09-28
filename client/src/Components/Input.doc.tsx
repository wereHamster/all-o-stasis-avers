import * as React from "react";
import { markdown, ReactSpecimen } from "@catalog/core";
import { Input } from "./Input";

export default () => markdown`
${(
  <ReactSpecimen noSource>
    <Input defaultValue="" />
  </ReactSpecimen>
)}
`;
