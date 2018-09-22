import * as React from "react";
import { markdown, ReactSpecimen } from "catalog";
import { Input } from "./Input";

export default () => markdown`
${(
  <ReactSpecimen noSource>
    <Input defaultValue="" />
  </ReactSpecimen>
)}
`;
