import * as React from "react";
import { markdown, ReactSpecimen } from "catalog";
import { Button } from "./Button";

export default () => markdown`
# Disabled / Normal

${(
  <ReactSpecimen noSource span={3}>
    <Button disabled>I Am Disabled</Button>
  </ReactSpecimen>
)}
${(
  <ReactSpecimen noSource span={3}>
    <Button>Click Me!</Button>
  </ReactSpecimen>
)}
`;
