import * as React from "react";

import { markdown, ReactSpecimen } from "@catalog/core";
import { Form, AwaitingConfirmation } from "../../pages/login";

const nop = () => {};

export default () => markdown`
# Form (empty, email entered, submitting)

${(
  <ReactSpecimen noSource span={3}>
    <Form email="" onChangeEmail={nop} doLogin={nop} isSubmitting={false} error={undefined} />
  </ReactSpecimen>
)}

${(
  <ReactSpecimen noSource span={3}>
    <Form email="tomas.carnecky@gmail.com" onChangeEmail={nop} doLogin={nop} isSubmitting={false} error={undefined} />
  </ReactSpecimen>
)}

${(
  <ReactSpecimen noSource span={3}>
    <Form email="tomas.carnecky@gmail.com" onChangeEmail={nop} doLogin={nop} isSubmitting={true} error={undefined} />
  </ReactSpecimen>
)}

${(
  <ReactSpecimen noSource span={3}>
    <Form
      email="tomas.carnecky@gmail.com"
      onChangeEmail={nop}
      doLogin={nop}
      isSubmitting={false}
      error={"Failed to fetch"}
    />
  </ReactSpecimen>
)}


# AwaitingConfirmation

${(
  <ReactSpecimen noSource>
    <AwaitingConfirmation email="tomas.carnecky@gmail.com" onReset={nop} securityCode="Blauer Truthan" />
  </ReactSpecimen>
)}
`;
