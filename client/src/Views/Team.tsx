import * as Avers from "avers";
import * as React from "react";
import styled from "styled-components";

import { App } from "../app";
import { Account } from "../storage";

import { Site } from "./Components/Site";
import { SetterBlock } from "../Components/SetterBlock";

export function teamView(app: App) {
  const setters = app.data.adminAccountCollection.ids.get([]).map(accountId => {
    const accountC = Avers.lookupContent<Account>(app.data.aversH, accountId);
    return accountC
      .fmap(account => <SetterBlock key={accountId} app={app} accountId={accountId} account={account} />)
      .get(<SetterBlock key={accountId} app={app} accountId={accountId} account={undefined} />);
  });

  return (
    <Site app={app}>
      <Root>{setters}</Root>
    </Site>
  );
}

const Root = styled.div`
  margin: 0 24px;
`;
