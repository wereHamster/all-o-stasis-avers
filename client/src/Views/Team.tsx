import * as Avers from "avers";
import * as React from "react";
import Computation from "computation";
import styled from "styled-components";

import { App } from "../app";
import { Account, Boulder } from "../storage";

import { Site } from "./Components/Site";
import { SetterBlock } from "../Components/SetterBlock";

export default class extends React.Component<{ app: App }> {
  render() {
    const { app } = this.props;

    // This code is very inefficient because we have to fetch the content(!)
    // of all admin accounts and active boulders on the client to determine
    // which setters are active (have boulders on the wall).
    const activeSetters = Computation.liftA2(
      app.data.activeBouldersCollection.ids,
      app.data.adminAccountCollection.ids,
      (activeBoulderIds, adminAccountsIds) => {
        const accounts: Array<{ accountId: string; account: Account }> = [];

        const activeSetterIds = new Set<string>();
        activeBoulderIds.forEach(boulderId => {
          Avers.lookupContent<Boulder>(app.data.aversH, boulderId)
            .fmap(boulder => {
              boulder.setter.forEach(setterId => {
                activeSetterIds.add(setterId);
              });
            })
            .get(undefined);
        });

        adminAccountsIds.forEach(accountId => {
          if (activeSetterIds.has(accountId)) {
            Avers.lookupContent<Account>(app.data.aversH, accountId)
              .fmap(account => {
                accounts.push({ accountId, account });
              })
              .get(undefined);
          }
        });

        return accounts;
      }
    ).get([]);

    return (
      <Site app={app}>
        <Root>
          {activeSetters.map(({ accountId, account }) => (
            <SetterBlock key={accountId} app={app} accountId={accountId} account={account} />
          ))}
        </Root>
      </Site>
    );
  }
}

const Root = styled.div`
  margin: 0 24px;
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(300px, 1fr));
  grid-gap: 30px 30px;
  justify-items: stretch;
`;
