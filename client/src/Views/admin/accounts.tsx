import * as Avers from "avers";
import * as React from "react";
import styled from "styled-components";

import { Account, roles } from "../../storage";
import { App } from "../../app";

import { Site } from "../Components/Site";
import { DropDownInput } from "../Components/DropdownInput";

export default ({ app }: { app: App }) => (
  <Site app={app}>
    <Root>
      <table>
        <thead>
          <tr>
            <th style={{ width: 100 }}>Email</th>
            <th>Role</th>
          </tr>
        </thead>
        <tbody>
          {app.data.accountsCollection.ids.get([] as string[]).map(accountId => {
            return Avers.lookupEditable<Account>(app.data.aversH, accountId)
              .fmap(accountE => (
                <tr key={accountId}>
                  <td>{accountE.content.email}</td>
                  <td>
                    <DropDownInput object={accountE.content} field="role" options={roles()} />
                  </td>
                </tr>
              ))
              .get(null);
          })}
        </tbody>
      </table>
    </Root>
  </Site>
);

const Root = styled.div`
  margin: 16px 24px;

  table {
    width: 100%;

    th {
      text-align: left;
    }
  }
`;
