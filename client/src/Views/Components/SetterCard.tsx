import * as React from "react";
import styled from "styled-components";
import Link from "next/link";

import { accountAvatar } from "../../../pages/account";
import { App } from "../../app";
import { Account } from "../../storage";

export interface SetterCardProps {
  app: App;
  accountId: string;
  account: void | Account;
}

export const SetterCard = ({ app, accountId, account }: SetterCardProps) => (
  <Setter>
    <Link href={{ pathname: "/account", query: { id: accountId } }}>
      <div>
        <img src={accountAvatar(app.data.aversH, accountId)} />
        <div>{account && account.name !== "" ? account.name : accountId.slice(0, 2)}</div>
      </div>
    </Link>
  </Setter>
);

const Setter = styled.div`
  display: flex;
  flex-direction: row;
  align-items: center;
  text-align: center;
  margin-left: 10px;
  margin-right: 10px;

  img {
    height: 50px;
    border: 1px solid #999;
    border-radius: 50%;
  }
`;
