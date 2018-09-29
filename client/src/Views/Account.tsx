import * as Avers from "avers";
import * as React from "react";
import styled from "styled-components";
import Computation from "computation";

import { role } from "../actions";
import { App } from "../app";
import { Account, roles, publicProfile } from "../storage";

import { useTypeface, heading18 } from "../Materials/Typefaces";

import { DropDownInput } from "./Components/DropdownInput";
import { Site } from "./Components/Site";
import { Input } from "../Components/Input";

export const accountAvatar = (aversH: Avers.Handle, accountId: string): Computation<string> => {
  return Avers.staticValue(aversH, publicProfile(aversH, accountId)).fmap(({ avatar }) => avatar)
}

// only admins can edit all accounts with the exception of users
// changing their own accounts
export default (accountId: string) => ({ app }: { app: App }) => {
  return Avers.lookupEditable<Account>(app.data.aversH, accountId)
    .fmap(accountE => {
      const canEdit = role(app) === "admin" || accountId === app.data.session.objId;
      return <Site app={app}>{canEdit ? <AccountView app={app} accountE={accountE} /> : accountRep(app.data.aversH, accountE)}</Site>;
    })
    .get(<Site app={app} />);
};

// ----------------------------------------------------------------------------

// A simple account representation for non-owned and non-admin views.
function accountRep(aversH: Avers.Handle, account: Avers.Editable<Account>): JSX.Element {
  return (
    <Avatar>
      <img src={accountAvatar(aversH, account.objectId).get("")} />
      <Name>{account.content.name}</Name>
      <p className="about">{account.content.role}</p>
    </Avatar>
  );
}

// Render all fields as editables.
export interface AccountViewProps {
  app: App;
  accountE: Avers.Editable<Account>;
}

export class AccountView extends React.Component<AccountViewProps, {}> {
  render() {
    const { app, accountE } = this.props;

    return (
      <Root>
        <Header app={app} accountE={accountE} />
        <Editor app={app} accountE={accountE} />
      </Root>
    );
  }
}

class Header extends React.Component<{ app: App, accountE: Avers.Editable<Account> }> {
  render() {
    const { app, accountE } = this.props;

    return (
      <Avatar>
        <img src={accountAvatar(app.data.aversH, accountE.objectId).get("")} />
        <Name>{accountE.content.name}</Name>
      </Avatar>
    );
  }
}

class Editor extends React.Component<{ app: App; accountE: Avers.Editable<Account> }> {
  changeAccountName = (e: React.FormEvent<HTMLInputElement>) => {
    this.props.accountE.content.name = e.currentTarget.value;
  };

  render() {
    const { app, accountE } = this.props;

    const account = accountE.content;

    function onClick(e) {
      e.stopPropagation();
    }

    return (
      <div>
        <Form>
          <Field>
            <FieldLabel>Your Name</FieldLabel>
            <FieldDescription>
              Please enter your full name, or a display name you are comfortable with.
            </FieldDescription>
            <div className="content">
              <Input
                className="wide"
                type="text"
                value={account.name}
                onChange={this.changeAccountName}
                onClick={onClick}
              />
            </div>
          </Field>
          {role(app) === "admin" && (
            <Field>
              <FieldLabel>Role</FieldLabel>
              <FieldDescription>
                …
              </FieldDescription>
              <div className="content">
                <DropDownInput object={account} field="role" options={roles()} />
              </div>
            </Field>
          )}
        </Form>
      </div>
    );
  }
}

// ----------------------------------------------------------------------------

const Root = styled.div``;

const Avatar = styled.div`
  display: flex;
  align-items: center;
  padding: 8px 20px 20px;

  img {
    border-radius: 50%;
    height: 32px;
    width: 32px;
    margin-right: 4px;
  }
`;

const Name = styled.div`
  ${useTypeface(heading18)};
`;

const Form = styled.div`
  padding: 0 20px;
`;

const Field = styled.div`
    position: relative;
    border-radius: 5px;
    border 1px solid rgb(234, 234, 234);
    background-color: white;
    padding: 20px;
    margin-bottom: 20px;

    max-width: 600px;
`;

const FieldLabel = styled.div`
  color: rgb(0, 0, 0);
  font-size: 20px;
  font-weight: 500;
  line-height: 1;
  margin: 0px 0px 14px;
`;

const FieldDescription = styled.div`
  color: rgb(0, 0, 0);
  font-size: 12px;
  font-weight: 400;
  line-height: 1.5;
  margin: 0px 0px 14px;
`;
