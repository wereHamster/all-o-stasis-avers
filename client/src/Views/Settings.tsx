import * as Avers from "avers";
import * as React from "react";
import styled from "styled-components";

import { App } from "../app";
import { Account } from "../storage";

import * as C from "../Materials/Colors";
import { useTypeface, heading18 } from "../Materials/Typefaces";

import { Site } from "./Components/Site";
import { Input } from "../Components/Input";
import { accountAvatar } from "./Account";

export default ({ app }: { app: App }) => (
  <Site app={app}>
    <Settings app={app} accountId={app.data.session.objId} />
  </Site>
);

export interface SettingsProps {
  app: App;
  accountId: undefined | string;
}

export class Settings extends React.Component<SettingsProps> {
  render() {
    const { app, accountId } = this.props;
    const accountE = Avers.lookupEditable<Account>(app.data.aversH, accountId || "").get(undefined);

    if (accountE) {
      return (
        <Root>
          <Header app={app} accountE={accountE} />
          <Editor app={app} accountE={accountE} />
        </Root>
      );
    }
  }
}

class Header extends React.Component<{ app: App; accountE: Avers.Editable<Account> }> {
  render() {
    const { app, accountE } = this.props;

    return (
      <Avatar>
        <img src={accountAvatar(app.data.aversH, accountE.objectId)} />
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
    const { accountE } = this.props;
    const account = accountE.content;

    function onClick(e) {
      e.stopPropagation();
    }

    return (
      <div>
        <Form>
          <Field>
            <FieldLabel>Your Email</FieldLabel>
            <FieldDescription>
              The email address can not be changed at this time. If you'd like to change it please contact the admins.
            </FieldDescription>
            <FieldContent>
              <span style={{ color: C.darkPrimary }}>{account.email}</span>
            </FieldContent>
          </Field>

          <Field>
            <FieldLabel>Your Name</FieldLabel>
            <FieldDescription>
              Please enter your full name, or a display name you are comfortable with.
            </FieldDescription>
            <FieldContent>
              <Input
                className="wide"
                type="text"
                value={account.name}
                onChange={this.changeAccountName}
                onClick={onClick}
              />
            </FieldContent>
          </Field>

          <Field>
            <FieldLabel>Permissions</FieldLabel>
            <FieldDescription>
              Your role is: <span style={{ color: C.darkPrimary }}>{account.role}</span>.
              {account.role !== "setter" && (
                <p>
                  If you are setter and would like to get permissions to manage your own boulders please send{" "}
                  <a href={requestPermissionsHref(account)}>this email</a> to the admins.
                </p>
              )}
            </FieldDescription>
          </Field>
        </Form>
      </div>
    );
  }
}

const requestPermissionsHref = (account: Account) => {
  // TODO: Make the email address configurable.
  const email = "admin@boulder.app";

  const subject = `Please make me a setter`;
  const body = `Hi admin,\n\nI'm ${
    account.email
  } and am requesting permissions to manage my own boulders.\nPlease head over to ${
    window.location.origin
  }/admin/accounts and give me the 'setter' role.\n\nThank you.`;

  return `mailto:${email}?subject=${encodeURIComponent(subject)}&body=${encodeURIComponent(body)}`;
};

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

  a {
    color: ${C.darkPrimary};
  }

  & > *:last-child {
    margin-bottom: 0;
  }
`;

const FieldContent = styled.div`
  margin: 14px 0px 0px;
`;
