import * as Avers from "avers";
import * as React from "react";
import styled from "styled-components";
import * as MUI from "@material-ui/core";

import { App, config } from "../src/app";
import { Account } from "../src/storage";

import * as C from "../src/Materials/Colors";
import { useTypeface, heading18 } from "../src/Materials/Typefaces";

import { Site } from "../src/Views/Components/Site";
import { accountAvatar } from "./account";

export default ({ app }: { app: App }) => (
  <Site>
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
    } else {
      return null;
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
  changeAccountName = (e: React.FormEvent<HTMLInputElement | HTMLTextAreaElement>) => {
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
          <MUI.Paper style={{ maxWidth: 600, padding: 20, marginBottom: 20 }}>
            <FieldLabel>Your Email</FieldLabel>
            <FieldDescription>
              The email address can not be changed at this time. If you'd like to change it please contact the admins.
            </FieldDescription>
            <FieldContent>
              <span style={{ color: C.darkPrimary }}>{account.email}</span>
            </FieldContent>
          </MUI.Paper>

          <MUI.Paper style={{ maxWidth: 600, padding: 20, marginBottom: 20 }}>
            <FieldLabel>Your Name</FieldLabel>
            <FieldDescription>
              Please enter your full name, or a display name you are comfortable with.
            </FieldDescription>
            <FieldContent>
              <MUI.TextField
                variant="outlined"
                size="small"
                fullWidth
                className="wide"
                type="text"
                value={account.name}
                onChange={this.changeAccountName}
                onClick={onClick}
              />
            </FieldContent>
          </MUI.Paper>

          <MUI.Paper style={{ maxWidth: 600, padding: 20, marginBottom: 20 }}>
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
          </MUI.Paper>
        </Form>
      </div>
    );
  }
}

const requestPermissionsHref = (account: Account) => {
  const subject = `Please make me a setter`;
  const body = `Hi admin,\n\nI'm ${account.email} and am requesting permissions to manage my own boulders.\nPlease head over to ${window.location.origin}/admin/accounts and give me the 'setter' role.\n\nThank you.`;

  return `mailto:${config.adminEmail}?subject=${encodeURIComponent(subject)}&body=${encodeURIComponent(body)}`;
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
