import * as Avers from "avers";
import * as React from "react";
import styled from "styled-components";

import { App } from "../app";
import { Account } from "../storage";

import { useTypeface, heading18 } from "../Materials/Typefaces";

import { Site } from "./Components/Site";
import { Input } from "../Components/Input";
import { accountAvatar } from "./Account";

export default ({ app }: { app: App }) => {
  const accountId = app.data.session.objId;
  if (accountId) {
    return Avers.lookupEditable<Account>(app.data.aversH, accountId)
      .fmap(accountE => (
        <Site app={app}>
          <Settings app={app} accountE={accountE} />
        </Site>
      ))
      .get(<Site app={app} />);
  } else {
    return <Site app={app} />;
  }
};

// Render all fields as editables.
export interface SettingsProps {
  app: App;
  accountE: Avers.Editable<Account>;
}

export class Settings extends React.Component<SettingsProps, {}> {
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

class Header extends React.Component<{ app: App; accountE: Avers.Editable<Account> }> {
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
    const { accountE } = this.props;
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
