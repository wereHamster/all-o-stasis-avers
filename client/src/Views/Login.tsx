import * as Avers from "avers";
import * as React from "react";
import styled from "styled-components";

import { text, secondary, secondaryText } from "../Materials/Colors";
import { useTypeface, h1, copy16, copy16Bold } from "../Materials/Typefaces";

import { App, navigateTo } from "../app";
import { Button } from "../Components/Button";
import { Input } from "../Components/Input";
import { Site } from "./Components/Site";

interface LoginState {
  email: string;

  createPassportPromise: void | Promise<any>;
  createPassportResponse: void | { passportId: string; securityCode: string };

  awaitPassportConfirmationPromise: void | Promise<any>;
}

export default class extends React.Component<{ app: App }, LoginState> {
  state: LoginState = {
    email: "",

    createPassportPromise: undefined,
    createPassportResponse: undefined,

    awaitPassportConfirmationPromise: undefined
  };

  onChangeEmail = e => {
    this.setState({
      email: e.target.value,

      createPassportPromise: undefined,
      createPassportResponse: undefined,

      awaitPassportConfirmationPromise: undefined
    });
  };

  onReset = e => {
    e.preventDefault();
    this.setState({
      email: "",

      createPassportPromise: undefined,
      createPassportResponse: undefined,

      awaitPassportConfirmationPromise: undefined
    });
  };

  doLogin = e => {
    const {
      app: {
        data: {
          aversH: {
            config: { fetch, apiHost }
          },
          session
        }
      }
    } = this.props;
    const { email } = this.state;

    e.stopPropagation();
    e.preventDefault();

    const options: RequestInit = {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ email })
    };

    const createPassportPromise = fetch(apiHost + "/login", options).then(res => {
      res.json().then(json => {
        const awaitPassportConfirmationPromise = fetch(apiHost + "/login/verify?passportId=" + json.passportId, {
          credentials: "include"
        }).then(() => {
          Avers.restoreSession(session);
          navigateTo("/");
        });
        this.setState({
          createPassportPromise: undefined,
          createPassportResponse: json,
          awaitPassportConfirmationPromise
        });
      });
    });
    this.setState({ createPassportPromise });
  };

  render() {
    const { app } = this.props;
    const { email, createPassportPromise, createPassportResponse, awaitPassportConfirmationPromise } = this.state;

    if (!awaitPassportConfirmationPromise) {
      return (
        <Site app={app}>
          <Container>
            <Form
              email={email}
              onChangeEmail={this.onChangeEmail}
              doLogin={this.doLogin}
              isSubmitting={createPassportPromise !== undefined}
            />
          </Container>
        </Site>
      );
    } else if (createPassportResponse && awaitPassportConfirmationPromise) {
      return (
        <Site app={app}>
          <Container>
            <AwaitingConfirmation
              email={email}
              onReset={this.onReset}
              securityCode={createPassportResponse.securityCode}
            />
          </Container>
        </Site>
      );
    } else {
      return <div>IMPOSSIBLE</div>;
    }
  }
}

const Container = styled.div`
  flex: 1;
  display: flex;
  flex-direction: column;
  justify-content: center;
  align-items: center;
  padding-bottom: 20vh;
  text-align: center;

  > * {
    max-width: 528px;
  }

  input {
    text-align: center;
  }
`;

export const Form = ({ email, onChangeEmail, doLogin, isSubmitting }) => (
  <form>
    <H1>Authenticate</H1>
    <P>To sign up or log in, fill in your email address below:</P>
    <Input type="text" placeholder="you@domain.com" value={email} onChange={onChangeEmail} disabled={isSubmitting} />
    <div style={{ marginTop: 12 }}>
      <Button onClick={doLogin} disabled={isSubmitting || email.length === 0}>
        LOGIN
      </Button>
    </div>
  </form>
);

export const AwaitingConfirmation = ({ email, onReset, securityCode }) => (
  <div>
    <H1>Authenticating</H1>

    <P>
      We sent an email to <strong>{email}</strong> (
      <a href="#" onClick={onReset}>
        undo
      </a>
      ).
    </P>

    <P>Please verify that the provided security code in the email matches the following text:</P>

    <SecurityCode>{securityCode}</SecurityCode>

    <P>Waiting for your confirmation…</P>
  </div>
);

const H1 = styled.h1`
  ${useTypeface(h1)};
  color: ${text};
  margin: 0;
`;

const P = styled.p`
  ${useTypeface(copy16)};
  color: ${text};
`;

const SecurityCode = styled.div`
  ${useTypeface(copy16Bold)};
  background-color: ${secondary};
  color: ${secondaryText};
  padding: 16px 0;
  font-weight: bold;
  margin: 40px auto;
  display: flex;
  align-items: center;
  justify-content: center;
`;
