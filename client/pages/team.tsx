import * as React from "react";
import styled from "styled-components";

import { App, activeSetters } from ".././src/app";

import { Site } from "../src/Views/Components/Site";
import { SetterBlock } from "../src/Components/SetterBlock";

export default class extends React.Component<{ app: App }> {
  render() {
    const { app } = this.props;
    return (
      <Site app={app}>
        <Root>
          {activeSetters(app).get<string[]>([]).map(accountId => (
            <SetterBlock key={accountId} app={app} accountId={accountId} />
          ))}
        </Root>
      </Site>
    );
  }
}

const Root = styled.div`
  margin: 24px;
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(320px, 1fr));
  grid-gap: 24px;
  justify-items: stretch;
`;
