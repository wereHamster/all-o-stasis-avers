import * as React from "react";
import styled from "styled-components";

import { activeSetters } from ".././src/app";
import { useEnv } from "../src/env";

import { Site } from "../src/Views/Components/Site";
import { SetterBlock } from "../src/Components/SetterBlock";

export default () => {
  const { app } = useEnv();

  return (
    <Site>
      <Root>
        {activeSetters(app)
          .get<string[]>([])
          .map(accountId => (
            <SetterBlock key={accountId} accountId={accountId} />
          ))}
      </Root>
    </Site>
  );
};

const Root = styled.div`
  margin: 24px;
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(320px, 1fr));
  grid-gap: 24px;
  justify-items: stretch;
`;
