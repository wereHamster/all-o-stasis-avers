import * as React from "react";
import styled from "styled-components";

import { primaryText, darkPrimary } from "../Materials/Colors";
import { useTypeface, copy14 } from "../Materials/Typefaces";
import { createBoulder } from "../actions";
import { useEnv } from "../env";

export const SetterBar = React.memo(() => {
  const { app } = useEnv();

  const onClick = (e: React.SyntheticEvent) => {
    e.preventDefault();
    createBoulder(app);
  };

  return (
    <Root>
      <a href="#" onClick={onClick}>
        Hier
      </a>{" "}
      klicken um einen neuen Boulder einzutragen.
    </Root>
  );
});

const Root = styled.div`
  padding: 16px 24px;
  color: ${primaryText + "DD"};

  ${useTypeface(copy14)};

  a {
    color: ${darkPrimary};
    text-decoration: none;

    &:hover {
      text-decoration: underline;
    }
  }
`;
