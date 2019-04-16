import * as React from "react";
import styled from "styled-components";

import { App } from "../app";
import { primaryText, darkPrimary } from "../Materials/Colors";
import { useTypeface, copy14 } from "../Materials/Typefaces";
import { createBoulder } from "../actions";

interface SetterBarProps {
  app: App;
}

export class SetterBar extends React.Component<SetterBarProps> {
  render() {
    const { app } = this.props;

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
  }
}

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
