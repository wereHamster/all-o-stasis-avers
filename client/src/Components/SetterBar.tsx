import * as Avers from 'avers';
import * as React from "react";
import styled from "styled-components";

import { App } from "../app";
import { primaryText, darkPrimary } from "../Materials/Colors";
import { useTypeface, copy14 } from "../Materials/Typefaces";
import { createBoulder } from "../actions";
import { publicProfile } from '../storage';

interface SetterBarProps {
  app: App;
}

export class SetterBar extends React.Component<SetterBarProps> {
  render() {
    const { app } = this.props;

    const name = Avers.staticValue(app.data.aversH, publicProfile(app.data.aversH, app.data.session.objId!))
      .fmap(p => p.name)
      .get(undefined);

    if (!name) {
      return null;
    }

    return (
      <Root>
        Hello {name}. If you've just set a new route, please{" "}
        <a href="#" onClick={e => {
          e.preventDefault();
          createBoulder(app);
        }}>register</a> it in this boulder app. Thank you.
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
