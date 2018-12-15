import * as React from "react";
import styled from "styled-components";
import Link from "next/link";

import { App } from "../app";
import { darkSecondary, secondaryText } from "../Materials/Colors";
import { useTypeface, copy16 } from "../Materials/Typefaces";

interface AdminBarProps {
  app: App;
}

export class AdminBar extends React.Component<AdminBarProps> {
  render() {
    return (
      <Root>
        <span>Manage</span>

        <Link href="/admin/accounts">
          <a>Accounts</a>
        </Link>
      </Root>
    );
  }
}

const Root = styled.div`
  padding: 16px 24px;
  background: ${darkSecondary};
  color: ${secondaryText + "DD"};

  ${useTypeface(copy16)};
  display: flex;
  align-items: center;

  & > *:first-child {
    display: block;
    padding-right: 16px;
    color: ${secondaryText + "88"};
    border-right: 1px solid currentColor;
  }

  a {
    display: block;
    padding: 0 16px;
    text-decoration: none;
    color: currentColor;

    &:hover {
      text-decoration: underline;
    }
  }
`;
