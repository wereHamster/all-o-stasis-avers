import * as React from "react";
import styled from "styled-components";
import Link from "next/link";

import { useTypeface, copy14 } from "../../Materials/Typefaces";
import { text } from "../../Materials/Colors";

import { App } from "../../app";

const logo = "/static/logo.svg";

interface HeaderProps {
  app: App
}

export const Header = ({ app }: HeaderProps) => (
  <Root>
    <Logo href="https://minimum.ch">
      <LogoImage src={logo} />
    </Logo>

    <HomeItem />
    <TeamItem />
    <StatsItem />

    <FlexItem />

    {app.data.session.objId ? <AccountSettings app={app} /> : <SignInItem />}
  </Root>
);

// ----------------------------------------------------------------------------

const HomeItem = () => (
  <Link href="/" passHref>
    <Item isActive={window.location.pathname === "/"}>Boulders</Item>
  </Link>
);

const TeamItem = () => (
  <Link href="/team" passHref>
    <Item isActive={window.location.pathname === "/team"}>Team</Item>
  </Link>
);

const StatsItem = () => (
  <Link href="/stats" passHref>
    <Item isActive={window.location.pathname === "/stats"}>Stats</Item>
  </Link>
);

const SignInItem = () => (
  <Link href="/login" passHref>
    <Item>
      <i className="sign in icon" />
      Login
    </Item>
  </Link>
);

const AccountSettings = ({  }: { app: App }) => (
  <Link href="/settings" passHref>
    <Item>
      <i className="settings icon" />
      Settings
    </Item>
  </Link>
);

/*
const SignOut = ({app}: {app: App}) => (
    <Item onClick={doSignOutF(app)}>
        <i className='sign out icon'></i>Logout
    </Item>
)

function doSignOutF(app: App) {
    return e => {
        e.stopPropagation()
        Avers.signout(app.data.session).then(() => {
            navigateTo('/')
        })
    }
}
*/

const Root = styled.div`
  display: flex;
  flex-direction: row;
  align-items: center;
  height: 64px;

  ${useTypeface(copy14)};
  color: #22222288;

  text-transform: uppercase;
`;

const Logo = styled.a`
  display: block;
  text-decoration: none;
  padding: 0 16px 0 16px;

  @media (min-width: 480px) {
    padding: 0 48px 0 24px;
  }
`;
const LogoImage = styled.img`
  display: block;
  max-width: 40px;
  height: 32px;

  @media (min-width: 480px) {
    max-width: initial;
  }
`;

const FlexItem = styled.div`
  flex: 1;
`;

const Item = styled<{ isActive?: boolean }, "a">("a")`
  display: flex;
  align-items: center;
  align-self: stretch;
  padding-right: 16px;
  cursor: pointer;
  white-space: nowrap;
  text-decoration: none;

  color: ${({ isActive }) => (isActive ? "#000000EE" : "inherit")};

  &:hover {
    color: ${text};
  }

  @media (min-width: 480px) {
    padding-right: 32px;
  }
`;
