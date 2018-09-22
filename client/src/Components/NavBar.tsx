import * as React from "react";
import styled from "styled-components";

import { useTypeface, copy14 } from "../Materials/Typefaces";
import { text } from "../Materials/Colors";

import { createBoulder, role } from "../actions";
import { App, navigateToFn } from "../app";

import logo from "!!url-loader!../../assets/logo.svg";

export const NavBar = ({ app }: { app: App }) => (
  <Root>
    <Logo href="/">
      <LogoImage src={logo} />
    </Logo>

    <HomeItem />
    <TeamItem />
    {role(app) === "admin" && <SectorItem />}
    {role(app) === "admin" && <StatsItem />}

    <FlexItem />

    {role(app) !== "user" && <CreateBoulder app={app} />}
    {app.data.session.objId ? <AccountSettings app={app} /> : <SignInItem />}
  </Root>
);

// ----------------------------------------------------------------------------

const HomeItem = () => (
  <Item isActive={window.location.pathname === "/"} onClick={navigateToFn("/")}>
    Boulders
  </Item>
);

const TeamItem = () => (
  <Item isActive={window.location.pathname === "/team"} onClick={navigateToFn("/team")}>
    Setters
  </Item>
);

const SectorItem = () => (
  <Item isActive={window.location.pathname === "/sector"} onClick={navigateToFn("/sector")}>
    Sectors
  </Item>
);

const StatsItem = () => (
  <Item onClick={navigateToFn("/stats")}>
    <i className="line chart icon" />
  </Item>
);

const SignInItem = () => (
  <Item onClick={navigateToFn("/login")}>
    <i className="sign in icon" />
    Login
  </Item>
);

const CreateBoulder = ({ app }: { app: App }) => (
  <Item onClick={createNewBoulder(app)}>
    <i className="plus square outlined icon" />
    Boulder
  </Item>
);

const AccountSettings = ({ app }: { app: App }) => (
  <Item onClick={navigateToFn("/account/" + app.data.session.objId)}>
    <i className="settings icon" />
    Account Settings
  </Item>
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

function createNewBoulder(app: App) {
  return e => {
    e.stopPropagation();
    createBoulder(app);
  };
}

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

const Item = styled<{ isActive?: boolean }, "div">("div")`
  display: flex;
  align-items: center;
  align-self: stretch;
  padding-right: 16px;
  cursor: pointer;
  white-space: nowrap;

  color: ${({ isActive }) => (isActive ? "#000000EE" : "inherit")};

  &:hover {
    color: ${text};
  }

  @media (min-width: 480px) {
    padding-right: 32px;
  }
`;