import * as React from "react";
import styled, { css } from "styled-components";
import Link from "next/link";

import { useTypeface, heading20 } from "../../Materials/Typefaces";
import { primary, primaryText, darkPrimary } from "../../Materials/Colors";

import { Backdrop } from "./Backdrop";
import { useEnv } from "../../env";

const logo = "/static/logo.svg";

const M = {
  color01: primary,
  IcMenuClose24: props => (
    <svg width={24} height={24} {...props}>
      <path fill="none" stroke="currentColor" strokeLinecap="square" strokeWidth={2} d="M20 20L4 4m0 16L20 4" />
    </svg>
  ),
  IcMenuDefault24: props => (
    <svg width={24} height={24} {...props}>
      <path fill="currentColor" fillRule="evenodd" d="M0 5h24v2H0zm0 6h24v2H0zm0 6h24v2H0z" />
    </svg>
  )
};

const mq = {
  mobile: "@media screen and (max-width: 599px)",
  desktop: "@media screen and (min-width: 600px)"
};

export const Header = React.memo(() => {
  const [showMenu, setShowMenu] = React.useState(false);
  const closeMenu = React.useCallback(() => {
    setShowMenu(false);
  }, [setShowMenu]);

  const toggleMenu = React.useCallback(() => {
    setShowMenu(x => !x);
  }, [setShowMenu]);

  const clickNavigationContainer = React.useCallback(
    (ev: React.SyntheticEvent) => {
      if (ev.target === ev.currentTarget) {
        closeMenu();
      }
    },
    [closeMenu]
  );

  React.useLayoutEffect(() => {
    if (showMenu) {
      document.documentElement.style.top = `0px`;
      document.documentElement.style.position = "relative";
      document.body.style.overflow = "hidden";
      document.body.style.position = "fixed";
      document.body.style.left = "0";
      document.body.style.right = "0";
    } else {
      document.documentElement.style.top = "";
      document.documentElement.style.position = "";
      document.body.style.overflow = "";
      document.body.style.position = "";
      document.body.style.left = "";
      document.body.style.right = "";
    }
  }, [showMenu]);

  const { app } = useEnv();

  return (
    <Root>
      <Container>
        <Inner>
          <Top>
            <Link href="https://minimum.ch" passHref>
              <Logo>
                <img src={logo} />
              </Logo>
            </Link>

            <Buttons>
              <MenuButton onClick={toggleMenu}>{showMenu ? <M.IcMenuClose24 /> : <M.IcMenuDefault24 />}</MenuButton>
            </Buttons>
          </Top>

          <BackdropWrapper>
            <Backdrop open={showMenu} />
          </BackdropWrapper>

          <NavigationContainer visible={showMenu} onClick={clickNavigationContainer}>
            <Navigation visible={showMenu}>
              <PrimaryNavigation onClick={closeMenu}>
                <Link href="/" passHref>
                  <PrimaryNavigationLink active={window.location.pathname === "/"}>Boulders</PrimaryNavigationLink>
                </Link>
                <Link href="/team" passHref>
                  <PrimaryNavigationLink active={window.location.pathname === "/team"}>Team</PrimaryNavigationLink>
                </Link>
                <Link href="/stats" passHref>
                  <PrimaryNavigationLink active={window.location.pathname === "/stats"}>Stats</PrimaryNavigationLink>
                </Link>
              </PrimaryNavigation>

              <SecondaryNavigation>
                {(() => {
                  if (app.data.session.objId) {
                    return (
                      <Link href="/settings" passHref>
                        <a>Settings</a>
                      </Link>
                    );
                  } else {
                    return (
                      <Link href="/login" passHref>
                        <a>Login</a>
                      </Link>
                    );
                  }
                })()}

                {/* <a href="https://minimum.ch">minimum.ch</a> */}
              </SecondaryNavigation>
            </Navigation>
          </NavigationContainer>
        </Inner>
      </Container>
    </Root>
  );
});

// ----------------------------------------------------------------------------

const Root = styled("header")`
  /* stacking context */
  position: relative;
  z-index: 100;
  background: ${primary};
`;

const Inner = styled("div")`
  position: relative;
`;

const Top = styled("div")`
  display: flex;
  justify-content: space-between;
  align-items: center;
  background: ${primary};
  margin: 0 -24px;
  padding: 0 24px;
  height: 64px;
  ${mq.mobile} {
    position: relative;
    z-index: 20;
  }
  ${mq.desktop} {
    height: 80px;
  }
`;

const Logo = styled("a")`
  margin: auto 0;
  display: flex;
  align-items: center;
  text-decoration: none;
  height: 100%;

  img {
    display: block;
    height: 32px;
  }

  ${mq.desktop} {
    img {
      height: initial;
    }
  }
`;

const Buttons = styled("div")`
  display: flex;
  align-items: center;
  margin-right: -24px;
  ${mq.desktop} {
    display: none;
  }
`;

const MenuButton = styled("button")`
  color: ${primaryText};
  height: 64px;
  width: 72px;
  display: flex;
  align-items: center;
  justify-content: center;
  cursor: pointer;
  /* <button> reset */
  padding: 0;
  border: none;
  background: transparent;
  outline: none;
`;

const BackdropWrapper = styled("div")`
  ${mq.desktop} {
    display: none;
  }
`;

const NavigationContainer = styled("div")<{ visible: boolean }>`
  ${mq.mobile} {
    position: fixed;
    top: 0;
    left: 0;
    width: 100vw;
    height: ${({ visible }) => (visible ? "100vh" : 0)};
    opacity: ${({ visible }) => (visible ? 1 : 0)};
    transition: ${({ visible }) => (visible ? "opacity .3s" : "opacity .2s")};
    pointer-events: ${({ visible }) => (visible ? "all" : "none")};
    overflow-y: auto;
  }
`;

const Navigation = styled("nav")<{ visible: boolean }>`
  ${mq.mobile} {
    position: absolute;
    top: 0;
    left: 0;
    right: 0;
    padding: 84px 0 40px;
    background: ${M.color01};
    transform: ${({ visible }) => (visible ? "translateY(0)" : "translateY(-150px)")};
    transition: transform ${({ visible }) => (visible ? ".4s" : ".3s ease-in-out .1s")};
  }
  ${mq.desktop} {
    z-index: 20;
  }
`;

const primaryNavigationItem = (p: { active: boolean }) => css`
  position: relative;
  display: flex;
  align-items: center;
  ${useTypeface(heading20)};
  color: ${p.active ? primaryText : `${primaryText}BE`};
  text-decoration: none;
  cursor: pointer;
  transition: all 0.12s;
  background: ${p.active ? darkPrimary : "transparent"};

  &::after {
    content: "";
    position: absolute;
    bottom: 0;
    height: 1px;
    left: 24px;
    right: 24px;
    background: ${primaryText};
    opacity: 0;
    transition: all 0.12s;
  }
  padding: 8px 24px;
  &:hover {
    color: ${primaryText};
  }
  ${mq.desktop} {
    display: flex;
    padding: 0 32px;
    &::after {
      left: 16px;
      right: 16px;
      height: 4px;
      background: ${primaryText};
      opacity: ${p.active ? 0 : 0};
    }
    &:hover {
      &::after {
        opacity: 0;
      }
    }
  }
`;

const PrimaryNavigationLink = styled("a")<{ active: boolean }>`
  ${primaryNavigationItem};
`;

const PrimaryNavigation = styled("div")`
  ${mq.desktop} {
    position: absolute;
    top: 0;
    height: 80px;
    left: 300px;
    display: flex;
    align-items: stretch;
  }
`;

const SecondaryNavigation = styled("div")`
  a {
    display: block;
    ${useTypeface(heading20)};
    color: ${primaryText}BE;
    text-decoration: none;
    padding: 8px 24px;
    transition: all 0.12s;
    &:hover {
      color: ${primaryText};
    }
  }

  ${mq.mobile} {
    margin-top: 40px;
  }

  ${mq.desktop} {
    position: absolute;
    top: 0px;
    right: -24px;
    a {
      display: inline-flex;
      align-items: center;
      height: 80px;
    }
  }
`;

const Container = styled("div")`
  padding: 0 24px;
`;
