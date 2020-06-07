import * as React from "react";
import styled from "styled-components";
import { primary } from "../../Materials/Colors";

interface BackdropProps {
  open: boolean;
  invisible?: boolean;
}

export const Backdrop = ({ invisible, open }: BackdropProps) => {
  return open ? <Root invisible={invisible} aria-hidden="true" /> : null;
};

const Root = styled("div")<{ invisible?: boolean }>`
  z-index: -1;
  position: fixed;
  top: 0;
  right: 0;
  bottom: 0;
  left: 0;
  background: ${p => (p.invisible ? "transparent" : `${primary}66`)};
  -webkit-tap-highlight-color: transparent;
  touch-action: none;
`;
