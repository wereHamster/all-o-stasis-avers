import * as React from "react";
import styled from "styled-components";

import { useTypeface, heading24 } from "../Materials/Typefaces";

const logo = "/static/logo.svg";

export const Loader = () => (
  <Root>
    <Image src={logo} />
    <Text>Loading…</Text>
  </Root>
);

const Root = styled.div`
  flex: 1;
  display: flex;
  flex-direction: column;
  justify-content: center;
  align-items: center;
`;

const Image = styled.img`
  display: block;
  max-width: 50vw;
`;

const Text = styled.div`
  ${useTypeface(heading24)};
  margin-top: 24px;
`;
