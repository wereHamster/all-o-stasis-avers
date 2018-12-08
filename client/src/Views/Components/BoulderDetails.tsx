import * as React from "react";
import styled from "styled-components";
import Link from "next/link";

import { App } from "../../app";
import { Boulder } from "../../storage";

import { text } from "../../Materials/Colors";
import { useTypeface, copy16Bold } from "../../Materials/Typefaces";

import { SectorPicker } from "./SectorPicker";
import { BoulderSetterCard } from "./BoulderSetterCard";

export const BoulderDetails = ({ app, boulder }: { app: App; boulder: Boulder }) => (
  <Root>
    <Section>Sector</Section>
    <SectorPicker sectors={[boulder.sector]} onChange={() => {}} />

    <Section>Setters</Section>
    <div>
      {boulder.setter.map((setterId, index) => (
        <Link key={index} href={{ pathname: "/account", query: { id: setterId } }}>
          <BoulderSetterCard app={app} setterId={setterId} onClick={() => {}} />
        </Link>
      ))}
    </div>
  </Root>
);

// ----------------------------------------------------------------------------

const Root = styled.div`
  max-width: 400px;
`;

const Section = styled.div`
  ${useTypeface(copy16Bold)};
  color: ${text};

  padding: 40px 0 12px;
  &:first-of-type {
    padding: 0 0 12px;
  }
`;
