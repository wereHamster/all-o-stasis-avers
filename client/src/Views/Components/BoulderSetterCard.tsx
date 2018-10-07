import * as Avers from "avers";
import * as React from "react";
import styled from "styled-components";

import { App } from "../../app";
import { PublicProfile, publicProfile } from "../../storage";

import { text } from "../../Materials/Colors";
import { useTypeface, copy16 } from "../../Materials/Typefaces";

export class BoulderSetterCard extends React.Component<{ app: App; setterId: string; onClick: (setterId: string) => void }> {
  onClick = () => {
    this.props.onClick(this.props.setterId);
  };

  render() {
    const { app, setterId } = this.props;

    return Avers.staticValue<PublicProfile>(app.data.aversH, publicProfile(app.data.aversH, setterId))
      .fmap(({ name, avatar }) => (
        <SetterContainer onClick={this.onClick}>
          <SetterImage src={avatar} />
          <SetterName>{name !== "" ? name : setterId.slice(0, 2)}</SetterName>
        </SetterContainer>
      ))
      .get(<div />);
  }
}

const SetterContainer = styled.div`
  pointer: cursor;
  background: white;
  box-shadow: 0 0 4px 0 rgba(0, 0, 0, 0.1);
  transition: box-shadow 0.16s;
  display: flex;
  margin-bottom: 8px;

  &:hover {
    box-shadow: 0 0 8px 0 rgba(0, 0, 0, 0.2);
  }
`;

const SetterImage = styled.img`
  display: block;
  width: 80px;
  flex: 0 0 80px;
  align-self: stretch;
  object-fit: cover;
`;

const SetterName = styled.div`
  ${useTypeface(copy16)};
  color: ${text};
  text-align: center;
  margin: 10px 24px 24px 10px;
`;
