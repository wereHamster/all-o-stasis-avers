import * as React from "react";
import styled from "styled-components";

import { App } from "../app";

import { BoulderSetterCard } from "../Views/Components/BoulderSetterCard";

export interface SetterPickerProps {
  app: App;
  dismiss(): void;
  addSetter(accountId: string): void;
}

export class SetterPicker extends React.Component<SetterPickerProps> {
  onKeyPress = ev => {
    if (ev.key === "Escape") {
      this.props.dismiss();
    }
  };

  componentDidMount() {
    window.addEventListener("keyup", this.onKeyPress);
  }
  componentWillUnmount() {
    window.removeEventListener("keyup", this.onKeyPress);
  }

  render() {
    const { app, dismiss, addSetter } = this.props;

    const setters = app.data.adminAccountCollection.ids.get<string[]>([]).map((accountId, index) => {
      return (
        <div key={index} style={{ marginRight: 8 }}>
          <BoulderSetterCard
            app={app}
            setterId={accountId}
            onClick={() => {
              addSetter(accountId);
            }}
          />
        </div>
      );
    });

    return (
      <Root>
        <div style={{ position: "fixed", top: 20, right: 20 }} onClick={dismiss}>
          <svg width="40" height="40">
            <path d="M10 10 L30 30 M30 10 L10 30" stroke="black" strokeWidth="7" strokeLinecap="round" />
          </svg>
        </div>

        <div style={{ textAlign: "center", margin: 30, fontSize: 32 }}>Pick a setter</div>

        <GridContainer>
          <Grid>{setters}</Grid>
        </GridContainer>
      </Root>
    );
  }
}

const Root = styled.div`
  position: fixed;
  z-index: 10;
  top: 0;
  right: 0;
  bottom: 0;
  left: 0;
  background: #f1f1f1;
  display: flex;
  flex-direction: column;
`;

const GridContainer = styled.div`
  flex-grow: 1;
  overflow-y: auto;
  padding: 24px;
`;

const Grid = styled.div`
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(280px, 1fr));
  grid-gap: 12px;
  justify-items: stretch;
`;
