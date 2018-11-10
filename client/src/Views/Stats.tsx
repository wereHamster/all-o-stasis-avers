import * as Avers from "avers";
import * as React from "react";
import styled from "styled-components";

import { boulderStats } from "../storage";
import { App } from "../app";

import { Site } from "./Components/Site";
import { SectorSelector } from "./Components/Stats/SectorSelector";
import { SetterSelector } from "./Components/Stats/SetterSelector";
import { Visualization } from "./Components/Stats/Visualization";

interface StatsPageProps {
  app: App;
}

interface StatsPageState {
  sectors: string[];
  selectedSetters: string[]; // ObjId[]
}

export default class extends React.Component<StatsPageProps, StatsPageState> {
  state: StatsPageState = {
    sectors: [],
    selectedSetters: []
  };

  clearSectors = (): void => {
    this.setState({ sectors: [] });
  };
  toggleSector = (sector: string): void => {
    if (this.state.sectors.indexOf(sector) === -1) {
      this.setState({ sectors: [sector].concat(this.state.sectors) });
    } else {
      this.setState({ sectors: this.state.sectors.filter(x => x !== sector) });
    }
  };

  clearSetters = (): void => {
    this.setState({ selectedSetters: [] });
  };
  toggleSetter = (setterId: string): void => {
    if (this.state.selectedSetters.indexOf(setterId) === -1) {
      this.setState({ selectedSetters: [setterId].concat(this.state.selectedSetters) });
    } else {
      this.setState({ selectedSetters: this.state.selectedSetters.filter(x => x !== setterId) });
    }
  };

  render() {
    const {
      app,
      app: {
        data: { aversH }
      }
    } = this.props;
    const { sectors, selectedSetters } = this.state;

    const toEvents = bss =>
      bss
        .map(bs =>
          bs.removedOn === undefined
            ? [{ bs, type: "set", date: bs.setOn, setters: bs.setters, sector: bs.sector, grade: bs.grade }]
            : [
                { bs, type: "set", date: bs.setOn, setters: bs.setters, sector: bs.sector, grade: bs.grade },
                { bs, type: "removed", date: bs.removedOn, setters: bs.setters, sector: bs.sector, grade: bs.grade }
              ]
        )
        .reduce((a, x) => a.concat(x), [])
        .sort((a, b) => +a.date - +b.date)
        .filter(a => a.date.getTime() > 10000);

    const bssC = Avers.staticValue(aversH, boulderStats(aversH)).fmap(toEvents);

    return (
      <Site app={app}>
        <Root>
          <Side>
            <SideContent>
              <SectorSelector
                sectors={sectors}
                clear={sectors.length === 0 ? undefined : this.clearSectors}
                toggle={this.toggleSector}
              />

              <SetterSelector
                app={app}
                selectedSetters={selectedSetters}
                clear={selectedSetters.length === 0 ? undefined : this.clearSetters}
                toggle={this.toggleSetter}
              />
            </SideContent>
          </Side>
          <Main>
            <Visualization bssC={bssC} sectors={sectors} selectedSetters={selectedSetters} />
          </Main>
        </Root>
      </Site>
    );
  }
}

const Root = styled.div`
  flex: 1;
  padding: 20px 24px;
  display: flex;
`;

const Side = styled.div`
  flex: 0 0 300px;
  position: relative;
`;

const SideContent = styled.div`
  position: absolute;
  top: 0;
  right: 0;
  bottom: 0;
  left: 0;
  display: flex;
  flex-direction: column;
`;

const Main = styled.div`
  margin-left: 80px;
  flex: 1;
  display: flex;
  flex-direction: column;
`;
