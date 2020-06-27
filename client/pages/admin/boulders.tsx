import * as React from "react";
import styled from "styled-components";

import { boulderCompare, sectors, prettyPrintSector } from "../../src/storage";
import { App } from "../../src/app";
import { removeBoulders, activeBoulders, sectorBoulders } from "../../src/actions";

import { Site } from "../../src/Views/Components/Site";
import { BoulderId24 } from "../../src/Views/Components/BoulderId";
import * as MUI from "@material-ui/core";

interface Props {
  app: App;
}

interface State {
  sectorName: string;
}

export default class extends React.Component<Props, State> {
  state: State = {
      sectorName: sectors[0]
    }

  setSector = (sectorName: string): void => {
    this.setState({sectorName});
  };

  removeAllBoulders = (): void => {
      removeBoulders(this.props.app, activeBoulders(this.props.app))
  }

  removeAllSectorBoulders = (): void => {
      const boulders = sectorBoulders(this.props.app, this.state.sectorName)
      removeBoulders(this.props.app, boulders)
  }

  render() {
    const { app } = this.props;
    const { sectorName } = this.state;

    return (
      <Site>
        <Root>
          <table>
            <thead>
              <tr>
                <th style={{ width: 100 }}>Selection</th>
                <th style={{ width: 200 }}>Remove</th>
              </tr>
            </thead>
            <tbody>
              <tr>
                <td>all</td>
                <td>
                  <MUI.Button variant="contained" color="primary" onClick={() => this.removeAllBoulders()}>Remove all Boulders</MUI.Button>
                </td>
              </tr>
              <tr height="80px">
                <td colSpan="2"></td>
              </tr>
              <tr>
                <td>
                  <select id='sector_selection' defaultValue={sectorName} onChange={(e) => this.setSector(e.currentTarget.value)}>
                    {sectors.map((entry, index) => (
                      <option value={entry} key={index}>{prettyPrintSector(entry)}</option>
                    ))}
                  </select>
                </td>
                <td>
                  <MUI.Button variant="contained" color="primary" onClick={() => this.removeAllSectorBoulders()}>Remove Sector</MUI.Button>
                </td>
              </tr>
              {sectorBoulders(app, sectorName)
                .sort((a, b) => boulderCompare(a.content, b.content))
                .map(boulderE => {
                  return (<tr key={boulderE.objectId}>
                    <td><BoulderId24 grade={boulderE.content.grade}>{boulderE.content.gradeNr}</BoulderId24></td>
                    <td>
                      <MUI.Button variant="contained" color="primary" onClick={() => {
                        const now = Date.now()
                        boulderE.content.removed = now.valueOf()
                      }}>Remove Boulder</MUI.Button>
                  </td>
                </tr>
              )})}
            </tbody>
          </table>
        </Root>
      </Site>
    );
  }
}


const Root = styled.div`
  margin: 16px 24px;

  table {
    width: 100%;

    th {
      text-align: left;
    }
  }
`;
