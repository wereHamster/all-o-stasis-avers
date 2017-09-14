/*
module Sector
( sectorView
) where
*/

import * as Avers from 'avers';
import * as React from 'react';

import {role, sectorBoulders} from '../actions';
import {App, refresh} from '../app';
import {Boulder, grades, sectors, prettyPrintSector} from '../storage';

import {Site} from './Components/Site';

export interface SectorViewProps {
    app: App;
}

interface SectorViewState {
    sectorName: string;
    boulders: Avers.Editable<Boulder>[];
}

class SectorSpec extends React.Component<SectorViewProps, SectorViewState> {
    initialState(props) {
        const defaultSector = sectors()[0];
        return { sectorName : defaultSector, 
            boulders: sectorBoulders(this.props.app, defaultSector)};
    }

    constructor(props) {
        super(props);
        this.state = this.initialState(props);
    }

    renderSectorBoulders() {
        const {boulders} = this.state.boulders.reduce(({boulders}, boulder) => {
            return {
                boulders: boulders.concat([
                    <div className={`boulder-card-id ${boulder.content.grade}`} key={boulder.objectId}>
                        {boulder.content.gradeNr}
                    </div>
                ])
            };
        }, { boulders: [] });

        return (
            <div className="boulders">{boulders}</div>
        );
    }

    renderGradeDistribution() {
        // TODO: render grade distribution for this sector
        // https://hackernoon.com/how-and-why-to-use-d3-with-react-d239eb1ea274
    }

    removeAllBoulders() {
        // remove all boulders on the currently active sector
        const now = Date.now();
        this.state.boulders.map(boulder => boulder.content.removed = now.valueOf());
        Avers.resetObjectCollection(this.props.app.data.activeBouldersCollection);
        refresh(this.props.app);
    }

    onChange = (e: React.FormEvent<any>) => {
        let value = (e.target as HTMLSelectElement).value;
        this.setState({ sectorName: value, boulders: sectorBoulders(this.props.app, value) });
    };

    render() : JSX.Element {
        return (
          <div>
              {this.sectorHeader()}
              {this.renderSectorBoulders()}
          </div>
        );
    }

    sectorHeader() : JSX.Element {
        let options = sectors().map( (entry, index) => {
            return (<option value={entry} key={index}>{prettyPrintSector(entry)}</option>);
        });

        return (
        <div>
          <div className="header">
            <div className="logo">{prettyPrintSector(this.state.sectorName)}</div>
          </div>
          <div className='details'>
            <div className='form'>
              <div className='form-row'>
                  <div className="label">
                    <select defaultValue={this.state.sectorName}
                        onChange={this.onChange}>{options}}</select>
                  </div>
              </div>
              <div className='form-row'>
                  <div className="label">
                    <div className="button" onClick={this.removeAllBoulders}>remove all</div>
                  </div>
              </div>
            </div>
          </div>
        </div>
        );
    }
}

var SectorView = React.createFactory(SectorSpec);

export function
sectorView(app: App) : JSX.Element {
    if (role(app) == "user")
        return (<Site app={app}></Site>);

    return (
      <Site app={app}>
        <div className="sector">
          {SectorView({ app: app } )}
        </div>
      </Site>
    );
}
