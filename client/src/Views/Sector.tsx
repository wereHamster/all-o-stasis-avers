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


export function
sectorView(app: App) {
    if (role(app) === "user") {
        return (
            <Site app={app} />
        )
    } else {
        return (
            <Site app={app}>
                <Sector app={app} />
            </Site>
        )
    }
}



// ----------------------------------------------------------------------------
// Sector

export interface SectorProps {
    app: App;
}

interface SectorState {
    sectorName: string;
}

class Sector extends React.Component<SectorProps, SectorState> {
    constructor(props) {
        super(props)
        this.state = {sectorName: sectors()[0]}
    }

    renderGradeDistribution() {
        // TODO: render grade distribution for this sector
        // https://hackernoon.com/how-and-why-to-use-d3-with-react-d239eb1ea274
    }

    removeAllBoulders = () => {
        // remove all boulders on the currently active sector
        const now = Date.now();
        sectorBoulders(this.props.app, this.state.sectorName).forEach(boulder => {
            boulder.content.removed = now.valueOf()
        });
        Avers.resetObjectCollection(this.props.app.data.activeBouldersCollection);
        refresh(this.props.app);
    }

    onChange = (e: React.FormEvent<any>) => {
        const sectorName = (e.target as HTMLSelectElement).value;
        this.setState({sectorName});
    };

    render() {
        const boulders = sectorBoulders(this.props.app, this.state.sectorName)
        return (
            <div className="sector">
                <SectorHeader sectors={sectors()} sectorName={this.state.sectorName} onChange={this.onChange} removeAllBoulders={this.removeAllBoulders} />
                <SectorBoulders boulders={boulders} />
            </div>
        );
    }
}



// ----------------------------------------------------------------------------
// SectorHeader

interface SectorHeaderProps {
    sectors: string[]
    sectorName: string

    onChange(e: React.FormEvent<any>): void
    removeAllBoulders(): void
}

const SectorHeader = ({sectors, sectorName, onChange, removeAllBoulders}: SectorHeaderProps) => (
    <div>
        <div className="header">
            <div className="logo">{prettyPrintSector(sectorName)}</div>
        </div>
        <div className='details'>
            <div className='form'>
            <div className='form-row'>
                <div className="label">
                    <select defaultValue={sectorName} onChange={onChange}>
                        {sectors.map((entry, index) => (
                            <option value={entry} key={index}>{prettyPrintSector(entry)}</option>
                        ))}
                    </select>
                </div>
            </div>
            <div className='form-row'>
                <div className="label">
                    <div className="button" onClick={removeAllBoulders}>remove all</div>
                </div>
            </div>
            </div>
        </div>
    </div>
)



// ----------------------------------------------------------------------------
// SectorBoulders

interface SectorBouldersProps {
    boulders: Avers.Editable<Boulder>[]
}

const SectorBoulders = ({boulders}: SectorBouldersProps) => (
    <div className="boulders">
        {boulders.map(boulder => (
            <div className={`boulder-card-id ${boulder.content.grade}`} key={boulder.objectId}>
                {boulder.content.gradeNr}
            </div>
        ))}
    </div>
)
