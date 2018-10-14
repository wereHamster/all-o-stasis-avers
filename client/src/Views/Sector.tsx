import * as React from 'react'
import styled from 'styled-components'

import {role, removeBoulders, activeBoulders, sectorBoulders} from '../actions'
import {App} from '../app'
import {sectors, prettyPrintSector} from '../storage'

import {useTypeface, heading28} from '../Materials/Typefaces'

import {GradeBalance} from './Components/GradeBalance'
import {Site} from './Components/Site'

export default function
sectorView({ app }: { app: App }) {
    if (role(app) === 'user') {
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
    app: App
}

interface SectorState {
    sectorName: string
}

class Sector extends React.Component<SectorProps, SectorState> {
    constructor(props) {
        super(props)
        this.state = {sectorName: sectors[0]}
    }

    removeAllBoulders = () => {
        removeBoulders(this.props.app, activeBoulders(this.props.app))
    }

    removeAllSectorBoulders = () => {
        removeBoulders(this.props.app, sectorBoulders(this.props.app, this.state.sectorName))
    }

    onChange = (e: React.FormEvent<any>) => {
        const sectorName = (e.target as HTMLSelectElement).value
        this.setState({sectorName})
    }

    render() {
        const boulders = sectorBoulders(this.props.app, this.state.sectorName)
        const actBoulders = activeBoulders(this.props.app)
        return (
            <Views>
                <SectorView>
                <TotalHeader numBoulders={actBoulders.length} removeAllBoulders={this.removeAllBoulders} />
                <Stats>
                    <GradeBalance boulders={actBoulders} height={300} width={300} />
                </Stats>
                </SectorView>

                <SectorView>
                <SectorHeader sectorNames={sectors} numBoulders={boulders.length} sectorName={this.state.sectorName} onChange={this.onChange} removeAllBoulders={this.removeAllSectorBoulders} />
                <Stats>
                    <GradeBalance boulders={boulders} height={300} width={300} />
                </Stats>
                </SectorView>
            </Views>
        )
    }
}



// ----------------------------------------------------------------------------
// SectorHeader

interface TotalHeaderProps {
    numBoulders: number

    removeAllBoulders(): void
}

interface SectorHeaderProps {
    sectorNames: string[]
    numBoulders: number
    sectorName: string

    onChange(e: React.FormEvent<any>): void
    removeAllBoulders(): void
}

const TotalHeader = ({numBoulders, removeAllBoulders}: TotalHeaderProps) => (
    <Header>
        <Name>Total ({numBoulders})</Name>
        <Actions>
            <div className='form'>
                <div className='form-row'>
                    <div className='label'>
                        <div className='button' onClick={removeAllBoulders}>remove all</div>
                    </div>
                </div>
            </div>
        </Actions>
    </Header>
)

const SectorHeader = ({sectorNames, numBoulders, sectorName, onChange, removeAllBoulders}: SectorHeaderProps) => (
    <Header>
        <Name>{prettyPrintSector(sectorName)} ({numBoulders})</Name>
        <Actions>
            <div className='form'>
                <div className='form-row'>
                    <div className='label'>
                        <select defaultValue={sectorName} onChange={onChange}>
                            {sectorNames.map((entry, index) => (
                                <option value={entry} key={index}>{prettyPrintSector(entry)}</option>
                            ))}
                        </select>
                    </div>
                </div>
                <div className='form-row'>
                    <div className='label'>
                        <div className='button' onClick={removeAllBoulders}>remove all</div>
                    </div>
                </div>
            </div>
        </Actions>
    </Header>
)


// ----------------------------------------------------------------------------
const Header = styled.div`
`

const Views = styled.div`
    display: flex;
    flex-wrap: wrap;
    flex-direction: row;
    align-items: center;
    justify-content: center;
`

const SectorView = styled.div`
    .flex > * {
        padding-left: 5rem;
    }
    .flex > :first-child {
        padding-left: 0;
    }
`

const Name = styled.div`
    ${useTypeface(heading28)}
    text-align: center;
    margin: 4rem 0 2rem;
    text-transform: uppercase;
`

const Actions = styled.div`
    padding-top: 20px;
    display: flex;
    flex-direction: column;
    align-items: center;

    .button {
        width: 8rem;
        font-size: 1.0rem;
        padding: .7rem 1.1rem;
        font-weight: bold;
        background-color: rgb(55, 119, 110);
        color: rgb(245, 238, 249);
        margin-top: 1rem;
    }
`

const Stats = styled.div`
    display: flex;
    align-items: center;
    justify-content: center;
    margin-top: 6rem;
`
