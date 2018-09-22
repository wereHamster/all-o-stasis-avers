import * as Avers from 'avers'
import * as React from 'react'

import {boulderStats} from '../storage'
import {App} from '../app'

import {Site} from './Components/Site'
import {SectorSelector} from './Components/Stats/SectorSelector'
import {SetterSelector} from './Components/Stats/SetterSelector'
import {Visualization} from './Components/Stats/Visualization'



export const statsView = (app: App) => (
    <StatsPage app={app} />
)

interface StatsPageProps {
    app: App
}

interface StatsPageState {
    sectors: string[]
    selectedSetters: string[] // ObjId[]
}

class StatsPage extends React.Component<StatsPageProps, StatsPageState> {
    state: StatsPageState = {
        sectors: [],
        selectedSetters: [],
    }

    clearSectors = (): void => {
        this.setState({sectors: []})
    }
    toggleSector = (sector: string): void => {
        if (this.state.sectors.indexOf(sector) === -1) {
            this.setState({sectors: [sector].concat(this.state.sectors)})
        } else {
            this.setState({sectors: this.state.sectors.filter(x => x !== sector)})
        }
    }

    clearSetters = (): void => {
        this.setState({selectedSetters: []})
    }
    toggleSetter = (setterId: string): void => {
        if (this.state.selectedSetters.indexOf(setterId) === -1) {
            this.setState({selectedSetters: [setterId].concat(this.state.selectedSetters)})
        } else {
            this.setState({selectedSetters: this.state.selectedSetters.filter(x => x !== setterId)})
        }
    }

    render() {
        const {app, app: {data: {aversH}}} = this.props
        const {sectors, selectedSetters} = this.state

        const toEvents = (bss) => bss.map(bs => bs.removedOn === undefined
                ? [
                    { bs, type: 'set', date: bs.setOn, setters: bs.setters, sector: bs.sector, grade: bs.grade },
                  ]
                : [
                    { bs, type: 'set', date: bs.setOn, setters: bs.setters, sector: bs.sector, grade: bs.grade },
                    { bs, type: 'removed', date: bs.removedOn, setters: bs.setters, sector: bs.sector, grade: bs.grade },
                  ],
            )
            .reduce((a, x) => a.concat(x), [])
            .sort((a, b) => +a.date - +b.date)
            .filter(a => a.date.getTime() > 10000)

        const bssC = Avers.staticValue(aversH, boulderStats(aversH)).fmap(toEvents)

        return (
            <Site app={app}>
                <div style={{margin: '20px 24px', display: 'flex', flex: 1}}>
                    <div style={{flexBasis: '400px', width: '400px'}}>
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
                    </div>
                    <div style={{marginLeft: 80, flex: 1, display: 'flex', flexDirection: 'column'}}>
                        <Visualization
                            bssC={bssC}

                            sectors={sectors}
                            selectedSetters={selectedSetters}
                        />
                    </div>
                </div>
            </Site>
        )
    }
}
