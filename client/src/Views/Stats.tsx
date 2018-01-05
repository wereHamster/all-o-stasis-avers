import * as Avers from 'avers'
import * as React from 'react'
import styled from 'styled-components'
import Measure from 'react-measure'
import {Motion, spring} from 'react-motion'

import {scaleTime, scaleLinear, scaleOrdinal} from 'd3-scale'
import {stack, area, line, curveLinear} from 'd3-shape'
import {select} from 'd3-selection'
import {axisBottom} from 'd3-axis'

import {BoulderStat, boulderStats, grades} from '../storage'
import {App} from '../app'

import {text, lightGrey} from '../Materials/Colors'
import {yellow100, green100, orange100, blue100, red100} from '../Materials/Colors'
import {useTypeface, copy16Bold, copy14} from '../Materials/Typefaces'

import {Site} from './Components/Site'
import {SectorSelector} from './Components/Stats/SectorSelector'
import {SetterSelector} from './Components/Stats/SetterSelector'



const curve = curveLinear



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

const matchSector = sectors => sectors.length === 0
    ? () => true
    : (bs: BoulderStat) => sectors.indexOf(bs.sector) !== -1

const matchSetter = selectedSetters => selectedSetters.length === 0
    ? () => true
    : (bs: BoulderStat) => bs.setters.some(setterId => selectedSetters.indexOf(setterId) !== -1)

const Visualization = ({bssC, sectors, selectedSetters}) => (
    <Measure bounds>
        {({measureRef, contentRect}) => (
            <div ref={measureRef} style={{flex: 1}}>
                {contentRect.bounds && <VisualizationRenderer bssC={bssC} bounds={contentRect.bounds} sectors={sectors} selectedSetters={selectedSetters} />}
            </div>
        )}
    </Measure>
)

const VisualizationRenderer = ({bssC, sectors, selectedSetters, bounds}) => {
    const events = bssC.get([])

    const values = (() => {
        const res = events.reduce((a, ev) => {
            if (matchSector(sectors)(ev.bs) && matchSetter(selectedSetters)(ev.bs)) {
                if (ev.grade in a.acc) {
                    a.acc[ev.grade] = Math.max(0, a.acc[ev.grade] + (ev.type === 'set' ? 1 : -1))
                } else if (ev.type === 'set') {
                    a.acc[ev.grade] = 1
                }
            }

            if (+a.date === +ev.date) {
                return a
            } else {
                return {
                    values: a.values.concat([{
                        date: a.date,
                        ...Object.keys(a.acc).reduce((o, k) => ({ ...o, [k]: a.acc[k] }), {}),
                    }]),
                    date: ev.date,
                    acc: a.acc,
                }
            }
        }, {
            values: [] as any,
            date: undefined as any,
            acc: grades().reduce((o, grade) => ({...o, [grade]: 0}), {}),
        })

        return res.values.concat([{
            date: res.date,
            ...Object.keys(res.acc).reduce((o, k) => ({ ...o, [k]: res.acc[k] }), {}),
        }]).filter(x => x.date !== undefined)
    })()

    const xScale = scaleTime().range([0, bounds.width])
    const yScale = scaleLinear().range([400, 0])
    const colorScale = scaleOrdinal([yellow100, green100, orange100, blue100, red100, '#FFFFFF'])

    const skeys = grades()
    const s = stack()
        .keys(skeys)
        .value(function(d, key) { return d[key] || 0 })

    colorScale.domain(skeys)

    const a = area()
        .curve(curve)
        .x(function(d) { return xScale(d.data.date) })
        .y0(function(d) { return yScale(d[0]) })
        .y1(function(d) { return yScale(d[1]) })

    const data = s(values)

    xScale.domain([
        new Date(Date.now() - 4 * 30 * 24 * 60 * 60 * 1000),
        new Date(Date.now()),
    ])
    yScale.domain([0, 30])

    return (
        <svg width={bounds.width} height={bounds.height} style={{display: 'block'}}>
            <g transform={`translate(0,80)`}>
                {data.map((d, i) => (
                    <Area
                        key={i}
                        index={i}
                        colorScale={colorScale}
                        data={d}
                        a={a}
                    />
                ))}

                <TotalLine xScale={xScale} yScale={yScale} data={data} />

                <g
                    transform={`translate(0,410)`}
                    ref={el => {
                        if (el) {
                            select(el).call(axisBottom(xScale))
                        }
                    }}
                />
            </g>
        </svg>
    )
}

const Area = ({index, colorScale, data, a}) => {
    if (data.length === 0) {
        return <g />
    }

    const defaultStyle = {}
    data.forEach((d, i) => {
        defaultStyle[`v0_${i}`] = d[0]
        defaultStyle[`v1_${i}`] = d[1]
    })

    const style = {}
    data.forEach((d, i) => {
        style[`v0_${i}`] = spring(d[0])
        style[`v1_${i}`] = spring(d[1])
    })

    return (
        <Motion defaultStyle={defaultStyle} style={style}>
            {interpolatingStyle => {
                const dt = data.map((d, i) => {
                    const x: any = [interpolatingStyle[`v0_${i}`], interpolatingStyle[`v1_${i}`]]
                    x.data = d.data
                    return x
                })

                return (
                    <path
                        fill={colorScale(grades()[index])}
                        d={a(dt)}
                    />
                )
            }}
        </Motion>
    )
}


const TotalLine = ({xScale, yScale, data}) => {
    const lineGenerator = line()
        .curve(curve)
        .x(d => xScale(d[0]))
        .y(d => yScale(d[1]))

    const total = data[data.length - 1]
    if (total.length === 0) {
        return <g />
    }

    const defaultStyle = {}
    total.forEach((d, i) => { defaultStyle[`v${i}`] = d[1] })

    const style = {}
    total.forEach((d, i) => { style[`v${i}`] = spring(d[1]) })

    return (
        <Motion defaultStyle={defaultStyle} style={style}>
            {interpolatingStyle => {
                const dt = total.map((d, i) => [d.data.date, interpolatingStyle[`v${i}`]])

                return (
                    <path
                        fill='none'
                        stroke='#222222'
                        strokeWidth={2}
                        d={lineGenerator(dt)}
                    />
                )
            }}
        </Motion>
    )
}
