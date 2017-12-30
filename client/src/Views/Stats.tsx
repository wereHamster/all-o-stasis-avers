import * as Avers from 'avers'
import * as React from 'react'
import styled from 'styled-components'
import VegaLite from 'react-vega-lite'
import Measure from 'react-measure'

import {scaleTime, scaleLinear, scaleOrdinal} from 'd3-scale'
import {stack, area, line, curveStepAfter} from 'd3-shape'
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
        const {app} = this.props
        const {sectors, selectedSetters} = this.state
        const aversH = app.data.aversH

        const matchSector = sectors.length === 0
            ? () => true
            : (bs: BoulderStat) => sectors.indexOf(bs.sector) !== -1

        const matchSetter = selectedSetters.length === 0
            ? () => true
            : (bs: BoulderStat) => bs.setters.some(setterId => selectedSetters.indexOf(setterId) !== -1)

        const bssC = Avers.staticValue(aversH, boulderStats(aversH)).fmap(bss =>
            bss.filter(bs => matchSector(bs) && matchSetter(bs)))

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
                        <Visualization bssC={bssC} />
                    </div>
                </div>
            </Site>
        )
    }
}

const Visualization = ({bssC}) => (
    <Measure bounds>
        {({measureRef, contentRect}) => (
            <div ref={measureRef} style={{flex: 1}}>
                {contentRect.bounds && <VisualizationRenderer bssC={bssC} bounds={contentRect.bounds} />}
            </div>
        )}
    </Measure>
)

const VisualizationRenderer = ({bssC, bounds}) => {
    const vs = bssC.get([])

    const values = (() => {
        const events = vs.map(bs => bs.removedOn === undefined
            ? [
                { type: 'set', date: bs.setOn, sector: bs.sector, grade: bs.grade },
              ]
            : [
                { type: 'set', date: bs.setOn, sector: bs.sector, grade: bs.grade },
                { type: 'removed', date: bs.removedOn, sector: bs.sector, grade: bs.grade },
              ],
        )
        .reduce((a, x) => a.concat(x), [])
        .sort((a, b) => +a.date - +b.date)
        .filter(a => a.date.getTime() > 10000)

        const res = events.reduce((a, ev) => {
            if (ev.grade in a.acc) {
                a.acc[ev.grade] = Math.max(0, a.acc[ev.grade] + (ev.type === 'set' ? 1 : -1))
            } else if (ev.type === 'set') {
                a.acc[ev.grade] = 1
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
        }, { values: [] as any, date: undefined as any, acc: {} as any })

        return res.values.concat([{
            date: res.date,
            ...Object.keys(res.acc).reduce((o, k) => ({ ...o, [k]: res.acc[k] }), {}),
        }]).filter(x => x.date !== undefined)
    })()

    var xScale = scaleTime().range([0, bounds.width])
    var yScale = scaleLinear().range([400, 0])
    var colorScale = scaleOrdinal([yellow100, green100, orange100, blue100, red100, '#FFFFFF'])

    const skeys = grades()
    var s = stack()
        .keys(skeys)
        .value(function(d, key) { return d[key] || 0 })

    colorScale.domain(skeys)

    var a = area()
        .curve(curveStepAfter)
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
                    <path
                        key={i}
                        fill={colorScale(d)}
                        d={a(d)}
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

const TotalLine = ({xScale, yScale, data}) => (
    <path
        fill='none'
        stroke='#222222'
        strokeWidth={2}
        d={line()
            .curve(curveStepAfter)
            .x(d => xScale(d.data.date))
            .y(d => yScale(d[1]))
            (data[data.length - 1])}
    />
)

const GradeVisContainer = ({bssC}) => {
    return (
        <Measure bounds>
            {({measureRef, contentRect}) => (
                <div ref={measureRef} style={{height: 345, display: 'flex', justifyContent: 'center', alignItems: 'center'}}>
                    {contentRect.bounds && bssC
                        .fmap(bss => <GradesVis width={contentRect.bounds.width - 200} height={300} bss={bss} />)
                        .get(<div>Loading…</div>)}
                </div>
            )}
        </Measure>
    )
}

const GradesVis = ({width, height, bss}: {width: number, height: number, bss: BoulderStat[]}) => {
    const spec = {
        description: 'Boulder grade distribution for a sector.',
        width,
        height,
        mark: 'area',
        encoding: {
            x: {
                field: 'date',
                type: 'temporal',
                timeUnit: 'yearmonthdate',
                scale: {nice: 'month'},
                axis: {domain: false, format: '%Y %m %d', labelAngle: 0, tickSize: 0},
            },

            y: {
                aggregate: 'sum',
                field: 'count',
                type: 'quantitative',
            },

            color: {
                field: 'grade',
                type: 'nominal',
                scale: {
                    range: [blue100, green100, orange100, red100, '#FFFFFF', yellow100],
                },
            },
        },
    }

    const events = bss.map(bs => bs.removedOn === undefined
        ? [
            { type: 'set', date: bs.setOn, sector: bs.sector, grade: bs.grade },
          ]
        : [
            { type: 'set', date: bs.setOn, sector: bs.sector, grade: bs.grade },
            { type: 'removed', date: bs.removedOn, sector: bs.sector, grade: bs.grade },
          ],
    )
    .reduce((a, x) => a.concat(x), [])
    .sort((a, b) => +a.date - +b.date)
    .filter(a => a.date.getTime() > 10000)

    const res = events.reduce((a, ev) => {
        if (ev.grade in a.acc) {
            a.acc[ev.grade] = Math.max(0, a.acc[ev.grade] + (ev.type === 'set' ? 1 : -1))
        } else if (ev.type === 'set') {
            a.acc[ev.grade] = 1
        }

        if (+a.date === +ev.date) {
            return a
        } else {
            return {
                values: a.values.concat(Object.keys(a.acc).map(k => ({
                    date: a.date,
                    grade: k,
                    count: a.acc[k],
                }))),
                date: ev.date,
                acc: a.acc,
            }
        }
    }, { values: [] as any, date: undefined as any, acc: {} as any })

    const values = res.values.concat(Object.keys(res.acc).map(k => ({
        date: res.date,
        grade: k,
        count: res.acc[k],
    })))

    return (
        <VegaLite spec={spec} data={{values}} />
    )
}

const AdditionsVisContainer = ({bssC}) => {
    return (
        <Measure bounds>
            {({measureRef, contentRect}) => (
                <div ref={measureRef} style={{height: 345, display: 'flex', justifyContent: 'center', alignItems: 'center'}}>
                    {contentRect.bounds && bssC
                        .fmap(bss => <AdditionsVis width={contentRect.bounds.width - 200} height={300} bss={bss} />)
                        .get(<div>Loading…</div>)}
                </div>
            )}
        </Measure>
    )
}

const AdditionsVis = ({width, height, bss}: {width: number, height: number, bss: BoulderStat[]}) => {
    const spec = {
        description: 'Newly set boulders',
        width,
        height,
        mark: 'bar',
        encoding: {
            x: {
                field: 'date',
                type: 'temporal',
                timeUnit: 'yearmonthdate',
                // scale: {nice: 'month'},
                axis: {domain: false, format: '%Y %m %d', labelAngle: 0, tickSize: 0},
            },

            y: {
                aggregate: 'count',
                field: '*',
                type: 'quantitative',
            },

            color: {
                field: 'grade',
                type: 'nominal',
                scale: {
                    range: [blue100, green100, orange100, red100, '#FFFFFF', yellow100],
                },
            },
        },
    }

    const values = bss.filter(bs => !bs.removedOn)
        .map(bs => ({date: bs.setOn, grade: bs.grade}))
        .sort((a, b) => +a.date - +b.date)
        .filter(a => a.date.getTime() > 10000)

    return (
        <VegaLite spec={spec} data={{values}} />
    )
}


// ----------------------------------------------------------------------------

const Section = styled.div`
${useTypeface(copy16Bold)}
color: ${text};

padding: 80px 0 20px;
&:first-of-type {
    padding: 0 0 12px;
}
`

const DisplaySelectButton = styled.span`
${useTypeface(copy14)}
color: ${lightGrey};

margin: 0 10px;
cursor: pointer;

transition: all .16s;

&:hover {
    color: ${text};
}
`
