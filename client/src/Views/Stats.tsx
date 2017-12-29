import * as Avers from 'avers'
import * as React from 'react'
import styled from 'styled-components'
import VegaLite from 'react-vega-lite'

import {Account, BoulderStat, boulderStats} from '../storage'
import {App} from '../app'
import {accountGravatarUrl} from './Account'

import {text, darkGrey, lightGrey} from '../Materials/Colors'
import {yellow100, green100, orange100, blue100, red100} from '../Materials/Colors'
import {useTypeface, heading18, copy16, copy16Bold, copy14} from '../Materials/Typefaces'

import {Site} from './Components/Site'
import {SectorPicker} from './Components/SectorPicker'
import {SectorSelector} from './Components/Stats/SectorSelector'

export const statsView = (app: App) => (
    <Site app={app}>
        <div style={{margin: '20px 24px', display: 'flex', flex: 1}}>
            <div style={{flexBasis: '400px', width: '400px'}}>
                <SectorSelector
                    sectors={['bigboss']}
                    clear={() => {}}
                    toggle={() => {}}
                />

                <Section>Setter</Section>
                <SettersPicker app={app} />
            </div>
            <div style={{marginLeft: 80, flex: 1, display: 'flex', flexDirection: 'column'}}>
                <Section>
                    Boulders

                    <div style={{display: 'inline', marginLeft: 20}}>
                        <DisplaySelectButton style={{color: text}}>
                            total
                        </DisplaySelectButton>
                        <DisplaySelectButton>
                            by grade
                        </DisplaySelectButton>
                        <DisplaySelectButton>
                            by sector
                        </DisplaySelectButton>
                        <DisplaySelectButton>
                            by setter
                        </DisplaySelectButton>
                    </div>
                </Section>

                <GradeVisContainer aversH={app.data.aversH} />
            </div>
        </div>
    </Site>
)

// Replace with react-measure
const Measure = ({children}) => (
    children({ref: null})
)

const GradeVisContainer = ({aversH}) => {
    return (
        <Measure bounds>
            {({ref}) => (
                <div ref={ref} style={{height: 345, display: 'flex', justifyContent: 'center', alignItems: 'center'}}>
                    {Avers.staticValue(aversH, boulderStats(aversH))
                        .fmap(bss => <GradesVis width={1050} height={300} bss={bss} />)
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
                axis: {domain: false, format: '%Y %m', labelAngle: 0, tickSize: 0},
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

const SettersPicker = ({app}: {app: App}) => {
    const setters = app.data.accountsCollection.ids.get([]).map(accountId => {
        const accountC = Avers.lookupContent<Account>(app.data.aversH, accountId)
        return accountC.fmap(account =>
            <Setter key={accountId} app={app} accountId={accountId} account={account} />)
        .get(<Setter key={accountId} app={app} accountId={accountId} account={undefined} />)
    })

    return (
        <div>
            {setters}
        </div>
    )
}

const placeholderImageSrc = 'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAoAAAAKCAAAAACoWZBhAAAAF0lEQVQI12P4BAI/QICBFCaYBPNJYQIAkUZftTbC4sIAAAAASUVORK5CYII='
const Setter = ({accountId, account}) => (
    <SetterC>
        <SetterImage src={account ? accountGravatarUrl(account.email) : placeholderImageSrc} />
        <SetterName>{(account && account.name !== '') ? account.name : accountId.slice(0, 7)}</SetterName>
    </SetterC>
)

const SetterC = styled.div`
display: flex;
align-items: center;
margin: 4px 0;
padding: 0 0 0 2px;
border-left: 4px solid transparent;
cursor: pointer;
user-select: none;

transition all .2s;

&:hover {
    border-left-color: ${text};
}
`

const SetterImage = styled.img`
display: block;
width: 24px;
height: 24px;
margin-right: 8px;
border-radius: 2px;
`

const SetterName = styled.div`
${useTypeface(copy14)}
color: ${text};
`

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
