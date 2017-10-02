import * as Avers from 'avers'
import * as React from 'react'
import VegaLite from 'react-vega-lite'

import {Boulder, grades} from '../../storage'


export interface GradeBalanceProps {
    boulders: Array<Avers.Editable<Boulder>>
    height: number
    width: number
}

export class GradeBalance extends React.Component<GradeBalanceProps, {}> {
    spec = {
        description: 'Boulder grade distribution for a sector.',
        width: this.props.width,
        height: this.props.height,
        mark: 'bar',
        encoding: {
            x: {field: 'grade', type: 'nominal', sort: null},
            y: {field: 'count', type: 'quantitative'},
        },
    }

    prepareData() {
        const data = { values: [] }
        grades().forEach( gradeName => {
            data.values.push({grade: gradeName, count: 0})
        })
        this.props.boulders.map( boulder => {
            data.values[grades().indexOf(boulder.content.grade)].count++
        })
        return data
    }

    render(): JSX.Element {
        return (
            <VegaLite spec={this.spec} data={this.prepareData()} />
        )
    }
}
