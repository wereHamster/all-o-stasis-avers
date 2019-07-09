import * as Avers from 'avers'
import * as React from 'react'
import Loadable from "react-loadable";

import {Boulder, grades} from '../../storage'

import {yellow100, green100, orange100, blue100, red100} from '../../Materials/Colors'

export interface GradeBalanceProps {
    boulders: Array<Avers.Editable<Boulder>>
    height: number
    width: number
}

const AsyncVegaLite: React.ComponentType<any> = Loadable({
    loader: () => import("react-vega-lite"),
    render: (VegaLite, props) => <VegaLite.default {...props} />,
    loading: () => <div />
})

export class GradeBalance extends React.Component<GradeBalanceProps, {}> {
    spec = {
        description: 'Boulder grade distribution for a sector.',
        width: this.props.width,
        height: this.props.height,
        mark: {type: 'bar', style: 'bar' },
        encoding: {
            x: {field: 'grade', type: 'nominal', sort: null},
            y: {field: 'count', type: 'quantitative'},
            color: {field: 'grade', type: 'nominal',
                    scale: {range: ['#000000', blue100, green100, orange100, red100, '#FFFFFF', yellow100]},
                   },
        },
        config: {
            style: {
                bar: {
                    stroke: '#000000',
                    strokeWidth: 0.5,
                },
            },
        },
    }

    prepareData() {
        const data: {values: Array<{grade: string, count: number}>} = { values: [] }
        grades.forEach( gradeName => {
            data.values.push({grade: gradeName, count: 0})
        })
        this.props.boulders.map( boulder => {
            data.values[grades.indexOf(boulder.content.grade)].count++
        })
        return data
    }

    render(): JSX.Element {
        return (
            <AsyncVegaLite spec={this.spec} data={this.prepareData()} />
        )
    }
}
