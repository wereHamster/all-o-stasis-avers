import * as React from 'react'
import styled from 'styled-components'

import SectorPickerSVG from '../../../static/SectorPicker.svg'

export interface SectorPickerProps {
    sectors: string[]
    onChange(sector: string): void
}

export class SectorPicker extends React.Component<SectorPickerProps> {
    ref: null | HTMLElement = null
    refFn = (ref: null | HTMLElement): void => {
        this.ref = ref
    }

    componentDidMount() {
        if (this.ref) {
            Array.from(this.ref.querySelectorAll('#sectors > rect')).forEach(el => {
                el.addEventListener('click', () => {
                    this.props.onChange(el.id)
                })
            })
        }
    }

    render() {
        return (
            <div ref={this.refFn}>
                <Root sectors={this.props.sectors}>
                    <SectorPickerSVG />
                </Root>
            </div>
        )
    }
}


const Root: any = styled.div`
& svg {
    display: block;
    width: 100%;
    height: 100%;
}

${({sectors}: {sectors: string[]}) => sectors.length === 0 ? '& .ignore' : sectors.map(s => `& svg #sectors path[id="${s}"]`).join(', ')} {
    fill: red;
}

& svg #sectors > g > path {
    cursor: pointer;
    fill: #1E2A4C;
    transition: fill .12s;
}
& svg #sectors > g > path:hover {
    fill: red;
}
`
