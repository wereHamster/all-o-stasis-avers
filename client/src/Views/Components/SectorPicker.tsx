import * as Avers from 'avers'
import * as React from 'react'
import styled from 'styled-components'

import {accountGravatarUrl} from '../Account'
import {App, navigateTo} from '../../app'
import {Account, Boulder, prettyPrintSector} from '../../storage'

import {lightGrey, darkGrey, text, gradeBackgroundColor, gradeBorderColor, gradeColor} from '../../Materials/Colors'
import {useTypeface, copy16} from '../../Materials/Typefaces'

import SectorPickerSVG from '../../../assets/SectorPicker.svg'

export interface SectorPickerProps {
    sector: string
    onChange(sector: string): void
}

export class SectorPicker extends React.Component<SectorPickerProps> {
    ref: null | HTMLElement = null
    refFn = (ref: null | HTMLElement): void => {
        this.ref = ref
    }

    componentDidMount() {
        if (this.ref) {
            this.ref.addEventListener('click', ev => {
                const target: HTMLElement = ev.target as any
                this.props.onChange(target.parentElement.id)
            })
        }
    }

    render() {
        return (
            <div ref={this.refFn}>
                <Root sector={this.props.sector}>
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

& svg #sectors g[id="${({sector}: any) => sector}"] > use {
    fill: red;
}

& svg #sectors > g > use {
    cursor: pointer;
    fill: #1E2A4C;
    transition: fill .12s;
}
& svg #sectors > g > use:hover {
    fill: red;
}
`
