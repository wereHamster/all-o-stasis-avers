import * as React from 'react'
import styled from 'styled-components'

import {text, darkGrey, lightGrey} from '../../../Materials/Colors'
import {useTypeface, copy16Bold, copy14} from '../../../Materials/Typefaces'

import {SectorPicker} from '../SectorPicker'
import {Section, SectionLink} from './Internal'


export interface SectorSelectorProps {
    sectors: string[]

    clear: undefined | (() => void)
    toggle(sector: string): void
}

export const SectorSelector = ({sectors, clear, toggle}: SectorSelectorProps) => (
    <div>
        <Section>
            Sector
            <SectionLink onClick={clear}>(reset)</SectionLink>
        </Section>

        <SectorPicker
            sectors={sectors}
            onChange={toggle}
        />
    </div>
)
