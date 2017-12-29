import * as React from 'react'
import styled from 'styled-components'

import {text, darkGrey, lightGrey} from '../../../Materials/Colors'
import {useTypeface, copy16Bold, copy14} from '../../../Materials/Typefaces'

import {SectorPicker} from '..//SectorPicker'


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

// ----------------------------------------------------------------------------

const Section = styled.div`
${useTypeface(copy16Bold)}
color: ${text};

padding: 80px 0 20px;
&:first-of-type {
    padding: 0 0 12px;
}
`

const SectionLink = styled.span`
${useTypeface(copy14)}
color: ${lightGrey};

margin-left: 6px;
cursor: pointer;
transition all .16s;

opacity: ${({onClick}) => onClick ? 1 : 0};

&:hover {
    color: ${text};
}
`
