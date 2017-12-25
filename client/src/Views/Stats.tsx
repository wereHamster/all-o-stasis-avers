import * as Avers from 'avers'
import * as React from 'react'
import styled from 'styled-components'

import {text, darkGrey, lightGrey} from '../Materials/Colors'
import {useTypeface, heading18, copy16, copy16Bold, copy14} from '../Materials/Typefaces'

import {Site} from './Components/Site'
import {SectorPicker} from './Components/SectorPicker'


export const statsView = (app) => (
    <Site app={app}>
        <div style={{margin: '20px 24px', display: 'flex'}}>
            <div style={{flexBasis: '400px'}}>
                <Section>Sector</Section>
                <SectorPicker sector='bigboss' onChange={() => {}} />

                <Section>Setter</Section>
            </div>
            <div style={{marginLeft: 40}}>
                <div>
                    <Section>Grades</Section>
                    <div>
                        Grades
                    </div>
                </div>
            </div>
        </div>
    </Site>
)

// ----------------------------------------------------------------------------

const Section = styled.div`
${useTypeface(copy16Bold)}
color: ${text};

padding: 40px 0 12px;
&:first-of-type {
    padding: 0 0 12px;
}
`
