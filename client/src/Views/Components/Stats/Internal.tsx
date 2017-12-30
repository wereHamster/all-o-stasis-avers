import * as React from 'react'
import styled from 'styled-components'

import {text, darkGrey, lightGrey} from '../../../Materials/Colors'
import {useTypeface, copy16Bold, copy14} from '../../../Materials/Typefaces'


export const Section = styled.div`
${useTypeface(copy16Bold)}
color: ${text};

padding: 80px 0 20px;
&:first-of-type {
    padding: 0 0 12px;
}
`

export const SectionLink = styled.span`
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
