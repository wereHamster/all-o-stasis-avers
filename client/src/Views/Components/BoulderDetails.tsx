import * as React from 'react'
import * as moment from 'moment'
import styled from 'styled-components'

import {Boulder} from '../../storage'

export const BoulderDetails = ({boulder}: {boulder: Boulder}) => (
    <Root>
        <div><strong>Name:</strong> {boulder.name}</div>
        <div><strong>Sector:</strong> {boulder.sector}</div>
        <div><strong>Grade:</strong> {boulder.gradeNr} {boulder.grade}</div>
        <div><strong>Date:</strong> {moment.unix(boulder.setDate / 1000).toISOString()}</div>
    </Root>
)


// ----------------------------------------------------------------------------

const Root = styled.div`
    padding-top: 80px;
    display: flex;
    flex-direction: column;
`
