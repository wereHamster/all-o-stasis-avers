import * as Avers from 'avers'
import * as React from 'react'

import {markdown, ReactSpecimen} from 'catalog'

import {App, Data, infoTable} from '../../app'
import {Boulder} from '../../storage'

import {BoulderId} from './BoulderId'

export default () => markdown`
${<ReactSpecimen noSource span={1}>
    <BoulderId grade='yellow'>3</BoulderId>
</ReactSpecimen>}
${<ReactSpecimen noSource span={1}>
    <BoulderId grade='green'>5</BoulderId>
</ReactSpecimen>}
${<ReactSpecimen noSource span={1}>
    <BoulderId grade='orange'>11</BoulderId>
</ReactSpecimen>}
${<ReactSpecimen noSource span={1}>
    <BoulderId grade='blue'>47</BoulderId>
</ReactSpecimen>}
${<ReactSpecimen noSource span={1}>
    <BoulderId grade='red'>99</BoulderId>
</ReactSpecimen>}
${<ReactSpecimen noSource span={1}>
    <BoulderId grade='white'>26</BoulderId>
</ReactSpecimen>}
`
