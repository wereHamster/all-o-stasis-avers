import * as React from 'react'

import {markdown, ReactSpecimen} from 'catalog'
import {SectorPicker} from './SectorPicker'

export default () => markdown`
${<ReactSpecimen noSource>
    <SectorPicker />
</ReactSpecimen>}
`
